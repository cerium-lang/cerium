/* body.c -- pass 4: fn bodies.
 *
 * Everything before this pass looked at declarations; this one walks
 * the statements. One environment serves the whole walk:
 *
 *   - types: every expression gets a type, every statement is checked
 *     against what it claims -- a let's annotation, a call's
 *     signature, a match's arms agreeing. A literal takes its type
 *     from the context when there is one (xyz has no literal
 *     suffixes, 15-grammar.md) and defaults to i32/f32 otherwise.
 *   - flow (03-move.md): the same environment is flow-sensitive. A
 *     binding is live or dead (moved), a place is free or borrowed
 *     (& freezes writes, &mut everything), and a ?T narrows to T
 *     inside a check that proved the value is there. Branches fork
 *     the environment and join conservatively: dead and borrowed
 *     survive a join, narrowing does not -- unless the other path
 *     already left.
 *
 * What this pass deliberately leaves alone: trait method calls and
 * operators on user types (dispatch, the next milestone -- the
 * scalar and pointer built-ins are checked here), const evaluation
 * (the one after), pack spreads, ranges, and type values ($$t,
 * reflection). Option and Result are checked as what they are --
 * prelude enums -- with their variants as constructors and their
 * narrowing spelled by the == None / != Err forms (01-types.md).
 */

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "check.h"
#include "die.h"
#include "lex.h"
#include "sym.h"
#include "type.h"

/* -- the flow environment ------------------------------------------------ */

enum
{
  FZ_NONE, /* not borrowed */
  FZ_SHR,  /* &x is live: no writes, no moves out; reads fine */
  FZ_MUT   /* &mut x is live: nothing may reach x (01-types.md) */
};

typedef struct Local Local;
struct Local
{
  char *name;    /* the binding's name */
  Type *ty;      /* its declared type, never narrowed away */
  int   mut;     /* let mut */
  int   dead;    /* moved from: unusable until its scope ends */
  int   frz;     /* FZ_*: what a live borrow forbids */
  int   frzby;   /* the borrowing binding's index, to thaw when it dies */
  char *frzpath; /* the borrowed field chain, ".a.b"; NULL is the root */
  Type *cur;     /* the narrowed type, ty until a check narrows it */
};

typedef struct Fenv Fenv;
struct Fenv
{
  Local *ls;       /* the bindings, innermost last */
  usize  n;        /* their count */
  Env    env;      /* the type-level names: Self, generics (sym.h) */
  int    loopd;    /* for's depth: break/continue, moves in a loop */
  usize  loopbase; /* bindings alive when the outermost loop began: a
                    * move of one of those repeats every round (03) */
  Type *fnret;     /* the enclosing fn's return, for return and ? */
};

static Local *
locfind(Fenv *fe, char *name)
{
  usize i;

  for (i = fe->n; i > 0; i--) /* innermost binding first */
    if (strcmp(fe->ls[i - 1].name, name) == 0)
      return &fe->ls[i - 1];
  return 0;
}

/* the binding's index, or fe->n when the name is not local: the loop
 * question -- did this binding begin outside every loop? -- is about
 * the index, not the pointer */
static usize
locfindi(Fenv *fe, char *name)
{
  usize i;

  for (i = fe->n; i > 0; i--)
    if (strcmp(fe->ls[i - 1].name, name) == 0)
      return i - 1;
  return fe->n;
}

/* one more binding -- lets are rare, the array is fresh each time */
static void
locpush(Fenv *fe, char *name, Type *ty, int mut)
{
  Local *ls = arenaalloc((fe->n + 1) * sizeof *ls);

  if (fe->n)
    memcpy(ls, fe->ls, fe->n * sizeof *ls);
  ls[fe->n].name = name;
  ls[fe->n].ty = ty;
  ls[fe->n].mut = mut;
  ls[fe->n].dead = 0;
  ls[fe->n].frz = FZ_NONE;
  ls[fe->n].frzby = -1;
  ls[fe->n].frzpath = 0;
  ls[fe->n].cur = ty;
  fe->ls = ls;
  fe->n++;
}

/* pop the bindings a block pushed -- and thaw what they borrowed:
 * a borrow lives as long as the binding that holds it (03-move.md) */
static void
locpop(Fenv *fe, usize nbase)
{
  usize i;

  for (i = nbase; i < fe->n; i++)
    if (fe->ls[i].frz != FZ_NONE) {
      /* the frozen one may sit below nbase (an outer binding) */
      usize j;

      for (j = 0; j < nbase; j++)
        if (fe->ls[j].frzby == (int) i) {
          fe->ls[j].frz = FZ_NONE;
          fe->ls[j].frzby = -1;
          fe->ls[j].frzpath = 0;
        }
    }
  fe->n = nbase;
}

/* a copy of the environment -- a branch fork */
static Fenv
fefork(Fenv *fe)
{
  Fenv r = *fe;

  if (fe->n) {
    r.ls = arenaalloc(fe->n * sizeof *r.ls);
    memcpy(r.ls, fe->ls, fe->n * sizeof *r.ls);
  }
  return r;
}

/* narrow a binding's current type: p != None proved the branch holds
 * the payload, so ?T reads as T until the branches join (01-types.md) */
static void
locnarrow(Fenv *fe, char *name, Type *t)
{
  Local *l = locfind(fe, name);

  if (l)
    l->cur = t;
}

/* -- copy and drop (03-move.md) ------------------------------------------ */

/* is a value of this type duplicated by an assignment rather than
 * moved? Typaram answers no: without its bounds resolved (dispatch),
 * move is the conservative reading, and a T that really is Copy gets
 * its bound checked when that pass arrives. */
static int
iscopy(Type *t)
{
  usize i;

  if (!t)
    return 0;
  switch (t->k) {
  case Tyunit:
  case Tybool:
  case Tyint:
  case Tyvoidptr:
  case Typtr:
  case Tyslice:
  case Tydyn:
    return 1;
  case Tymut: /* only under a slot; the answer is the child's */
    return iscopy(t->t);
  case Tytuple:
    for (i = 0; i < t->nargs; i++)
      if (!iscopy(t->args[i]))
        return 0;
    return 1;
  case Tyarray:
  case Tyenum:
    for (i = 0; i < t->nargs; i++)
      if (!iscopy(t->args[i]))
        return 0;
    return t->k == Tyarray ? iscopy(t->t) : 1;
  case Tystruct: {
    Sym *s = t->sym;

    for (i = 0; i < s->nfields; i++)
      if (!iscopy(s->fields[i].ty))
        return 0;
    return 1;
  }
  case Tyunion: /* a union forgets (03): always Copy */
    return 1;
  default: /* Typaram, Tytrait, Typroj, Tyfn, Tytype */
    return 0;
  }
}

/* -- diagnostics --------------------------------------------------------- */

static void
berr(Ast *a, const char *fmt, ...)
{
  va_list ap;

  fprintf(stderr, "%s:%u:%u: ", lexpath(), a->line, a->col);
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  fputc('\n', stderr);
  exit(1);
}

/* four rotating buffers, so one diagnostic can name several types */
static char *
btys(Type *t)
{
  static char     bufs[4][256];
  static unsigned which;

  return tysprint(bufs[which++ & 3u], sizeof bufs[0], t);
}

/* -- small type helpers --------------------------------------------------- */

/* the enum behind a ?T, or NULL */
static Type *
optchild(Type *t)
{
  if (t && t->k == Tyenum && t->sym == sym_option && t->nargs == 1)
    return t->args[0];
  return 0;
}

/* what an E?T holds when it went wrong, or NULL; *ok gets the payload */
static Type *
reschild(Type *t, Type **ok)
{
  if (t && t->k == Tyenum && t->sym == sym_result && t->nargs == 2) {
    *ok = t->args[0];
    return t->args[1];
  }
  return 0;
}

static int
isintty(Type *t)
{
  return t && t->k == Tyint && t->num != IN_F32 && t->num != IN_F64;
}

static int
isnumty(Type *t)
{
  return t && t->k == Tyint;
}

/* the value v fits the integer type t (resolve.c's rule, restated) */
static int
fitsv(u64 v, Type *t)
{
  switch (t->num) {
  case IN_I8:
    return v <= 0x7f;
  case IN_I16:
    return v <= 0x7fff;
  case IN_I32:
    return v <= 0x7fffffff;
  case IN_I64:
    return v <= ((u64) -1 >> 1);
  case IN_U8:
    return v <= 0xff;
  case IN_U16:
    return v <= 0xffff;
  case IN_U32:
    return v <= 0xffffffff;
  case IN_U64:
  case IN_USIZE:
    return 1;
  case IN_I128:
  case IN_U128:
    return 1;
  default: /* isize, floats: the 63-bit cut resolve.c makes */
    return (v >> 63) == 0;
  }
}

/* -- places and borrows (01-types.md, 03-move.md) ------------------------ */

static Type *rexpr(Ast *e, Fenv *fe, Type *want);
static Type *rexpr1(Ast *e, Fenv *fe, Type *want);

/* a pointer is dereferenced as far as it needs to be to reach a
 * member: sp.b is (*sp).b, chain and all. A *mut T's pointee is the
 * mut slot mut T, so that layer comes off too (01-types.md). Lives
 * in type.c now -- the emitter's field walks need it too. */

/* a slice's two named slots (01-types.md): s.ptr is *T -- *mut T
 * for a []mut T, which is the Tymut child -- and s.len a usize.
 * Both behave like mut struct fields. */
static Type *
slicefield(Type *t, char *name)
{
  if (!t || t->k != Tyslice)
    return 0;
  if (strcmp(name, "ptr") == 0)
    return typtr(t->t);
  if (strcmp(name, "len") == 0)
    return tyint(IN_USIZE);
  return 0;
}

/* a place expression: something & can point at and = can write to */
static int
isplace(Ast *e)
{
  if (e->k == Npath || e->k == Naccess || e->k == Nindex || e->k == Nrangeindex)
    return 1;
  return e->k == Nun && e->v.un.op == Tstar;
}

/* the root binding of a place chain, walking down to it; *p stops at
 * p -- the memory it points at is not a binding the walk can see */
static Local *
placeroot(Ast *e, Fenv *fe, char *path, usize psz)
{
  usize n = 0;

  path[0] = 0;
  while (e->k == Naccess || e->k == Nindex || e->k == Nrangeindex) {
    if (e->k == Naccess && strlen(e->v.fld.name) < 126 && n + strlen(e->v.fld.name) + 2 < psz) {
      char cat[128];

      sprintf(cat, ".%s", e->v.fld.name);
      if (n) {
        char old[256];

        strncpy(old, path, sizeof old - 1);
        old[(sizeof old) - 1] = 0;
        sprintf(path, "%s%s", old, cat);
      } else
        strcpy(path, cat);
      n = strlen(path);
    } else { /* an index, a slice, or a long name: the whole root */
      path[0] = 0;
      n = 0;
    }
    if (e->k == Naccess)
      e = e->v.fld.e;
    else if (e->k == Nindex)
      e = e->v.n2.a;
    else
      e = e->v.ridx.e;
  }
  if (e->k == Nun && e->v.un.op == Tstar)
    return 0; /* *p: whatever it reaches is not a binding here */
  if (e->k != Npath || vlen(e->v.path.segs) != 1)
    return 0;
  return locfind(fe, e->v.path.segs[0]->v.seg.name);
}

/* freeze a place: & sets FZ_SHR, &mut FZ_MUT. The field chain is the
 * borrowed place itself; an index or a deref freezes the root. The
 * freeze thaws when the binding that holds the borrow dies (locpop). */
static void
freeze(Ast *place, Fenv *fe, int mut, int by)
{
  char   buf[256];
  Local *root = placeroot(place, fe, buf, sizeof buf);

  if (!root)
    return; /* *p: across the pointer is a promise, not a proof (01) */
  if (mut)
    root->frz = FZ_MUT; /* exclusive: whatever was there, now harder */
  else if (root->frz == FZ_NONE)
    root->frz = FZ_SHR;
  if (root->frzby < 0) { /* the first borrow names its holder */
    root->frzby = by;
    root->frzpath = buf[0] ? arenaalloc(strlen(buf) + 1) : 0;
    if (root->frzpath)
      strcpy(root->frzpath, buf);
  }
}

/* what a place was frozen as before a call borrowed its receiver:
 * the call's borrow lasts the call, so what was there goes back */
typedef struct Frzsave Frzsave;
struct Frzsave
{
  Local *root; /* NULL: the call froze nothing */
  int    frz;
  int    frzby;
  char  *frzpath;
};

static void
frzrestore(Frzsave *sv)
{
  if (!sv->root)
    return;
  sv->root->frz = sv->frz;
  sv->root->frzby = sv->frzby;
  sv->root->frzpath = sv->frzpath;
}

/* does touching this place cross a live borrow? A mut borrow forbids
 * everything; a shared one forbids writes and moves out. The field
 * chains must overlap: a prefix either way means they do. */
static int
touchconflict(Ast *place, Fenv *fe, int writing)
{
  char   buf[256];
  Local *root = placeroot(place, fe, buf, sizeof buf);

  if (!root || root->frz == FZ_NONE)
    return 0;
  if (root->frz == FZ_MUT)
    return 1;
  if (!writing)
    return 0; /* FZ_SHR: reads are fine */
  if (!root->frzpath || !buf[0])
    return 1; /* the whole root, or an index into it */
  return strncmp(root->frzpath, buf, strlen(buf)) == 0 ||
         strncmp(buf, root->frzpath, strlen(root->frzpath)) == 0;
}

static Type *rplace(Ast *e, Fenv *fe);
static Type *rplace1(Ast *e, Fenv *fe);

/* a place, read: the base chain is checked, nothing moves -- reads
 * of fields and elements do not take what they read (03-move.md).
 * NULL when e is not a place at all. */
static Type *
rplace(Ast *e, Fenv *fe)
{
  Type *t = rplace1(e, fe);
  Type *s = t;

  while (s && s->k == Tymut) /* as in rexpr: the permission stays
                              * with the checker, the type goes out */
    s = s->t;
  if (t)
    e->ty = s;
  return t;
}

static Type *
rplace1(Ast *e, Fenv *fe)
{
  switch (e->k) {
  case Npath: {
    char   buf[256];
    Local *root = placeroot(e, fe, buf, sizeof buf);

    if (root) { /* a local: dead and borrow checks, no move */
      if (root->dead)
        berr(e, "'%s' has been moved", root->name);
      if (touchconflict(e, fe, 0))
        berr(e, "'%s' is borrowed (01-types.md)", root->name);
      return root->cur;
    }
    return 0; /* a global path: rexpr's own ground */
  }
  case Naccess: {
    Type *bt = rplace(e->v.fld.e, fe);
    usize i;

    if (!bt)
      bt = rexpr(e->v.fld.e, fe, 0); /* a computed base: a value read */
    if (!bt)
      return 0;
    bt = derefthrough(bt);
    if (bt->k == Typaram || bt->k == Typroj)
      berr(e, "a field of %s arrives with dispatch (06)", btys(bt));
    {
      Type *sf = slicefield(bt, e->v.fld.name);

      if (sf)
        return sf; /* the slice's own two (01-types.md) */
    }
    if (bt->k != Tystruct && bt->k != Tyunion)
      berr(e, "%s has no fields", btys(bt));
    for (i = 0; i < bt->sym->nfields; i++)
      if (strcmp(bt->sym->fields[i].name, e->v.fld.name) == 0)
        return bt->sym->fields[i].ty;
    berr(e, "'%s' has no field '%s'", bt->sym->name, e->v.fld.name);
    return 0; /* unreachable */
  }
  case Nindex:
  case Nrangeindex: {
    Type *bt = e->k == Nindex ? rplace(e->v.n2.a, fe) : rplace(e->v.ridx.e, fe);

    if (!bt && e->k == Nindex)
      bt = rexpr(e->v.n2.a, fe, 0);
    if (!bt && e->k == Nrangeindex)
      bt = rexpr(e->v.ridx.e, fe, 0);
    if (!bt)
      return 0;
    bt = derefthrough(bt);
    return bt->k == Tyarray || bt->k == Tyslice ? bt->t : 0;
  }
  case Nun:
    if (e->v.un.op == Tstar) {
      Type *pt = rexpr(e->v.un.e, fe, 0); /* the pointer itself: a Copy */

      return pt && pt->k == Typtr ? pt->t : 0;
    }
    return 0;
  default:
    return 0;
  }
}

/* -- narrowing (01-types.md, Nullability) -------------------------------- */

/* the == None / != Err forms: *name gets the binding the cond checks,
 * *child its narrowed type, and the return says which branch the
 * narrowing lands in -- 1 for the then, -1 for the else, 0 for none */
static int
narrowcond(Ast *cond, Fenv *fe, char **name, Type **child)
{
  Tok    op;
  Ast   *l, *r, *pl = 0, *pn = 0;
  Local *lo;
  Type  *t;
  int    iserr, isnone;

  if (cond->k != Nbin)
    return 0;
  op = cond->v.bin.op;
  if (op != Teqeq && op != Tne)
    return 0;
  l = cond->v.bin.l;
  r = cond->v.bin.r;
  if (l->k == Npath && vlen(l->v.path.segs) == 1)
    pl = l;
  if (r->k == Npath && vlen(r->v.path.segs) == 1)
    pn = r;
  if (!pl || !pn)
    return 0;
  {
    char *nn = pn->v.path.segs[0]->v.seg.name;

    iserr = strcmp(nn, "Err") == 0;
    isnone = strcmp(nn, "None") == 0;
    if (!iserr && !isnone)
      return 0;
    lo = locfind(fe, pl->v.path.segs[0]->v.seg.name);
    if (!lo)
      return 0;
    t = lo->cur;
  }
  if (isnone) {
    Type *c = optchild(t);

    if (!c)
      return 0;
    *name = pl->v.path.segs[0]->v.seg.name;
    *child = c;
  } else {
    Type *ok = 0;
    Type *err = reschild(t, &ok);

    if (!err)
      return 0;
    *name = pl->v.path.segs[0]->v.seg.name;
    *child = ok;
  }
  return op == Tne ? 1 : -1; /* != narrows the then branch, == the else */
}

/* -- joins (03-move.md, Branches) ----------------------------------------- */

/* join two branch states back into fe. Dead is the union -- the code
 * after the join runs on either path, so a move down one of them is
 * enough -- and frozen is the union too, a borrow the compiler cannot
 * disprove. A narrowing survives only when both branches agree. */
static void
fejoin(Fenv *fe, Fenv *a, Fenv *b)
{
  usize i;

  for (i = 0; i < fe->n; i++) {
    Local *d = &fe->ls[i];
    Local *da = &a->ls[i];
    Local *db = &b->ls[i];

    d->dead = da->dead || db->dead;
    if (da->frz == FZ_MUT || db->frz == FZ_MUT) {
      d->frz = FZ_MUT;
      d->frzby = da->frz != FZ_NONE ? da->frzby : db->frzby;
    } else if (da->frz == FZ_SHR || db->frz == FZ_SHR) {
      d->frz = FZ_SHR;
      d->frzby = da->frz != FZ_NONE ? da->frzby : db->frzby;
    } else {
      d->frz = FZ_NONE;
      d->frzby = -1;
    }
    d->cur = tysame(da->cur, db->cur) ? da->cur : d->ty;
  }
}

/* a branch that must leave never reaches the join point: what it
 * moved or froze in there stays there (03-move.md, Timing) */
static void
unreach(Fenv *fe)
{
  usize i;

  for (i = 0; i < fe->n; i++) {
    fe->ls[i].dead = 0;
    fe->ls[i].frz = FZ_NONE;
    fe->ls[i].frzby = -1;
  }
}

/* must the statement leave -- return/break/continue, or a branch
 * where every path does (03-move.md, Timing) */
static int
mustexit(Ast *st)
{
  if (!st)
    return 0;
  switch (st->k) {
  case Nreturn:
  case Nbreak:
  case Ncontinue:
    return 1;
  case Nblock: {
    Ast **ss = st->v.blk.stmts;
    usize n = vlen(ss);

    return n ? mustexit(ss[n - 1]) : mustexit(st->v.blk.tail);
  }
  case Nif:
    return st->v.ifx.els && mustexit(st->v.ifx.then) && mustexit(st->v.ifx.els);
  case Nmatch: {
    Ast **arms = st->v.call.args;
    usize i;

    for (i = 0; i < vlen(arms); i++)
      if (!mustexit(arms[i]->v.n2.b))
        return 0;
    return 1;
  }
  default:
    return 0;
  }
}

static struct Variant *
varfind(Sym *s, const char *name)
{
  usize i;

  for (i = 0; i < s->nvariants; i++)
    if (strcmp(s->variants[i].name, name) == 0)
      return &s->variants[i];
  return 0;
}

/* substitute generic parameters in a fn's signature by the types the
 * call bound them to: id(3) gives T = i32, so the return reads i32.
 * Lives in type.c now -- the emitter's layouts need it too. */

/* bind a signature's generic parameters from one argument's type:
 * walking both in step, a Typaram on the left records the right */
static int
gunify(Type *sig, Type *arg, Ast **gps, Type **tys, usize n)
{
  usize i;

  if (!sig || !arg)
    return 1;
  if (sig->k == Typaram) {
    for (i = 0; i < n; i++)
      if (sig->gp == gps[i]) {
        if (!tys[i])
          tys[i] = arg;
        return tysame(tys[i], arg);
      }
    return 1; /* someone else's parameter: Self's, an outer fn's */
  }
  if (sig->k != arg->k)
    return 0; /* a real mismatch: this overload does not fit */
  switch (sig->k) {
  case Typtr:
  case Tyslice:
  case Tymut:
    return gunify(sig->t, arg->t, gps, tys, n);
  case Tyarray:
    if (sig->n != arg->n)
      return 0;
    return gunify(sig->t, arg->t, gps, tys, n);
  case Tytuple:
  case Tyfn:
    if (sig->nargs != arg->nargs)
      return 0;
    for (i = 0; i < sig->nargs; i++)
      if (!gunify(sig->args[i], arg->args[i], gps, tys, n))
        return 0;
    return sig->k == Tyfn ? gunify(sig->t, arg->t, gps, tys, n) : 1;
  case Tystruct:
  case Tyenum:
  case Tyunion:
  case Tytrait:
  case Tydyn: /* named: the same sym, then the arguments in step */
    if (sig->sym != arg->sym || sig->nargs != arg->nargs)
      return 0;
    for (i = 0; i < sig->nargs; i++)
      if (!gunify(sig->args[i], arg->args[i], gps, tys, n))
        return 0;
    return 1;
  default:
    return tysame(sig, arg); /* interned: equal or not, nothing to bind inside */
  }
}

/* -- inherent impl members ------------------------------------------------ */

/* the member of an inherent impl for the type s, or NULL: pass 3's
 * table, walked. Trait impls are not looked at here -- those calls
 * arrive with dispatch. */
static Member *
inherentfind(Sym *s, const char *name)
{
  usize i;

  for (i = 0; i < chk_nimpls; i++) {
    Sym *im = chk_impls[i];

    if (im->ifort || !im->ipath || im->ipath->sym != s)
      continue;
    {
      Member *m = 0;
      usize   j;

      for (j = 0; j < im->nmembers; j++)
        if (strcmp(im->members[j].name, name) == 0) {
          m = &im->members[j];
          break;
        }
      if (m)
        return m;
    }
  }
  return 0;
}

/* -- expressions --------------------------------------------------------- */

static void rstmt(Ast *st, Fenv *fe);
static int  placewritable(Ast *p, Fenv *fe);

/* re-derive a literal against the type the other side of an
 * operator or an assignment turned out to be: 42 < u32's length */
static Type *
recoerce(Ast *e, Type *t, Fenv *fe)
{
  if (!t)
    return 0;
  if (e->k == Nint && isintty(t))
    return rexpr(e, fe, t);
  if (e->k == Nflt && isnumty(t) && (t->num == IN_F32 || t->num == IN_F64))
    return rexpr(e, fe, t);
  return 0;
}

/* a call to a named fn, overload chain and all. tys holds the
 * generic bindings while the arguments are walked. */
static Type *
callfn(Sym *s, Ast *a, Ast **args, usize n, Fenv *fe)
{
  const char *nm = s->name;
  Type      **ats = n ? arenaalloc(n * sizeof *ats) : 0;

  for (; s; s = s->next) {
    Type  *fnty = s->fnty;
    Type **tys = s->ngparams ? tyargs(s->ngparams) : 0;
    usize  i;
    int    ok = n == fnty->nargs;

    if (!ok)
      continue;
    for (i = 0; ok && i < n; i++) {
      ats[i] = rexpr(args[i], fe, fnty->args[i]);
      if (!ats[i] || !fnty->args[i])
        continue;
      if (tysame(ats[i], fnty->args[i]))
        continue;
      {
        Type *c = recoerce(args[i], fnty->args[i], fe);

        if (c) {
          ats[i] = c;
          continue;
        }
      }
      if (!gunify(fnty->args[i], ats[i], s->gparams, tys, s->ngparams))
        ok = 0;
    }
    if (ok) {
      for (i = 0; i < s->ngparams; i++)
        if (!tys[i])
          berr(a, "cannot infer '%s' for '%s' from the call", s->gparams[i]->v.gp.name, s->name);
      return gsubst(fnty->t, s->gparams, tys, s->ngparams);
    }
  }
  berr(a, "no '%s' takes these argument types", nm);
  return 0; /* unreachable */
}

/* an enum's variant as a constructor: Some(3), Ok(File{..}). The
 * enum's own parameters bind from the payload types when they can;
 * a want carries them in for the empty ones (None). */
static Type *
mkvariant(Sym *s, struct Variant *v, Ast *a, Ast **args, usize n, Fenv *fe, Type *want)
{
  usize  np = v->named ? v->nfields : (v->payload ? v->npayload : 0);
  Type **tys = s->ngparams ? tyargs(s->ngparams) : 0;
  usize  i;

  if (n != np)
    berr(a, "'%s' carries %lu payload%s, %lu given", v->name, (unsigned long) np,
         np == 1 ? "" : "s", (unsigned long) n);
  if (want && want->k == Tyenum && want->sym == s && want->nargs == s->ngparams)
    memcpy(tys, want->args, s->ngparams * sizeof *tys);
  for (i = 0; i < n; i++) {
    Type *pt = v->named ? v->fields[i].ty : v->payload[i];
    Type *at = rexpr(args[i], fe, pt);

    if (at && pt && !gunify(pt, at, s->gparams, tys, s->ngparams))
      berr(args[i], "'%s' carries %s here, %s given", v->name, btys(pt), btys(at));
  }
  for (i = 0; i < s->ngparams; i++)
    if (!tys[i])
      berr(a, "cannot infer '%s' for '%s::%s' from the arguments", s->gparams[i]->v.gp.name,
           s->name, v->name);
  return tysym(s, tys, s->ngparams);
}

/* a method's receiver, adapted the way the sugar defines it
 * (05-traits.md): a *Self takes &place, a *mut Self takes &mut of a
 * mut slot, a Self by value moves the receiver in. A pointer
 * receiver is dereferenced first -- &*sp is sp again -- so a pointer
 * as written fits the pointer selves directly. *recv is the
 * receiver's type as written, ty the dereferenced one the method was
 * found under. */
static void
recvadapt(Ast *x, Type *selfty, Type *rty, Type *ty, Fenv *fe, Frzsave *sv)
{
  char pbuf[256];
  int  isplacebinding;

  if (!rty || !selfty || !ty)
    return;
  isplacebinding = placeroot(x, fe, pbuf, sizeof pbuf) != 0;
  if (selfty->k == Typtr) { /* a pointer self: &place, or as written */
    Type *want = selfty->t->k == Tymut ? selfty->t->t : selfty->t;

    if (tysame(rty, selfty))
      return; /* &*sp is sp: the pointer already is the address */
    if (!tysame(ty, want))
      berr(x, "this receiver is %s, the method wants %s", btys(rty), btys(selfty));
    sv->root = placeroot(x, fe, pbuf, sizeof pbuf);
    if (sv->root) { /* what was frozen, to put back after the call */
      sv->frz = sv->root->frz;
      sv->frzby = sv->root->frzby;
      sv->frzpath = sv->root->frzpath;
    }
    if (selfty->t->k == Tymut) { /* &mut: a mut slot, and it freezes */
      if (!placewritable(x, fe))
        berr(x, "a &mut receiver needs a mut slot (01-types.md)");
      if (touchconflict(x, fe, 1))
        berr(x, "this place is already borrowed (01-types.md)");
      freeze(x, fe, 1, (int) fe->n);
    } else { /* &: shared, reads stay fine */
      if (touchconflict(x, fe, 0))
        berr(x, "this place is already borrowed (01-types.md)");
      freeze(x, fe, 0, (int) fe->n);
    }
    return;
  }
  if (tysame(ty, selfty)) { /* by value: the receiver moves in */
    if (isplacebinding && !iscopy(ty)) {
      Local *root = placeroot(x, fe, pbuf, sizeof pbuf);

      if (fe->loopd > 0 && locfindi(fe, root->name) < fe->loopbase)
        berr(x, "'%s' began before the for and would be moved every round", root->name);
      root->dead = 1; /* the binding is the move's one legal start */
    }
    return;
  }
  berr(x, "this receiver is %s, the method wants %s", btys(rty), btys(selfty));
}

/* -- the @ builtins (08-reflection.md) ----------------------------------- */

static Type *
rbuiltin(Ast *e, Fenv *fe)
{
  char *nm = e->v.blt.name;
  Ast **targs = e->v.blt.targs;
  Ast **args = e->v.blt.args;
  usize nt = vlen(targs), na = vlen(args);

  if (strcmp(nm, "sizeof") == 0 || strcmp(nm, "alignof") == 0) {
    if (nt != 1 || na != 0)
      berr(e, "@%s takes one type argument and no value", nm);
    targs[0]->ty = rty(targs[0], &fe->env);
    return tyint(IN_USIZE);
  }
  if (strcmp(nm, "cast") == 0) {
    Type *to;

    if (nt != 1 || na != 1)
      berr(e, "@cast takes one type argument and one value");
    to = rty(targs[0], &fe->env);
    targs[0]->ty = to;
    rexpr(args[0], fe, 0);
    return to;
  }
  if (strcmp(nm, "take") == 0) {
    Type *pt;

    if (nt != 0 || na != 1)
      berr(e, "@take takes one place");
    pt = rexpr(args[0], fe, 0);
    if (!pt || pt->k != Typtr || pt->t->k != Tymut)
      berr(args[0], "@take wants a *mut T place, this is %s", btys(pt));
    return pt->t->t;
  }
  if (strcmp(nm, "compileError") == 0) {
    Type *st;

    if (na != 1)
      berr(e, "@compileError takes the message");
    st = rexpr(args[0], fe, 0);
    if (!st || st->k != Tyslice || !st->t || st->t->k != Tyint || st->t->num != IN_U8)
      berr(args[0], "@compileError takes a string");
    berr(e, "%.*s", (int) args[0]->v.s.len, args[0]->v.s.s);
  }
  /* offset, field, count, typeinfo, typeof: reflection's own pass */
  berr(e, "@%s arrives with reflection (08-reflection.md)", nm);
  return 0; /* unreachable */
}

/* -- the operator table (07-operators.md) --------------------------------- */

/* what a binary operator does with two operand types, or NULL when
 * they do not fit it. *res gets the result type. */
static int
binop(Tok op, Type *a, Type *b, Type **res)
{
  switch (op) {
  case Tplus:
  case Tminus:
  case Tstar:
  case Tslash:
    if (isnumty(a) && tysame(a, b)) {
      *res = a;
      return 1;
    }
    if ((op == Tplus || op == Tminus) && a && a->k == Typtr && isintty(b)) {
      *res = a; /* pointer arithmetic (01-types.md) */
      return 1;
    }
    return 0;
  case Tpercent:
  case Tamp:
  case Tbar:
  case Tcaret:
  case Tshl:
  case Tshr:
    if (isintty(a) && tysame(a, b)) {
      *res = a;
      return 1;
    }
    if ((op == Tshl || op == Tshr) && isintty(a) && isintty(b)) {
      *res = a; /* any integer shifts (07-operators.md) */
      return 1;
    }
    return 0;
  case Teqeq:
  case Tne:
    if (a && b && tysame(a, b) &&
        (isnumty(a) || a->k == Typtr || a->k == Tybool || a->k == Tyenum || a->k == Tyunit)) {
      *res = tybool();
      return 1;
    }
    return 0;
  case Tlt:
  case Tgt:
  case Tle:
  case Tge:
    if (isnumty(a) && tysame(a, b)) {
      *res = tybool();
      return 1;
    }
    if (a && a->k == Typtr && tysame(a, b)) { /* a walk's stop (01) */
      *res = tybool();
      return 1;
    }
    return 0;
  case Tampamp:
  case Tbarbar:
    if (a && a->k == Tybool && b && b->k == Tybool) {
      *res = tybool();
      return 1;
    }
    return 0;
  default:
    return 0;
  }
}

/* the operator's spelling, for diagnostics */
static const char *
opname(Tok op)
{
  switch (op) {
  case Tplus:
    return "+";
  case Tminus:
    return "-";
  case Tstar:
    return "*";
  case Tslash:
    return "/";
  case Tpercent:
    return "%";
  case Tamp:
    return "&";
  case Tbar:
    return "|";
  case Tcaret:
    return "^";
  case Tshl:
    return "<<";
  case Tshr:
    return ">>";
  case Teqeq:
    return "==";
  case Tne:
    return "!=";
  case Tlt:
    return "<";
  case Tgt:
    return ">";
  case Tle:
    return "<=";
  case Tge:
    return ">=";
  case Tampamp:
    return "&&";
  case Tbarbar:
    return "||";
  default:
    return "?";
  }
}

/* -- the walk ------------------------------------------------------------- */

static Type *rmatch(Ast *e, Fenv *fe, Type *want);
static Type *rblock(Ast *b, Fenv *fe, Type *want);
static Type *rclosure(Ast *c, Fenv *fe);

/* the enum a variant's short name belongs to: the prelude's four,
 * the ones ?T's sugar rides on, are the only variant names an
 * expression may spell bare (01-types.md) */
static Sym *
variantowner(char *name)
{
  if (sym_option && varfind(sym_option, name))
    return sym_option;
  if (sym_result && varfind(sym_result, name))
    return sym_result;
  return 0;
}

/* a one-segment path's target, expressions only: a local, a const,
 * a fn (its own type), or an enum variant's short name -- which the
 * scrutinee's type picks out in a pattern, but must be spelled out
 * here, so only a payloadless variant reads as a value. A generic
 * enum's None takes its parameters from the context, p != None */
static Type *
rexprpath1(Ast *e, Fenv *fe, char *name, Type *want)
{
  Local *l = locfind(fe, name);
  Sym   *s;

  if (l) {
    if (l->dead)
      berr(e, "'%s' has been moved", name);
    if (touchconflict(e, fe, 1)) /* reading it out moves what it holds */
      berr(e, "'%s' is borrowed (01-types.md)", name);
    if (!iscopy(l->cur)) { /* assignment moves by default (03-move.md) */
      if (fe->loopd > 0 && locfindi(fe, name) < fe->loopbase)
        berr(e, "'%s' began before the for and would be moved every round", name);
      l->dead = 1;
    }
    return l->cur;
  }
  s = symfind(name);
  if (!s)
    s = variantowner(name); /* Some, None, Ok, Err: bare (01-types.md) */
  if (!s)
    berr(e, "unknown name '%s'", name);
  switch (s->kind) {
  case Sconst:
  case Sstatic:
    return s->cty;
  case Sfn:
    return s->fnty;
  case Stype:
    if (s->tykind == TYenum) {
      struct Variant *v = varfind(s, name);

      if (v && !v->named && !v->payload) {
        if (s->ngparams) { /* the context hands the parameters */
          if (want && want->k == Tyenum && want->sym == s && want->nargs == (usize) s->ngparams)
            return want;
          berr(e, "cannot infer '%s' for '%s' here; spell out the other side",
               s->gparams[0]->v.gp.name, s->name);
        }
        return tysym(s, 0, 0); /* a payloadless variant: its enum */
      }
      if (v)
        berr(e, "'%s' carries a payload; write %s::%s(...)", name, s->name, v->name);
    }
    berr(e, "'%s' is a type; type values arrive with reflection (08)", name);
    return 0; /* unreachable */
  default:
    berr(e, "'%s' is not a value", name);
  }
  return 0; /* unreachable */
}

/* the wrappers that write the type back: every expression the
 * checker walks leaves its verdict on the node, so the emitter
 * reads instead of deriving. rplace's chain writes too -- a field
 * access knows its base's type only there. */
static Type *
rexpr(Ast *e, Fenv *fe, Type *want)
{
  Type *t = rexpr1(e, fe, want);
  Type *s = t;

  while (s && s->k == Tymut) /* the mut layer is the permission a
                              * place lends, not its storage type:
                              * the emitter wants the latter */
    s = s->t;
  e->ty = s;
  return t;
}

static Type *
rexpr1(Ast *e, Fenv *fe, Type *want)
{
  switch (e->k) {
  case Nint:
    if (want && isintty(want)) {
      if (!fitsv(e->v.i.num, want))
        berr(e, "%lu does not fit %s", (unsigned long) e->v.i.num, btys(want));
      return want;
    }
    return tyint(IN_I32);
  case Nflt:
    if (want && isnumty(want) && (want->num == IN_F32 || want->num == IN_F64))
      return want;
    return tyint(IN_F32);
  case Nbyte:
    return tyint(IN_U8);
  case Nbool:
    return tybool();
  case Nstr:
    return tyslice(tyint(IN_U8));
  case Nunit:
    return tyunit();
  case Npath: {
    Ast **segs = e->v.path.segs;
    usize nsegs = vlen(segs);
    char *nm0 = segs[0]->v.seg.name;

    if (nsegs == 1)
      return rexprpath1(e, fe, nm0, want);
    if (nsegs == 2) {
      char *nm1 = segs[1]->v.seg.name;
      Sym  *s = symfind(nm0);

      if (!s || s->kind != Stype)
        berr(e, "unknown name '%s'", nm0);
      if (s->tykind == TYenum) {
        struct Variant *v = varfind(s, nm1);

        if (!v)
          berr(e, "'%s' has no variant '%s'", s->name, nm1);
        if (v->named || v->payload)
          berr(e, "'%s::%s' carries a payload; construct it", s->name, nm1);
        if (s->ngparams)
          berr(e, "cannot infer '%s' for '%s' here", s->gparams[0]->v.gp.name, s->name);
        return tysym(s, 0, 0);
      }
      { /* an inherent impl's member: a fn value or a const */
        Member *m = inherentfind(s, nm1);

        if (!m)
          berr(e, "'%s' has no '%s'; trait items arrive with dispatch (06)", s->name, nm1);
        if (m->kind == Mfn)
          return m->ty;
        if (m->kind == Mconst)
          return m->ty;
        berr(e, "'%s::%s' is a type, not a value", s->name, nm1);
      }
    }
    berr(e, "a path this long arrives with namespaces (11)");
    return 0; /* unreachable */
  }
  case Ntuple: {
    Ast  **es = e->v.list.ts;
    usize  n = vlen(es), i;
    Type **ts = n ? tyargs(n) : 0;

    for (i = 0; i < n; i++)
      ts[i] = rexpr(es[i], fe, want && want->k == Tytuple && i < want->nargs ? want->args[i] : 0);
    return tytuple(ts, n);
  }
  case Nbin: {
    Tok   op = e->v.bin.op;
    Type *ta = rexpr(e->v.bin.l, fe, 0);
    Type *tb = rexpr(e->v.bin.r, fe, ta); /* the other side names ?T for None */
    Type *res = 0;

    if (!tysame(ta, tb)) { /* a literal yields to the other side */
      Type *c = recoerce(e->v.bin.l, tb, fe);

      if (c)
        ta = c;
      else if ((c = recoerce(e->v.bin.r, ta, fe)))
        tb = c;
    }
    if (op == Tshl || op == Tshr) { /* the constant-range rule (07) */
      Ast *amt = e->v.bin.r;

      if (amt->k == Nint && ta && isintty(ta)) {
        u64 w = ta->num == IN_I8 || ta->num == IN_U8     ? 8
                : ta->num == IN_I16 || ta->num == IN_U16 ? 16
                : ta->num == IN_I32 || ta->num == IN_U32 ? 32
                : ta->num == IN_I64 || ta->num == IN_U64 ? 64
                                                         : 128;

        if (amt->v.i.num >= w)
          berr(amt, "shift amount out of range for %s", btys(ta));
      }
    }
    if (binop(op, ta, tb, &res))
      return res;
    if (ta && tb && !tysame(ta, tb))
      berr(e, "'%s' wants both sides the same type: %s and %s", opname(op), btys(ta), btys(tb));
    berr(e, "'%s' is not defined for %s", opname(op), btys(ta));
    return 0; /* unreachable */
  }
  case Nun: {
    Tok op = e->v.un.op;

    if (op == Tamp) { /* &x / &mut x: the place is borrowed, not read */
      Type *t = rplace(e->v.un.e, fe);

      if (!t)
        berr(e->v.un.e, "cannot borrow a temporary");
      if (e->v.un.mut && !placewritable(e->v.un.e, fe))
        berr(e->v.un.e, "a &mut needs a mut slot (01-types.md)");
      if (touchconflict(e->v.un.e, fe, 1))
        berr(e->v.un.e, "this place is already borrowed (01-types.md)");
      freeze(e->v.un.e, fe, e->v.un.mut, (int) fe->n);
      return e->v.un.mut ? typtr(tymut(t)) : typtr(t);
    }
    {
      Type *t = rexpr(e->v.un.e, fe, 0);

      switch (op) {
      case Tminus:
        if (t && isnumty(t))
          return t;
        break;
      case Ttilde:
        if (t && isintty(t))
          return t;
        break;
      case Tbang:
        if (t && t->k == Tybool)
          return t;
        break;
      case Tstar: { /* the deref, as a value: a move-out unless Copy */
        if (!t)
          return 0;
        if (t->k != Typtr)
          berr(e, "cannot dereference %s", btys(t));
        if (!iscopy(t->t))
          berr(e, "cannot move out of a place: %s is not Copy (@take, 03)", btys(t->t));
        return t->t;
      }
      default:
        break;
      }
      berr(e, "this operand does not take unary '%s'", opname(op));
      return 0; /* unreachable */
    }
  }
  case Ncall: {
    Ast  *f = e->v.call.f;
    Ast **args = e->v.call.args;
    usize n = vlen(args);

    if (f->k == Npath) {
      Ast **segs = f->v.path.segs;
      usize nsegs = vlen(segs);

      if (nsegs == 1) {
        char  *nm = segs[0]->v.seg.name;
        Local *l = locfind(fe, nm);
        Sym   *s;

        if (l) { /* an indirect call: a fn-typed local */
          Type *t = l->cur;

          if (!t || t->k != Tyfn)
            berr(e, "'%s' is %s, not callable", nm, btys(t));
          if (n != t->nargs)
            berr(e, "'%s' takes %lu arguments, %lu given", nm, (unsigned long) t->nargs,
                 (unsigned long) n);
          {
            usize i;

            for (i = 0; i < n; i++) {
              Type *at = rexpr(args[i], fe, t->args[i]);

              if (at && t->args[i] && !tysame(at, t->args[i])) {
                Type *c = recoerce(args[i], t->args[i], fe);

                if (!c || !tysame(c, t->args[i]))
                  berr(args[i], "'%s' wants %s here, this is %s", nm, btys(t->args[i]), btys(at));
              }
            }
          }
          return t->t;
        }
        s = symfind(nm);
        if (!s) { /* Some(3), Ok(v): the prelude's bare constructors */
          Sym *owner = variantowner(nm);

          if (owner)
            return mkvariant(owner, varfind(owner, nm), e, args, n, fe, want);
        }
        if (!s)
          berr(e, "unknown name '%s'", nm);
        if (s->kind == Sfn)
          return callfn(s, e, args, n, fe);
        berr(e, "'%s' is not callable", nm);
      }
      if (nsegs == 2) { /* Enum::Variant(...) or Type::member(...) */
        char *nm0 = segs[0]->v.seg.name;
        char *nm1 = segs[1]->v.seg.name;
        Sym  *s = symfind(nm0);

        if (!s || s->kind != Stype)
          berr(e, "unknown name '%s'", nm0);
        if (s->tykind == TYenum) {
          struct Variant *v = varfind(s, nm1);

          if (!v)
            berr(e, "'%s' has no variant '%s'", s->name, nm1);
          return mkvariant(s, v, e, args, n, fe, want);
        }
        {
          Member *m = inherentfind(s, nm1);

          if (!m)
            berr(e, "'%s' has no '%s'; trait items arrive with dispatch (06)", s->name, nm1);
          if (m->kind != Mfn)
            berr(e, "'%s::%s' is not callable", s->name, nm1);
          {
            Type *t = m->ty;

            if (n != t->nargs)
              berr(e, "'%s::%s' takes %lu arguments, %lu given", s->name, nm1,
                   (unsigned long) t->nargs, (unsigned long) n);
            {
              usize i;

              for (i = 0; i < n; i++) {
                Type *at = rexpr(args[i], fe, t->args[i]);

                if (at && t->args[i] && !tysame(at, t->args[i])) {
                  Type *c = recoerce(args[i], t->args[i], fe);

                  if (!c || !tysame(c, t->args[i]))
                    berr(args[i], "'%s::%s' wants %s here, this is %s", s->name, nm1,
                         btys(t->args[i]), btys(at));
                }
              }
            }
            return t->t;
          }
        }
      }
      berr(e, "a path this long arrives with namespaces (11)");
    }
    if (f->k == Naccess) {                 /* the method sugar: x.f(...) (05-traits.md) */
      Type   *rt = rplace(f->v.fld.e, fe); /* a place: no move just to call */
      Member *m;
      Type   *t, *ty;

      if (!rt)
        rt = rexpr(f->v.fld.e, fe, 0); /* a computed receiver: f().m() */
      if (!rt)
        return 0;
      ty = derefthrough(rt); /* a pointer receiver is dereferenced first */
      if (ty->k != Tystruct && ty->k != Tyunion && ty->k != Tyenum)
        berr(f, "a method call needs a struct, union, or enum receiver; trait calls "
                "arrive with dispatch (06)");
      m = inherentfind(ty->sym, f->v.fld.name);
      if (!m)
        berr(f, "'%s' has no method '%s'; trait calls arrive with dispatch (06)", ty->sym->name,
             f->v.fld.name);
      if (m->kind != Mfn)
        berr(f, "'%s::%s' is not a method", ty->sym->name, f->v.fld.name);
      t = m->ty;
      {
        Frzsave sv;

        memset(&sv, 0, sizeof sv);
        recvadapt(f->v.fld.e,
                  t->nargs ? gsubst(t->args[0], ty->sym->gparams, ty->args, ty->nargs) : 0, rt, ty,
                  fe, &sv);
        if (n + 1 != t->nargs)
          berr(e, "'%s' takes %lu arguments, %lu given", f->v.fld.name,
               (unsigned long) (t->nargs - 1), (unsigned long) n);
        {
          usize i;

          for (i = 0; i < n; i++) {
            Type *at = rexpr(args[i], fe, t->args[i + 1]);

            if (at && t->args[i + 1] && !tysame(at, t->args[i + 1])) {
              Type *c = recoerce(args[i], t->args[i + 1], fe);

              if (!c || !tysame(c, t->args[i + 1]))
                berr(args[i], "'%s' wants %s here, this is %s", f->v.fld.name, btys(t->args[i + 1]),
                     btys(at));
            }
          }
        }
        frzrestore(&sv); /* the receiver's borrow ends with the call */
      }
      return t->t;
    }
    { /* an arbitrary callee: a fn-typed expression */
      Type *t = rexpr(f, fe, 0);

      if (!t || t->k != Tyfn)
        berr(f, "this is %s, not callable", btys(t));
      if (n != t->nargs)
        berr(e, "this call takes %lu arguments, %lu given", (unsigned long) t->nargs,
             (unsigned long) n);
      {
        usize i;

        for (i = 0; i < n; i++) {
          Type *at = rexpr(args[i], fe, t->args[i]);

          if (at && t->args[i] && !tysame(at, t->args[i]))
            berr(args[i], "this call wants %s here, this is %s", btys(t->args[i]), btys(at));
        }
      }
      return t->t;
    }
  }
  case Naccess: {
    Type *bt = rplace(e->v.fld.e, fe);
    char  pbuf[256];
    usize i;

    if (!bt)
      bt = rexpr(e->v.fld.e, fe, 0); /* a computed base: f().x, a value */
    if (!bt)
      return 0;
    bt = derefthrough(bt);
    if (bt->k == Typaram || bt->k == Typroj)
      berr(e, "a field of %s arrives with dispatch (06)", btys(bt));
    {
      Type *sf = slicefield(bt, e->v.fld.name);

      if (sf)
        return sf; /* ptr and len: scalars both, Copy */
    }
    if (bt->k != Tystruct && bt->k != Tyunion)
      berr(e, "%s has no fields", btys(bt));
    for (i = 0; i < bt->sym->nfields; i++)
      if (strcmp(bt->sym->fields[i].name, e->v.fld.name) == 0) {
        Type *ft = bt->sym->fields[i].ty;

        /* a field read out of a place moves it when it is not Copy;
         * @take is the way out (03-move.md). A computed base is a
         * value already -- a field out of it moves fine. */
        if (placeroot(e, fe, pbuf, sizeof pbuf) && !iscopy(ft))
          berr(e, "cannot move out of a place: %s is not Copy (@take, 03)", btys(ft));
        return ft;
      }
    berr(e, "'%s' has no field '%s'", bt->sym->name, e->v.fld.name);
    return 0; /* unreachable */
  }
  case Nindex: {
    Type *bt = rplace(e->v.n2.a, fe);
    Type *it = rexpr(e->v.n2.b, fe, 0);
    char  pbuf[256];

    if (!bt)
      bt = rexpr(e->v.n2.a, fe, 0); /* a computed base: f()[i], a value */
    if (!bt)
      return 0;
    bt = derefthrough(bt);
    if (bt->k != Tyarray && bt->k != Tyslice) {
      berr(e, "%s cannot be indexed", btys(bt));
    }
    if (!it || !isintty(it))
      berr(e->v.n2.b, "an index is an integer, this is %s", btys(it));
    if (e->v.n2.b->k == Nint && bt->k == Tyarray && e->v.n2.b->v.i.num >= bt->n)
      berr(e->v.n2.b, "index %lu out of range for %s", (unsigned long) e->v.n2.b->v.i.num,
           btys(bt));
    /* an element read out of a place moves it when it is not Copy
     * (03-move.md); a computed base is a value already */
    if (placeroot(e, fe, pbuf, sizeof pbuf) && !iscopy(bt->t))
      berr(e, "cannot move out of a place: %s is not Copy (@take, 03)", btys(bt->t));
    return bt->t;
  }
  case Nrangeindex: { /* a[..] or a[lo..hi]: the slice view (01) */
    Type *bt = rplace(e->v.ridx.e, fe);

    if (!bt)
      bt = rexpr(e->v.ridx.e, fe, 0); /* a computed base: f()[..], a value */
    if (!bt)
      return 0;
    bt = derefthrough(bt);
    if (bt->k != Tyarray && bt->k != Tyslice)
      berr(e, "%s cannot be sliced", btys(bt));
    if (e->v.ridx.lo) {
      Type *lt = rexpr(e->v.ridx.lo, fe, 0);

      if (!lt || !isintty(lt))
        berr(e->v.ridx.lo, "a slice bound is an integer, this is %s", btys(lt));
    }
    if (e->v.ridx.hi) {
      Type *ht = rexpr(e->v.ridx.hi, fe, 0);

      if (!ht || !isintty(ht))
        berr(e->v.ridx.hi, "a slice bound is an integer, this is %s", btys(ht));
    }
    if (e->v.ridx.lo && e->v.ridx.hi && e->v.ridx.lo->k == Nint && e->v.ridx.hi->k == Nint &&
        bt->k == Tyarray &&
        (e->v.ridx.lo->v.i.num > e->v.ridx.hi->v.i.num || e->v.ridx.hi->v.i.num > bt->n))
      berr(e, "slice bounds out of range for %s", btys(bt));
    return tyslice(bt->t);
  }
  case Ntupidx: {
    Type *bt = rexpr(e->v.tup.e, fe, 0);

    if (!bt)
      return 0;
    if (bt->k != Tytuple)
      berr(e, "%s is not a tuple", btys(bt));
    if (e->v.tup.idx >= bt->nargs)
      berr(e, "tuple index %lu out of range", (unsigned long) e->v.tup.idx);
    return bt->args[e->v.tup.idx];
  }
  case Ntry: {
    Type *t = rexpr(e->v.n1.e, fe, 0);
    Type *ok = 0, *err = 0;

    if (!t)
      return 0;
    err = reschild(t, &ok);
    if (!err)
      berr(e, "'?' wants an E?T, this is %s", btys(t));
    if (!fe->fnret)
      berr(e, "'?' outside a fn");
    {
      Type *rok = 0;
      Type *rerr = reschild(fe->fnret, &rok);

      if (!rerr || !tysame(rerr, err))
        berr(e, "'?' hands back %s, the fn returns %s", btys(err), btys(fe->fnret));
    }
    return ok;
  }
  case Nif: {
    Type *ct = rexpr(e->v.ifx.cond, fe, 0);
    Fenv  ft, ff;
    Type *tt, *tf;
    char *nm = 0;
    Type *child = 0;
    int   side;

    if (!ct || ct->k != Tybool)
      berr(e->v.ifx.cond, "an if condition is a bool, this is %s", btys(ct));
    side = narrowcond(e->v.ifx.cond, fe, &nm, &child);
    ft = fefork(fe);
    if (side == 1) /* != narrows the then (01-types.md, Nullability) */
      locnarrow(&ft, nm, child);
    tt = rexpr(e->v.ifx.then, &ft, want);
    if (mustexit(e->v.ifx.then))
      unreach(&ft); /* the then never reaches what follows */
    if (e->v.ifx.els) {
      ff = fefork(fe);
      if (side == -1) /* == narrows the else */
        locnarrow(&ff, nm, child);
      tf = rexpr(e->v.ifx.els, &ff, want);
      if (mustexit(e->v.ifx.els))
        unreach(&ff);
      if (!tysame(tt, tf))
        berr(e, "the branches disagree: %s and %s", btys(tt), btys(tf));
      fejoin(fe, &ft, &ff);
      return tt;
    }
    { /* no else: the implicit fall-through is the untouched state */
      Fenv f0 = fefork(fe);

      fejoin(fe, &ft, &f0);
    }
    return tyunit(); /* the statement form: no value */
  }
  case Nmatch:
    return rmatch(e, fe, want);
  case Nblock:
    return rblock(e, fe, want);
  case Narraylit: {
    Type *et = rty(e->v.arrlit.t, &fe->env);
    Ast **es = e->v.arrlit.es;
    usize n = vlen(es), i;

    if (!et)
      return 0;
    for (i = 0; i < n; i++) {
      Type *at = rexpr(es[i], fe, et);

      if (at && !tysame(at, et)) {
        Type *c = recoerce(es[i], et, fe);

        if (!c || !tysame(c, et))
          berr(es[i], "the elements are %s, this is %s", btys(et), btys(at));
      }
    }
    if (e->v.arrlit.len && e->v.arrlit.len->k == Nint) {
      if (e->v.arrlit.len->v.i.num != n)
        berr(e, "[%lu] holds %lu elements, %lu given", (unsigned long) e->v.arrlit.len->v.i.num,
             (unsigned long) n, (unsigned long) n);
      return tyarray(e->v.arrlit.len->v.i.num, et);
    }
    return tyslice(et); /* []T: the unsized literal */
  }
  case Nstructlit: {
    Type *st = rpath(e->v.slit.path, &fe->env);
    Ast **inits = e->v.slit.inits;
    usize n = vlen(inits), i, j;

    if (!st)
      return 0;
    if (st->k != Tystruct && st->k != Tyunion)
      berr(e, "%s is not a struct or union", btys(st));
    for (i = 0; i < n; i++) {
      Ast   *in = inits[i];
      Field *f = 0;
      usize  k;

      for (k = 0; k < st->sym->nfields; k++)
        if (strcmp(st->sym->fields[k].name, in->v.init.name) == 0) {
          f = &st->sym->fields[k];
          break;
        }
      if (!f)
        berr(in, "'%s' has no field '%s'", st->sym->name, in->v.init.name);
      for (j = 0; j < i; j++)
        if (strcmp(inits[j]->v.init.name, in->v.init.name) == 0)
          berr(in, "field '%s' given twice", in->v.init.name);
      {
        Type *at = rexpr(in->v.init.e, fe, f->ty);

        if (at && !tysame(at, f->ty)) {
          Type *c = recoerce(in->v.init.e, f->ty, fe);

          if (!c || !tysame(c, f->ty))
            berr(in->v.init.e, "field '%s' is %s, this is %s", f->name, btys(f->ty), btys(at));
        }
      }
    }
    return st; /* fields left out are zero (01-types.md) */
  }
  case Nbarestructlit: {
    Type *st = want;

    if (!st || (st->k != Tystruct && st->k != Tyunion))
      berr(e, "a bare literal needs the struct from its context");
    return st;
  }
  case Nclosure:
    return rclosure(e, fe);
  case Nbuiltin:
    return rbuiltin(e, fe);
  case Nrange:
    berr(e, "ranges arrive with iteration (10-iteration.md)");
    return 0; /* unreachable */
  case Nspread:
    berr(e, "pack spreads arrive with generics (04-generics.md)");
    return 0; /* unreachable */
  default:    /* the type nodes: type values arrive with reflection */
    berr(e, "this is a type, not an expression; $$ arrives with reflection (08)");
  }
  return 0; /* unreachable */
}

/* -- patterns (09-match.md) ---------------------------------------------- */

/* check a pattern against the type it must fit, binding what it
 * names. Exhaustiveness is the caller's -- a pattern only fits. */
static void
rpat(Ast *p, Type *t, Fenv *fe, int mut)
{
  switch (p->k) {
  case Npwild:
    return;
  case Npath: {
    char *nm = p->v.path.segs[0]->v.seg.name;

    if (vlen(p->v.path.segs) != 1)
      berr(p, "a binding is one name; the variant form is below");
    p->ty = t;
    locpush(fe, nm, t, mut);
    return;
  }
  case Nppath: {
    Ast           **segs = p->v.ppath.path->v.path.segs;
    char           *en, *vn;
    Sym            *s;
    struct Variant *v;

    if (vlen(segs) == 1 && !p->v.ppath.payload && !p->v.ppath.named &&
        (!t || t->k != Tyenum || !varfind(t->sym, segs[0]->v.seg.name))) {
      /* a bare name that names no variant of the scrutinee's enum:
       * the binding form -- the parser sends every ident-headed
       * pattern here, variant or not */
      p->ty = t;
      locpush(fe, segs[0]->v.seg.name, t, mut);
      return;
    }
    if (vlen(segs) == 2) { /* Enum::Variant */
      en = segs[0]->v.seg.name;
      vn = segs[1]->v.seg.name;
      s = symfind(en);
      if (!s || s->kind != Stype || s->tykind != TYenum)
        berr(p, "'%s' is not an enum", en);
      if (t && (t->k != Tyenum || t->sym != s))
        berr(p, "this pattern fits %s, not %s's values", btys(t), s->name);
    } else { /* the short name: the scrutinee picks it out */
      vn = segs[0]->v.seg.name;
      if (!t || t->k != Tyenum)
        berr(p, "'%s' names a variant; the scrutinee is %s", vn, btys(t));
      s = t->sym;
      en = s->name;
    }
    v = varfind(s, vn);
    if (!v)
      berr(p, "'%s' has no variant '%s'", en, vn);
    if (p->v.ppath.payload) {
      Ast **ps = p->v.ppath.payload;
      usize n = vlen(ps), i;

      if (p->v.ppath.named) { /* by field name, mirroring the decl */
        if (!v->named)
          berr(p, "'%s' is destructured by position", vn);
        for (i = 0; i < n; i++) {
          Ast   *pf = ps[i];
          Field *f = 0;
          usize  k;

          for (k = 0; k < v->nfields; k++)
            if (strcmp(v->fields[k].name, pf->v.init.name) == 0) {
              f = &v->fields[k];
              break;
            }
          if (!f)
            berr(pf, "'%s' has no field '%s'", vn, pf->v.init.name);
          {
            Type *ft = f->ty;

            if (t && t->nargs == (usize) s->ngparams)
              ft = gsubst(ft, s->gparams, t->args, t->nargs);
            if (pf->v.init.e)
              rpat(pf->v.init.e, ft, fe, mut || f->mut);
            else { /* the field name is the binding name (09-match.md) */
              pf->ty = ft;
              locpush(fe, pf->v.init.name, ft, mut || f->mut);
            }
          }
        }
        return;
      }
      if (v->named)
        berr(p, "'%s' is destructured by field name", vn);
      if (n != (v->payload ? v->npayload : 0))
        berr(p, "'%s' carries %lu payloads, %lu given", vn,
             (unsigned long) (v->payload ? v->npayload : 0), (unsigned long) n);
      for (i = 0; i < n; i++) {
        Type *pt = v->payload[i];

        if (t && t->nargs == (usize) s->ngparams) /* ?i32: T is i32 here */
          pt = gsubst(pt, s->gparams, t->args, t->nargs);
        rpat(ps[i], pt, fe, mut);
      }
    } else if (v->payload && v->npayload == 1 && t && t->nargs == (usize) s->ngparams &&
               gsubst(v->payload[0], s->gparams, t->args, t->nargs)->k == Tyunit)
      ; /* the one payload instantiated to (): nothing to bind --
         * the niche side of an E?T (01-types.md, Results) */
    else if (v->payload || v->named)
      berr(p, "'%s' carries a payload; bind it", vn);
    return;
  }
  case Nptuple: {
    Ast **ps = p->v.list.ts;
    usize n = vlen(ps), i;

    if (!t || t->k != Tytuple || t->nargs != n)
      berr(p, "this pattern fits %lu things, the type is %s", (unsigned long) n, btys(t));
    for (i = 0; i < n; i++)
      rpat(ps[i], t->args[i], fe, mut);
    return;
  }
  case Npstruct: {
    Ast **fs = p->v.pstruct.fields;
    usize n = vlen(fs), i;

    if (!t || t->k != Tystruct)
      berr(p, "a struct pattern fits a struct, this is %s", btys(t));
    for (i = 0; i < n; i++) {
      Ast   *pf = fs[i];
      Field *f = 0;
      usize  k;

      for (k = 0; k < t->sym->nfields; k++)
        if (strcmp(t->sym->fields[k].name, pf->v.init.name) == 0) {
          f = &t->sym->fields[k];
          break;
        }
      if (!f)
        berr(pf, "'%s' has no field '%s'", t->sym->name, pf->v.init.name);
      {
        Type *ft = f->ty;

        if (t->nargs == (usize) t->sym->ngparams)
          ft = gsubst(ft, t->sym->gparams, t->args, t->nargs);
        if (pf->v.init.e)
          rpat(pf->v.init.e, ft, fe, mut || f->mut);
        else { /* the field name is the binding name when no
                * sub-pattern is given -- "{ x }" reads as
                * "{ x: x }" (09-match.md) */
          pf->ty = ft;
          locpush(fe, pf->v.init.name, ft, mut || f->mut);
        }
      }
    }
    return;
  }
  case Npor: {
    Ast **ps = p->v.list.ts;
    usize n = vlen(ps), i;

    for (i = 0; i < n; i++)
      rpat(ps[i], t, fe, mut);
    return;
  }
  case Nint:
  case Nflt:
  case Nbyte:
  case Nstr:
  case Nbool:
    rexpr(p, fe, t); /* a literal pattern: the same check as a value */
    return;
  case Nunit:
    if (!t || t->k != Tyunit)
      berr(p, "() fits (), this is %s", btys(t));
    return;
  default:
    berr(p, "this is not a pattern");
  }
}

/* -- match (09-match.md) -------------------------------------------------- */

/* does an arm's pattern bind anything -- the move question below */
static int
patbinds(Ast *p, Sym *scr) /* scr: the enum a short name may pick
                            * a variant out of, or NULL */
{
  switch (p->k) {
  case Npwild:
  case Nint:
  case Nflt:
  case Nbyte:
  case Nstr:
  case Nbool:
  case Nunit:
    return 0;
  case Npath:
    return 1;
  case Nppath:
    if (vlen(p->v.ppath.path->v.path.segs) == 1 && !p->v.ppath.payload && !p->v.ppath.named) {
      if (scr && varfind(scr, p->v.ppath.path->v.path.segs[0]->v.seg.name))
        return 0; /* a short variant name binds nothing (09) */
      return 1;   /* the bare-name binding */
    }
    return p->v.ppath.payload ? 1 : 0; /* Enum::Variant: the payload binds, not the whole */
  case Nptuple:
  case Npor: {
    Ast **ps = p->v.list.ts;
    usize i;

    for (i = 0; i < vlen(ps); i++)
      if (patbinds(ps[i], scr))
        return 1;
    return 0;
  }
  case Npstruct: {
    Ast **fs = p->v.pstruct.fields;
    usize i;

    for (i = 0; i < vlen(fs); i++)
      if (!fs[i]->v.init.e || patbinds(fs[i]->v.init.e, scr))
        return 1; /* a bare field name binds (09) */
    return 0;
  }
  default:
    return 0;
  }
}

/* the top-level names a pattern covers: the enum variants themselves
 * -- one arm covers one variant, not the whole enum -- or "whatever"
 * for a binding or a wildcard. scr is the scrutinee's enum, when it
 * is one, so a short variant name still counts. vs and whatever fill
 * in. */
static void
patcovers(Ast *p, Sym *scr, struct Variant **vs, usize *nv, int *whatever)
{
  switch (p->k) {
  case Npwild:
  case Npath:
    *whatever = 1;
    return;
  case Nppath: {
    Ast           **segs = p->v.ppath.path->v.path.segs;
    struct Variant *v;

    if (vlen(segs) == 1 && !p->v.ppath.payload && !p->v.ppath.named) {
      if (scr && (v = varfind(scr, segs[0]->v.seg.name))) {
        if (*nv < 32) /* the short name of a payloadless variant (09) */
          vs[(*nv)++] = v;
        return;
      }
      *whatever = 1; /* the bare-name binding */
      return;
    }
    if (vlen(segs) == 2) {
      Sym *s = symfind(segs[0]->v.seg.name);

      v = s && s->kind == Stype && s->tykind == TYenum ? varfind(s, segs[1]->v.seg.name) : 0;
    } else if (vlen(segs) == 1 && scr)
      v = varfind(scr, segs[0]->v.seg.name); /* short name: scr picks it */
    else {
      *whatever = 1;
      return;
    }
    if (v) {
      if (*nv < 32)
        vs[(*nv)++] = v;
    } else
      *whatever = 1; /* not a variant we can see: assume it covers */
    return;
  }
  case Npor: {
    Ast **ps = p->v.list.ts;
    usize i;

    for (i = 0; i < vlen(ps); i++)
      patcovers(ps[i], scr, vs, nv, whatever);
    return;
  }
  default:
    *whatever = 1; /* tuples and struct patterns: rpat type-checks
                    * them; exhaustiveness stays out of it */
    return;
  }
}

static Type *
rmatch(Ast *e, Fenv *fe, Type *want)
{
  Ast           **arms = e->v.call.args;
  usize           n = vlen(arms), i;
  Type           *st = rplace(e->v.call.f, fe); /* the scrutinee, read as a place */
  int             binds = 0;
  Type           *rt = 0;
  struct Variant *vs[32];
  usize           nv = 0;
  int             whatever = 0;
  int             joined = 0;
  Fenv            acc;

  if (!st)
    st = rexpr(e->v.call.f, fe, 0); /* a computed scrutinee: f() */
  if (!st)
    return 0;
  for (i = 0; i < n; i++)
    if (patbinds(arms[i]->v.n2.a, st->k == Tyenum ? st->sym : 0))
      binds = 1;
  if (binds && !iscopy(st)) { /* an arm that binds takes what the
                               * scrutinee holds (09-match.md) */
    char   pbuf[256];
    Local *root = placeroot(e->v.call.f, fe, pbuf, sizeof pbuf);

    if (root) {
      if (touchconflict(e->v.call.f, fe, 1))
        berr(e->v.call.f, "this place is borrowed (01-types.md)");
      if (fe->loopd > 0 && locfindi(fe, root->name) < fe->loopbase)
        berr(e->v.call.f, "'%s' began before the for and would be moved every round", root->name);
      root->dead = 1;
    }
  }
  if (st->k == Tyenum) { /* the covered variants, by name */
    for (i = 0; i < n; i++)
      patcovers(arms[i]->v.n2.a, st->sym, vs, &nv, &whatever);
    if (!whatever) {
      for (i = 0; i < st->sym->nvariants; i++) {
        usize j;
        int   seen = 0;

        for (j = 0; j < nv; j++)
          if (vs[j] == &st->sym->variants[i])
            seen = 1; /* the arm named this very variant */
        if (!seen)
          berr(e, "the match misses '%s'", st->sym->variants[i].name);
      }
    }
  } else {
    for (i = 0; i < n; i++)
      patcovers(arms[i]->v.n2.a, 0, vs, &nv, &whatever);
    if (!whatever)
      berr(e, "the match misses values it does not list; add '_' or a binding");
  }
  for (i = 0; i < n; i++) {
    Ast  *arm = arms[i];
    Fenv  fa = fefork(fe);
    usize nbase = fa.n;
    Type *at;

    rpat(arm->v.n2.a, st, &fa, 0);
    at = arm->v.n2.b->k == Nblock ? rblock(arm->v.n2.b, &fa, want) : rexpr(arm->v.n2.b, &fa, want);
    if (!rt)
      rt = at;
    else if (at && !tysame(rt, at))
      berr(arm->v.n2.b, "the arms disagree: %s and %s", btys(rt), btys(at));
    locpop(&fa, nbase); /* the arm's bindings -- and thaws -- end here */
    if (mustexit(arm->v.n2.b))
      continue; /* never reaches the join */
    if (!joined) {
      acc = fa; /* the first reachable arm seeds the join */
      joined = 1;
    } else
      fejoin(&acc, &acc, &fa);
  }
  if (joined)
    *fe = acc; /* every arm checked against the same pre-state */
  return rt;   /* every arm leaving: what follows is unreachable anyway */
}

/* -- blocks, closures, statements ----------------------------------------- */

static Type *
rblock(Ast *b, Fenv *fe, Type *want)
{
  usize nbase = fe->n;
  Ast **ss = b->v.blk.stmts;
  usize n = vlen(ss), i;
  Type *t;

  for (i = 0; i < n; i++)
    rstmt(ss[i], fe);
  t = b->v.blk.tail ? rexpr(b->v.blk.tail, fe, want) : tyunit();
  locpop(fe, nbase);
  return t;
}

static Type *
rclosure(Ast *c, Fenv *fe)
{
  Ast  **ps = c->v.clos.params;
  usize  np = vlen(ps), i;
  Ast  **caps = c->v.clos.caps;
  usize  nc = vlen(caps);
  Type **ts = np ? tyargs(np) : 0;
  Type  *ret = c->v.clos.ret ? rty(c->v.clos.ret, &fe->env) : tyunit();
  Fenv   fb;

  for (i = 0; i < np; i++) {
    if (!ps[i]->v.param.t)
      berr(ps[i], "a closure parameter carries its type");
    ts[i] = rty(ps[i]->v.param.t, &fe->env);
  }
  fb = fefork(fe);
  fb.fnret = ret;
  for (i = 0; i < nc; i++) { /* captures: the body sees them as locals */
    Ast   *cp = caps[i];
    Local *l = locfind(fe, cp->v.cap.name);

    if (!l)
      berr(cp, "the capture names nothing outside");
    locpush(&fb, cp->v.cap.name, l->ty, cp->v.cap.mut);
  }
  for (i = 0; i < np; i++)
    locpush(&fb, ps[i]->v.param.name, ts[i], ps[i]->v.param.mut);
  rblock(c->v.clos.body, &fb, ret);
  return tyfn(ts, np, ret);
}

/* is a place writable? The slot rules of 01-types.md: a mut binding,
 * a mut field under any binding, or a mut slot (a *mut, []mut, [N]mut,
 * or a mut tuple element) on the way down */
/* the mut of every pointer on the way: only a *mut lends
 * writability, so the chain must be *mut at each hop (README: "no
 * write through a shared pointer"). */
static int
ptrwritable(Type *t)
{
  while (t && (t->k == Typtr || t->k == Tymut)) {
    if (t->k == Typtr && (!t->t || t->t->k != Tymut))
      return 0;
    t = t->t;
  }
  return 1;
}

/* a place's base -- the chain of explicit derefs down to the name --
 * must be *mut all the way: an interior write rides on the pointer's
 * writability, which a plain *T does not lend. The types come from
 * rplace: writability is a fact about a place, and deriving it must
 * not move anything. */
static int
derefswritable(Ast *x, Fenv *fe)
{
  Type *t;

  if (!x)
    return 1;
  if (x->k == Nun && x->v.un.op == Tstar) { /* (*p).f, (*p)[i] */
    t = rplace(x->v.un.e, fe);              /* the pointer, as a place */
    if (!t || !ptrwritable(t))
      return 0;
    return derefswritable(x->v.un.e, fe);
  }
  return ptrwritable(rplace(x, fe));
}

static int
placewritable(Ast *p, Fenv *fe)
{
  switch (p->k) {
  case Npath: {
    Local *l = locfind(fe, p->v.path.segs[0]->v.seg.name);

    return l && l->mut;
  }
  case Naccess: { /* the two mut levels are orthogonal (01-types.md):
                   * a.b is writable by b's own mut, never by the
                   * binding's -- and a pointer base must be *mut all
                   * the way, for it lends what it lends (README) */
    Type *bt;
    usize i;

    if (!derefswritable(p->v.fld.e, fe))
      return 0; /* a *T lends nothing writable */
    bt = rplace(p->v.fld.e, fe);
    if (!bt)
      bt = rexpr(p->v.fld.e, fe, 0); /* a global or computed base */
    bt = derefthrough(bt);
    if (!bt)
      return 0;
    if (bt->k == Tyslice)
      return slicefield(bt, p->v.fld.name) != 0; /* the two are mut
                                                  * fields (01-types.md) */
    if (bt->k != Tystruct && bt->k != Tyunion)
      return 0;
    for (i = 0; i < bt->sym->nfields; i++)
      if (strcmp(bt->sym->fields[i].name, p->v.fld.name) == 0)
        return bt->sym->fields[i].mut; /* the field is its own slot */
    return 0;
  }
  case Nindex: {
    Type *bt;

    if (!derefswritable(p->v.n2.a, fe))
      return 0;
    bt = rplace(p->v.n2.a, fe);
    if (!bt)
      bt = rexpr(p->v.n2.a, fe, 0);
    if (!bt)
      return 0;
    if (bt->k == Tyslice || bt->k == Tyarray)
      return bt->t->k == Tymut; /* []mut T / [N]mut T */
    return 0;
  }
  case Nrangeindex: {
    Type *bt;

    if (!derefswritable(p->v.ridx.e, fe))
      return 0;
    bt = rplace(p->v.ridx.e, fe);
    if (!bt)
      bt = rexpr(p->v.ridx.e, fe, 0);
    return bt && bt->k == Tyarray && bt->t->k == Tymut;
  }
  case Nun:
    if (p->v.un.op == Tstar) { /* *p = v: p must be a *mut */
      Type *pt = rexpr(p->v.un.e, fe, 0);

      return pt && pt->k == Typtr && pt->t->k == Tymut;
    }
    return 0;
  default:
    return 0; /* a temporary: not writable */
  }
}

static void
rstmt(Ast *st, Fenv *fe)
{
  switch (st->k) {
  case Nlet: {
    Type *t = st->v.let.t ? rty(st->v.let.t, &fe->env) : 0;
    Type *et = st->v.let.e ? rexpr(st->v.let.e, fe, t) : 0;

    if (!t && !et)
      berr(st, "a let binds something: a type, a value, or both");
    while (et && et->k == Tymut) /* a place's mut layer is the
                                  * permission, not the type */
      et = et->t;
    if (t && et && !tysame(t, et)) {
      Type *c = st->v.let.e ? recoerce(st->v.let.e, t, fe) : 0;

      if (!c || !tysame(c, t))
        berr(st->v.let.e, "the binding is %s, the value is %s", btys(t), btys(et));
    }
    st->ty = t ? t : et; /* what the pattern binds, for the emitter */
    rpat(st->v.let.pat, t ? t : et, fe, st->v.let.mut);
    return;
  }
  case Nassign: {
    Tok   op = st->v.bin.op;
    Type *lt = rexpr(st->v.bin.l, fe, 0);
    Type *rt;

    if (!isplace(st->v.bin.l))
      berr(st->v.bin.l, "assignment needs a place on the left");
    if (!placewritable(st->v.bin.l, fe))
      berr(st->v.bin.l, "this place is not a mut slot (01-types.md)");
    if (touchconflict(st->v.bin.l, fe, 1))
      berr(st->v.bin.l, "this place is borrowed (01-types.md)");
    while (lt && lt->k == Tymut) /* the place's mut layer is the
                                  * permission, not the type */
      lt = lt->t;
    if (op == Teq) {
      rt = rexpr(st->v.bin.r, fe, lt);
      if (rt && lt && !tysame(rt, lt)) {
        Type *c = recoerce(st->v.bin.r, lt, fe);

        if (!c || !tysame(c, lt))
          berr(st->v.bin.r, "the place is %s, the value is %s", btys(lt), btys(rt));
      }
      return;
    }
    /* the compound forms: a = a op b, the operator's own rules */
    rt = rexpr(st->v.bin.r, fe, lt);
    {
      Type *res = 0;

      if (!binop(op == Tpluseq    ? Tplus
                 : op == Tminuseq ? Tminus
                 : op == Tstareq  ? Tstar
                 : op == Tslasheq ? Tslash
                 : op == Tshleq   ? Tshl
                                  : Tshr,
                 lt, rt, &res))
        berr(st, "this compound assignment does not fit %s and %s", btys(lt), btys(rt));
    }
    return;
  }
  case Nreturn: {
    Type *t = st->v.n1.e ? rexpr(st->v.n1.e, fe, fe->fnret) : tyunit();

    if (!fe->fnret)
      berr(st, "return outside a fn");
    if (!tysame(t, fe->fnret)) {
      Type *c = st->v.n1.e ? recoerce(st->v.n1.e, fe->fnret, fe) : 0;

      if (!c || !tysame(c, fe->fnret))
        berr(st, "the fn returns %s, this is %s", btys(fe->fnret), btys(t));
    }
    return;
  }
  case Nbreak:
  case Ncontinue:
    if (fe->loopd <= 0)
      berr(st, "%s outside a for", st->k == Nbreak ? "break" : "continue");
    return;
  case Nfor: {
    Ast  *body = st->v.forx.body;
    Fenv  fb = fefork(fe);
    usize nbase = fb.n;

    if (st->v.forx.cnst)
      berr(st, "const for arrives with const evaluation");
    /* the loop marks: moves of bindings from before the outermost
     * loop repeat every round (03-move.md) */
    fb.loopbase = fe->loopd > 0 ? fe->loopbase : fb.n;
    fb.loopd++;
    switch (st->v.forx.shape) {
    case FCOND: {
      Type *ct = rexpr(st->v.forx.a, &fb, 0); /* re-checked every round */

      if (!ct || ct->k != Tybool)
        berr(st->v.forx.a, "a for condition is a bool, this is %s", btys(ct));
      break;
    }
    case FLET: { /* for let pat = e: e is matched every round (10) */
      Type *et = rexpr(st->v.forx.b, &fb, 0);

      while (et && et->k == Tymut) /* a *mut read: the emitter
                                    * strips this too (emafor) */
        et = et->t;
      rpat(st->v.forx.a, et, &fb, 0);
      break;
    }
    case FIN: { /* for pat in e: what e yields, one binding a round */
      Type *et = rexpr(st->v.forx.b, &fb, 0);

      while (et && et->k == Tymut) /* ditto */
        et = et->t;
      if (!et)
        break;
      if (et->k == Tyslice ||
          et->k == Tyarray) /* a slice lends each
                             * element out: the binding is a pointer, never a move (10) */
        rpat(st->v.forx.a, typtr(et->t), &fb, 0);
      else if (et->k == Tyenum && et->sym == sym_option)
        rpat(st->v.forx.a, et->args[0], &fb, 0); /* ?T iterates T or ends */
      else
        berr(st->v.forx.b, "iterating %s arrives with its iterators (10)", btys(et));
      break;
    }
    default:
      berr(st, "this for shape is not one of the three");
    }
    rblock(body, &fb, 0);
    locpop(&fb, nbase); /* what the round bound -- and froze -- ends here */
    return;
  }
  case Ncif:
  case Ncfor:
    berr(st, "const control flow arrives with const evaluation");
    return; /* unreachable */
  case Nexprstmt:
    rexpr(st->v.n1.e, fe, 0);
    return;
  default: /* if, match, blocks: expressions in statement position */
    rexpr(st, fe, 0);
    return;
  }
}

/* -- the driver ------------------------------------------------------------ */

/* one fn's body: the parameters bind, the return is the want, and
 * the walk is statement-first (the last expression is the value) */
void
checkbodyfn(Sym *s, Ast *it)
{
  Fenv  fe;
  Env   env = envgparams(0, it->v.fn.gparams, vlen(it->v.fn.gparams));
  Ast **ps = it->v.fn.params;
  usize n = vlen(ps), i;
  Type *ret = s->fnty->t;

  memset(&fe, 0, sizeof fe);
  fe.env = env;
  fe.fnret = ret;
  for (i = 0; i < n; i++)
    locpush(&fe, ps[i]->v.param.name, s->fnty->args[i], ps[i]->v.param.mut);
  rblock(it->v.fn.body, &fe, ret);
}

/* one impl's member fns: the same env resolveimplmembers built --
 * Self is the impl's type, the impl's own generics are in scope */
void
checkbodyimpl(Sym *s, Ast *it)
{
  Env   env = envgparams(0, it->v.impl.gparams, vlen(it->v.impl.gparams));
  Ast **ms = it->v.impl.members;
  usize n = vlen(ms), i;

  env.impl = s;
  env = envpush(&env, "Self", s->ifort ? s->ifort : s->ipath);
  for (i = 0; i < n; i++)
    if (ms[i]->k == Nfn && ms[i]->v.fn.body) {
      Fenv  fe;
      Env   e2 = envgparams(&env, ms[i]->v.fn.gparams, vlen(ms[i]->v.fn.gparams));
      Ast **ps = ms[i]->v.fn.params;
      usize np = vlen(ps), j;
      Type *fnty = s->members[i].ty;

      memset(&fe, 0, sizeof fe);
      fe.env = e2;
      fe.fnret = fnty->t;
      for (j = 0; j < np; j++)
        locpush(&fe, ps[j]->v.param.name, fnty->args[j], ps[j]->v.param.mut);
      rblock(ms[i]->v.fn.body, &fe, fnty->t);
    }
}
