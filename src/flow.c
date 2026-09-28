/* flow.c -- pass 4's tool layer: the flow environment and the
 * helpers the body walk calls. Bindings live and die here, borrows
 * freeze and thaw (03-move.md), branches join conservatively, and
 * the diagnostics speak. The walk itself is body.c; body.h is the
 * face between the two. */

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "body.h"
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

Local *
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
usize
locfindi(Fenv *fe, char *name)
{
  usize i;

  for (i = fe->n; i > 0; i--)
    if (strcmp(fe->ls[i - 1].name, name) == 0)
      return i - 1;
  return fe->n;
}

/* one more binding -- lets are rare, the array is fresh each time */
void
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
void
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
Fenv
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
void
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
int
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

void
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
char *
btys(Type *t)
{
  static char     bufs[4][256];
  static unsigned which;

  return tysprint(bufs[which++ & 3u], sizeof bufs[0], t);
}

/* -- small type helpers --------------------------------------------------- */

/* the enum behind a ?T, or NULL -- narrowcond's own, not the walk's */
static Type *
optchild(Type *t)
{
  if (t && t->k == Tyenum && t->sym == sym_option && t->nargs == 1)
    return t->args[0];
  return 0;
}

/* what an E?T holds when it went wrong, or NULL; *ok gets the payload */
Type *
reschild(Type *t, Type **ok)
{
  if (t && t->k == Tyenum && t->sym == sym_result && t->nargs == 2) {
    *ok = t->args[0];
    return t->args[1];
  }
  return 0;
}

int
isintty(Type *t)
{
  return t && t->k == Tyint && t->num != IN_F32 && t->num != IN_F64;
}

int
isnumty(Type *t)
{
  return t && t->k == Tyint;
}

/* the value v fits the integer type t (resolve.c's rule, restated) */
int
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

/* a pointer is dereferenced as far as it needs to be to reach a
 * member: sp.b is (*sp).b, chain and all. A *mut T's pointee is the
 * mut slot mut T, so that layer comes off too (01-types.md). Lives
 * in type.c now -- the emitter's field walks need it too. */

/* a slice's two named slots (01-types.md): s.ptr is *T -- *mut T
 * for a []mut T, which is the Tymut child -- and s.len a usize.
 * Both behave like mut struct fields. */
Type *
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
int
isplace(Ast *e)
{
  if (e->k == Npath || e->k == Naccess || e->k == Nindex || e->k == Nrangeindex || e->k == Ntupidx)
    return 1;
  return e->k == Nun && e->v.un.op == Tstar;
}

/* the root binding of a place chain, walking down to it; *p stops at
 * p -- the memory it points at is not a binding the walk can see */
Local *
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
void
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

void
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
int
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

/* -- narrowing (01-types.md, Nullability) -------------------------------- */

/* the == None / != Err forms: *name gets the binding the cond checks,
 * *child its narrowed type, and the return says which branch the
 * narrowing lands in -- 1 for the then, -1 for the else, 0 for none */
int
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
void
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
void
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
int
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

Variant *
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
int
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
Member *
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
