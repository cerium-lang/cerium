/* flow.c -- pass 4's tool layer: the flow environment and the
 * helpers the body walk calls. Bindings live and die here, borrows
 * freeze and thaw (03-move.md), branches join conservatively, and
 * the diagnostics speak. The walk itself is body.c; body.h is the
 * face the pass's files share. */

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "body.h"
#include "check.h" /* the pattern order the finders share with the
                    * declaration's overlap check (04-generics.md) */
#include "eval.h"
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
  ls[fe->n].cv = 0;
  fe->ls = ls;
  fe->n++;
}

/* pop the bindings a block pushed -- and thaw what they borrowed:
 * a borrow lives as long as the binding that holds it (03-move.md).
 * The freeze's own book names the holder -- frzby is the first
 * borrower's slot -- so a binding outside the block frozen by one
 * inside it thaws here: the borrower is going, the freeze goes
 * with it. The borrower itself was never marked (its own frz is
 * the mark of something borrowing *it*), which is why the walk
 * below reads the frozen side, not the dying side */
void
locpop(Fenv *fe, usize nbase)
{
  usize j;

  for (j = 0; j < nbase; j++)
    if (fe->ls[j].frzby >= (int) nbase) {
      fe->ls[j].frz = FZ_NONE;
      fe->ls[j].frzby = -1;
      fe->ls[j].frzpath = 0;
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

/* the move picture, one bit per binding: a row's trial walks the
 * call's arguments and moves what it reads (03-move.md), and a row
 * that did not take them cannot leave its moves behind -- the next
 * row's walk would read them as gone. Nothing else in the
 * environment moves on an argument walk: narrowing is the branch
 * forms' own, borrows unwind through the freezes' own pictures, and
 * a let is a statement, not an expression (07-operators.md). */
int *
movsnap(Fenv *fe)
{
  int  *d;
  usize i;

  if (!fe->n)
    return 0;
  d = arenaalloc(fe->n * sizeof *d);
  for (i = 0; i < fe->n; i++)
    d[i] = fe->ls[i].dead;
  return d;
}

void
movrestore(Fenv *fe, int *snap)
{
  usize i;

  if (!snap)
    return;
  for (i = 0; i < fe->n; i++)
    fe->ls[i].dead = snap[i];
}

/* -- copy and drop (03-move.md) ------------------------------------------ */

int        hasdrop(Type *t); /* the exclusion's other half, below */
static int iscopy1(Type *t); /* the walk itself, iscopy's memoized core */

/* is a value of this type duplicated by an assignment rather than
 * moved? Typaram answers no: without its bounds resolved (dispatch),
 * move is the conservative reading, and a T that really is Copy gets
 * its bound checked when that pass arrives. The answer is memoized
 * on the type: types are interned, and a tuple's rows would ask
 * again on every read -- a pack's recursion walks them thousands of
 * times over (04-generics.md). */
int
iscopy(Type *t)
{
  int v;

  if (!t)
    return 0;
  if (t->copyknown == 1) /* the interned type asked once; the answer
                          * holds for every read after */
    return t->copyval;
  if (t->copyknown == 2) /* asked again under itself: an impl whose
                          * own bound is this very question is not
                          * the answer -- the same word the
                          * satisfies walk keeps, below */
    return 0;
  t->copyknown = 2;
  v = iscopy1(t);
  t->copyval = (u8) v;
  t->copyknown = 1;
  return v;
}

static int
iscopy1(Type *t)
{
  usize i;

  switch (t->k) {
  case Tyunit:
  case Tybool:
  case Tyint:
  case Tyvoidptr:
  case Typtr:
  case Tyslice:
  case Tydyn:
  case Tytype: /* a type reference is a compile-time label: no
                * bits, no destructor, nothing to move -- it is
                * stored in a field and passed along (08) */
    return 1;
  case Tymut: /* only under a slot; the answer is the child's */
    return iscopy(t->t);
  case Tytuple:
    if (hasdrop(t)) /* a destructor inside kills the copy (03-move.md) */
      return 0;
    for (i = 0; i < t->nargs; i++)
      if (!iscopy(t->args[i]))
        return 0;
    return 1;
  case Tyarray:
  case Tyenum:
    if (hasdrop(t)) /* a destructor inside kills the copy (03-move.md) */
      return 0;
    for (i = 0; i < t->nargs; i++)
      if (!iscopy(t->args[i]))
        return 0;
    return t->k == Tyarray ? iscopy(t->t) : 1;
  case Tystruct: {
    Sym *s = t->sym;

    if (hasdrop(t)) /* a destructor inside kills the copy (03-move.md) */
      return 0;
    if (!implfor(sym_copy, t, 0)) /* the impl the spec asks for: a
                                   * struct is Copy when it says it
                                   * is, the row accepted only because
                                   * every field already is (03) --
                                   * the fields walk below, the
                                   * instance's own words
                                   * (04-generics.md) */
      return 0;
    for (i = 0; i < s->nfields; i++) {
      Type *ft = s->fields[i].ty;

      if (t->nargs == (usize) s->ngparams) /* a generic struct is
                                            * Copy under the
                                            * instance: Box<i32>
                                            * is, Box<File> is not */
        ft = gsubst(ft, s->gparams, t->args, t->nargs);
      if (!iscopy(ft))
        return 0;
    }
    return 1;
  }
  case Tyunion: /* a union forgets (03): always Copy */
    return 1;
  case Typaram: { /* a parameter copies under a Copy bound -- the
                   * bound promised it, or nobody could have written
                   * the impl that supplies it (05-traits.md). The
                   * bound's cache names the trait itself: a Copy of
                   * the file's own making grants nothing */
    Ast **bs = t->gp->v.gp.bounds;
    usize i;

    for (i = 0; i < vlen(bs); i++)
      if (bs[i]->v.path.sym == sym_copy)
        return 1;
    return 0;
  }
  default: /* Tytrait, Typroj, Tyfn */
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
 * Both read for every ABI that hands a pair to C; neither writes --
 * @slice builds the view whole. */
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
  while (e->k == Naccess || e->k == Nindex || e->k == Nrangeindex || e->k == Ntupidx) {
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
    } else { /* an index, a row, a slice, or a long name: the whole root */
      path[0] = 0;
      n = 0;
    }
    if (e->k == Naccess)
      e = e->v.fld.e;
    else if (e->k == Nindex)
      e = e->v.n2.a;
    else if (e->k == Ntupidx)
      e = e->v.tup.e;
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
 * borrowed place itself; an index, a row, or a deref freezes the root. The
 * freeze thaws when the binding that holds the borrow dies (locpop). */
void
freeze(Ast *place, Fenv *fe, int mut, int by)
{
  char   buf[256];
  Local *root;

  if (fe->nofreeze)
    return; /* a borrow being spent by the deref around it: it dies
             * the moment it is made, so it holds nothing (01) */
  root = placeroot(place, fe, buf, sizeof buf);
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

/* must the statement leave -- return/break/continue, a call to a
 * #[noreturn] fn, or a branch where every path does (03-move.md,
 * Timing; 10-iteration.md) */
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
  case Ncall: { /* the callee resolved the way the checker's own call
                 * site does -- a free fn by its path, the mark on its
                 * Sym. The judgement stays syntactic: the name's shape
                 * is all it reads, no control flow traced
                 * (10-iteration.md) */
    Ast  *f = st->v.call.f;
    Ast **segs;
    usize nsegs, k;
    Ns   *ns;
    Sym  *s;

    if (f->k != Npath)
      return 0; /* an indirect callee, or the method sugar: a local's
                 * fn value and a receiver's own are not this
                 * milestone's */
    segs = f->v.path.segs;
    nsegs = vlen(segs);
    k = nshead(segs, nsegs, &ns, f->v.path.root);
    if (k == nsegs || nsegs - k != 1)
      return 0; /* a namespace walk, or Type::member(...): the latter
                 * arrives with methods of its own */
    s = k ? nsitem(ns, segs[k]->v.seg.name) : symfind(segs[k]->v.seg.name);
    return s && s->kind == Sfn && s->noreturn;
  }
  case Nlet: /* the binding's own init leaving takes the statement with
              * it: the code after never runs (10-iteration.md) */
    return st->v.let.e && mustexit(st->v.let.e);
  case Nexprstmt: /* a bare call stands alone: its own leaving is the
                   * statement's */
    return mustexit(st->v.n1.e);
  case Nblock: {
    Ast **ss = st->v.blk.stmts;
    usize n = vlen(ss);

    return n ? mustexit(ss[n - 1]) : mustexit(st->v.blk.tail);
  }
  case Nif:
  case Ncif: /* both branches leaving is leaving, whichever runs -- a
              * syntax fact the black box cannot blur (03-move.md) */
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
 * walking both in step, a Typaram on the left records the right.
 * The const generic parameters bind their numbers beside the types
 * -- [N]T meeting [3]u32 records N = 3 -- and a length the argument
 * itself holds as a parameter stays the box, the outer instance's
 * re-check answering it (08-reflection.md) */
int
gunifyv(Type *sig, Type *arg, Ast **gps, Type **tys, Val **gcvals, usize n)
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
    return gunifyv(sig->t, arg->t, gps, tys, gcvals, n);
  case Tyarray:
    if (sig->gp) { /* [N]T: the length is the binding's own number */
      for (i = 0; i < n; i++)
        if (sig->gp == gps[i]) {
          if (!tys[i])
            tys[i] = tyint(IN_USIZE); /* the slot's shape, and the
                                       * infer check's non-empty */
          if (arg->gp)
            return gunifyv(sig->t, arg->t, gps, tys, gcvals,
                           n); /* a length another generic names: the
                                * box, the re-check's to bind */
          if (!gcvals[i]) {
            gcvals[i] = arenaalloc(sizeof **gcvals);
            *gcvals[i] = valint(arg->n, tyint(IN_USIZE));
          } else if (gcvals[i]->i != arg->n)
            return 0; /* the same length twice, two numbers: no fit */
          return gunifyv(sig->t, arg->t, gps, tys, gcvals, n);
        }
      return gunifyv(sig->t, arg->t, gps, tys, gcvals, n); /* someone else's */
    }
    if (sig->n != arg->n)
      return 0;
    return gunifyv(sig->t, arg->t, gps, tys, gcvals, n);
  case Tytuple: { /* a spread row among the sig's: the pack the rows
                   * walk, the argument's own rows from the same
                   * place feeding it -- each row meeting the
                   * template alone (the ...mut Ts spelling's wrapper
                   * among the meetings), the pack bound to the rows
                   * the meetings leave, the same gather a
                   * reference's arguments take (04-generics.md) */
    usize sp = sig->nargs;

    if (!sig->nargs && arg->k == Tyunit)
      return 1; /* the empty rows, both spellings of them */
    for (i = 0; i < sig->nargs; i++)
      if (sig->args[i]->k == Tyspread) {
        sp = i;
        break;
      }
    if (sp == sig->nargs) { /* no spread walks here: the rows meet
                             * one a one */
      if (sig->nargs != arg->nargs)
        return 0;
      for (i = 0; i < sig->nargs; i++)
        if (!gunifyv(sig->args[i], arg->args[i], gps, tys, gcvals, n))
          return 0;
      return 1;
    }
    if (sp != sig->nargs - 1 || arg->nargs < sp)
      return 0; /* the spread walks the tail alone -- one pack a
                 * tuple's own (04-generics.md) */
    for (i = 0; i < sp; i++)
      if (!gunifyv(sig->args[i], arg->args[i], gps, tys, gcvals, n))
        return 0;
    {
      Ast   *gp = sig->args[sp]->gp;
      Type  *tmpl = sig->args[sp]->t;
      usize  nr = arg->nargs - sp;
      usize  j = n;
      Type **rows;

      for (i = 0; i < n; i++)
        if (gps[i] == gp) {
          j = i;
          break;
        }
      if (nr == 1 && arg->args[sp]->k == Tyspread)
        /* the argument's own spread: template meets template -- a
         * generic body's pass-through, the packs standing for each
         * other, the real binding the instance's re-check makes
         * (04-generics.md) */
        return gunifyv(tmpl, arg->args[sp]->t, gps, tys, gcvals, n);
      if (j == n)
        return 1; /* someone else's pack: nothing here to bind */
      rows = nr ? tyargs(nr) : 0;
      { /* each row meets the template alone: the meeting's own
         * bindings -- the pack's its row, the rest this call's --
         * merge back, the pack's alone gathering into the tuple
         * the binding is */
        usize k;

        for (k = 0; k < nr; k++) {
          Type **loc = n ? tyargs(n) : 0;
          Val  **lcv = gcvals ? arenaalloc(n * sizeof *lcv) : 0;

          memcpy(loc, tys, n * sizeof *loc);
          if (lcv)
            memcpy(lcv, gcvals, n * sizeof *lcv);
          loc[j] = 0; /* the pack's slot fresh: this row its own */
          if (lcv)
            lcv[j] = 0;
          if (!gunifyv(tmpl, arg->args[sp + k], gps, loc, lcv, n))
            return 0;
          rows[k] = loc[j];
          if (!rows[k])
            return 0; /* the template names the pack: a row that
                       * binds it nothing fits none of them */
          {           /* the other slots: a binding the meeting added joins,
                       * one it clashed with the walk already refused */
            usize q;

            for (q = 0; q < n; q++)
              if (q != j) {
                if (!tys[q])
                  tys[q] = loc[q];
                if (lcv && !gcvals[q])
                  gcvals[q] = lcv[q];
              }
          }
        }
      }
      { /* the gather: the pack's binding the rows the meetings
         * left -- a binding an earlier argument already made, the
         * same rows must repeat (04-generics.md) */
        Type *g = tytuple(rows, nr);

        if (tys[j] && !tysame(tys[j], g))
          return 0;
        tys[j] = g;
      }
      return 1;
    }
  }
  case Tyfn:
    if (sig->nargs != arg->nargs)
      return 0;
    for (i = 0; i < sig->nargs; i++)
      if (!gunifyv(sig->args[i], arg->args[i], gps, tys, gcvals, n))
        return 0;
    return gunifyv(sig->t, arg->t, gps, tys, gcvals, n);
  case Tystruct:
  case Tyenum:
  case Tyunion:
  case Tytrait:
  case Tydyn: /* named: the same sym, then the arguments in step */
    if (sig->sym != arg->sym || sig->nargs != arg->nargs)
      return 0;
    for (i = 0; i < sig->nargs; i++)
      if (!gunifyv(sig->args[i], arg->args[i], gps, tys, gcvals, n))
        return 0;
    return 1;
  default:
    return tysame(sig, arg); /* interned: equal or not, nothing to bind inside */
  }
}

int
gunify(Type *sig, Type *arg, Ast **gps, Type **tys, usize n) /* no
                                                              * const
                                                              * length
                                                              * among
                                                              * these
                                                              * (04) */
{
  return gunifyv(sig, arg, gps, tys, 0, n);
}

/* -- inherent impl members ------------------------------------------------ */

/* the member of an inherent impl for the named type, or NULL: pass
 * 3's table, walked. *imp, when not NULL, receives the impl that
 * supplied it -- the caller asks it about genericity. Trait impls
 * are not looked at here -- those calls arrive with dispatch. */
Member *
inherentfind(Sym *s, const char *name, Sym **imp)
{
  usize i;

  if (imp)
    *imp = 0;
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
      if (m) {
        if (imp)
          *imp = im;
        return m;
      }
    }
  }
  return 0;
}

/* does an impl's target pattern fit this type, and what does it bind
 * the pattern's variables to? The same walk gunify runs, with every
 * variable owned by the impl: a repeated one must land the same type
 * twice ((T, T) in 04-generics.md), an open slot stays open only
 * until the walk ends -- the caller demands them all bound. */
static int
implatch(Type *pat, Type *ty, Ast **gps, Type **tys, usize n)
{
  usize i;

  if (!pat || !ty)
    return 0;
  if (pat->k == Typaram) {
    for (i = 0; i < n; i++)
      if (pat->gp == gps[i]) {
        if (pat->gp->v.gp.pack) { /* the pack: the whole tuple the
                                   * receiver's rows make, the
                                   * binding Ts = (i32, u8) the same
                                   * unification a handed-over
                                   * tuple makes (04-generics.md) */
          if (ty->k != Tytuple && ty->k != Tyunit)
            return 0; /* the pack stands for tuples, this is not one */
        }
        if (!tys[i]) {
          tys[i] = ty;
          return 1;
        }
        return tysame(tys[i], ty);
      }
    return 0; /* someone else's variable: not this pattern's to bind */
  }
  if (pat->k != ty->k)
    return 0;
  switch (pat->k) {
  case Typtr:
  case Tyslice:
  case Tymut:
    return implatch(pat->t, ty->t, gps, tys, n);
  case Tyarray:
    if (pat->n != ty->n)
      return 0;
    return implatch(pat->t, ty->t, gps, tys, n);
  case Tytuple:
  case Tystruct:
  case Tyenum:
  case Tyunion:
  case Tytrait:
  case Tydyn:
    if (pat->sym != ty->sym || pat->nargs != ty->nargs)
      return 0;
    for (i = 0; i < pat->nargs; i++)
      if (!implatch(pat->args[i], ty->args[i], gps, tys, n))
        return 0;
    return 1;
  default:
    return tysame(pat, ty);
  }
}
/* does a destructor live in this type -- its own impl, or a field's,
 * inherited (03-move.md)? The exclusion's other half: what iscopy is
 * not. A parameter's is its instantiation's business; the
 * monomorphized re-check reads the real type. */
int
hasdrop(Type *t)
{
  usize i, j;

  if (!t)
    return 0;
  switch (t->k) {
  case Tymut: /* a permission layer, not a type of its own: an
               * element's mut ness changes nothing about its
               * destructor, the structural walk peeling it the same
               * way (03-move.md) */
    return hasdrop(t->t);
  case Tystruct: {
    Sym *s = t->sym;

    for (j = 0; j < chk_nimpls; j++) {
      Sym *im = chk_impls[j];

      if (im->ifort && im->ipath && im->ipath->sym == sym_drop && tysame(im->ifort, t))
        return 1;
    }
    for (i = 0; i < s->nfields; i++) {
      Type *ft = s->fields[i].ty;

      if (t->nargs == (usize) s->ngparams) /* a generic struct drops
                                            * under the instance */
        ft = gsubst(ft, s->gparams, t->args, t->nargs);
      if (hasdrop(ft))
        return 1;
    }
    return 0;
  }
  case Tyenum:
  case Tytuple:
    for (i = 0; i < t->nargs; i++)
      if (hasdrop(t->args[i]))
        return 1;
    return 0;
  case Tyarray:
    return hasdrop(t->t);
  default: /* scalars, pointers, unions: nothing to destruct */
    return 0;
  }
}

/* does a type answer the bound an impl's parameter carries? Copy has
 * no impls anywhere -- it is what a type is, read structurally
 * against the exclusion (03-move.md, 04-generics.md) -- so the
 * question is iscopy's; every other trait's is the impl table's. */
int
boundsok(Sym *im, Type **tys)
{
  Ast **gps = im->decl->v.impl.gparams;
  usize i, j;

  for (i = 0; i < im->ngparams; i++) {
    Ast **bs = gps[i]->v.gp.bounds;

    for (j = 0; j < vlen(bs); j++) {
      if (gps[i]->v.gp.pack) { /* the bound holds every row: the
                                * whole tuple asked as one would
                                * find this impl itself answering --
                                * the row-by-row question is the
                                * honest one (04-generics.md) */
        usize rn = tys[i]->k == Tytuple ? tys[i]->nargs : 0;
        usize ri;

        for (ri = 0; ri < rn; ri++)
          if (!boundsatisfies(bs[j], tys[i]->args[ri], gps, tys, im->ngparams, 0, 0, 0, 0))
            return 0; /* a row the bound does not answer */
        continue;     /* the empty pack: every row it has answers */
      }
      if (!boundsatisfies(bs[j], tys[i], gps, tys, im->ngparams, 0, 0, 0, 0))
        return 0; /* the receiver does not answer this bound */
    }
  }
  return 1;
}

/* one impl's fit for a type, the receiver's binding alone: the
 * target pattern matched, and whatever it lands comes back through
 * *tysp -- impl->ngparams slots, or NULL for an exact target. The
 * slots a pattern's own shape cannot land stay open: a row whose
 * variables live only in the trait's arguments (07-operators.md)
 * waits on the call's arguments to finish them. */
static int
implfitp(Sym *im, Type *t, Type ***tysp)
{
  Type  *pat = im->ifort ? im->ifort : im->ipath;
  Type **tys;
  usize  j;

  if (im->ngparams) {
    tys = tyargs(im->ngparams);
    for (j = 0; j < im->ngparams; j++)
      tys[j] = 0;
    if (!implatch(pat, t, im->gparams, tys, im->ngparams))
      return 0;
    if (tysp)
      *tysp = tys;
    return 1;
  }
  if (!tysame(pat, t))
    return 0;
  if (tysp)
    *tysp = 0;
  return 1;
}

/* the finished question -- the receiver's walk, every variable it
 * could name landed, every bound answered (04-generics.md): the
 * finders' own ask, where no arguments follow to bind the rest. */
static int
implfit(Sym *im, Type *t, Type ***tysp)
{
  Type **tys;
  usize  g;

  if (!implfitp(im, t, &tys))
    return 0;
  if (!tys) {
    if (tysp)
      *tysp = 0;
    return 1;
  }
  for (g = 0; g < im->ngparams; g++)
    if (!tys[g])
      return 0; /* the pattern left a slot open: not this one */
  if (!boundsok(im, tys))
    return 0; /* a bound the receiver does not answer */
  if (tysp)
    *tysp = tys;
  return 1;
}

/* the joint specificity order the declaration check ran: the
 * whole row first -- a trait impl's arguments beside its
 * for-type (04-generics.md) -- then bounds. Among one type's
 * matches disjointness never differs them -- both matched the
 * same type -- so this walk alone settles the pick. */
static int
implspecific(Sym *a, Sym *b)
{
  int ab, ba;

  if (a->ifort) { /* a trait row: its arguments are its shape too */
    ab = rowspec(a, b);
    ba = rowspec(b, a);
  } else {
    Type *fa = a->ifort ? a->ifort : a->ipath;
    Type *fb = b->ifort ? b->ifort : b->ipath;

    ab = specializes(fa, fb);
    ba = specializes(fb, fa);
  }
  if (ab != ba)
    return ab;                                  /* strictly ordered by shape */
  return boundsincl(a, b) && !boundsincl(b, a); /* equal shape: bounds */
}

/* the same walk keyed by a receiver's full type: the matches ordered
 * by the specialization the declaration checked (04-generics.md) --
 * the most specific wins, exact targets above patterns, and a bound
 * a binding cannot answer keeps its impl out of the running. The
 * member names which impls run at all; the order picks among those
 * that do. */
Member *
inherentfindt(Type *t, const char *name, Sym **imp, Type ***tysp)
{
  usize   i, j;
  Sym    *best = 0;
  Type  **btys = 0;
  Member *bm = 0;

  if (imp)
    *imp = 0;
  if (tysp)
    *tysp = 0;
  if (!t)
    return 0;
  for (i = 0; i < chk_nimpls; i++) {
    Sym    *im = chk_impls[i];
    Type  **tys;
    Member *m = 0;

    if (im->ifort || !im->ipath || im->ipath->sym != t->sym)
      continue;
    for (j = 0; j < im->nmembers; j++)
      if (strcmp(im->members[j].name, name) == 0) {
        m = &im->members[j];
        break;
      }
    if (!m)
      continue; /* this impl does not carry the member */
    if (!implfit(im, t, &tys))
      continue;
    if (!best || implspecific(im, best)) {
      best = im;
      btys = tys;
      bm = m;
    }
  }
  if (!best)
    return 0;
  if (imp)
    *imp = best;
  if (tysp && btys)
    *tysp = btys;
  return bm;
}

/* the impl of this trait for this type, any member of it: the
 * question a handle's construction asks (06-dispatch.md). The same
 * order the finders keep -- the most specific fit, bounds answered. */
Sym *
implfor(Sym *trait, Type *t, Type ***tysp)
{
  usize  i;
  Sym   *best = 0;
  Type **btys = 0;

  if (tysp)
    *tysp = 0;
  if (!t)
    return 0;
  for (i = 0; i < chk_nimpls; i++) {
    Sym   *im = chk_impls[i];
    Type **tys;

    if (!im->ifort || !im->ipath || im->ipath->sym != trait)
      continue;
    if (!implfit(im, t, &tys))
      continue;
    if (!best || implspecific(im, best)) {
      best = im;
      btys = tys;
    }
  }
  if (best && tysp && btys)
    *tysp = btys;
  return best;
}

/* a trait impl's member for this type, the trait named --
 * Trait::method(&p) spells both out, so the walk narrows to that
 * trait's impls. The most specific fit wins, exactly as the sugar's
 * and a handle's walks (04-generics.md); pass 3 kept incomparables
 * out, so the order here is total. */
Member *
implfind(Sym *trait, Type *t, const char *name, Sym **imp, Type ***tysp)
{
  usize   i, j;
  Sym    *best = 0;
  Type  **btys = 0;
  Member *bm = 0;

  if (imp)
    *imp = 0;
  if (tysp)
    *tysp = 0;
  if (!t)
    return 0;
  for (i = 0; i < chk_nimpls; i++) {
    Sym    *im = chk_impls[i];
    Type  **tys;
    Member *m = 0;

    if (!im->ifort || !im->ipath || im->ipath->sym != trait)
      continue;
    for (j = 0; j < im->nmembers; j++)
      if (strcmp(im->members[j].name, name) == 0) {
        m = &im->members[j];
        break;
      }
    if (!m)
      continue;
    if (!implfit(im, t, &tys))
      continue;
    if (!best || implspecific(im, best)) {
      best = im;
      btys = tys;
      bm = m;
    }
  }
  if (!best)
    return 0;
  if (imp)
    *imp = best;
  if (tysp && btys)
    *tysp = btys;
  return bm;
}

/* the trait's rows that carry this member and fit this type, the
 * most specific first (07-operators.md). The receiver orders them
 * -- implspecific, the declaration check's own total order among
 * one type's rows -- but a row's signature is a claim about the
 * call's other arguments too, and the spelled arguments decide
 * among the rows the receiver could not. A row whose variables
 * the receiver alone cannot land joins the walk with its slots
 * open (implfitp): the trial binds them from the arguments, the
 * same landing a generic fn's own call makes (07-operators.md).
 * The caller walks them in this order: the first row that takes
 * the arguments is the call's, and the specificity order keeps
 * the pick the receiver's own when the arguments fit more than
 * one. */
usize
implcands(Sym *trait, Type *t, const char *name, Implcand *cs, usize cap)
{
  usize i, j, nc = 0;

  if (!t)
    return 0;
  for (i = 0; i < chk_nimpls; i++) {
    Sym     *im = chk_impls[i];
    Type   **tys;
    Member  *m = 0;
    Implcand c;

    if (!im->ifort || !im->ipath || im->ipath->sym != trait)
      continue;
    for (j = 0; j < im->nmembers; j++)
      if (strcmp(im->members[j].name, name) == 0) {
        m = &im->members[j];
        break;
      }
    if (!m)
      continue;
    if (!implfitp(im, t, &tys))
      continue;
    if (nc == cap)
      continue; /* a pathological table: the first rows carry the
                 * answer anyway */
    c.imp = im;
    c.m = m;
    c.tys = tys;
    for (j = nc; j > 0 && implspecific(im, cs[j - 1].imp); j--)
      cs[j] = cs[j - 1];
    cs[j] = c;
    nc++;
  }
  return nc;
}

/* the same walk for the sugar: no trait named, so every impl of
 * every trait that carries this member and fits this type is a
 * candidate. The inherent table was already walked and came up
 * empty, so what lands here is a trait method by elimination. The
 * order crosses trait lines: the most specific fit first, and
 * impls incomparable across traits keep the declaration's first --
 * the explicit form is how a crossed sugar is disambiguated
 * (05-traits.md). Slots the receiver cannot land stay open here
 * too, for the arguments to finish (implfitp, 07-operators.md). */
usize
traitcands(Type *t, const char *name, Implcand *cs, usize cap)
{
  usize i, j, nc = 0;

  if (!t)
    return 0;
  for (i = 0; i < chk_nimpls; i++) {
    Sym     *im = chk_impls[i];
    Type   **tys;
    Member  *m = 0;
    Implcand c;

    if (!im->ifort || !im->ipath)
      continue;
    for (j = 0; j < im->nmembers; j++)
      if (im->members[j].kind == Mfn && im->members[j].sym &&
          strcmp(im->members[j].name, name) == 0) { /* a row with no
                                                     * fn of its own -- a closure literal's -- names
                                                     * a vtable slot,
                                                     * never a call
                                                     * (05-traits.md) */
        m = &im->members[j];
        break;
      }
    if (!m)
      continue;
    if (!implfitp(im, t, &tys))
      continue;
    if (nc == cap)
      continue;
    c.imp = im;
    c.m = m;
    c.tys = tys;
    for (j = nc; j > 0 && implspecific(im, cs[j - 1].imp); j--)
      cs[j] = cs[j - 1];
    cs[j] = c;
    nc++;
  }
  return nc;
}

/* does this type implement this trait? A bound's question at a call
 * site (04-generics.md): the impl table answers, and what it finds
 * carries no binding -- the question is satisfied, not resolved.
 * The arguments the bound spelled ride along: a row whose own head
 * named others does not answer it (07-operators.md). Copy is the
 * exception: no impl table ever answers it, the
 * structure does (03-move.md). A blanket impl whose bound asks the
 * question again under itself proves nothing -- another impl must
 * answer, or the bound fails. */
static struct
{
  Sym  *tr;
  Type *ty;
} satq[64]; /* the asks in flight, cycle depth's bound */
static usize satn;

int
implsatisfies(Sym *trait, Type *t, Type **targs, usize ntargs, Ast **pins, Type **ptys, usize npins)
{
  usize i;
  int   r;

  if (!t)
    return 0;
  if (trait == sym_copy)
    return iscopy(t);    /* the Copy question entire: the marker's own
                          * answer, impl and fields both, asked without
                          * the table walk the other traits take -- the
                          * cycle an impl's bound could spell is broken
                          * where iscopy breaks it (03-move.md) */
  if (t->k == Typaram) { /* the type is a parameter of the fn that
                          * asked: its own declaration's bounds are
                          * the answer. Everything the body does to T
                          * must be justified by a bound, checked once
                          * at the declaration, not per-instantiation
                          * (04-generics.md) -- and passing T on is
                          * done by that promise. A bound that spells
                          * the trait's arguments answers only the ask
                          * that spells the same ones, the tail
                          * falling to the defaults the way any ask's
                          * does, Self the parameter under the bound.
                          * The pins the ask demands it must spell
                          * too, the same words: a promise it did not
                          * make is not kept */
    Ast **bs = t->gp->v.gp.bounds;
    usize bi, nb = vlen(bs);

    for (bi = 0; bi < nb; bi++) {
      Ast   *bnd = bs[bi];
      Type **btys = bnd->v.path.tys;
      Ast  **pas = bnd->v.path.segs[0]->v.seg.args;
      usize  pna = vlen(pas);
      usize  nn = 0, j, pa, pi;

      for (pa = 0; pa < pna; pa++) /* the pins stand behind the
                                    * arguments again (04) */
        if (pas[pa]->k != Nassoc)
          nn++;
      if (bnd->v.path.sym != trait)
        continue;
      if (nn < trait->ngparams &&
          !(trait->ngparams && trait->gparams[trait->ngparams - 1]->v.gp.pack &&
            nn >= trait->ngparams - 1))
        /* the pack needs no tail: its slot is the whole tuple, the
         * empty one included (04-generics.md) -- only a prefix left
         * unspelled asks the defaults */
        btys = dflttail(trait, btys, nn, 0, t, bnd);
      nn = trait->ngparams; /* the cache's own count: the pack's slot one */
      for (j = 0; j < nn && j < ntargs; j++)
        if (!tysame(btys[j], targs[j]))
          break;
      if (j == nn && j == ntargs) {
        for (pi = 0; pi < npins; pi++) {
          Ast *own = 0;

          for (pa = 0; pa < pna; pa++)
            if (pas[pa]->k == Nassoc &&
                strcmp(pas[pa]->v.assoc.name, pins[pi]->v.assoc.name) == 0) {
              own = pas[pa];
              break;
            }
          if (!own || !own->v.assoc.rt || !tysame(own->v.assoc.rt, ptys[pi]))
            break;
        }
        if (pi == npins)
          return 1;
      }
    }
    return 0;
  }
  if (t->k == Tyfn && trait == sym_fn) { /* a fn pointer is Fn for its
                                          * own signature, the compiler's
                                          * own knowledge, the way Copy's
                                          * marker is: no impl a file
                                          * spells, no row the table
                                          * holds (05-traits.md). The
                                          * pins it answers by the same
                                          * knowledge -- the return is
                                          * the family's Output */
    usize j, pi;

    if (trait->ngparams && trait->gparams[trait->ngparams - 1]->v.gp.pack) {
      /* the pack's slot, the whole tuple: the signature's own rows
       * the same spelling the bound fed it, one for one
       * (04-generics.md) */
      Type *pk = ntargs == trait->ngparams ? targs[trait->ngparams - 1] : 0;
      usize rows = pk && pk->k == Tytuple ? pk->nargs : 0;

      if (!pk || rows != t->nargs)
        return 0;
      for (j = 0; j < rows; j++)
        if (!tysame(pk->args[j], t->args[j]))
          return 0;
    } else {
      if (ntargs != t->nargs)
        return 0;
      for (j = 0; j < ntargs; j++)
        if (!tysame(targs[j], t->args[j]))
          return 0;
    }
    for (pi = 0; pi < npins; pi++)
      if (!tysame(ptys[pi], t->t))
        return 0;
    return 1;
  }
  for (i = 0; i < satn; i++)
    if (satq[i].tr == trait && tysame(satq[i].ty, t))
      return 0; /* asked again under itself: this impl is not the answer */
  if (satn >= sizeof satq / sizeof satq[0])
    return 0;
  satq[satn].tr = trait;
  satq[satn].ty = t;
  satn++;
  r = 0;
  for (i = 0; i < chk_nimpls; i++) {
    Sym *im = chk_impls[i];

    if (!im->ifort || !im->ipath || im->ipath->sym != trait)
      continue;
    if (ntargs && im->ipath->nargs >= ntargs) {
      /* the row's own head must take the arguments the bound spelled
       * -- the same pattern walk its own receiver takes, the row's
       * variables bound against them (07-operators.md) */
      Type **rt = tyargs(im->ngparams);
      usize  j;

      for (j = 0; j < im->ngparams; j++)
        rt[j] = 0;
      for (j = 0; j < ntargs; j++)
        if (!implatch(im->ipath->args[j], targs[j], im->gparams, rt, im->ngparams))
          break;
      if (j < ntargs)
        continue; /* this row's arguments are other ones */
    }
    {
      Type **fit = 0;

      if (implfit(im, t, &fit)) { /* the receiver's own match, its
                                   * variables landed -- the binding
                                   * the pins read */
        usize pk;                 /* the row's own members answer the pins, each the
                                   * same type the ask pinned (04-generics.md) */

        for (pk = 0; pk < npins; pk++) {
          Member *m = 0;
          Type   *val;
          usize   mj;

          for (mj = 0; mj < im->nmembers; mj++)
            if (im->members[mj].kind == Mtype &&
                strcmp(im->members[mj].name, pins[pk]->v.assoc.name) == 0) {
              m = &im->members[mj];
              break;
            }
          if (!m)
            break;
          val = fit ? gsubst(m->val, im->gparams, fit, im->ngparams) : m->val;
          if (!tysame(val, ptys[pk]))
            break;
        }
        if (pk == npins) {
          r = 1;
          break;
        }
      }
    }
  }
  satn--;
  return r;
}

/* a bound's question with its own arguments: the trait and the type
 * arguments it spelled, read where the bound was written, the
 * owner's binding landed in them -- a bound may name the parameters
 * around it (04-generics.md). A method's owner is an impl: its own
 * parameters land first (ig/itys, or ni 0), the member's around
 * them. The pins the bound spells take the same two rounds, the
 * ask's own substitutions landing in them before they are asked.
 * The tail the bound left unspelled is the trait's own defaults,
 * this type the Self they read: T: Add asks Add<T>, the row's own
 * Self (07-operators.md). The arguments the question asked come
 * back through *ta, for the diagnostic that names them; NULL says
 * nobody will. */
int
boundsatisfies(Ast *b, Type *t, Ast **gps, Type **tys, usize n, Type ***ta, Ast **ig, Type **itys,
               usize ni)
{
  Sym   *tr = b->v.path.sym;
  Type **btys = b->v.path.tys;
  Ast  **pas = b->v.path.segs[0]->v.seg.args;
  usize  na = vlen(pas);
  usize  nb = 0; /* the positional alone: the pins stand behind
                  * every argument (04-generics.md) */
  Ast  **pins = 0;
  Type **ptys = 0;
  usize  np = 0;
  usize  k, j;

  for (k = 0; k < na; k++)
    if (pas[k]->k == Nassoc)
      np++;
    else
      nb++;
  if (tr->ngparams && tr->gparams[tr->ngparams - 1]->v.gp.pack && nb >= tr->ngparams - 1)
    nb = tr->ngparams; /* the pack's slot: the whole tuple, one slot
                        * whatever the spelling spelled -- the count
                        * the cache holds, not the words (04-generics.md) */
  if (np) {            /* the bound's own pins, resolved where it was written
                        * and cached on their nodes (04) */
    pins = arenaalloc(np * sizeof *pins);
    ptys = tyargs(np);
    for (k = 0, j = 0; k < na; k++)
      if (pas[k]->k == Nassoc) {
        pins[j] = pas[k];
        ptys[j] = pas[k]->v.assoc.rt;
        j++;
      }
    if (ig)
      for (j = 0; j < np; j++)
        ptys[j] = gsubst(ptys[j], ig, itys, ni);
    if (gps)
      for (j = 0; j < np; j++)
        ptys[j] = gsubst(ptys[j], gps, tys, n);
  }
  if (btys && ig) { /* the impl's own words first: a bound a method
                     * spells may name the parameters above it
                     * (04-generics.md) */
    Type **st = tyargs(nb);
    usize  j2;

    for (j2 = 0; j2 < nb; j2++)
      st[j2] = gsubst(btys[j2], ig, itys, ni);
    btys = st;
  }
  if (btys && gps) {
    Type **st = tyargs(nb);
    usize  j2;

    for (j2 = 0; j2 < nb; j2++)
      st[j2] = gsubst(btys[j2], gps, tys, n);
    btys = st;
  }
  if (nb < tr->ngparams) {
    btys = dflttail(tr, btys, nb, 0, t, b);
    nb = tr->ngparams;
  }
  if (ta)
    *ta = btys;
  return implsatisfies(tr, t, btys, nb, pins, ptys, np);
}
