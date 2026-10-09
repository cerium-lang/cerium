/* body.c -- pass 4: fn bodies.
 *
 * Everything before this pass looked at declarations; this one walks
 * the statements. One environment serves the whole walk:
 *
 *   - types: every expression gets a type, every statement is checked
 *     against what it claims -- a let's annotation, a call's
 *     signature, a match's arms agreeing. A literal takes its type
 *     from the context when there is one (Cerium has no literal
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
 * beyond the leaf layer -- a literal length is the evaluator's own
 * here (08-reflection.md), an fn call in it waits -- pack spreads,
 * ranges, and type values ($$t, reflection). Option and Result are
 * checked as what they are -- prelude enums -- with their variants as
 * constructors and their narrowing spelled by the == None / != Err
 * forms (01-types.md).
 *
 * The environment and the helpers it walks with -- bindings,
 * borrows, joins, the diagnostics -- live in flow.c; the shapes the
 * language spells itself -- the @ builtins, the operators -- in
 * operators.c; the patterns a match arms, in patterns.c. body.h is
 * the face the four share. This file is the walk itself.
 */

#define _POSIX_C_SOURCE 200809L /* snprintf: c89 hides it */

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "body.h"
#include "check.h"
#include "die.h"
#include "eval.h"
#include "layout.h"
#include "lex.h"
#include "sym.h"
#include "type.h"

static Type *rexpr1(Ast *e, Fenv *fe, Type *want); /* the want normalization
                                                    * rexpr wraps */

/* the fn whose body pass 4 is walking: an @compileError it holds is
 * a report only when the evaluator never ran the body -- the run
 * itself decides the branch (08-reflection.md) */
Sym *bodyfn;

/* -- places, read (03-move.md) ------------------------------------------- */

static Type *rplace1(Ast *e, Fenv *fe);

/* the rows a spread of a whole binding spells: the expansion below
 * makes them ordinary row reads -- the arity the call counts is the
 * rows', and the trial folds them back for the pack slot as it does
 * any tail (04-generics.md) -- and each row would be a move out of a
 * place the checker refuses (03-move.md). But the whole binding's
 * rows move together, the binding dying whole, never half: the rows
 * the expansion itself made are remembered here, their reads the
 * move's own spelling -- the marking they leave is the binding's
 * death, and a trial that refuses unwinds it with every other move
 * (movsnap's own discipline). A row the program spelled by hand is
 * no member: it refuses alone, the way any row read does. The
 * tables are never cleared: a node remembered is only ever walked
 * as the spread's own row, and a clone re-expands its own (04). */
static Ast **sprrows;  /* the generated row reads themselves */
static Ast **sprbases; /* the bases they read: the spread's operand */

static int
issprrow(Ast *e)
{
  usize i;

  for (i = 0; i < vlen(sprrows); i++)
    if (sprrows[i] == e)
      return 1;
  return 0;
}

static int
issprbase(Ast *e)
{
  usize i;

  for (i = 0; i < vlen(sprbases); i++)
    if (sprbases[i] == e)
      return 1;
  return 0;
}

/* a const []u8 parameter's bake, held by name: is it one -- the
 * shape the literal spells, no wider slice (08-reflection.md) */
static int
isconststr(Val *cv)
{
  return cv->t->k == Tyslice && cv->t->t->k == Tyint && cv->t->t->num == IN_U8;
}

/* the bake's bytes back as the literal they spelled: every read
 * after them folds -- the len, an index, another const fn's own
 * argument -- and a runtime position takes it for the literal it
 * is, the data segment its home (08-reflection.md) */
static void
foldconststr(Ast *e, Val *cv)
{
  char *b = cv->len ? arenaalloc(cv->len + 1) : 0;
  usize i;

  for (i = 0; i < cv->len; i++)
    b[i] = (char) cv->elems[i].i;
  if (b)
    b[cv->len] = 0; /* the NUL the data segment's line wants */
  memset(&e->v, 0, sizeof e->v);
  e->k = Nstr;
  e->v.s.s = b;
  e->v.s.len = cv->len;
}

/* a place, read: the base chain is checked, nothing moves -- reads
 * of fields and elements do not take what they read (03-move.md).
 * NULL when e is not a place at all. */
Type *
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

    if (root) {                        /* a local: dead and borrow checks, no move */
      if (root->dead && !issprbase(e)) /* a spread's own rows read
                                        * their base after the first
                                        * row moved the binding: the
                                        * walk is the move itself
                                        * (03-move.md, 04) */
        berr(e, "'%s' has been moved", root->name);
      if (touchconflict(e, fe, 0))
        berr(e, "'%s' is borrowed (01-types.md)", root->name);
      /* a const []u8's only place is the literal it folds to -- the
       * value walk's own fold, here for a place walk the value walk
       * reads the base of: fmt.len's fmt is a place, and without
       * the fold the len reads a runtime slot the bake never gave
       * it (08-reflection.md) */
      if (root->isconst && root->cv && isconststr(root->cv))
        foldconststr(e, root->cv);
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
      if (strcmp(bt->sym->fields[i].name, e->v.fld.name) == 0) {
        Type *ft = bt->sym->fields[i].ty;

        if (bt->nargs == (usize) bt->sym->ngparams) /* a generic
                                                     * struct's field
                                                     * reads under the
                                                     * instance (04) */
          ft = gsubst(ft, bt->sym->gparams, bt->args, bt->nargs);
        return ft;
      }
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
    if (bt->k == Typaram && bt->gp->v.gp.pack) { /* the pack's rows:
                                                  * the binding's own,
                                                  * and the place defers
                                                  * with the read
                                                  * (04-generics.md) */
      evalblackbox++;
      return 0;
    }
    if (bt->k == Tytuple) { /* a spread's rows: the binding's own,
                             * their count its answer -- a row's place,
                             * a range's, defers with the read the
                             * same way (04-generics.md) */
      usize ri;

      for (ri = 0; ri < bt->nargs; ri++)
        if (bt->args[ri]->k == Tyspread) {
          evalblackbox++;
          return 0;
        }
    }
    if (e->k == Nindex && (bt->k == Tytuple || bt->k == Tyunit)) { /*
      a tuple's row by number, the dot's own spelling (04-generics.md):
      the rewrite the value walk lands, taken here too so a borrow
      points at the row the emitter addresses -- never a copy a
      materialised let would hold, the row's own Copy beside the
      point (01-types.md) */
      Ast *base = e->v.n2.a;

      if (bt->k == Tyunit || e->v.n2.b->k != Nint)
        berr(e, "a tuple's index is a row number the compiler reads (04-generics.md)");
      { /* the rewrite lands the row place the checker already has */
        u64 ix = e->v.n2.b->v.i.num;

        if (ix >= bt->nargs)
          berr(e->v.n2.b, "row %lu out of range for %s", (unsigned long) ix, btys(bt));
        e->k = Ntupidx;
        e->v.tup.e = base;
        e->v.tup.idx = ix;
        return rplace(e, fe);
      }
    }
    if (e->k == Nrangeindex && (bt->k == Tytuple || bt->k == Tyunit)) {
      /* ts[1..]: the tail's own place, the rows the range holds --
       * each one its slot, the borrow a pointer into the tuple
       * itself, never the copy the value read builds
       * (04-generics.md) */
      usize n = bt->k == Tytuple ? bt->nargs : 0;
      int   bb = evalblackbox;
      u64   lo = 0, hi = n;

      if (e->v.ridx.lo) { /* the bounds are compile-time facts, the
                           * rows the type holds (01-types.md) */
        Val v;

        rexpr(e->v.ridx.lo, fe, 0);
        v = ceval(e->v.ridx.lo, fe->env, tyint(IN_USIZE));
        lo = v.i;
      }
      if (evalblackbox != bb)
        return 0; /* a bound the binding answers: the place defers
                   * with the read (04-generics.md) */
      if (e->v.ridx.hi) {
        Val v;

        rexpr(e->v.ridx.hi, fe, 0);
        v = ceval(e->v.ridx.hi, fe->env, tyint(IN_USIZE));
        hi = v.i;
      }
      if (evalblackbox != bb)
        return 0;
      if (lo > n || hi > n || lo > hi)
        berr(e, "rows %lu..%lu out of range for %s", (unsigned long) lo, (unsigned long) hi,
             btys(bt));
      { /* the bounds land as numbers on the node itself: the
         * emitter's walk reads them where they stand, the rows'
         * walk its own (01-types.md) */
        if (e->v.ridx.lo) {
          Ast *l = mknear(Nint, e->v.ridx.lo);

          l->v.i.num = lo;
          l->ty = tyint(IN_USIZE);
          e->v.ridx.lo = l;
        }
        if (e->v.ridx.hi) {
          Ast *h = mknear(Nint, e->v.ridx.hi);

          h->v.i.num = hi;
          h->ty = tyint(IN_USIZE);
          e->v.ridx.hi = h;
        }
        e->ty = tytuple(lo < hi ? bt->args + lo : 0, hi - lo);
        return e->ty;
      }
    }
    if (bt->k != Tyarray && bt->k != Tyslice)
      return 0;           /* a range over a tuple: rexpr's own ground,
                           * the sub-tuple rewrite it answers there */
    if (e->k == Nindex) { /* the index: a value the store reads, and
                           * one no other walk takes when this node is
                           * a place -- or a base below one -- so it is
                           * checked here or never (01-types.md) */
      Type *it = rexpr(e->v.n2.b, fe, 0);

      if (!it || !isintty(it))
        berr(e->v.n2.b, "an index is an integer, this is %s", btys(it));
      if (e->v.n2.b->k == Nint && bt->k == Tyarray &&
          !bt->gp /* the
                   * length a const parameter names has
                   * no number here (08-reflection.md) */
          && e->v.n2.b->v.i.num >= bt->n)
        berr(e->v.n2.b, "index %lu out of range for %s", (unsigned long) e->v.n2.b->v.i.num,
             btys(bt));
      return bt->t;
    }
    { /* the bounds, the same walk */
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
          bt->k == Tyarray && !bt->gp &&
          (e->v.ridx.lo->v.i.num > e->v.ridx.hi->v.i.num || e->v.ridx.hi->v.i.num > bt->n))
        berr(e, "slice bounds out of range for %s", btys(bt));
    }
    return tyslice(bt->t); /* the view itself, the same answer the
                            * value read gives -- a place the store
                            * does not yet write through */
  }
  case Ntupidx: { /* t.0, or the rewrite above: the row is a place the
                   * same way a field is -- its address the emitter's
                   * own offset walk, its borrow freezing the root
                   * (01-types.md) */
    Type *bt = rplace(e->v.tup.e, fe);

    if (!bt)
      bt = rexpr(e->v.tup.e, fe, 0); /* a computed base: f().0, a value */
    if (!bt)
      return 0;
    bt = derefthrough(bt);
    if (bt->k == Typaram && bt->gp->v.gp.pack) { /* the pack's rows:
                                                  * the binding's own,
                                                  * and the place defers
                                                  * with the read
                                                  * (04-generics.md) */
      evalblackbox++;
      return 0;
    }
    if (bt->k == Tytuple && e->v.tup.idx < bt->nargs &&
        bt->args[e->v.tup.idx]->k == Tyspread) { /* a spread's row:
                                                  * the binding's own,
                                                  * the place defers
                                                  * with the read
                                                  * (04-generics.md) */
      evalblackbox++;
      return 0;
    }
    if (bt->k != Tytuple && bt->k != Tyunit)
      berr(e, "%s is not a tuple", btys(bt));
    if (bt->k == Tyunit || e->v.tup.idx >= bt->nargs)
      berr(e, "tuple index %lu out of range", (unsigned long) e->v.tup.idx);
    return bt->args[e->v.tup.idx];
  }
  case Nun:
    if (e->v.un.op == Tstar) {
      Type *pt;
      int   spent = spentborrow(e->v.un.e);

      if (spent) /* the same spend the value read takes: the inline
                  * borrow dies the moment it is made, so the walk
                  * freezes nothing the store below would then find
                  * borrowed (01-types.md) */
        fe->nofreeze++;
      pt = rexpr(e->v.un.e, fe, 0); /* the pointer itself: a Copy */
      if (spent)
        fe->nofreeze--;
      return pt && pt->k == Typtr ? pt->t : 0;
    }
    return 0;
  default:
    return 0;
  }
}

/* -- expressions --------------------------------------------------------- */

/* inside a const for's unroll walk: a const for met here sits below
 * a shared body's top level, and one unroll slot cannot serve the
 * rounds it would appear in (10-iteration.md) */
static int ununroll;

/* re-derive a literal against the type the other side of an
 * operator or an assignment turned out to be: 42 < u32's length */
Type *
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

/* peel Self out of a receiver's type: a trait's declared self is a
 * shape over Self (*Self, []mut Self, (Self, u32), ...), the
 * argument is that shape over a concrete type. The walk hands back
 * the concrete type Self must be, or 0 when the shapes do not line
 * up -- Self hides under a shape, never inside a named one. */
static Type *
selfpeel(Type *sig, Type *arg)
{
  usize i;

  if (!sig || !arg)
    return 0;
  if (sig->k == Typaram && sig->gp == sym_selfgp)
    return arg;
  if (sig->k != arg->k)
    return 0;
  switch (sig->k) {
  case Typtr:
  case Tyslice:
  case Tymut:
    return selfpeel(sig->t, arg->t);
  case Tyarray:
    if (sig->n != arg->n)
      return 0;
    return selfpeel(sig->t, arg->t);
  case Tytuple: {
    Type *r;

    if (sig->nargs != arg->nargs)
      return 0;
    for (i = 0; i < sig->nargs; i++) {
      r = selfpeel(sig->args[i], arg->args[i]);
      if (r)
        return r; /* Self appears once in a receiver's shape */
    }
    return 0;
  }
  default:
    return 0;
  }
}

/* a trait's declared signature under a Self that is itself a
 * parameter: the walk inside a generic fn needs the shape, not the
 * impl -- the instantiation picks that (04-generics.md), and the
 * bound there already promised one exists. The same shapes
 * tsubst walks, keyed on the one Self parameter. */
static Type *
selfsubst(Type *t, Type *self)
{
  Type **as;
  usize  i;

  if (!t)
    return t;
  switch (t->k) {
  case Typaram:
    return t->gp == sym_selfgp ? self : t;
  case Typtr:
    return typtr(selfsubst(t->t, self));
  case Tyslice:
    return tyslice(selfsubst(t->t, self));
  case Tymut:
    return tymut(selfsubst(t->t, self));
  case Typroj: /* Self::Item: the projection hangs on the Self this
                * walk replaces, wherever it sits */
    return typroj(t->sym, selfsubst(t->t, self), t->name);
  case Tyarray:
    return tyarray(t->n, selfsubst(t->t, self));
  case Tytuple:
  case Tyfn:
  case Tyenum:
  case Tystruct:
  case Tyunion:
  case Tytrait:
  case Tydyn:
    as = t->nargs ? tyargs(t->nargs) : 0;
    for (i = 0; i < t->nargs; i++)
      as[i] = selfsubst(t->args[i], self);
    switch (t->k) {
    case Tytuple:
      return tytuple(as, t->nargs);
    case Tyfn:
      return tyfn(as, t->nargs, selfsubst(t->t, self));
    case Tydyn:
      return tydyn(t->sym, as, t->nargs, t->mut);
    default:
      return tysym(t->sym, as, t->nargs);
    }
  default:
    return t;
  }
}

/* a projection the call made concrete: the Self it hangs on is a
 * type now, so the impl's own answer reads -- the same lookup a
 * spelled name takes (05-traits.md). A Self still a parameter
 * stays the projection: the instantiation's re-check answers it
 * (04-generics.md). An impl's row may answer in a projection of
 * its own -- a generic row's Output is the Self's -- so the walk
 * loops until a type stands. The depth opens too: a projection
 * nested in a shape's own arguments -- Result<T, F::Output>, a
 * tuple's row -- reads the same answers the top one does, the
 * whole type the instantiation's own words (04-generics.md). */
Type *
projopen(Type *t, Ast *at)
{
  Type **as;
  usize  i;

  while (t && t->k == Typroj && t->t && t->t->k != Typaram) {
    Sym    *imp;
    Type  **tys;
    Member *m;

    if (t->t->k == Tyfn && t->sym == sym_fn) { /* a fn pointer's own
                                                * answer: the signature's
                                                * return, the same
                                                * built-in Fn the
                                                * satisfies walk reads
                                                * (05-traits.md) */
      t = t->t->t;
      continue;
    }
    m = implfind(t->sym, t->t, t->name, &imp, &tys);

    if (!m || m->kind != Mtype)
      berr(at, "no '%s' for %s", t->sym->name, btys(t->t));
    t = tys ? gsubst(m->val, imp->gparams, tys, imp->ngparams) : m->val;
  }
  if (!t)
    return t;
  switch (t->k) { /* the shape's own arguments, the same depth a
                   * substitution walks (selfsubst) */
  case Typtr:
    return typtr(projopen(t->t, at));
  case Tyslice:
    return tyslice(projopen(t->t, at));
  case Tymut:
    return tymut(projopen(t->t, at));
  case Tyarray:
    return tyarray(t->n, projopen(t->t, at));
  case Tytuple:
  case Tyfn:
  case Tyenum:
  case Tystruct:
  case Tyunion:
  case Tytrait:
  case Tydyn:
    if (!t->nargs && t->k != Tyfn)
      return t;
    as = t->nargs ? tyargs(t->nargs) : 0;
    for (i = 0; i < t->nargs; i++)
      as[i] = projopen(t->args[i], at);
    switch (t->k) {
    case Tytuple:
      return tytuple(as, t->nargs);
    case Tyfn:
      return tyfn(as, t->nargs, projopen(t->t, at));
    case Tydyn:
      return tydyn(t->sym, as, t->nargs, t->mut);
    default:
      return tysym(t->sym, as, t->nargs);
    }
  default:
    return t;
  }
}

/* what a declared projection was is the handle's own spelling: the
 * vtable erased the impl's choice, and the spelling gives it back
 * (06-dispatch.md). The handle's args are the trait's own gparam
 * slots ahead, the Mtype slots behind them, the trait's declaration
 * order both */
static Type *
projsubst(Type *t, Type *h)
{
  Type **as;
  usize  i;

  if (!t)
    return t;
  switch (t->k) {
  case Typroj: { /* Self::Item under this handle's trait */
    usize mi = h->sym->ngparams;

    if (t->sym != h->sym)
      return t;
    for (i = 0; i < h->sym->nmembers; i++) {
      Member *m = &h->sym->members[i];

      if (m->kind != Mtype)
        continue;
      if (strcmp(m->name, t->name) == 0)
        return h->args[mi]; /* given: objectsafety checked the slot */
      mi++;
    }
    return t;
  }
  case Typtr:
    return typtr(projsubst(t->t, h));
  case Tyslice:
    return tyslice(projsubst(t->t, h));
  case Tymut:
    return tymut(projsubst(t->t, h));
  case Tyarray:
    return tyarray(t->n, projsubst(t->t, h));
  case Tytuple:
  case Tyfn:
  case Tyenum:
  case Tystruct:
  case Tyunion:
  case Tytrait:
  case Tydyn:
    as = t->nargs ? tyargs(t->nargs) : 0;
    for (i = 0; i < t->nargs; i++)
      as[i] = projsubst(t->args[i], h);
    switch (t->k) {
    case Tytuple:
      return tytuple(as, t->nargs);
    case Tyfn:
      return tyfn(as, t->nargs, projsubst(t->t, h));
    case Tydyn:
      return tydyn(t->sym, as, t->nargs, t->mut);
    default:
      return tysym(t->sym, as, t->nargs);
    }
  default:
    return t;
  }
}

/* does a signature mention Self bare -- not behind a pointer, where
 * its size is not the caller's to know? Object safety's question,
 * asked of every position but the self one (06-dispatch.md) */
static int
bareself(Type *t)
{
  usize i;

  if (!t)
    return 0;
  switch (t->k) {
  case Typaram:
    return t->gp == sym_selfgp;
  case Typtr:
  case Tyslice:
  case Tymut:
    return 0; /* the shape knows the width; the pointee is erased */
  case Tytuple:
  case Tyfn:
  case Tyenum:
  case Tystruct:
  case Tyunion:
  case Tytrait:
  case Tydyn:
    for (i = 0; i < t->nargs; i++)
      if (bareself(t->args[i]))
        return 1;
    return 0;
  default:
    return 0;
  }
}

/* a vtable can only be built for a trait whose methods all
 * dispatch without knowing Self (06-dispatch.md) */
static void
objectsafety(Sym *tr, Ast *at, Type **given, usize ngiven)
{
  usize i, mi = 0;

  for (i = 0; i < tr->nmembers; i++) {
    Member *m = &tr->members[i];
    Type   *sig;

    if (m->kind == Mtype) { /* the handle's own spelling gives it */
      if (mi >= ngiven || !given[mi])
        berr(at, "'%s' has an associated type; a handle needs it given (06-dispatch.md)", tr->name);
      mi++;
      continue;
    }
    if (m->kind != Mfn)
      continue;
    sig = m->ty;
    if (m->decl && vlen(m->decl->v.fn.gparams))
      berr(at, "'%s::%s' is generic: no one address fits a vtable (06-dispatch.md)", tr->name,
           m->name);
    if (sig->nargs && sig->args[0] && sig->args[0]->k == Typaram && sig->args[0]->gp == sym_selfgp)
      berr(at, "'%s::%s' takes Self by value: its size is not known (06-dispatch.md)", tr->name,
           m->name);
    { /* every other Self in the signature hides behind a pointer */
      usize j;

      for (j = 1; j < sig->nargs; j++)
        if (bareself(sig->args[j]))
          berr(at, "'%s::%s' takes Self bare: its size is not known (06-dispatch.md)", tr->name,
               m->name);
      if (bareself(sig->t))
        berr(at, "'%s::%s' returns Self bare: its size is not known (06-dispatch.md)", tr->name,
             m->name);
    }
  }
}

/* a member fn may name a family of its own, on top of its impl's
 * (04-generics.md): these four carry the call-site half. The
 * declaration side -- the signature's resolution, the body's env --
 * was always ready; what was missing is the binding. */

/* the member's own parameter list, from whichever decl supplied the
 * signature the call holds -- a trait's declaration inside a generic
 * body, the impl's everywhere else. Both name the same family; the
 * nodes differ, and the binding keys on the ones the signature
 * carries. */
static Ast **
membergps(Member *m, usize *np)
{
  if (m->kind != Mfn || !m->decl || m->decl->k != Nfn) {
    *np = 0;
    return 0;
  }
  *np = vlen(m->decl->v.fn.gparams);
  return m->decl->v.fn.gparams;
}

/* the report's own view of a parameter: a variable the call already
 * bound names what it bound to -- the row's claim in the caller's
 * own words, not its declaration's (07-operators.md) */
static Type *
boundty(Type *pt, Ast **g1, Type **t1, usize n1, Ast **g2, Type **t2, usize n2)
{
  usize g;

  if (!pt || pt->k != Typaram)
    return pt;
  for (g = 0; g < n1; g++)
    if (pt->gp == g1[g] && t1[g])
      return t1[g];
  for (g = 0; g < n2; g++)
    if (pt->gp == g2[g] && t2[g])
      return t2[g];
  return pt;
}

/* one argument against its parameter: the check the call always
 * ran, plus the binding when the parameter names a variable -- the
 * member's own or the row's, whichever family the trial carries.
 * A pass-through stands for itself -- the re-check binds it for
 * real, exactly as a generic fn's recursive call inside a generic
 * body (04-generics.md). Soft is the row trial's own ask
 * (07-operators.md): a mismatch the caller can walk away from --
 * another row may take the argument -- answers 0 instead of the
 * report. */
static int
argfit(Ast *a, Type *pt, Type *at, Ast **mg, Type **mtys, usize nm, Ast **ig, Type **itys, usize ni,
       Fenv *fe, const char *who, int soft)
{
  if (!at || !pt)
    return 1;
  if (tysame(at, pt)) {
    if (mtys)
      gunify(pt, at, mg, mtys, nm);
    if (itys)
      gunify(pt, at, ig, itys, ni);
    return 1;
  }
  {
    Type *c = recoerce(a, pt, fe);

    if (c)
      return 1;
  }
  { /* the mismatch no conversion answers: a variable can still
     * take it -- any family the trial carries, every family it
     * carries -- and no family at all leaves the mismatch the
     * row's own to answer for */
    int any = 0, ok = 1;

    if (mtys) {
      any = 1;
      if (!gunify(pt, at, mg, mtys, nm))
        ok = 0;
    }
    if (itys) {
      any = 1;
      if (!gunify(pt, at, ig, itys, ni))
        ok = 0;
    }
    if (any && ok)
      return 1;
  }
  if (soft)
    return 0;
  berr(a, "'%s' wants %s here, this is %s", who, btys(boundty(pt, mg, mtys, nm, ig, itys, ni)),
       btys(at));
  return 1;
}

/* the binding, once the arguments spoke: every slot landed, every
 * bound the parameters carry answered (04-generics.md) -- the same
 * checks a generic fn's call runs over its own list. Soft is the
 * row trial's ask (07-operators.md): a slot the arguments left open
 * or a bound they could not answer is a row that did not take the
 * call, the next row's to try, not a report. */
static int
memberdone(Ast *e, Ast **mg, Type **mtys, usize nm, const char *who, int soft, Ast **ig,
           Type **itys, usize ni)
{
  usize g, bi;

  for (g = 0; g < nm; g++)
    if (!mtys[g]) {
      if (soft)
        return 0;
      berr(e, "cannot infer '%s' for '%s' from the call", mg[g]->v.gp.name, who);
    }
  for (g = 0; g < nm; g++) {
    Ast **bs = mg[g]->v.gp.bounds;

    for (bi = 0; bi < vlen(bs); bi++) {
      Sym   *tr = bs[bi]->v.path.sym; /* the bound's own cache (04) */
      Type **ta;

      if (!boundsatisfies(bs[bi], mtys[g], mg, mtys, nm, &ta, ig, itys, ni)) {
        if (soft)
          return 0;
        berr(e, "'%s' does not implement '%s'; '%s' cannot take it", btys(mtys[g]),
             btys(tysym(tr, ta, tr->ngparams)), who);
      }
    }
  }
  return 1;
}

/* the row's own shape for a report: the trait with its arguments,
 * the for-type after it -- `Add<T> for S`, the declaration's own
 * words (07-operators.md) */
static const char *
rowname(Sym *imp)
{
  static char     bufs[2][192];
  static unsigned which;
  char           *b = bufs[which++ & 1u];

  snprintf(b, sizeof bufs[0], "%s for %s", btys(imp->ipath), btys(imp->ifort));
  return b;
}

/* the row's own bounds, once the arguments landed its slots: the
 * same report the member's own walk gives, the row named
 * (07-operators.md). The ask ran already in its silent shape --
 * boundsok -- the hard round is the only caller. */
static void
rowbounds(Sym *imp, Type **tys, Ast *at)
{
  usize g, bi;

  for (g = 0; g < imp->ngparams; g++) {
    Ast **bs = imp->gparams[g]->v.gp.bounds;

    for (bi = 0; bi < vlen(bs); bi++) {
      Sym   *tr = bs[bi]->v.path.sym; /* the bound's own cache (04) */
      Type **ta;

      if (!boundsatisfies(bs[bi], tys[g], imp->gparams, tys, imp->ngparams, &ta, 0, 0, 0))
        berr(at, "'%s' does not implement '%s'; the row cannot take it", btys(tys[g]),
             btys(tysym(tr, ta, tr->ngparams)));
    }
  }
}

/* the instance the call writes back: the impl's binding -- from the
 * receiver -- with the member's own behind it, matching the method
 * Sym's concatenated list (04-generics.md). */
static Type **
insttys(Sym *imp, Type **tys, Ast **mg, Type **mtys, usize nm)
{
  usize  ni = imp ? imp->ngparams : 0;
  Type **c;
  usize  i, z;

  if (!ni && !nm)
    return 0;
  c = tyargs(ni + nm);
  for (i = 0; i < ni; i++)
    c[i] = tys ? tys[i] : 0;
  for (z = 0; z < nm; z++)
    c[ni + z] = mtys ? mtys[z] : typaram(mg[z]); /* an unbound slot
                                                  * cannot happen --
                                                  * memberdone spoke */
  return c;
}

/* an argument written as a borrow: the callee's parameter holds it,
 * so the freeze ends when the call does (01-types.md) -- the same
 * rule the sugar's receiver follows. What it froze goes back after;
 * escaping the pointer out of the callee is the caller's promise to
 * keep, exactly as across any fn boundary. */
static int
argborrow(Ast *a, Fenv *fe, Frzsave *sv)
{
  char   pbuf[256];
  Local *root;

  if (a->k != Nun || a->v.un.op != Tamp)
    return 0;
  root = placeroot(a->v.un.e, fe, pbuf, sizeof pbuf);
  if (!root)
    return 0;
  sv->root = root;
  sv->frz = root->frz;
  sv->frzby = root->frzby;
  sv->frzpath = root->frzpath;
  return 1;
}

static void
thawargs(Frzsave *svs, usize n)
{
  usize i;

  for (i = n; i > 0; i--) /* reverse: two borrows of one root, LIFO */
    frzrestore(&svs[i - 1]);
}

/* the operand of a deref that is -- or by its own rewrite will have
 * become -- an inline borrow: &mut p.x, or @field(p, "x"), the same
 * borrow spelled by name (08-reflection.md). The deref spends the
 * borrow whole: it dies the moment it is made, so the walk around
 * it tells freeze to hold its hand (01-types.md) */
int
spentborrow(Ast *operand)
{
  if (operand->k == Nun && operand->v.un.op == Tamp)
    return 1;
  return operand->k == Nbuiltin && strcmp(operand->v.blt.name, "field") == 0 &&
         !vlen(operand->v.blt.targs) && vlen(operand->v.blt.args) == 2;
}

/* a const parameter's argument, one of three answers: the value, the
 * black box, or none to have. The question is static -- the
 * evaluator's refusals are exits, not values -- so the forms that
 * can answer are named here, and anything else is a runtime thing
 * honestly said (08-reflection.md) */
#define CV_NONE 0 /* a runtime value: no answer this side of the call */
#define CV_VAL  1 /* *out holds the value */
#define CV_BOX                                                                                     \
  2 /* a const parameter this frame holds no value for:                                            \
     * the re-check under the binding has it */

static int
cargval(Ast *a, Fenv *fe, Val *out)
{
  switch (a->k) {
  case Nstr: /* the literal's bytes, a []u8 (08-reflection.md) */
  case Nint:
  case Nflt:
  case Nbool:
  case Nbyte:
    *out = ceval(a, envnone(), 0);
    return CV_VAL;
  case Nun: /* the sign rides a literal: -2147483648 is i32's least
             * (08-reflection.md) */
    if (a->v.un.op == Tminus && a->v.un.e->k == Nint) {
      *out = ceval(a, envnone(), 0);
      return CV_VAL;
    }
    return CV_NONE;
  case Nbin: { /* the operator over known sides, its own answer */
    Val v;
    int l = cargval(a->v.bin.l, fe, out);

    if (l == CV_NONE)
      return CV_NONE;
    if (l == CV_VAL) {
      int r = cargval(a->v.bin.r, fe, &v);

      if (r == CV_NONE)
        return CV_NONE;
      if (r == CV_BOX)
        return CV_BOX;
    } /* l == CV_BOX: the left alone settles it */
    else
      cargval(a->v.bin.r, fe, &v); /* walked anyway: the re-check
                                    * asks the same question, and a
                                    * runtime side is still none */
    *out = ceval(a, envnone(), 0);
    return CV_VAL;
  }
  case Npath: {
    char  *nm;
    Local *l;

    if (vlen(a->v.path.segs) != 1 || a->v.path.root)
      return CV_NONE; /* a variant's path: not tried here, the list
                       * stays honest */
    nm = a->v.path.segs[0]->v.seg.name;
    l = locfind(fe, nm);
    if (l) { /* this frame's own name first: a runtime local is not
              * the evaluator's to answer for */
      if (!l->isconst)
        return CV_NONE;
      if (l->cv) {
        *out = *l->cv;
        return CV_VAL;
      }
      return CV_BOX;
    }
    { /* a const's own name: the chain answers, and its initializer's
       * error is its own (08-reflection.md) */
      Sym *s = symfind(nm);

      if (s && s->kind == Sconst) {
        *out = ceval(a, envnone(), 0);
        return CV_VAL;
      }
    }
    return CV_NONE;
  }
  default:
    return CV_NONE;
  }
}

/* a row's own refusal, in the row's own words: the signature named
 * before the reason -- the report the hard error gave, each row its
 * own name, the chain's end joining them (04-generics.md,
 * Overloading). The arity and unify refusals leave no words; the old
 * report's own still holds them. */
static char *
inferwhy(Sym *s, Ast *gp)
{
  char *b = arenaalloc(256);

  snprintf(b, 256, "%s: cannot infer '%s' from the call", btys(s->fnty), gp->v.gp.name);
  return b;
}

static char *
boundwhy(Sym *s, Type *ty, Sym *tr, Type **ta)
{
  char *b = arenaalloc(512);

  snprintf(b, 512, "%s: '%s' does not implement '%s'", btys(s->fnty), btys(ty),
           btys(tysym(tr, ta, tr->ngparams)));
  return b;
}

/* one row's refusal joined to those before it, the arena's own
 * strings, "; " the seam */
static char *
whyjoin(char *a, char *b)
{
  usize la = a ? strlen(a) : 0, lb = strlen(b);
  char *j = arenaalloc(la + lb + 3);

  if (la) {
    memcpy(j, a, la);
    memcpy(j + la, "; ", 2);
  }
  memcpy(j + la + (la ? 2 : 0), b, lb + 1);
  return j;
}

/* one signature's trial: the arguments walked against it, the
 * binding it spells picked and written back when it takes them. The
 * answer is the call's type, or 0 when it does not. cvals holds the
 * const parameters' own test -- a runtime argument there fails the
 * signature and notes it, the black box defers with the pick (08).
 * nfreeze is the caller's argument count, the freezes to unwind --
 * the rolled trial walks a folded view. why holds the row's own
 * refusal when it had one: a bound the landing failed, a binding
 * the call never made -- the chain's end report joins them, a
 * signature that steps aside leaving its reason behind (04) */
static Type *
tryonesig(Sym *s, Ast *a, Ast **args, usize n, usize nfreeze, Fenv *fe, Ast *seg, Frzsave *svs,
          Val **cvals, int *runtime, char **why)
{
  Type  *fnty = s->fnty;
  Type **tys = s->ngparams ? tyargs(s->ngparams) : 0;
  Val  **gcvals = s->ngparams ? arenaalloc(s->ngparams * sizeof *gcvals)
                              : 0; /* the
                                    * const generic parameters' numbers, the
                                    * unifier's binding (08-reflection.md) */
  Type **sigs;                     /* what the arguments are checked against: the
                                    * signature's own, or its substituted form under
                                    * a spelled-out binding */
  Type **ats;
  usize  i;
  int   *snap; /* the argument walks' moves, to unwind with the
                * freezes when this signature does not take them --
                * the next one's walk reads the moved bindings as
                * live again (03-move.md) */
  int ok = n == fnty->nargs;

  if (!ok) {
    thawargs(svs, nfreeze);
    return 0;
  }
  if (gcvals)
    memset(gcvals, 0, s->ngparams * sizeof *gcvals);
  ats = n ? arenaalloc(n * sizeof *ats) : 0;
  sigs = fnty->args;
  if (seg && seg->v.seg.args) { /* f<i32>(...): the binding is the
                                 * call's own words, not inference */
    if (vlen(seg->v.seg.args) != s->ngparams) {
      thawargs(svs, nfreeze);
      return 0; /* an overload this spelling does not fit */
    }
    for (i = 0; i < s->ngparams; i++) {
      if (s->gparams[i]->v.gp.cnst) /* a const generic's argument is a
                                     * number, not a type -- the
                                     * words cannot spell it; the
                                     * argument's own type binds it
                                     * (04-generics.md, 08) */
        berr(a,
             "'%s' is a const generic parameter: its argument is a value, and the call's "
             "arguments bind it -- a length in a type cannot be spelled here (08-reflection.md)",
             s->gparams[i]->v.gp.name);
      tys[i] = rty(seg->v.seg.args[i], &fe->env);
    }
    if (fnty->nargs) {
      sigs = tyargs(fnty->nargs);
      for (i = 0; i < fnty->nargs; i++)
        sigs[i] = gsubst(fnty->args[i], s->gparams, tys, s->ngparams);
    }
  }
  snap = movsnap(fe);
  for (i = 0; ok && i < n; i++) {
    ats[i] = rexpr(args[i], fe, sigs[i]);
    if (!ats[i] || !sigs[i])
      continue;
    if (tysame(ats[i], sigs[i])) {
      /* a parameter passed through: T stands for itself here,
       * and an instantiation's re-check binds it for real (a
       * recursive call inside a generic body) */
      if (tys)
        gunifyv(sigs[i], ats[i], s->gparams, tys, gcvals, s->ngparams);
      continue;
    }
    {
      Type *c = recoerce(args[i], sigs[i], fe);

      if (c) {
        ats[i] = c;
        continue;
      }
    }
    if (!gunifyv(sigs[i], ats[i], s->gparams, tys, gcvals, s->ngparams))
      ok = 0;
  }
  if (ok && cvals) { /* the const parameters' own test: the argument
                      * names a compile-time value, or the box the
                      * re-check opens (08-reflection.md) */
    Ast **ps = s->decl->v.fn.params;

    for (i = 0; ok && i < n; i++)
      if (ps[i]->v.param.cnst) {
        Val v;
        int cv = cargval(args[i], fe, &v);

        if (cv == CV_NONE) { /* a runtime value: the call site's
                              * refusal, not the body's (08) */
          ok = 0;
          if (runtime)
            *runtime = 1;
        } else if (cv == CV_VAL) {
          cvals[i] = arenaalloc(sizeof **cvals);
          *cvals[i] = v;
        } /* CV_BOX: the slot stays NULL, the re-check's to fill */
      }
  }
  if (ok) {
    for (i = 0; i < s->ngparams; i++) /* a const generic's slot fills
                                       * when the unifier meets its
                                       * length -- a number or a black
                                       * box; an empty one was never
                                       * met at all */
      if (!tys[i]) {                  /* the binding the call never made: this row's
                                       * own refusal, not the chain's end -- the next
                                       * row's walk reads the call anew (04-generics.md) */
        if (why)
          *why = inferwhy(s, s->gparams[i]);
        ok = 0;
        break;
      }
  }
  if (ok) { /* every bound, once the binding is known: does the type
             * the call landed implement the trait (04-generics.md)? The
             * impl table answers -- a bound nobody can satisfy was
             * already diagnosed where the fn was declared. A bound the
             * landing fails is this row's own refusal too: the row
             * steps aside, its reason left for the chain's end (04) */
    Ast **gps = s->decl->v.fn.gparams;
    usize gi, bi;

    for (gi = 0; ok && gi < vlen(gps); gi++) {
      Ast **bs = gps[gi]->v.gp.bounds;
      int   pk = gps[gi]->v.gp.pack;
      usize ri, rn = 0;

      if (pk && tys[gi]) /* a pack's bound is every row's own
                          * (04-generics.md): the binding holds the
                          * tuple the rows came in */
        rn = tys[gi]->k == Tytuple ? tys[gi]->nargs : 0;
      for (bi = 0; ok && bi < vlen(bs); bi++) {
        Sym   *tr = bs[bi]->v.path.sym; /* the bound's own cache (04) */
        Type **ta;

        if (pk) {
          for (ri = 0; ok && ri < rn; ri++)
            if (!boundsatisfies(bs[bi], tys[gi]->args[ri], s->gparams, tys, s->ngparams, &ta, 0, 0,
                                0)) {
              if (why)
                *why = boundwhy(s, tys[gi]->args[ri], tr, ta);
              ok = 0;
            }
          continue; /* the empty pack: no row, no bound to fail */
        }
        if (!boundsatisfies(bs[bi], tys[gi], s->gparams, tys, s->ngparams, &ta, 0, 0, 0)) {
          if (why)
            *why = boundwhy(s, tys[gi], tr, ta);
          ok = 0;
        }
      }
    }
  }
  if (ok) { /* the emitter's pick: which overload, which
             * instantiation. The tys live in the arena, so the
             * writeback outlives the walk (04-generics.md) */
    a->v.call.sym = s;
    a->v.call.tys = s->ngparams ? tys : 0;
    a->v.call.cvals = cvals;
    a->v.call.gcvals = s->ngparams ? gcvals : 0;
    thawargs(svs, nfreeze); /* the call is done; its borrows ended with it */
    return projopen(gsubstv(fnty->t, s->gparams, tys, gcvals, s->ngparams), a);
  }
  thawargs(svs, nfreeze); /* this signature did not take: its freezes
                           * unwound, its moves with them */
  movrestore(fe, snap);
  return 0;
}

/* one signature's trial against a call: the plain signature, or --
 * when the fn's last parameter takes the pack -- the pack's own two
 * spellings. The spread's rows walk on faith, the re-check under the
 * binding reading them for real; any other shape folds the tail
 * arguments into one tuple argument, the empty tail the unit -- a
 * tuple handed over on its own is that fold's one row, the pack one
 * wide (04-generics.md) */
static Type *
trysig(Sym *s, Ast *a, Ast **args, usize n, Fenv *fe, Ast *seg, Frzsave *svs, Val **cvals,
       int *runtime, char **why)
{
  Ast **ps = s->decl->v.fn.params;
  usize np = vlen(ps);
  Type *r;

  if (a->v.call.folded) /* the arguments already the pack's folded
                         * view -- a walk's own writeback, this one
                         * its re-check: the rows stand as the fold
                         * left them, met as they are, no fold again
                         * (04-generics.md) */
    return tryonesig(s, a, args, n, n, fe, seg, svs, cvals, runtime, why);
  if (np && ps[np - 1]->v.param.t->k == Ntpack) { /* the pack is
                                                   * last (the parser
                                                   * saw to it) */
    { /* the pack's own spread in the arguments: the rows the
       * binding holds, the arity with them. This declaration's
       * walk cannot count them -- the signature takes the call on
       * faith, and the re-check under the binding walks it for
       * real (04-generics.md). The spread's operand was walked
       * above, where the tuple spreads folded: what stands here is
       * the pack's own, the only one left */
      usize i;
      int   faith = 0;

      for (i = 0; i < n; i++)
        if (args[i]->k == Nspread)
          faith = 1;
      if (faith) {
        Type **tys = s->ngparams ? tyargs(s->ngparams) : 0;
        Type  *ret;

        for (i = 0; i < s->ngparams; i++) /* each parameter, its own
                                           * placeholder: the return
                                           * reads what it reads, and
                                           * the instance's walk binds
                                           * for real */
          tys[i] = typaram(s->gparams[i]);
        a->v.call.sym = s;
        a->v.call.tys = tys;
        a->v.call.cvals = 0;
        a->v.call.gcvals = 0;
        thawargs(svs, n);
        ret = projopen(gsubstv(s->fnty->t, s->gparams, tys, 0, s->ngparams), a);
        return ret;
      }
    }
    if (n + 1 >= s->fnty->nargs) { /* the folded spelling: the
                                    * parameters before the pack keep
                                    * their arguments, the rest fold
                                    * into one tuple argument */
      Ast **rargs = vnew(Ast *, np);
      usize head = np - 1, i;

      for (i = 0; i < head; i++)
        vappend(&rargs, &args[i]);
      if (n > head) {
        Ast *t = mknear(Ntuple, a);

        t->v.list.ts = vnew(Ast *, n - head);
        for (i = head; i < n; i++)
          vappend(&t->v.list.ts, &args[i]);
        vappend(&rargs, &t);
      } else { /* the empty pack: its tuple is () (04-generics.md) */
        Ast *u = mknear(Nunit, a);

        vappend(&rargs, &u);
      }
      r = tryonesig(s, a, rargs, np, n, fe, seg, svs, cvals, runtime, why);
      if (r) { /* the fold stands: matching and emit read the
                * folded view (the spread's own writeback, 01) */
        a->v.call.args = rargs;
        a->v.call.folded = 1;
        return r;
      }
      return 0;
    }
    thawargs(svs, n);
    return 0;
  }
  return tryonesig(s, a, args, n, n, fe, seg, svs, cvals, runtime, why);
}

/* inside a mode-gated fn's arguments: @take may not appear there,
 * for the modes that remove the call never make the move
 * (01-types.md). Set around the whole pick, restored after: a trial
 * that returns walks the arguments, and the ban holds on every
 * trial's walk alike. */
int gatedargs;

/* a call's callee chain, read ahead of the value walk: the local
 * that shadows the name, the namespaces the path walks, the privacy
 * a qualified read crosses -- anything the walk itself would
 * refuse, this leaves alone too, the error the walk's own to say.
 * The doors below each judge the chain their own way. */
static Sym *
callchain(Ast *e, Fenv *fe)
{
  Ast  *f = e->v.call.f;
  Ast **segs;
  usize nsegs, k;
  Ns   *ns;
  Sym  *s;

  if (f->k != Npath)
    return 0; /* a method's sugar, a local's fat call: a gated fn
               * is a free fn (01-types.md) */
  segs = f->v.path.segs;
  nsegs = vlen(segs);
  k = nshead(segs, nsegs, &ns, f->v.path.root);
  if (nsegs - k != 1)
    return 0; /* a namespace whole, a variant's or a trait's
               * qualified door: none of them a gated fn's shape */
  {
    char  *nm = segs[k]->v.seg.name;
    Local *l = locfind(fe, nm);

    if (l)
      return 0; /* the name a local owns is the local's
                 * (11-namespaces.md) */
    s = k ? nsitem(ns, nm) : symfind(nm);
  }
  if (!s || s->kind != Sfn || (k && !s->pub))
    return 0;
  return s;
}

/* a mode-gated fn's own door: the path a gated fn answers by, when
 * every gated row of its chain is closed in this build
 * (01-types.md, Mode-gated functions) -- the head Sym, the chain
 * the pick below walks */
Sym *
gatedcall(Ast *e, Fenv *fe)
{
  Sym *s = callchain(e, fe), *c;

  if (!s)
    return 0;
  for (c = s; c; c = c->next)
    if (!modegated(c, chk_rel))
      return 0; /* a kept row answers the name: the call is real here */
  return s;     /* every row held away: the call names nothing in this mode */
}

/* the artifact's own door, the statement shape: every row of the
 * chain a test this build holds away, and the call has nothing
 * outside the artifact to name (13-testing.md). The mode's removal
 * is a feature, the statement gone whole -- a test reached from
 * outside its artifact is a hand's error, and the hand hears it */
static Sym *
testcall(Ast *e, Fenv *fe)
{
  Sym *s = callchain(e, fe), *c;

  if (!s)
    return 0;
  for (c = s; c; c = c->next)
    if (!testheld(c, chk_test))
      return 0; /* a row the product carries: the call is real here */
  return s;     /* the artifact's rows alone: no call outside one */
}

/* the chain's own walk: one row a trial, the first that takes these
 * arguments answers (04-generics.md). A gated row this mode
 * holds away never answers -- a trial is the arguments' own walk,
 * and a call that does not exist walks nothing
 * (01-types.md, Mode-gated functions). A row whose binding the call
 * never made, or whose bounds the landing failed, steps aside the
 * same way, its refusal left behind: the chain's end report joins
 * them, each row its own words (04). */
static Type *
callpick(Sym *s, Ast *a, Ast **args, usize n, Fenv *fe, Ast *seg, Frzsave *svs, Val **cvals,
         int constmode, int *runtime, const char *nm)
{
  Sym  *head = s;
  Type *r;
  char *why = 0; /* the rows' own refusals, joined -- the chain's end
                  * report, when no row takes the call */

  if (constmode) {
    for (; s; s = s->next) {
      if (modegated(s, chk_rel) || testheld(s, chk_test) || !fnconstparams(s)) {
        thawargs(svs, n);
        continue; /* the plain spellings wait below */
      }
      {
        char *rowwhy = 0;

        r = trysig(s, a, args, n, fe, seg, svs, cvals, runtime, &rowwhy);
        if (r)
          return r;
        if (rowwhy)
          why = whyjoin(why, rowwhy);
      }
    }
    for (s = head; s; s = s->next) { /* the plain spellings: the
                                      * runtime arguments' own
                                      * (08-reflection.md) */
      if (modegated(s, chk_rel) || testheld(s, chk_test) || fnconstparams(s)) {
        thawargs(svs, n);
        continue; /* tried above */
      }
      {
        char *rowwhy = 0;

        r = trysig(s, a, args, n, fe, seg, svs, 0, 0, &rowwhy);
        if (r)
          return r;
        if (rowwhy)
          why = whyjoin(why, rowwhy);
      }
    }
  } else
    for (; s; s = s->next) {
      if (modegated(s, chk_rel) || testheld(s, chk_test)) {
        thawargs(svs, n);
        continue;
      }
      {
        char *rowwhy = 0;

        r = trysig(s, a, args, n, fe, seg, svs, 0, 0, &rowwhy);
        if (r)
          return r;
        if (rowwhy)
          why = whyjoin(why, rowwhy);
      }
    }
  if (*runtime)
    berr(a, "the argument is not compile-time known; '%s' takes it const (08-reflection.md)", nm);
  if (why) /* the rows that came close, each its own refusal -- the
            * arity and unify refusals the old words hold below */
    berr(a, "no '%s' takes these arguments -- %s", nm, why);
  berr(a, "no '%s' takes these argument types", nm);
  return 0; /* unreachable */
}

/* a call to a named fn, overload chain and all. tys holds the
 * generic bindings while the arguments are walked. */
static Type *
callfn(Sym *s, Ast *a, Ast **args, usize n, Fenv *fe)
{
  const char *nm = s->name;
  Ast        *seg; /* the callee's one segment, when the call
                    * spells its generic arguments out (04) */
  Frzsave *svs = n ? arenaalloc(n * sizeof *svs) : 0;
  Sym     *head = s, *q;
  Val    **cvals;
  Type    *r;
  int      constmode, runtime;
  usize    k;

  { /* the mode's own door first (01-types.md, Mode-gated functions):
     * every row of the chain held out of this build, and the call
     * names nothing here. The statement form the block walk removed
     * already -- what reaches this door is a value use, the one the
     * mode refuses with the gating named */
    int kept = 0;

    for (q = s; q; q = q->next)
      if (!modegated(q, chk_rel))
        kept = 1;
    if (!kept) {
      char modes[64];

      modewords(s, modes, sizeof modes);
      berr(a,
           "no call to '%s' in %s: #[cfg] holds it to %s -- the call's value"
           " does not exist in this mode (01-types.md)",
           nm, chk_rel ? "release" : "debug", modes);
    }
    kept = 0; /* the artifact's own door beside the mode's: a test is
               * the artifact's fn alone, and no call outside one
               * reaches it (13-testing.md) */
    for (q = s; q; q = q->next)
      if (!testheld(q, chk_test))
        kept = 1;
    if (!kept)
      berr(a,
           "'%s' is a test, the artifact's own fn -- the library and the"
           " executable do not carry it (13-testing.md)",
           nm);
  }

  if (svs) { /* the borrow arguments' freezes go back with the call,
              * on every path out but the errors (01-types.md) */
    memset(svs, 0, n * sizeof *svs);
    for (k = 0; k < n; k++)
      argborrow(args[k], fe, &svs[k]);
  }

  seg = 0;
  if (a->v.call.f->k == Npath && vlen(a->v.call.f->v.path.segs) == 1)
    seg = a->v.call.f->v.path.segs[0];
  constmode = 0; /* a const spelling is the more specific signature:
                  * the chain's plain ones wait behind it, and a
                  * compile-time-known argument picks it first
                  * (08-reflection.md) */
  for (q = head; q; q = q->next)
    if (fnconstparams(q)) { /* the pack takes the tail whole, its
                             * rows any count: the arity the plain
                             * spellings match on is the head's own
                             * (04-generics.md) */
      usize qn = vlen(q->decl->v.fn.params);

      if (n == qn || (qn && q->decl->v.fn.params[qn - 1]->v.param.t->k == Ntpack && n + 1 >= qn))
        constmode = 1;
    }
  {
    usize mx = n; /* the folded view: the pack's own tuple is an
                   * argument the count here does not see, and the
                   * emitter reads the bake by the declaration's own
                   * count -- the array holds both (08) */

    for (q = head; q; q = q->next)
      if (fnconstparams(q)) {
        usize qn = vlen(q->decl->v.fn.params);

        if (qn > mx)
          mx = qn;
      }
    cvals = constmode && mx ? arenaalloc(mx * sizeof *cvals) : 0;
    if (cvals)
      memset(cvals, 0, mx * sizeof *cvals);
  }
  runtime = 0;
  { /* the @take ban rides the whole pick: a gated fn's arguments
     * move in no mode -- what one mode never evaluates, neither may
     * (01-types.md) */
    int   sv = gatedargs;
    Type *rr;

    for (q = head; q; q = q->next)
      if (declmodes(q->decl))
        gatedargs = 1;
    rr = callpick(s, a, args, n, fe, seg, svs, cvals, constmode, &runtime, nm);
    gatedargs = sv;
    r = rr;
  }
  return r;
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
 * found under. Soft is the row trial's ask (07-operators.md): a
 * receiver this row's own self cannot take answers 0, the next
 * row's to try. */
static int
recvadapt(Ast *x, Type *selfty, Type *rty, Type *ty, Fenv *fe, Frzsave *sv, int soft)
{
  char pbuf[256];
  int  isplacebinding;

  if (!rty || !selfty || !ty)
    return 1;
  isplacebinding = placeroot(x, fe, pbuf, sizeof pbuf) != 0;
  if (selfty->k == Typtr) { /* a pointer self: &place, or as written */
    Type *want = selfty->t->k == Tymut ? selfty->t->t : selfty->t;

    if (tysame(rty, selfty))
      return 1; /* &*sp is sp: the pointer already is the address */
    if (!tysame(ty, want)) {
      if (soft)
        return 0;
      berr(x, "this receiver is %s, the method wants %s", btys(rty), btys(selfty));
    }
    sv->root = placeroot(x, fe, pbuf, sizeof pbuf);
    if (sv->root) { /* what was frozen, to put back after the call */
      sv->frz = sv->root->frz;
      sv->frzby = sv->root->frzby;
      sv->frzpath = sv->root->frzpath;
    }
    if (selfty->t->k == Tymut) { /* &mut: a mut slot, and it freezes */
      if (!placewritable(x, fe)) {
        if (soft)
          return 0;
        berr(x, "a &mut receiver needs a mut slot (01-types.md)");
      }
      if (touchconflict(x, fe, 1)) {
        if (soft)
          return 0;
        berr(x, "this place is already borrowed (01-types.md)");
      }
      freeze(x, fe, 1, (int) fe->n);
    } else { /* &: shared, reads stay fine */
      if (touchconflict(x, fe, 0)) {
        if (soft)
          return 0;
        berr(x, "this place is already borrowed (01-types.md)");
      }
      freeze(x, fe, 0, (int) fe->n);
    }
    return 1;
  }
  if (tysame(ty, selfty)) { /* by value: the receiver moves in */
    if (isplacebinding && !iscopy(ty)) {
      Local *root = placeroot(x, fe, pbuf, sizeof pbuf);

      if (fe->loopd > 0 && locfindi(fe, root->name) < fe->loopbase) {
        if (soft)
          return 0;
        berr(x, "'%s' began before the for and would be moved every round", root->name);
      }
      root->dead = 1; /* the binding is the move's one legal start */
    }
    return 1;
  }
  if (soft)
    return 0;
  berr(x, "this receiver is %s, the method wants %s", btys(rty), btys(selfty));
  return 0; /* unreachable */
}

/* the sugar's receiver, when the method takes it by a shared
 * pointer and the receiver names no place -- a literal, a call's
 * answer -- is materialised exactly as an & materialises its
 * operand (01-types.md): the call wrapped in a block that binds
 * the value into a nameless slot, the receiver reading the name.
 * The slot dies with the statement's own block, so the pointer
 * the call borrows never outlives it -- the borrow's safest
 * shape, the one an argument's own & already owns. A mut self
 * keeps the refusal an &mut keeps: a writable temporary has no
 * honest reader. Returns the re-entered walk's answer, 0 when
 * nothing was materialised. */
static Type *
recvmat(Ast *e, Ast *f, Type *selfty, Type *rt, Fenv *fe, Type *want)
{
  char pbuf[256];

  if (!selfty || selfty->k != Typtr)
    return 0; /* a value self: the receiver moves in as written */
  if (rt && rt->k == Typtr && tysame(rt, selfty))
    return 0; /* &*sp is sp: the pointer already is the address */
  if (placeroot(f->v.fld.e, fe, pbuf, sizeof pbuf))
    return 0; /* a place: the borrow reads it where it lies */
  if (selfty->t->k == Tymut)
    berr(f->v.fld.e, "a &mut receiver needs a place (01-types.md)");
  {
    static usize nm; /* the materialised names, unique in the
                      * compile: '%' is no identifier's first
                      * byte, so no binding of the program's own
                      * can collide */
    char  nbuf[24];
    Ast  *blk = opnode(Nblock, e);
    Ast  *c = opnode(Ncall, e);
    Ast  *ls = opnode(Nlet, e);
    Ast  *pat = opnode(Nppath, e);
    Ast  *pp = opnode(Npath, e);
    Ast  *rp = opnode(Npath, e);
    Ast **ss = vnew(Ast *, 1);

    sprintf(nbuf, "%%t%lu", (unsigned long) nm++);
    pp->v.path.segs = vnew(Ast *, 1);
    opvpush(&pp->v.path.segs, opseg(nbuf, e));
    pat->v.ppath.path = pp;
    rp->v.path.segs = vnew(Ast *, 1);
    opvpush(&rp->v.path.segs, opseg(nbuf, e));
    ls->v.let.pat = pat;
    ls->v.let.e = f->v.fld.e; /* the value, into the slot */
    c->v = e->v;              /* the call, whole: its f the same
                               * node, the receiver about to read
                               * the name */
    f->v.fld.e = rp;          /* the name, in the call's own f */
    opvpush(&ss, ls);
    blk->v.blk.stmts = ss;
    blk->v.blk.tail = c;
    e->k = Nblock;
    memcpy(&e->v, &blk->v, sizeof e->v);
    return rexpr(e, fe, want); /* re-entered: the block's own walk */
  }
}

/* -- the walk ------------------------------------------------------------- */

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
 * enum's None takes its parameters from the context, p != None.
 * ns is the namespace a path's walk landed in, when one walked: the
 * name reads there and only there (11-namespaces.md) */
static Type *
rexprpath1(Ast *e, Fenv *fe, char *name, Type *want, Ns *ns)
{
  Local *l = ns ? 0 : locfind(fe, name); /* a namespaced name is no
                                          * local's */
  Sym *s;

  if (l) {
    if (l->dead)
      berr(e, "'%s' has been moved", name);
    if (touchconflict(e, fe, !iscopy(l->cur))) { /* reading it out
                                                  * moves what it holds
                                                  * -- a Copy reads out
                                                  * a copy, which a
                                                  * shared borrow
                                                  * tolerates
                                                  * (01-types.md) */
      berr(e, "'%s' is borrowed (01-types.md)", name);
    }
    /* a const parameter this walk holds an integer for: the read
     * folds where it stands -- a const generic parameter has no
     * runtime slot of its own, and the number is the instance's
     * (08-reflection.md) */
    if (l->isconst && l->cv && l->cv->t->k == Tyint && l->cv->t->num != IN_F32 &&
        l->cv->t->num != IN_F64) {
      memset(&e->v, 0, sizeof e->v);
      e->k = Nint;
      e->v.i.num = l->cv->i;
      return l->cur;
    }
    /* a const []u8 parameter the bake holds a string for: the read
     * folds to the literal it spelled, and every read after it
     * folds too -- the len, an index, another const fn's own
     * argument -- while a runtime position takes it for the literal
     * it is, the data segment its home (08-reflection.md) */
    if (l->isconst && l->cv && isconststr(l->cv)) {
      foldconststr(e, l->cv);
      return l->cur;
    }
    if (!iscopy(l->cur)) { /* assignment moves by default (03-move.md) */
      if (fe->loopd > 0 && locfindi(fe, name) < fe->loopbase)
        berr(e, "'%s' began before the for and would be moved every round", name);
      l->dead = 1;
    }
    return l->cur;
  }
  s = ns ? nsitem(ns, name) : symfind(name);
  if (!s && !ns)
    s = variantowner(name); /* Some, None, Ok, Err: bare (01-types.md) */
  if (!s)
    berr(e, "unknown name '%s'", name);
  if (ns && !s->pub) /* a qualified read crosses namespaces: a
                      * private item stays its own namespace's
                      * (11-namespaces.md) */
    berr(e, "'%s' is private to %s (11-namespaces.md)", name, nsname(ns));
  switch (s->kind) {
  case Sconst:
  case Sstatic:
    return s->cty;
  case Sfn: {
    Sym *c;
    int  kept = 0;

    for (c = s; c; c = c->next)
      if (!modegated(c, chk_rel))
        kept = 1;
    if (!kept) { /* the mode's own door: a fn the build holds away
                  * is no value here, a pointer to it nothing to
                  * point (01-types.md, Mode-gated functions) */
      char modes[64];

      modewords(s, modes, sizeof modes);
      berr(e,
           "no '%s' in %s: #[cfg] holds it to %s -- the fn is not a value in"
           " this mode (01-types.md)",
           name, chk_rel ? "release" : "debug", modes);
    }
    kept = 0; /* the artifact's own door beside the mode's: a test
               * is no value outside the artifact that collects it
               * (13-testing.md) */
    for (c = s; c; c = c->next)
      if (!testheld(c, chk_test))
        kept = 1;
    if (!kept)
      berr(e,
           "'%s' is a test, the artifact's own fn -- the library and the"
           " executable do not carry it (13-testing.md)",
           name);
    if (fnconstparams(s)) /* a compile-time tool, no value: the baked
                           * arguments have nowhere to cross a pointer
                           * call, and the check it would silently
                           * skip is the whole point (08-reflection.md) */
      berr(e,
           "'%s' takes a const argument: it is no value -- a fn pointer has nowhere to hand "
           "one over; call it, or split the plain half out (08-reflection.md)",
           name);
    if (s->ngparams || s->next) { /* a stencil as a value: the expected
                                   * type is the whole binding -- id
                                   * against fn(i32) -> i32 is
                                   * id<i32> (04-generics.md). A chain
                                   * picks the member the want fits. */

      if (!want || want->k != Tyfn)
        berr(e, "'%s' needs an expected fn type here", name);
      for (c = s; c; c = c->next) {
        Type **tys = c->ngparams ? tyargs(c->ngparams) : 0;
        Val  **gcvals = c->ngparams ? arenaalloc(c->ngparams * sizeof *gcvals) : 0;
        usize  i;

        if (modegated(c, chk_rel) || testheld(c, chk_test))
          continue; /* a row the mode holds away, or the artifact's
                     * own: not a fit to find (01-types.md,
                     * 13-testing.md) */
        if (!tys) { /* an ungeneric member: it fits or it does not */
          if (c->fnty->nargs == want->nargs && tysame(c->fnty, want)) {
            e->v.path.sym = c;
            return c->fnty;
          }
          continue;
        }
        memset(gcvals, 0, c->ngparams * sizeof *gcvals);
        if (!gunifyv(c->fnty, want, c->gparams, tys, gcvals, c->ngparams))
          continue;
        for (i = 0; i < c->ngparams; i++) /* a const generic's slot
                                           * fills when the unifier
                                           * meets its length; an
                                           * empty one was never met */
          if (!tys[i])
            berr(e, "cannot infer '%s' for '%s' from the expected type", c->gparams[i]->v.gp.name,
                 c->name);
        /* the emitter's pick, as a call's writeback (04) */
        e->v.path.sym = c;
        e->v.path.tys = tys;
        e->v.path.gcvals = gcvals;
        return gsubstv(c->fnty, c->gparams, tys, gcvals, c->ngparams);
      }
      berr(e, "no '%s' fits %s", name, btys(want));
    }
    return s->fnty;
  }
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

/* ?'s own half of the check: the operand a Result, the error the
 * hands-back one -- the arms the rewrite spells carry the rest (the
 * fn's return is the shape the Err half answers in). t is the
 * operand's type, the caller having read it out one way or another. */
static void
trychk(Ast *e, Fenv *fe, Type *t)
{
  Type *ok = 0;
  Type *err = reschild(t, &ok);

  if (!err)
    berr(e, "'?' wants an E?T, this is %s", btys(t));
  {
    Type *rok = 0;
    Type *rerr = reschild(fe->fnret, &rok);

    if (!rerr || !tysame(rerr, err))
      berr(e, "'?' hands back %s, the fn returns %s", btys(err), btys(fe->fnret));
  }
}

/* the wrappers that write the type back: every expression the
 * checker walks leaves its verdict on the node, so the emitter
 * reads instead of deriving. rplace's chain writes too -- a field
 * access knows its base's type only there. */
Type *
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
    Ns   *ns;
    usize k = nshead(segs, nsegs, &ns, e->v.path.root);

    if (k == nsegs) /* the whole path a namespace walk: a namespace
                     * names no value (11-namespaces.md) */
      berr(e, "a namespace names no value; name what is in it (11-namespaces.md)");
    segs += k; /* the namespaces walked fall away; what is left
                * reads as it stood -- the first segment in the
                * namespace the walk landed in, when one walked
                * (11-namespaces.md) */
    nsegs -= k;
    {
      char *nm0 = segs[0]->v.seg.name;

      if (nsegs == 1)
        return rexprpath1(e, fe, nm0, want, k ? ns : 0);
      if (nsegs == 2) {
        char *nm1 = segs[1]->v.seg.name;
        Sym  *s = k ? nsitem(ns, nm0) : symfind(nm0);

        if (s && k && !s->pub) /* the same door the plain value and
                                * the call walk -- a qualified name
                                * across namespaces finds only what
                                * the namespace gives away
                                * (11-namespaces.md) */
          berr(e, "'%s' is private to %s (11-namespaces.md)", s->name, nsname(ns));
        if (!s || s->kind != Stype) {
          if (s && s->kind == Strait) /* a trait method as a value
                                       * names a family: one impl per
                                       * receiver, and a value has
                                       * none -- dyn A (06) carries
                                       * that, when it arrives */
            berr(e, "'%s::%s' names one impl per receiver; call it", s->name, nm1);
          berr(e, "unknown name '%s'", nm0);
        }
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
          Sym    *imp;
          Member *m = inherentfind(s, nm1, &imp);
          Ast   **gas = segs[0]->v.seg.args; /* the path's own <...>:
                                              * the instance it names */
          Type **tys = 0;

          if (vlen(gas)) { /* the args name a receiver: the impls
                            * fitted against it, the most specific
                            * one's member the read -- the (T, T) of an
                            * is_same landing true where it fits, the
                            * <A, B> false behind it (05-traits.md) */
            Type *recv;
            usize gi, k = vlen(gas);

            if (k != s->ngparams)
              berr(e, "'%s' takes %lu parameters, %lu given", s->name, (unsigned long) s->ngparams,
                   (unsigned long) k);
            tys = tyargs(k);
            for (gi = 0; gi < k; gi++)
              tys[gi] = rty(gas[gi], &fe->env); /* the args resolve in
                                                 * the body's own
                                                 * scope: a fn's
                                                 * variables included
                                                 * (04-generics.md) */
            for (gi = 0; gi < k; gi++)          /* a fn's own variable among the
                                                 * args: the read is the
                                                 * instance's, deferred -- the
                                                 * re-check under the binding
                                                 * folds it (04-generics.md) */
              if (tys[gi]->k == Typaram) {
                evalblackbox++;
                return want ? want : tybool();
              }
            recv = tysym(s, tys, k);
            { /* the pick: the receiver's own, most specific first */
              Member *bm = inherentfindt(recv, nm1, &imp, &tys);

              if (!bm)
                berr(e, "no '%s' of '%s' fits %s", nm1, s->name, btys(recv));
              m = bm; /* tys: the binding the pick made */
            }
          } else if (s->ngparams && m && m->kind == Mconst)
            berr(e, "cannot infer '%s' for '%s' here", s->gparams[0]->v.gp.name, s->name);
          if (!m)
            berr(e, "'%s' has no '%s'", s->name, nm1);
          if (m->kind == Mfn && m->decl && vlen(m->decl->v.fn.gparams))
            berr(e,
                 "'%s' names a family of its own: call it, and the "
                 "arguments pick one (04-generics.md)",
                 nm1);
          if (m->kind == Mfn) {
            e->v.path.sym = m->sym; /* the fn it names: its address */
            e->v.path.tys = 0;
            return m->ty;
          }
          if (m->kind == Mconst) {
            Type *ct =
                imp && imp->ngparams ? gsubst(m->ty, imp->gparams, tys, imp->ngparams) : m->ty;

            if (m->decl && (ct->k == Tybool || (ct->k == Tyint && ct->num != IN_F32 &&
                                                ct->num != IN_F64))) { /* the value
                                                                        * folded in place:
                                                                        * the const is a
                                                                        * compile-time
                                                                        * fact, the read
                                                                        * its own literal
                                                                        * (05-traits.md) */
              Ast  *init = m->decl->v.cst.e;
              Env   env = envnone();
              Val   v;
              usize gi;

              for (gi = 0; imp && gi < imp->ngparams; gi++)
                env = envpush(&env, imp->gparams[gi]->v.gp.name, tys[gi]);
              v = ceval(init, env, ct);
              e->k = ct->k == Tybool ? Nbool : Nint;
              e->v.i.num = v.i;
              return ct;
            }
            return ct;
          }
          berr(e, "'%s::%s' is a type, not a value", s->name, nm1);
        }
      }
      berr(e, "a path this long arrives with namespaces (11)");
      return 0; /* unreachable */
    }
  }
  case Ntuple: {
    Ast  **es = e->v.list.ts;
    usize  n = vlen(es), i;
    Type **ts;

    { /* the spread: (x, ...t) spells every row of a tuple as an
       * element of its own (04-generics.md) -- the same rewrite the
       * call's arguments take, the rows read where they stand */
      int sp = 0;

      for (i = 0; i < n; i++)
        if (es[i]->k == Nspread)
          sp = 1;
      if (sp) {
        Ast **as = vnew(Ast *, n + 1);

        for (i = 0; i < n; i++) {
          Ast *a = es[i];

          if (a->k != Nspread) {
            vappend(&as, &a);
            continue;
          }
          { /* the operand read here is a look, not a take: the moves
             * it spells on a non-Copy row are undone before the
             * elements walk -- the real read happens there, once,
             * where the element stands (03-move.md) */
            int  *snap = movsnap(fe);
            Type *t = rexpr(a->v.un.e, fe, 0);

            movrestore(fe, snap);
            if (!t)
              berr(a->v.un.e, "the spread operand is not known here (04-generics.md)");
            if (t->k == Tytuple) { /* the rows the type names, every
                                    * one an element of its own, the
                                    * reads the row's own checks
                                    * hold (04-generics.md) */
              usize k;

              for (k = 0; k < t->nargs; k++) {
                Ast *ix = mknear(Ntupidx, a);

                ix->v.tup.e = a->v.un.e;
                ix->v.tup.idx = k;
                if (!sprrows)
                  sprrows = vnew(Ast *, 16);
                vappend(&sprrows, &ix); /* the spread's own row: the
                                         * whole binding's move, this
                                         * row its spelling (03, 04) */
                if (!k) {
                  if (!sprbases)
                    sprbases = vnew(Ast *, 4);
                  vappend(&sprbases, &a->v.un.e);
                }
                vappend(&as, &ix);
              }
              continue;
            }
            if (t->k == Tyunit) /* the unit: no rows at all */
              continue;
            if (t->k == Typaram && t->gp &&
                t->gp->v.gp.pack) { /*
                                     * the pack's own rows: the black box takes the spread
                                     * on faith, the instance's re-check the rows
                                     * (04-generics.md) */
              vappend(&as, &a);
              continue;
            }
            berr(a, "the ... spreads a tuple's rows, this is %s (04-generics.md)", btys(t));
          }
        }
        e->v.list.ts = as; /* the widened list: every pass below
                            * walks it as the one the words spelled,
                            * the emitter included */
        es = as;
        n = vlen(as);
      }
    }
    ts = n ? tyargs(n) : 0;
    for (i = 0; i < n; i++) {
      Type *wt = want && want->k == Tytuple && i < want->nargs ? want->args[i] : 0;
      Type *vt = wt;

      while (vt && vt->k == Tymut) /* (T, mut U): the row's slot
                                    * permission, not the value's own
                                    * type (01-types.md) */
        vt = vt->t;
      ts[i] = rexpr(es[i], fe, vt);
      if (ts[i] && wt && wt->k == Tymut) /* the value takes the row's
                                          * writable slot with it, as
                                          * [N]mut T literals do */
        ts[i] = tymut(ts[i]);
    }
    return n ? tytuple(ts, n) : tyunit(); /* every row spent: the
                                           * empty tuple's own one
                                           * spelling, the literal's
                                           * (01-types.md) */
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
    if (ta && tb && (!opscalars(ta, tb) || tysame(ta, tb)) &&
        optrait(e, fe)) { /* the operator's own trait answers where
                           * the built-in table ends (07-operators.md)
                           * -- std's scalar impls included, so a pair
                           * the language holds no row of its own for
                           * finds its trait's: bool < bool is Ord's;
                           * a mixed scalar pair is the language's own
                           * error instead, the scalar rows all Rhs =
                           * Self (opscalars above); the table was
                           * asked first, so n + m never comes here,
                           * and % with the bitwise ones are language,
                           * the errors below answering for them */
      return rexpr(e, fe, want);
    }
    if (ta && tb && !tysame(ta, tb))
      berr(e, "'%s' wants both sides the same type: %s and %s", opname(op), btys(ta), btys(tb));
    berr(e, "'%s' is not defined for %s", opname(op), btys(ta));
    return 0; /* unreachable */
  }
  case Nun: {
    Tok op = e->v.un.op;

    if (op == Tdyn) { /* &dyn b / &mut dyn b: the fat handle. Which
                       * trait is meant comes from the type the
                       * handle is being made for -- the want names
                       * it (06-dispatch.md) */
      Type *t = rplace(e->v.un.e, fe);
      Type *w = want;

      while (w && w->k == Tymut) /* a slot's permission, not the
                                  * handle's own shape */
        w = w->t;
      if (!t) { /* a global name, a literal: the value's own ground.
                 * A fn the words name -- a named fn, a captureless
                 * closure -- is a value, the pointer itself; an
                 * env-holding literal is not, and no place lent it
                 * one (05-traits.md) */
        t = rexpr(e->v.un.e, fe, 0);
        if (!t)
          berr(e->v.un.e, "cannot make a handle of a temporary");
      }
      if (e->v.un.mut && !placewritable(e->v.un.e, fe))
        berr(e->v.un.e, "a &mut dyn needs a mut slot (01-types.md)");
      if (touchconflict(e->v.un.e, fe, e->v.un.mut))
        berr(e->v.un.e, "this place is already borrowed (01-types.md)");
      if (!w || w->k != Tydyn)
        berr(e, "the trait a handle is made for comes from its expected type (06-dispatch.md)");
      { /* the impl the vtable carries, chosen here -- the whole
         * point of dyn: the choice travels (06-dispatch.md) */
        Sym *im = implfor(w->sym, t, 0);

        if (!im && t->k == Tyfn && w->sym == sym_fn) { /* the fn
                                                        * pointer's own row, the compiler's
                                                        * knowledge, no impl a file spells
                                                        * (05-traits.md): the words must
                                                        * spell the signature back, the
                                                        * pack's own slot the whole tuple,
                                                        * the answer the return */
          usize ng = w->sym->ngparams;
          Type *pk = w->nargs >= ng ? w->args[ng - 1] : 0; /* the
                                                            * pack's own slot, the
                                                            * whole tuple -- the
                                                            * Mtype slots stand
                                                            * behind it (06) */
          usize pi;

          if (!pk || (pk->k != Tytuple && pk->k != Tyunit) ||
              (pk->k == Tytuple ? pk->nargs : 0) != t->nargs)
            berr(e->v.un.e, "no 'Fn' for %s: the handle's words spell another signature", btys(t));
          if (pk->k == Tytuple)
            for (pi = 0; pi < pk->nargs; pi++)
              if (!tysame(pk->args[pi], t->args[pi]))
                berr(e->v.un.e, "no 'Fn' for %s: the handle's words spell another signature",
                     btys(t));
          if (!tysame(w->args[ng], t->t)) /* Output: the family's
                                           * only Mtype, the first
                                           * slot behind the
                                           * positional (06) */
            berr(e->v.un.e, "no 'Fn' for %s: the handle's words spell another answer", btys(t));
        } else if (!im)
          berr(e->v.un.e, "no '%s' for %s", w->sym->name, btys(t));
        if (w->sym == sym_fnonce) /* the family's own spent call: the
                                   * handle would hold what the one
                                   * call already took (06) */
          berr(e, "'FnOnce' has no handle: a handle that may be called once is not a handle "
                  "(06-dispatch.md)");
        objectsafety(w->sym, e, w->args + w->sym->ngparams, w->nargs - w->sym->ngparams);
      }
      freeze(e->v.un.e, fe, e->v.un.mut, (int) fe->n);
      return w; /* rplace wrote the concrete type on the operand;
                 * the emitter re-finds the impl from the pair */
    }
    if (op == Tamp) { /* &x / &mut x: the place is borrowed, not read */
      int   bb = evalblackbox;
      Type *t = rplace(e->v.un.e, fe);

      if (evalblackbox != bb) {        /* a pack-rooted place: the rows the
                                        * binding holds, and the instance's
                                        * walk types the borrow for real
                                        * (04-generics.md) -- never a
                                        * materialised copy, the row itself
                                        * is the place the emitter addresses */
        evalblackbox = bb;             /* the flag dies with the walk that raised it */
        return want ? want : tyunit(); /* the shape the world around
                                        * it wants, or none when no
                                        * slot names one (04) */
      }
      if (!t) {          /* a value with no place of its own -- &3, &make(),
                          * &(b - 1) -- is materialised: a nameless slot the
                          * statement's own block holds, the value stored
                          * into it, its address the answer. The slot's
                          * binding dies with the block, so nothing after can
                          * touch what the pointer points at -- the borrow's
                          * own safest shape, the lifetime a call's argument's
                          * borrow already owns (01-types.md). A &mut keeps
                          * the refusal: a writable temporary has no honest
                          * reader */
        static usize nm; /* the materialised names, unique in the
                          * compile: '%' is no identifier's first
                          * byte, so no binding of the program's own
                          * can collide */
        char  nbuf[24];
        Ast  *blk = opnode(Nblock, e);
        Ast  *ls = opnode(Nlet, e);
        Ast  *pat = opnode(Nppath, e);
        Ast  *pp = opnode(Npath, e);
        Ast  *rp = opnode(Npath, e);
        Ast  *bor = opnode(Nun, e);
        Ast **ss = vnew(Ast *, 1);

        if (e->v.un.mut)
          berr(e->v.un.e, "a &mut needs a place (01-types.md)");
        sprintf(nbuf, "%%t%lu", (unsigned long) nm++);
        pp->v.path.segs = vnew(Ast *, 1);
        opvpush(&pp->v.path.segs, opseg(nbuf, e));
        pat->v.ppath.path = pp;
        rp->v.path.segs = vnew(Ast *, 1);
        opvpush(&rp->v.path.segs, opseg(nbuf, e));
        ls->v.let.pat = pat;
        ls->v.let.e = e->v.un.e;
        bor->v.un.op = Tamp;
        bor->v.un.e = rp;
        opvpush(&ss, ls);
        blk->v.blk.stmts = ss;
        blk->v.blk.tail = bor;
        e->k = Nblock;
        memcpy(&e->v, &blk->v, sizeof e->v);
        return rexpr(e, fe, want); /* re-entered: the block's own walk */
      }
      if (e->v.un.mut && !placewritable(e->v.un.e, fe))
        berr(e->v.un.e, "a &mut needs a mut slot (01-types.md)");
      /* a shared & may stack on a live shared borrow (01-types.md: a
       * *T is not exclusive); only a &mut touches what it may not */
      if (touchconflict(e->v.un.e, fe, e->v.un.mut))
        berr(e->v.un.e, "this place is already borrowed (01-types.md)");
      freeze(e->v.un.e, fe, e->v.un.mut, (int) fe->n);
      { /* &mut a mut slot's element: the place's own type already
         * carries the writable layer ([N]mut T's rows are mut
         * slots, and & lends them *mut the same way) -- the wrap
         * is the pointer's mut, one layer, never two
         * (01-types.md). The borrow's own exclusiveness is the
         * checker's freeze above, not the type's spelling */
        if (e->v.un.mut && t->k != Tymut)
          t = tymut(t);
        return typtr(t);
      }
    }
    if (op == Tminus && want && want->k == Tyint &&
        (e->v.un.e->k == Nint || e->v.un.e->k == Nflt)) {
      /* the sign rides the literal (01-types.md): a signed type's
       * least -- i8's -128, i32's -2147483648 -- spells with it,
       * and no other way is. The domain check reads the signed
       * whole, so the least passes and one past it does not */
      if (e->v.un.e->k == Nflt) { /* a float's negative: it rounds,
                                   * it does not overflow */
        if (want->num != IN_F32 && want->num != IN_F64)
          berr(e, "-%g does not fit %s", e->v.un.e->v.f.flt, btys(want));
      } else {
        u64 m = e->v.un.e->v.i.num;
        int ok;

        switch (want->num) {
        case IN_I8:
          ok = m <= 0x80;
          break;
        case IN_I16:
          ok = m <= 0x8000;
          break;
        case IN_I32:
          ok = m <= 0x80000000u;
          break;
        case IN_I64:
        case IN_ISIZE: /* the sizes are the machine's: 64 (layout.c) */
          ok = m <= ((u64) 1 << 63);
          break;
        default: /* an unsigned want: a negative never fits one */
          ok = 0;
          break;
        }
        if (!ok)
          berr(e, "-%lu does not fit %s", (unsigned long) m, btys(want));
      }
      e->v.un.e->ty = want; /* the operand carries the type the fold
                             * landed in; the emitter negates it */
      return want;
    }
    if (op == Tcaret2) {        /* the lift: the operand is a type spelled
                                 * in a value's slot, the value a reference
                                 * to it (08-reflection.md) */
      rty(e->v.un.e, &fe->env); /* the operand resolves as a type, or
                                 * says why it is not one */
      return tytype();
    }
    if (op == Tdollar2) /* a splice names a type slot; this is a
                         * value's (08-reflection.md) */
      berr(e, "a splice names a type slot (08-reflection.md)");
    {
      Type *t;
      int   spent = op == Tstar && spentborrow(e->v.un.e);

      if (spent) /* the deref spends the borrow whole: it freezes
                  * nothing past the expression (01-types.md) */
        fe->nofreeze++;
      t = rexpr(e->v.un.e, fe, 0);
      if (spent)
        fe->nofreeze--;
      switch (op) {
      case Tminus:
        if (t && isnumty(t))
          return t;
        if (t && !opscalar1(t)) { /* -v is Neg's call, the
                                   * checker's own unary
                                   * answering the scalars first
                                   * (07-operators.md) */
          opuntrait(e, fe, "Neg", "neg");
          return rexpr(e, fe, want);
        }
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
        Type *t2;

        if (!t)
          return 0;
        if (t->k != Typtr)
          berr(e, "cannot dereference %s", btys(t));
        t2 = t->t;
        if (t2 && t2->k == Tymut) /* a *mut T's mut layer is the slot's
                                   * permission, not the value's type */
          t2 = t2->t;
        if (!iscopy(t2))
          berr(e, "cannot move out of a place: %s is not Copy (@take, 03)", btys(t2));
        return t2;
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

    { /* the spread: f(...t) spells every element of a tuple as an
       * argument of its own (01-types.md) -- the operand's type
       * known here, the spread rewritten to the row reads, and the
       * call walking on as though the rows were spelled by hand. A
       * slice's length is a runtime thing: its spread needs the
       * pack it feeds, and packs arrive with generics
       * (04-generics.md) */
      usize i;
      int   sp = 0;

      for (i = 0; i < n; i++)
        if (args[i]->k == Nspread)
          sp = 1;
      if (sp) {
        Ast **as = vnew(Ast *, n + 1);
        usize k;

        for (i = 0; i < n; i++) {
          Ast  *a = args[i];
          Type *t;

          if (a->k != Nspread) {
            vappend(&as, &a);
            continue;
          }
          { /* the operand read here is a look, not a take: the moves
             * it spells on a non-Copy operand are undone before the
             * arguments walk -- the real read happens there, once,
             * where the argument stands (03-move.md, the same
             * unwinding a trial's own walk takes) */
            int  *snap = movsnap(fe);
            Type *st = rexpr(a->v.un.e, fe, 0);

            t = st;
            movrestore(fe, snap);
          }
          if (!t)
            berr(a->v.un.e, "the spread operand is not known here (01-types.md)");
          if (t->k == Tyslice) /* the length is a runtime thing: the
                                * spread wants a length the compiler
                                * reads -- an array's type names one,
                                * a value the evaluator knows does
                                * (04-generics.md) */
            berr(a, "a slice's length is a runtime thing: its spread wants an array, or a "
                    "value the compiler knows (04-generics.md)");
          if (t->k == Tyarray && t->gp) { /* the length a const
                                           * parameter names: the
                                           * binding's own answer,
                                           * this defers (08) */
            evalblackbox++;
            vappend(&as, &a);
            continue;
          }
          if (t->k == Tyarray) { /* [N]T: the length the type names,
                                  * every element an argument of its
                                  * own, the reads the index's own
                                  * checks hold (04-generics.md) */
            for (k = 0; k < t->n; k++) {
              Ast *ix = mknear(Nindex, a);
              Ast *num = mknear(Nint, a);

              num->v.i.num = k;
              ix->v.n2.a = a->v.un.e;
              ix->v.n2.b = num;
              if (!sprrows)
                sprrows = vnew(Ast *, 16);
              vappend(&sprrows, &ix); /* the spread's own row: the
                                       * whole binding's move, this
                                       * row its spelling (03, 04) */
              if (!k) {
                if (!sprbases)
                  sprbases = vnew(Ast *, 4);
                vappend(&sprbases, &a->v.un.e);
              }
              vappend(&as, &ix);
            }
            continue;
          }
          if (t->k == Typaram && t->gp->v.gp.pack) { /* the pack's own
                                                      * rows: the binding holds them, and the
                                                      * arity with them -- the spread stays, the
                                                      * signature taking the call on faith
                                                      * (04-generics.md) */
            vappend(&as, &a);
            continue;
          }
          if (t->k != Tytuple && t->k != Tyunit) /* the empty tuple
                                                  * is the unit type:
                                                  * it spreads
                                                  * nothing
                                                  * (01-types.md) */
            berr(a, "the spread expands a tuple, this is %s (01-types.md)", btys(t));
          if (a->v.un.e->k == Ntuple) { /* a slice of the pack rewrote
                                         * to a tuple literal of row
                                         * reads -- the rows stand as
                                         * the arguments they are, no
                                         * re-index through the whole
                                         * of it */
            for (k = 0; k < t->nargs; k++)
              vappend(&as, &a->v.un.e->v.list.ts[k]);
          } else
            for (k = 0; k < t->nargs; k++) { /* the rows, each its own
                                              * read standing where the
                                              * spread stood */
              Ast *ix = mknear(Ntupidx, a);

              ix->v.tup.e = a->v.un.e;
              ix->v.tup.idx = k;
              if (!sprrows)
                sprrows = vnew(Ast *, 16);
              vappend(&sprrows, &ix); /* the spread's own row: the
                                       * whole binding's move, this
                                       * row its spelling -- a slice's
                                       * rewrite above is no member,
                                       * its rows the program's own
                                       * partial move (03, 04) */
              if (!k) {
                if (!sprbases)
                  sprbases = vnew(Ast *, 4);
                vappend(&sprbases, &a->v.un.e);
              }
              vappend(&as, &ix);
            }
        }
        e->v.call.args = as; /* the widened list: every pass below
                              * walks it as the one the words
                              * spelled, the emitter included */
        args = as;
        n = vlen(as);
      }
    }
    if (f->k == Npath && !f->v.path.root && vlen(f->v.path.segs) == 1) {
      /* a handle held in a local, called by its own sugar: f(x) is
       * f.call(x) spelled, the callable value's own operator
       * (05-traits.md). Rewritten here, the receiver's own branch
       * below walks it -- the fat call the whole language of it
       * (06-dispatch.md) */
      Local *dl = locfind(fe, f->v.path.segs[0]->v.seg.name);
      Type  *dt = dl && dl->cur ? derefthrough(dl->cur) : 0;

      if (dt && dt->k == Tydyn && (dt->sym == sym_fn || dt->sym == sym_fnmut)) {
        Ast *a = mk(Naccess);

        a->v.fld.e = f;
        a->v.fld.name = dt->sym == sym_fn ? "call" : "call_mut";
        e->v.call.f = f = a; /* the Naccess branch takes it from
                              * here; the local's own dead and borrow
                              * checks ride the place walk it opens */
      }
    }
    if (f->k == Npath) {
      Ast **segs = f->v.path.segs;
      usize nsegs = vlen(segs);
      Ns   *ns;
      usize k = nshead(segs, nsegs, &ns, f->v.path.root);

      if (k == nsegs) /* the whole callee a namespace walk
                       * (11-namespaces.md) */
        berr(e, "a namespace names no fn; call what is in it (11-namespaces.md)");
      segs += k; /* the namespaces walked fall away; the callee left
                  * reads as it stood (11-namespaces.md) */
      nsegs -= k;

      if (nsegs == 1) {
        char  *nm = segs[0]->v.seg.name;
        Local *l = locfind(fe, nm);
        Sym   *s;

        if (l) { /* an indirect call: a fn-typed local, or a closure
                  * held in one -- the env it rides in is its value */
          Type    *t = l->cur;
          Frzsave *svs = n ? arenaalloc(n * sizeof *svs) : 0;
          usize    k;

          if (l->dead) /* the name a call reads is a read like any
                        * other: a closure spent by its once row, a
                        * fn pointer moved on -- the call refuses the
                        * dead name exactly as a plain read does
                        * (03-move.md) */
            berr(f, "'%s' has been moved", nm);
          if (t && t->k == Tystruct && t->sym && t->sym->decl &&
              t->sym->decl->k == Nclosure) { /* the closure's own call:
                                              * the env rides first,
                                              * the fn a name the
                                              * literal carries
                                              * (05-traits.md) */
            Ast  *cl = t->sym->decl;
            Type *sig = cl->v.clos.sig;

            f->ty = t; /* the callee is the env's address: the emitter
                        * hands it to the fn as its first word */
            if (svs) { /* a borrow argument's freeze ends with the call
                        * (01-types.md) */
              memset(svs, 0, n * sizeof *svs);
              for (k = 0; k < n; k++)
                argborrow(args[k], fe, &svs[k]);
            }
            if (n != sig->nargs)
              berr(e, "'%s' takes %lu arguments, %lu given", nm, (unsigned long) sig->nargs,
                   (unsigned long) n);
            {
              usize a;

              for (a = 0; a < n; a++) {
                Type *at = rexpr(args[a], fe, sig->args[a]);

                if (at && sig->args[a] && !tysame(at, sig->args[a])) {
                  Type *cc = recoerce(args[a], sig->args[a], fe);

                  if (!cc || !tysame(cc, sig->args[a]))
                    berr(args[a], "'%s' wants %s here, this is %s", nm, btys(sig->args[a]),
                         btys(at));
                }
              }
            }
            thawargs(svs, n);    /* the call is done; its borrows ended with it */
            if (cl->v.clos.once) /* the family's once row: the call is
                                  * the move that spends the env, the
                                  * binding dead after it (03-move.md) */
              l->dead = 1;
            return sig->t;
          }
          if (t && t->k == Typaram) { /* the Fn family a bound spells on
                                       * the parameter, the call's own
                                       * sugar (05-traits.md): f(x) is
                                       * call(f, x) spelled, the family
                                       * the least demanding bound that
                                       * answers it -- Fn before FnMut
                                       * before FnOnce, exactly as the
                                       * literal's own row is picked
                                       * (01-types.md). The declaration
                                       * reads the bound; the
                                       * instantiation's re-check walks
                                       * this node with the parameter a
                                       * type already, the local
                                       * callee's own two doors above
                                       * (04-generics.md) */
            Ast    **bs = t->gp->v.gp.bounds;
            Ast     *hit = 0;
            Sym     *fams[3];
            Frzsave *svs2 = n ? arenaalloc(n * sizeof *svs2) : 0;
            usize    fi, bi;

            fams[0] = sym_fn;
            fams[1] = sym_fnmut;
            fams[2] = sym_fnonce;
            for (fi = 0; fi < 3 && !hit; fi++)
              for (bi = 0; bi < vlen(bs); bi++)
                if (bs[bi]->v.path.sym == fams[fi]) { /* the bound's
                                                       * own cache
                                                       * (04) */
                  hit = bs[bi];
                  break;
                }
            if (!hit)
              berr(e,
                   "'%s' is a parameter: a bound of the Fn family is what makes it callable "
                   "(05-traits.md)",
                   nm);
            { /* the bound's own words: the arguments the pack spells,
               * one a parameter (05-traits.md) -- the pack's slot the
               * whole tuple, its rows spelled out here, one for one
               * (04-generics.md) */
              Type **bts = hit->v.path.tys;
              Ast  **pas = hit->v.path.segs[0]->v.seg.args;
              usize  pna = vlen(pas);
              usize  nb = 0;
              usize  pa;
              Ast   *pin = 0; /* the bound's own Output, when it
                               * spelled one: the answer the pin
                               * already knows, no projection left
                               * to open (04-generics.md) */
              Sym *fam = hit->v.path.sym;

              for (pa = 0; pa < pna; pa++) { /* the pins stand behind
                                              * every argument: the
                                              * count walks the
                                              * positional alone */
                if (pas[pa]->k == Nassoc) {
                  if (strcmp(pas[pa]->v.assoc.name, "Output") == 0)
                    pin = pas[pa];
                } else
                  nb++;
              }
              if (bts && fam->ngparams && fam->gparams[fam->ngparams - 1]->v.gp.pack) {
                Type  *pk = bts[fam->ngparams - 1];
                usize  rows = pk->k == Tytuple ? pk->nargs : 0;
                usize  pre = fam->ngparams - 1, r, w = 0;
                Type **flat = tyargs(pre + rows);

                for (r = 0; r < pre; r++)
                  flat[w++] = bts[r];
                for (r = 0; r < rows; r++)
                  flat[w++] = pk->args[r];
                bts = flat;
                nb = w;
              }

              if (svs2) {
                memset(svs2, 0, n * sizeof *svs2);
                for (bi = 0; bi < n; bi++)
                  argborrow(args[bi], fe, &svs2[bi]);
              }
              if (n != nb)
                berr(e, "'%s' takes %lu arguments, %lu given", nm, (unsigned long) nb,
                     (unsigned long) n);
              for (bi = 0; bi < n; bi++) {
                Type *at = rexpr(args[bi], fe, bts ? bts[bi] : 0);

                if (at && bts && bts[bi] && !tysame(at, bts[bi])) {
                  Type *cc = recoerce(args[bi], bts[bi], fe);

                  if (!cc || !tysame(cc, bts[bi]))
                    berr(args[bi], "'%s' wants %s here, this is %s", nm, btys(bts[bi]), btys(at));
                }
              }
              thawargs(svs2, n); /* the call is done; its borrows ended with it */
              return pin ? pin->v.assoc.rt : typroj(fam, t, "Output"); /* the answer the
                                                                        * impl decides, opened
                                                                        * where the instance
                                                                        * lands it
                                                                        * (05-traits.md) */
            }
          }
          if (!t || t->k != Tyfn)
            berr(e, "'%s' is %s, not callable", nm, btys(t));
          f->ty = t; /* the callee is a value here: the emitter loads
                      * this pointer out of its slot */
          if (svs) { /* a borrow argument's freeze ends with the call
                      * (01-types.md) */
            memset(svs, 0, n * sizeof *svs);
            for (k = 0; k < n; k++)
              argborrow(args[k], fe, &svs[k]);
          }
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
          thawargs(svs, n); /* the call is done; its borrows ended with it */
          return t->t;
        }
        s = k ? nsitem(ns, nm) : symfind(nm);
        if (!s && !k) { /* Some(3), Ok(v): the prelude's bare constructors */
          Sym *owner = variantowner(nm);

          if (owner)
            return mkvariant(owner, varfind(owner, nm), e, args, n, fe, want);
        }
        if (!s)
          berr(e, "unknown name '%s'", nm);
        if (k && !s->pub) /* a qualified call crosses namespaces: a
                           * private item stays its own namespace's
                           * (11-namespaces.md) */
          berr(e, "'%s' is private to %s (11-namespaces.md)", nm, nsname(ns));
        if (s->kind == Sfn)
          return callfn(s, e, args, n, fe);
        berr(e, "'%s' is not callable", nm);
      }
      if (nsegs == 2) { /* Enum::Variant(...), Type::member(...), or Trait::member(&p) */
        char *nm0 = segs[0]->v.seg.name;
        char *nm1 = segs[1]->v.seg.name;
        Sym  *s = k ? nsitem(ns, nm0) : symfind(nm0);

        if (s && k && !s->pub) /* the explicit trait call and the
                                * variant's own construction answer
                                * the same door as the plain call:
                                * qualified across namespaces, the
                                * private stays home
                                * (11-namespaces.md) */
          berr(e, "'%s' is private to %s (11-namespaces.md)", s->name, nsname(ns));
        if (s && s->kind == Strait) { /* the explicit trait call:
                                       * the receiver -- the first
                                       * argument -- picks the impl
                                       * (05-traits.md) */
          Member *dm = 0;
          usize   di;

          for (di = 0; di < s->nmembers; di++)
            if (strcmp(s->members[di].name, nm1) == 0) {
              dm = &s->members[di];
              break;
            }
          if (!dm)
            berr(e, "'%s' has no '%s'", s->name, nm1);
          if (dm->kind != Mfn)
            berr(e, "'%s::%s' is not callable", s->name, nm1);
          if (!n)
            berr(e, "'%s::%s' takes the receiver as its first argument", s->name, nm1);
          {
            /* the receiver walks below, ahead of the snapshots the
             * blocks under it take: its freeze unwinds with the call
             * like any argument's (01-types.md), so the picture is
             * taken before anything walked -- the receiver's own
             * borrow among them, for what it walks is what freezes */
            Frzsave *svs = n ? arenaalloc(n * sizeof *svs) : 0;
            usize    k;
            /* peel Self out of the receiver: the declared self is a
             * pattern over Self (*Self, []mut Self, ...), the
             * argument is that shape over a concrete type. The
             * receiver walks once, want-less -- the impl's signature
             * is not known until Self is */
            Type *sig0 = dm->ty->nargs ? dm->ty->args[0] : 0;
            Type *rt;
            Type *self;

            if (svs) {
              memset(svs, 0, n * sizeof *svs);
              for (k = 0; k < n; k++)
                argborrow(args[k], fe, &svs[k]);
            }
            rt = rexpr(args[0], fe, 0);

            if (!rt)
              return 0;
            self = selfpeel(sig0, rt);
            if (!self)
              berr(args[0], "'%s::%s' wants a %s receiver", s->name, nm1, btys(sig0));
            {
              Sym    *imp;
              Type  **tys = 0;
              Member *im;
              Type   *t;
              usize   i, nmg, zi;

              if (self->k == Typaram) {
                /* inside a generic fn: the declaration's signature
                 * carries the walk -- when the parameter carries
                 * this bound -- and the instantiation's re-check
                 * picks the impl (04-generics.md) */
                Ast **bs = self->gp->v.gp.bounds;
                Ast  *hit = 0;
                usize bi;

                for (bi = 0; bi < vlen(bs); bi++)
                  if (bs[bi]->v.path.sym == s) { /* the bound's own
                                                  * cache (04) */
                    hit = bs[bi];
                    break;
                  }
                if (!hit)
                  berr(args[0], "'%s' is not a bound on '%s'", s->name, self->gp->v.gp.name);
                t = selfsubst(dm->ty, self);
                if (s->ngparams) { /* the trait's own generics: the
                                    * ones the bound spelled, the
                                    * rest their defaults -- a bound
                                    * that spelled none takes this
                                    * Self, the parameter under it
                                    * (07-operators.md) */
                  Type **btys = hit->v.path.tys;
                  usize  nb = vlen(hit->v.path.segs[0]->v.seg.args);
                  Type **ttys = dflttail(s, btys, nb, 0, self, args[0]);

                  t = gsubst(t, s->gparams, ttys, s->ngparams);
                }
                e->v.call.sym = 0; /* the re-check writes the impl's pick */
                e->v.call.tys = 0;
                { /* the member's own family, bound from the arguments */
                  Ast  **mg = membergps(dm, &nmg);
                  Type **mtys;

                  if (nmg) {
                    mtys = tyargs(nmg);
                    for (zi = 0; zi < nmg; zi++)
                      mtys[zi] = 0;
                  } else
                    mtys = 0;
                  { /* the call's own freeze picture is the outer
                     * one's: the receiver among the arguments, taken
                     * before it walked (01-types.md) */
                    if (n != t->nargs)
                      berr(e, "'%s::%s' takes %lu arguments, %lu given", s->name, nm1,
                           (unsigned long) t->nargs, (unsigned long) n);
                    { /* the receiver already walked: check its type
                       * where it stands, the rest against the shape */
                      Type *c;

                      if (tysame(rt, t->args[0]))
                        c = rt;
                      else
                        c = recoerce(args[0], t->args[0], fe);
                      if (!c || !tysame(c, t->args[0]))
                        berr(args[0], "'%s::%s' wants %s here, this is %s", s->name, nm1,
                             btys(t->args[0]), btys(rt));
                    }
                    for (i = 1; i < n; i++) {
                      Type *at = rexpr(args[i], fe, t->args[i]);

                      argfit(args[i], t->args[i], at, mg, mtys, nmg, 0, 0, 0, fe, nm1, 0);
                    }
                    thawargs(svs, n); /* the call is done; its borrows ended with it */
                  }
                  memberdone(e, mg, mtys, nmg, nm1, 0, 0, 0, 0);
                  return projopen(nmg ? gsubst(t->t, mg, mtys, nmg) : t->t, e);
                }
              }
              { /* the rows the receiver alone cannot order: a row's
                 * signature is a claim about the call's other
                 * arguments too -- `Add::add(p, n)` with n usize and
                 * the pointer's own row both wait under the borrowed
                 * row's narrower one. The call walks the rows itself,
                 * the most specific first, and the first that takes
                 * its arguments is the call's. A row whose slots the
                 * receiver alone cannot land reads its own words here
                 * -- the variables stand in the signature, and the
                 * arguments bind them, the landing a generic fn's
                 * call makes (07-operators.md); the binding runs on a
                 * copy, so a row the trial walks away from leaves the
                 * candidate's slots as open as it found them. A row
                 * that did not take unwinds what its walk froze and
                 * moved -- the overload chain's own discipline (04)
                 * -- and the report, when none takes them, is the
                 * first row's: the pick the receiver would have made
                 * alone (07-operators.md). */
                Implcand cs[64];
                usize    nc = implcands(s, self, nm1, cs, 64);
                int      soft = 1;
                Ast    **mg;
                Type   **mtys;
                Type    *at;
                int     *snap;
                int      ok;
                usize    ci;

                if (!nc)
                  berr(args[0], "no '%s' for %s", s->name, btys(self));
              trial:
                for (ci = 0; ci < nc; ci++) {
                  Type **rtys; /* the row's binding, worked on a copy */
                  int    part = 0;

                  im = cs[ci].m;
                  imp = cs[ci].imp;
                  tys = cs[ci].tys;
                  if (tys) {
                    usize g;

                    rtys = tyargs(imp->ngparams);
                    memcpy(rtys, tys, imp->ngparams * sizeof *tys);
                    for (g = 0; g < imp->ngparams; g++)
                      if (!rtys[g])
                        part = 1;
                  } else
                    rtys = 0;
                  /* a partial binding keeps the row's own words: the
                   * variables stand, the arguments land them below */
                  t = part ? im->ty
                           : (rtys ? gsubst(im->ty, imp->gparams, rtys, imp->ngparams) : im->ty);
                  mg = membergps(im, &nmg);
                  if (nmg) {
                    mtys = tyargs(nmg);
                    for (zi = 0; zi < nmg; zi++)
                      mtys[zi] = 0;
                  } else
                    mtys = 0;
                  if (n != t->nargs) {
                    if (soft)
                      continue;
                    berr(e, "'%s::%s' takes %lu arguments, %lu given", s->name, nm1,
                         (unsigned long) t->nargs, (unsigned long) n);
                  }
                  { /* the receiver already walked: check its type
                     * where it stands, the rest against the row */
                    if (!tysame(rt, t->args[0])) {
                      Type *c = recoerce(args[0], t->args[0], fe);

                      if (!c || !tysame(c, t->args[0])) {
                        if (soft)
                          continue;
                        berr(args[0], "'%s::%s' wants %s here, this is %s", s->name, nm1,
                             btys(t->args[0]), btys(rt));
                      }
                    }
                  }
                  snap = movsnap(fe);
                  ok = 1;
                  for (i = 1; ok && i < n; i++) {
                    at = rexpr(args[i], fe, t->args[i]);
                    ok = argfit(args[i], t->args[i], at, mg, mtys, nmg, imp->gparams, rtys,
                                imp->ngparams, fe, nm1, soft);
                  }
                  if (ok)
                    ok = memberdone(e, mg, mtys, nmg, nm1, soft, imp->gparams, rtys, imp->ngparams);
                  if (ok && rtys) { /* the row's own slots: the
                                     * receiver's landing and the
                                     * arguments' together, the bounds
                                     * with them (07-operators.md) */
                    usize g;

                    for (g = 0; g < imp->ngparams; g++)
                      if (!rtys[g]) {
                        if (soft) {
                          ok = 0;
                          break;
                        }
                        berr(e, "cannot infer '%s' for '%s': it rides no argument this call passes",
                             imp->gparams[g]->v.gp.name, rowname(imp));
                      }
                    if (ok && !boundsok(imp, rtys)) {
                      if (soft)
                        ok = 0;
                      else
                        rowbounds(imp, rtys, e);
                    }
                  }
                  if (!ok) { /* this row did not take: its freezes
                              * unwound, its moves with them */
                    thawargs(svs, n);
                    movrestore(fe, snap);
                    continue;
                  }
                  e->v.call.sym = im->sym; /* the row's member: the body that runs */
                  thawargs(svs, n);        /* the call is done; its borrows ended with it */
                  e->v.call.tys = insttys(imp, rtys, mg, mtys, nmg);
                  /* the answer under both landings -- the row's own
                   * slots first (a no-op where the receiver landed
                   * them whole), the member's own after. A local
                   * walk: the signature the row shares with every
                   * other call must not hear it */
                  {
                    Type *rt = gsubst(t->t, imp->gparams, rtys, imp->ngparams);

                    return projopen(nmg ? gsubst(rt, mg, mtys, nmg) : rt, e);
                  }
                }
                if (soft) {
                  soft = 0;
                  nc = 1;
                  goto trial;
                }
                return 0; /* unreachable */
              }
            }
          }
        }
        if (!s || s->kind != Stype)
          berr(e, "unknown name '%s'", nm0);
        if (s->tykind == TYenum) {
          struct Variant *v = varfind(s, nm1);

          if (!v)
            berr(e, "'%s' has no variant '%s'", s->name, nm1);
          return mkvariant(s, v, e, args, n, fe, want);
        }
        {
          Sym    *imp;
          Member *m = inherentfind(s, nm1, &imp);
          usize   nmg, zi;

          if (!m)
            berr(e,
                 "'%s' has no inherent '%s'; a trait method is '%s' spelled "
                 "with its trait (05-traits.md)",
                 s->name, nm1, nm1);
          if (m->kind != Mfn)
            berr(e, "'%s::%s' is not callable", s->name, nm1);
          if (imp && imp->ngparams) /* the binding comes from a
                                     * receiver's type, and this form
                                     * has none: call it as a method */
            berr(e,
                 "'%s' belongs to a generic impl; call it as a method, where the "
                 "receiver binds the parameters",
                 nm1);
          { /* the member's own family: this form has no receiver
             * binding to carry -- the impl is exact -- so the
             * arguments bind all of it */
            Ast  **mg = membergps(m, &nmg);
            Type **mtys;
            Type  *t = m->ty;

            if (nmg) {
              mtys = tyargs(nmg);
              for (zi = 0; zi < nmg; zi++)
                mtys[zi] = 0;
            } else
              mtys = 0;
            e->v.call.sym = m->sym; /* the method's own fn: the emitter's pick */
            {
              Frzsave *svs = n ? arenaalloc(n * sizeof *svs) : 0;
              usize    k;

              if (svs) { /* as in callfn: a borrow argument's freeze
                          * ends with the call (01-types.md) */
                memset(svs, 0, n * sizeof *svs);
                for (k = 0; k < n; k++)
                  argborrow(args[k], fe, &svs[k]);
              }
              if (n != t->nargs)
                berr(e, "'%s::%s' takes %lu arguments, %lu given", s->name, nm1,
                     (unsigned long) t->nargs, (unsigned long) n);
              {
                usize i;

                for (i = 0; i < n; i++) {
                  Type *at = rexpr(args[i], fe, t->args[i]);

                  argfit(args[i], t->args[i], at, mg, mtys, nmg, 0, 0, 0, fe, nm1, 0);
                }
              }
              thawargs(svs, n); /* the call is done; its borrows ended with it */
            }
            memberdone(e, mg, mtys, nmg, nm1, 0, 0, 0, 0);
            e->v.call.tys = insttys(0, 0, mg, mtys, nmg);
            return projopen(nmg ? gsubst(t->t, mg, mtys, nmg) : t->t, e);
          }
        }
      }
      berr(e, "a path this long arrives with namespaces (11)");
    }
    if (f->k == Naccess) {                 /* the method sugar: x.f(...) (05-traits.md) */
      Type   *rt = rplace(f->v.fld.e, fe); /* a place: no move just to call */
      Member *m;
      Sym    *imp;
      Type   *t, *ty;
      usize   nmg, zi;

      if (!rt)
        rt = rexpr(f->v.fld.e, fe, 0); /* a computed receiver: f().m() */
      if (!rt)
        return 0;
      ty = derefthrough(rt); /* a pointer receiver is dereferenced first */
      if (ty->k == Tydyn) {  /* a handle's call: the vtable's, not any
                              * table's -- the choice was made where
                              * the handle was (06-dispatch.md) */
        Member *dm = 0;
        usize   di;
        Type   *t;

        for (di = 0; di < ty->sym->nmembers; di++)
          if (strcmp(ty->sym->members[di].name, f->v.fld.name) == 0) {
            dm = &ty->sym->members[di];
            break;
          }
        if (!dm)
          berr(f, "'%s' has no '%s'", ty->sym->name, f->v.fld.name);
        if (dm->kind != Mfn)
          berr(f, "'%s::%s' is not a method", ty->sym->name, f->v.fld.name);
        t = projsubst(selfsubst(dm->ty, tyvoidptr()), ty); /* Self
                                                            * erased and the
                                                            * projections given:
                                                            * what survives object
                                                            * safety is pointers,
                                                            * all one width */
        if (ty->sym->ngparams)                             /* the family's own arguments: a pack
                                                            * parameter bound the whole tuple
                                                            * spells out one row a parameter --
                                                            * the signature every caller reads
                                                            * (04-generics.md, 06-dispatch.md) */
          t = tyfnspread(t, ty->sym->gparams, ty->args, ty->sym->ngparams);
        if (t->nargs && t->args[0] && t->args[0]->k == Typtr && t->args[0]->t->k == Tymut &&
            !ty->mut)
          berr(f, "'%s' is a mut method; a 'dyn mut %s' handle carries it", f->v.fld.name,
               ty->sym->name);
        e->v.call.sym = 0; /* no direct fn: the emitter reads the vtable */
        e->v.call.tys = 0;
        {
          Frzsave *svs = n ? arenaalloc(n * sizeof *svs) : 0;
          usize    k;

          if (svs) {
            memset(svs, 0, n * sizeof *svs);
            for (k = 0; k < n; k++)
              argborrow(args[k], fe, &svs[k]);
          }
          if (n + 1 != t->nargs)
            berr(e, "'%s' takes %lu arguments, %lu given", f->v.fld.name,
                 (unsigned long) (t->nargs - 1), (unsigned long) n);
          { /* the receiver is the fat itself: its type is the
             * handle's, not any self -- the emitter adapts it */
            usize i;

            for (i = 0; i < n; i++) {
              Type *at = rexpr(args[i], fe, t->args[i + 1]);

              if (at && t->args[i + 1] && !tysame(at, t->args[i + 1])) {
                Type *c = recoerce(args[i], t->args[i + 1], fe);

                if (!c || !tysame(c, t->args[i + 1]))
                  berr(args[i], "'%s' wants %s here, this is %s", f->v.fld.name,
                       btys(t->args[i + 1]), btys(at));
              }
            }
          }
          thawargs(svs, n); /* the call is done; its borrows ended with it */
        }
        return t->t;
      }
      if (ty->k != Tystruct && ty->k != Tyunion && ty->k != Tyenum && ty->k != Typaram &&
          ty->k != Tyint && ty->k != Tybool && ty->k != Tyunit && ty->k != Tyvoidptr &&
          ty->k != Typtr && ty->k != Tyslice && ty->k != Tyarray && ty->k != Tytuple)
        berr(f, "a method call needs a receiver an impl can name; a trait's calls "
                "come by its impls (05-traits.md)");
      { /* the inherent table first (05-traits.md: the namespaces are
         * separate), then the trait one: a match binds the impl's
         * parameters from the receiver's type, the same walk a
         * struct literal runs (04-generics.md) */
        Type **tys = 0;
        int    declared = 0; /* a bound's signature, inside a generic fn */
        Ast   *hitb = 0;     /* the bound that named the trait */

        m = inherentfindt(ty, f->v.fld.name, &imp, &tys);
        if (!m && ty->k == Typaram) {
          /* a generic fn's parameter: no impl resolves here -- the
           * bound names the trait, the declaration's signature
           * carries the walk, and the instantiation's re-check
           * picks the impl (04-generics.md) */
          Ast **bs = ty->gp->v.gp.bounds;
          usize bi, mi;

          for (bi = 0; bi < vlen(bs); bi++) {
            Sym *tr = bs[bi]->v.path.sym; /* the bound's own cache (04) */

            for (mi = 0; mi < tr->nmembers; mi++)
              if (tr->members[mi].kind == Mfn && strcmp(tr->members[mi].name, f->v.fld.name) == 0) {
                m = &tr->members[mi];
                hitb = bs[bi];
                declared = 1;
                break;
              }
            if (m)
              break;
          }
          if (!m)
            berr(f, "'%s' has no method '%s'; a bound on the parameter brings it", f->v.fld.name,
                 f->v.fld.name);
        }
        if (!m) {
          /* the trait rows, no trait named: every row that fits the
           * receiver and carries the member, the most specific
           * first. The receiver alone ordered them until now; a
           * row's signature is a claim about the call's other
           * arguments too, so the call walks the rows itself and
           * the first that takes its arguments is the call's --
           * the spelled call's own discipline (07-operators.md). A
           * row that did not take them unwinds what its walk
           * froze and moved, and the report, when none takes
           * them, is the first row's: the pick the receiver would
           * have made alone. */
          Implcand cs[64];
          usize    nc = traitcands(ty, f->v.fld.name, cs, 64);
          int      soft = 1;
          Ast    **mg;
          Type   **mtys;
          Type    *at;
          int     *snap;
          Frzsave *svs = n ? arenaalloc(n * sizeof *svs) : 0;
          usize    i, k;

          if (!nc)
            berr(f, "'%s' has no method '%s'", btys(ty), f->v.fld.name);
          if (svs) { /* the explicit arguments' borrows end with the
                      * call too, as any call's do (01-types.md) */
            memset(svs, 0, n * sizeof *svs);
            for (k = 0; k < n; k++)
              argborrow(args[k], fe, &svs[k]);
          }
        row:
          for (zi = 0; zi < nc; zi++) {
            Type **rtys; /* the row's binding, worked on a copy */
            int    part = 0;

            m = cs[zi].m;
            imp = cs[zi].imp;
            tys = cs[zi].tys;
            if (tys) {
              usize g;

              rtys = tyargs(imp->ngparams);
              memcpy(rtys, tys, imp->ngparams * sizeof *tys);
              for (g = 0; g < imp->ngparams; g++)
                if (!rtys[g])
                  part = 1;
            } else
              rtys = 0;
            /* a partial binding keeps the row's own words: the
             * variables stand, the arguments land them
             * (07-operators.md) */
            t = part ? m->ty : (rtys ? gsubst(m->ty, imp->gparams, rtys, imp->ngparams) : m->ty);
            mg = membergps(m, &nmg);
            if (nmg) {
              mtys = tyargs(nmg);
              for (k = 0; k < nmg; k++)
                mtys[k] = 0;
            } else
              mtys = 0;
            {
              Frzsave sv;
              int     ok = 1;

              memset(&sv, 0, sizeof sv);
              snap = movsnap(fe);
              if (!recvadapt(f->v.fld.e, t->nargs ? t->args[0] : 0, rt, ty, fe, &sv, soft))
                ok = 0;
              if (ok && n + 1 != t->nargs) {
                if (soft)
                  ok = 0;
                else
                  berr(e, "'%s' takes %lu arguments, %lu given", f->v.fld.name,
                       (unsigned long) (t->nargs - 1), (unsigned long) n);
              }
              for (i = 0; ok && i < n; i++) {
                at = rexpr(args[i], fe, t->args[i + 1]);
                ok = argfit(args[i], t->args[i + 1], at, mg, mtys, nmg, imp->gparams, rtys,
                            imp->ngparams, fe, f->v.fld.name, soft);
              }
              if (ok)
                ok = memberdone(f, mg, mtys, nmg, f->v.fld.name, soft, imp->gparams, rtys,
                                imp->ngparams);
              if (ok && rtys) { /* the row's own slots and their
                                 * bounds, the arguments' landing with
                                 * the receiver's (07-operators.md) */
                usize g;

                for (g = 0; g < imp->ngparams; g++)
                  if (!rtys[g]) {
                    if (soft) {
                      ok = 0;
                      break;
                    }
                    berr(e, "cannot infer '%s' for '%s': it rides no argument this call passes",
                         imp->gparams[g]->v.gp.name, rowname(imp));
                  }
                if (ok && !boundsok(imp, rtys)) {
                  if (soft)
                    ok = 0;
                  else
                    rowbounds(imp, rtys, e);
                }
              }
              if (!ok) { /* this row did not take: its freezes
                          * unwound, its moves with them */
                thawargs(svs, n);
                frzrestore(&sv);
                movrestore(fe, snap);
                continue;
              }
              e->v.call.sym = m->sym; /* the row's member: the body that runs */
              thawargs(svs, n);       /* the explicit arguments' borrows, LIFO */
              frzrestore(&sv);        /* the receiver's borrow ends with the call */
              {                       /* a shared pointer self over a receiver that names
                                       * no place -- the value materialised into one, the
                                       * walk re-entered on the name (01-types.md); the
                                       * row's own freezes gave the re-entry a clean
                                       * field, its answers land it again */
                Type *mt = recvmat(e, f, t->nargs ? t->args[0] : 0, rt, fe, want);

                if (mt)
                  return mt;
              }
              e->v.call.tys = insttys(imp, rtys, mg, mtys, nmg);
              /* the answer under both landings -- a local walk, the
               * row's shared signature left as it stood */
              {
                Type *rt = gsubst(t->t, imp->gparams, rtys, imp->ngparams);

                return projopen(nmg ? gsubst(rt, mg, mtys, nmg) : rt, e);
              }
            }
          }
          if (soft) {
            soft = 0;
            nc = 1;
            goto row;
          }
          return 0; /* unreachable */
        }
        if (m->kind != Mfn)
          berr(f, "'%s' is not a method", f->v.fld.name);
        if (declared) {
          Sym *tr = hitb->v.path.sym; /* the trait the bound named */

          t = selfsubst(m->ty, ty);
          if (tr->ngparams) { /* the trait's own generics: the ones
                               * the bound spelled, the rest their
                               * defaults -- the same filling the
                               * spelled call takes
                               * (07-operators.md) */
            Type **btys = hitb->v.path.tys;
            usize  nb = vlen(hitb->v.path.segs[0]->v.seg.args);
            Type **ttys = dflttail(tr, btys, nb, 0, ty, f);

            t = gsubst(t, tr->gparams, ttys, tr->ngparams);
          }
          e->v.call.sym = 0; /* the re-check writes the impl's pick */
          e->v.call.tys = 0;
        } else {
          t = m->ty;
          if (tys) /* a pattern impl's method: Self and the pattern's
                    * variables, under the receiver's binding */
            t = gsubst(t, imp->gparams, tys, imp->ngparams);
          e->v.call.sym = m->sym; /* the method's own fn: the emitter's pick */
          e->v.call.tys = tys;    /* the impl's binding; the member's
                                   * own joins it after the walk below */
        }
        { /* a shared pointer self over a receiver that names no
           * place -- a literal, a call's answer: the value
           * materialised into one, the walk re-entered on the
           * name (01-types.md) */
          Type *mt = recvmat(e, f, t->nargs ? t->args[0] : 0, rt, fe, want);

          if (mt)
            return mt;
        }
        { /* the member's own family, bound from the arguments */
          Ast  **mg = membergps(m, &nmg);
          Type **mtys;

          if (nmg) {
            mtys = tyargs(nmg);
            for (zi = 0; zi < nmg; zi++)
              mtys[zi] = 0;
          } else
            mtys = 0;
          {
            Frzsave  sv;
            Frzsave *svs = n ? arenaalloc(n * sizeof *svs) : 0;
            usize    k;

            memset(&sv, 0, sizeof sv);
            if (svs) { /* the explicit arguments' borrows end with the
                        * call too, as any call's do (01-types.md) */
              memset(svs, 0, n * sizeof *svs);
              for (k = 0; k < n; k++)
                argborrow(args[k], fe, &svs[k]);
            }
            recvadapt(f->v.fld.e, t->nargs ? t->args[0] : 0, rt, ty, fe, &sv, 0);
            if (n + 1 != t->nargs)
              berr(e, "'%s' takes %lu arguments, %lu given", f->v.fld.name,
                   (unsigned long) (t->nargs - 1), (unsigned long) n);
            {
              usize i;

              for (i = 0; i < n; i++) {
                Type *at = rexpr(args[i], fe, t->args[i + 1]);

                argfit(args[i], t->args[i + 1], at, mg, mtys, nmg, 0, 0, 0, fe, f->v.fld.name, 0);
              }
            }
            thawargs(svs, n); /* the explicit arguments' borrows, LIFO */
            frzrestore(&sv);  /* the receiver's borrow ends with the call */
          }
          memberdone(f, mg, mtys, nmg, f->v.fld.name, 0, imp ? imp->gparams : 0, imp ? tys : 0,
                     imp ? imp->ngparams : 0);
          if (!declared)
            e->v.call.tys = insttys(imp, tys, mg, mtys, nmg);
          return projopen(nmg ? gsubst(t->t, mg, mtys, nmg) : t->t, e);
        }
      }
    }
    { /* an arbitrary callee: a fn-typed expression */
      Type    *t = rexpr(f, fe, 0);
      Frzsave *svs = n ? arenaalloc(n * sizeof *svs) : 0;
      usize    k;

      if (!t || t->k != Tyfn)
        berr(f, "this is %s, not callable", btys(t));
      if (svs) { /* a borrow argument's freeze ends with the call
                  * (01-types.md) */
        memset(svs, 0, n * sizeof *svs);
        for (k = 0; k < n; k++)
          argborrow(args[k], fe, &svs[k]);
      }
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
      thawargs(svs, n); /* the call is done; its borrows ended with it */
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

        if (bt->nargs == (usize) bt->sym->ngparams) /* a generic
                                                     * struct's field
                                                     * reads under the
                                                     * instance (04) */
          ft = gsubst(ft, bt->sym->gparams, bt->args, bt->nargs);
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
    if (bt->k == Typaram && bt->gp->v.gp.pack) { /* the pack's rows:
                                                  * the binding's own, and this read defers to it
                                                  * (04-generics.md) */
      evalblackbox++;
      if (e->v.n2.b->k != Nint)
        berr(e->v.n2.b, "a pack's index is a row number the compiler reads (04-generics.md)");
      return want ? want : tyunit();
    }
    if (bt->k == Tytuple || bt->k == Tyunit) { /* a tuple's row by
                                                * number, the dot's
                                                * own spelling (04-generics.md) */
      Ast *base = e->v.n2.a;

      if (bt->k == Tyunit || e->v.n2.b->k != Nint)
        berr(e, "a tuple's index is a row number the compiler reads (04-generics.md)");
      { /* the rewrite lands the row read the checker already has */
        u64 ix = e->v.n2.b->v.i.num;

        if (ix >= bt->nargs)
          berr(e->v.n2.b, "row %lu out of range for %s", (unsigned long) ix, btys(bt));
        e->k = Ntupidx;
        e->v.tup.e = base;
        e->v.tup.idx = ix;
        return rexpr(e, fe, want);
      }
    }
    if (bt->k != Tyarray && bt->k != Tyslice) {
      berr(e, "%s cannot be indexed", btys(bt));
    }
    if (!it || !isintty(it))
      berr(e->v.n2.b, "an index is an integer, this is %s", btys(it));
    if (e->v.n2.b->k == Nint && bt->k == Tyarray && !bt->gp /* the
                                                             * length a const parameter names has
                                                             * no number here: the re-check under
                                                             * the binding checks it (08) */
        && e->v.n2.b->v.i.num >= bt->n)
      berr(e->v.n2.b, "index %lu out of range for %s", (unsigned long) e->v.n2.b->v.i.num,
           btys(bt));
    { /* an element read yields the value; the element's mut layer is
       * the slot's permission, not the value's own type -- one layer
       * only: a [2]mut [2]mut i32 read yields [2]mut i32 (01) */
      Type *t = bt->t;

      if (t && t->k == Tymut)
        t = t->t;
      { /* the read moves it when it is not Copy (03-move.md); a computed
         * base is a value already */
        Local *rt = placeroot(e, fe, pbuf, sizeof pbuf);

        if (rt && !iscopy(t)) {
          if (issprrow(e)) /* an array spread's own row: the whole
                            * binding's move, this row its spelling
                            * -- the binding dies whole, the marking
                            * a trial that refuses unwinds with every
                            * other move (04-generics.md) */
            rt->dead = 1;
          else
            berr(e, "cannot move out of a place: %s is not Copy (@take, 03)", btys(t));
        }
      }
      return t;
    }
  }
  case Nrangeindex: { /* a[..] or a[lo..hi]: the slice view (01); a
                       * tuple's own -- the sub-tuple, its rows spelled
                       * out, a compile-time fact (04-generics.md) */
    Type *bt = rplace(e->v.ridx.e, fe);

    if (!bt)
      bt = rexpr(e->v.ridx.e, fe, 0); /* a computed base: f()[..], a value */
    if (!bt)
      return 0;
    bt = derefthrough(bt);
    if (bt->k == Typaram && bt->gp->v.gp.pack) { /* the pack's rows:
                                                  * the binding's own, and this view defers to it
                                                  * (04-generics.md) */
      evalblackbox++;
      if (e->v.ridx.lo)
        rexpr(e->v.ridx.lo, fe, 0);
      if (e->v.ridx.hi)
        rexpr(e->v.ridx.hi, fe, 0);
      return want ? want : tyunit();
    }
    if (bt->k == Tytuple || bt->k == Tyunit) { /* ts[1..]: the
                                                * sub-tuple the rows hold */
      usize n = bt->k == Tytuple ? bt->nargs : 0;
      int   bb = evalblackbox;
      u64   lo = 0, hi = n;

      if (bt->k == Tytuple) { /* a spread's rows: the binding's own,
                               * their count its answer -- the view
                               * defers to it (04-generics.md) */
        usize ri;

        for (ri = 0; ri < n; ri++)
          if (bt->args[ri]->k == Tyspread) {
            evalblackbox++;
            return want ? want : tyunit();
          }
      }

      if (e->v.ridx.lo) { /* the bounds are compile-time facts -- the
                           * rows the type holds, a runtime bound names
                           * none of them */
        Val v;

        rexpr(e->v.ridx.lo, fe, 0);
        v = ceval(e->v.ridx.lo, fe->env, tyint(IN_USIZE));
        lo = v.i;
      }
      if (evalblackbox != bb)
        return want ? want : tyunit(); /* a bound the binding answers:
                                        * defer (04-generics.md) */
      if (e->v.ridx.hi) {
        Val v;

        rexpr(e->v.ridx.hi, fe, 0);
        v = ceval(e->v.ridx.hi, fe->env, tyint(IN_USIZE));
        hi = v.i;
      }
      if (evalblackbox != bb)
        return want ? want : tyunit();
      if (lo > n || hi > n || lo > hi)
        berr(e, "rows %lu..%lu out of range for %s", (unsigned long) lo, (unsigned long) hi,
             btys(bt));
      { /* the rewrite spells the sub-tuple as its rows, and every
         * pass below reads an ordinary tuple literal */
        Ast  *base = e->v.ridx.e;
        Ast **ts = vnew(Ast *, hi - lo ? hi - lo : 1);
        u64   k;

        for (k = lo; k < hi; k++) {
          Ast *ix = mknear(Ntupidx, e);

          ix->v.tup.e = base;
          ix->v.tup.idx = k;
          vappend(&ts, &ix);
        }
        e->k = Ntuple;
        e->v.list.ts = ts;
        return rexpr(e, fe, want);
      }
    }
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
        bt->k == Tyarray && !bt->gp /* the length a const parameter
                                     * names: the re-check checks it
                                     * (08-reflection.md) */
        && (e->v.ridx.lo->v.i.num > e->v.ridx.hi->v.i.num || e->v.ridx.hi->v.i.num > bt->n))
      berr(e, "slice bounds out of range for %s", btys(bt));
    return tyslice(bt->t);
  }
  case Ntupidx: {
    Type *bt = rplace(e->v.tup.e, fe); /* the base as a place: a row
                                        * read moves the row, never
                                        * the tuple that holds it
                                        * (03-move.md) */
    char pbuf[256];

    if (!bt)
      bt = rexpr(e->v.tup.e, fe, 0); /* a computed base: f().0, a value */
    if (!bt)
      return 0;
    bt = derefthrough(bt); /* a *mut base: the row the pointer lends,
                            * the same walk every base takes
                            * (01-types.md) */
    if (bt->k == Tytuple && e->v.tup.idx < bt->nargs &&
        bt->args[e->v.tup.idx]->k == Tyspread) { /* a spread's row: the
                                                  * binding's own, the
                                                  * read defers to it
                                                  * (04-generics.md) */
      evalblackbox++;
      return want ? want : tyunit();
    }
    if (bt->k != Tytuple)
      berr(e, "%s is not a tuple", btys(bt));
    if (e->v.tup.idx >= bt->nargs)
      berr(e, "tuple index %lu out of range", (unsigned long) e->v.tup.idx);
    { /* the row read out of a place moves it when it is not Copy
       * (03-move.md); a computed base is a value already */
      Type  *ft = bt->args[e->v.tup.idx];
      Local *rt = placeroot(e, fe, pbuf, sizeof pbuf);

      if (rt && !iscopy(ft)) {
        if (issprrow(e)) /* a spread's own row: the whole binding's
                          * move, this row its spelling -- the
                          * binding dies whole, the marking a trial
                          * that refuses unwinds with every other
                          * move (04-generics.md) */
          rt->dead = 1;
        else
          berr(e, "cannot move out of a place: %s is not Copy (@take, 03)", btys(ft));
      }
    }
    { /* (T, mut U): the row's slot permission stays with the
       * checker, the type goes out -- as an array read does */
      Type *ft = bt->args[e->v.tup.idx];

      while (ft && ft->k == Tymut)
        ft = ft->t;
      return ft;
    }
  }
  case Ntry: {
    Type *t = rplace(e->v.n1.e, fe); /* the operand, read as a place:
                                      * the move ?'s match takes is
                                      * rmatch's own scrutinee walk
                                      * (09-match.md), and a value
                                      * pre-walk would take it twice
                                      * -- the second reading a
                                      * binding the first moved. A
                                      * computed operand is no place:
                                      * its type is the match's own
                                      * walk's to have, the check
                                      * waits behind it */
    int early = t != 0;

    if (!fe->fnret)
      berr(e, "'?' outside a fn");
    if (early)
      trychk(e, fe, t);
    { /* the propagation spelled the match it is (01-types.md): the
       * value through, the error handed back the way a return hands
       * anything, the frame's bindings' destructors riding it out
       * (03-move.md) -- the return's own walk hangs them. The arms'
       * names carry a dot: nothing a program declared can meet them,
       * the enum's own match the pattern (03-move.md). */
      Ast  *op = e->v.n1.e;
      Ast **arms = vnew(Ast *, 2);
      Ast  *okarm = opnode(Narm, e);
      Ast  *erarm = opnode(Narm, e);
      Ast  *okpat = opnode(Nppath, e);
      Ast  *erpat = opnode(Nppath, e);

      okpat->v.ppath.path = opnode(Npath, e);
      okpat->v.ppath.path->v.path.segs = vnew(Ast *, 1);
      opvpush(&okpat->v.ppath.path->v.path.segs, opseg("Ok", e));
      okpat->v.ppath.payload = vnew(Ast *, 1);
      { /* Ok(.ok): the binding, the arm's value the name itself */
        Ast *b = opnode(Npath, e);

        b->v.path.segs = vnew(Ast *, 1);
        opvpush(&b->v.path.segs, opseg(".ok", e));
        opvpush(&okpat->v.ppath.payload, b);
      }
      okarm->v.n2.a = okpat;
      okarm->v.n2.b = opnode(Npath, e);
      okarm->v.n2.b->v.path.segs = vnew(Ast *, 1);
      opvpush(&okarm->v.n2.b->v.path.segs, opseg(".ok", e));
      erpat->v.ppath.path = opnode(Npath, e);
      erpat->v.ppath.path->v.path.segs = vnew(Ast *, 1);
      opvpush(&erpat->v.ppath.path->v.path.segs, opseg("Err", e));
      erpat->v.ppath.payload = vnew(Ast *, 1);
      { /* Err(.err): the binding, the arm's body the return */
        Ast *b = opnode(Npath, e);

        b->v.path.segs = vnew(Ast *, 1);
        opvpush(&b->v.path.segs, opseg(".err", e));
        opvpush(&erpat->v.ppath.payload, b);
      }
      { /* return Err(.err), the fn's own shape answering: the
         * construction's want is the fn's return, the error type
         * the same one both sides of the check above already
         * agreed on (01-types.md) */
        Ast *blk = opnode(Nblock, e);
        Ast *ret = opnode(Nreturn, e);
        Ast *call = opnode(Ncall, e);
        Ast *f = opnode(Npath, e);
        Ast *v = opnode(Npath, e);

        f->v.path.segs = vnew(Ast *, 2);
        opvpush(&f->v.path.segs, opseg("Result", e));
        opvpush(&f->v.path.segs, opseg("Err", e));
        v->v.path.segs = vnew(Ast *, 1);
        opvpush(&v->v.path.segs, opseg(".err", e));
        call->v.call.f = f;
        call->v.call.args = vnew(Ast *, 1);
        opvpush(&call->v.call.args, v);
        ret->v.n1.e = call;
        blk->v.blk.stmts = vnew(Ast *, 1);
        opvpush(&blk->v.blk.stmts, ret);
        erarm->v.n2.a = erpat;
        erarm->v.n2.b = blk;
      }
      opvpush(&arms, okarm);
      opvpush(&arms, erarm);
      memset(&e->v, 0, sizeof e->v);
      e->k = Nmatch;
      e->v.call.f = op;
      e->v.call.args = arms;
    }
    {
      Type *rt = rmatch(e, fe, want);

      if (!early && rt) /* the computed operand's type, the match's
                         * own walk the only one that had it */
        trychk(e, fe, e->v.call.f->ty);
      return rt;
    }
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
    {
      int dive = mustexit(e->v.ifx.then); /* the branch never lands:
                                           * its value nothing, it
                                           * agrees with the other
                                           * side whatever that is
                                           * (10-iteration.md) */

      tt = rexpr(e->v.ifx.then, &ft, dive ? 0 : want);
      if (dive)
        unreach(&ft); /* the then never reaches what follows */
      if (e->v.ifx.els) {
        int dive2;

        ff = fefork(fe);
        if (side == -1) /* == narrows the else */
          locnarrow(&ff, nm, child);
        dive2 = mustexit(e->v.ifx.els);
        tf = rexpr(e->v.ifx.els, &ff,
                   dive2 ? 0 : (want ? want : (dive ? 0 : tt))); /* None takes its
                                                                  * ?T from the other side: the then
                                                                  * spelled it out (01-types.md) */
        if (dive2)
          unreach(&ff);
        if (!dive && !dive2 && !tysame(tt, tf))
          berr(e, "the branches disagree: %s and %s", btys(tt), btys(tf));
        fejoin(fe, &ft, &ff);
        if (dive && dive2) /* neither branch lands: no value the if
                            * produces, the world's own want standing
                            * in (10-iteration.md) */
          return want ? want : tyunit();
        return dive ? tf : tt; /* the landing branch's shape alone */
      }
    }
    { /* no else: the implicit fall-through is the untouched state */
      Fenv f0 = fefork(fe);

      fejoin(fe, &ft, &f0);
    }
    return tyunit(); /* the statement form: no value */
  }
  case Ncif: { /* the condition must be compile-time known, and the
                * branch is picked here: the taken block the
                * conditional's own, the untaken discarded before
                * checking -- the code it holds may only compile for
                * some instantiations (04-generics.md). A condition
                * that names a generic's own defers the whole
                * conditional to the re-check under the binding, the
                * same routing a match on @typeinfo takes
                * (08-reflection.md) */
    Type *ct = rexpr(e->v.ifx.cond, fe, 0);
    int   bb;
    Val   cv;
    Ast  *tb, *eb;

    if (!ct || ct->k != Tybool)
      berr(e->v.ifx.cond, "a const if condition is a bool, this is %s", btys(ct));
    bb = evalblackbox;
    cv = ceval(e->v.ifx.cond, fe->env, tybool());
    if (evalblackbox != bb) {        /* the parameter, the black box: which
                                      * branch lives is the instance's own */
      evalblackbox = bb;             /* the flag dies with the walk that raised it */
      return want ? want : tyunit(); /* the shape the world around it
                                      * wants, or none when no slot
                                      * names one: the instance's
                                      * walk types it for real (04) */
    }
    tb = e->v.ifx.then;
    eb = e->v.ifx.els;
    if (cv.i) { /* taken: the block is the conditional's own now, the
                 * walk re-entering it as itself -- the match route's
                 * own trick (09-match.md) */
      e->k = Nblock;
      e->v.blk.stmts = tb->v.blk.stmts;
      e->v.blk.tail = tb->v.blk.tail;
      return rblock(e, fe, want);
    }
    if (eb) { /* untaken, an else in hand: it stands where the
               * conditional stood -- a block, an if-chain, another
               * const if alike */
      e->k = eb->k;
      e->v = eb->v;
      return rexpr(e, fe, want);
    }
    e->k = Nblock; /* untaken, no else: nothing runs, nothing checks */
    e->v.blk.stmts = 0;
    e->v.blk.tail = 0;
    return tyunit();
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
    if (e->v.arrlit.mut) /* [N]mut T / []mut T: the elements are
                          * writable (01-types.md) -- the same wrap
                          * a type-position [N]mut T takes */
      et = tymut(et);
    { /* the value the elements are checked against: the mut layer
       * is the slot's permission, not the element's own type */
      Type *vt = et;

      while (vt && vt->k == Tymut)
        vt = vt->t;
      for (i = 0; i < n; i++) {
        Type *at = rexpr(es[i], fe, vt);

        if (at && !tysame(at, vt)) {
          Type *c = recoerce(es[i], vt, fe);

          if (!c || !tysame(c, vt))
            berr(es[i], "the elements are %s, this is %s", btys(vt), btys(at));
        }
      }
    }
    if (e->v.arrlit.len) { /* the length is a const expression,
                            * evaluated here as a type's own is
                            * (08-reflection.md); more initializers
                            * than the length is the error; fewer is
                            * the zero fill (01-types.md) */
      u64 ln = cevallong(e->v.arrlit.len, fe->env, tyint(IN_USIZE));

      if (ln < n)
        berr(e, "[%lu] holds %lu elements, %lu given", (unsigned long) ln, (unsigned long) ln,
             (unsigned long) n);
      return tyarray(ln, et);
    }
    return tyslice(et); /* []T: the unsized literal */
  }
  case Nstructlit: {
    Ast  **inits = e->v.slit.inits;
    usize  n = vlen(inits), i, j;
    Ast  **segs = e->v.slit.path->v.path.segs;
    usize  nsegs = vlen(segs);
    Ns    *ns;
    usize  k;
    Sym   *s;
    Type **tys = 0;
    Type  *st;

    k = nshead(segs, nsegs, &ns, e->v.slit.path->v.path.root);
    if (k == nsegs) /* a namespace names no literal (11-namespaces.md) */
      berr(e, "a namespace names no literal; name what is in it (11-namespaces.md)");
    segs += k; /* the namespaces walked fall away: the literal's own
                * type lands where the walk did (11-namespaces.md) */
    nsegs -= k;
    s = nsegs == 1 && !e->v.slit.path->v.path.root
            ? (k ? nsitem(ns, segs[0]->v.seg.name) : symfind(segs[0]->v.seg.name))
            : 0;
    if (s && k && !s->pub) /* a literal is a value the same as any
                            * other: qualified across namespaces, a
                            * private type stays home
                            * (11-namespaces.md) */
      berr(e, "'%s' is private to %s (11-namespaces.md)", s->name, nsname(ns));

    if (nsegs == 2 && !e->v.slit.path->v.path.root) {
      Sym *es = k ? nsitem(ns, segs[0]->v.seg.name) : symfind(segs[0]->v.seg.name);

      if (es && k && !es->pub) /* the named-payload construction the
                                * same door (11-namespaces.md) */
        berr(e, "'%s' is private to %s (11-namespaces.md)", es->name, nsname(ns));
      if (es && es->kind == Stype && es->tykind == TYenum) {
        /* Enum::Variant{..}: a named payload constructed by name,
         * the fields' own order the same construction a positional
         * call is -- the inits reordered into it, the node
         * rewritten a call's shape, so every pass after walks it
         * as one (01-types.md) */
        struct Variant *v = varfind(es, segs[1]->v.seg.name);
        Ast           **args;
        usize           k;

        if (!v)
          berr(e, "'%s' has no variant '%s'", es->name, segs[1]->v.seg.name);
        if (!v->named)
          berr(e, "'%s::%s' carries a positional payload; construct it with parentheses", es->name,
               v->name);
        for (i = 0; i < n; i++) /* a name twice, before the order
                                 * would mask it behind a missing one */
          for (j = i + 1; j < n; j++)
            if (strcmp(inits[i]->v.init.name, inits[j]->v.init.name) == 0)
              berr(inits[j], "field '%s' given twice", inits[j]->v.init.name);
        args = vnew(Ast *, v->nfields ? v->nfields : 1);
        for (k = 0; k < v->nfields; k++) {
          Ast *in = 0;

          for (i = 0; i < n; i++)
            if (strcmp(inits[i]->v.init.name, v->fields[k].name) == 0) {
              for (j = 0; j < k; j++)
                if (args[j] == inits[i]->v.init.e)
                  berr(inits[i], "field '%s' given twice", v->fields[k].name);
              in = inits[i];
              break;
            }
          if (!in)
            berr(e, "'%s::%s' is missing '%s'", es->name, v->name, v->fields[k].name);
          { /* a vec: the passes after read the count off its head */
            Ast *x = in->v.init.e;

            vappend(&args, &x);
          }
        }
        for (i = 0; i < n; i++) { /* a name the payload does not carry */
          for (k = 0; k < v->nfields; k++)
            if (strcmp(inits[i]->v.init.name, v->fields[k].name) == 0)
              break;
          if (k == v->nfields)
            berr(inits[i], "'%s::%s' has no field '%s'", es->name, v->name, inits[i]->v.init.name);
        }
        { /* the rewrite: a call in the node's own place, the path
           * kept -- the emitter and the evaluator walk it as the
           * construction it is */
          Ast  *p = e->v.slit.path;
          Ast **as = args;
          Type *r;

          memset(&e->v, 0, sizeof e->v);
          e->k = Ncall;
          e->v.call.f = p;
          e->v.call.args = as;
          r = mkvariant(es, v, e, as, v->nfields, fe, want);
          e->ty = r;
          return r;
        }
      }
    }
    if (s && s->kind == Stype && (s->tykind == TYstruct || s->tykind == TYunion) && s->ngparams) {
      /* a generic struct or union literal: the binding comes the
       * way a call's does (04-generics.md) -- the type expected of
       * it first, the field values binding what that leaves open,
       * the declaration's defaults covering the rest */
      Type *w = want;

      while (w && w->k == Tymut) /* the permission a place lends is
                                  * not the type (01-types.md) */
        w = w->t;
      tys = tyargs(s->ngparams);
      if (w && (w->k == Tystruct || w->k == Tyunion) && w->sym == s && w->nargs == s->ngparams) {
        for (i = 0; i < s->ngparams; i++)
          tys[i] = w->args[i];
      }
      st = 0; /* the walk binds; tysym closes */
    } else {
      st = rpath(e->v.slit.path, &fe->env);
      if (!st)
        return 0;
      if (st->k != Tystruct && st->k != Tyunion)
        berr(e, "%s is not a struct or union", btys(st));
    }
    for (i = 0; i < n; i++) {
      Ast   *in = inits[i];
      Field *f = 0;
      usize  k;

      for (k = 0; k < (tys ? s : st->sym)->nfields; k++)
        if (strcmp((tys ? s : st->sym)->fields[k].name, in->v.init.name) == 0) {
          f = &(tys ? s : st->sym)->fields[k];
          break;
        }
      if (!f)
        berr(in, "'%s' has no field '%s'", (tys ? s : st->sym)->name, in->v.init.name);
      for (j = 0; j < i; j++)
        if (strcmp(inits[j]->v.init.name, in->v.init.name) == 0)
          berr(in, "field '%s' given twice", in->v.init.name);
      { /* the field's type under the binding so far: while a slot
         * is open gsubst would build a half-bound type, so the
         * field carries no want at all -- the value names the
         * open parameters itself, and T stands for T when a
         * binding rides along, the call rule again (04-generics.md) */
        Type *ft = f->ty;
        usize g;

        if (tys) {
          for (g = 0; g < s->ngparams; g++)
            if (!tys[g]) {
              ft = 0;
              break;
            }
          if (ft)
            ft = gsubst(f->ty, s->gparams, tys, s->ngparams);
        }
        {
          Type *at = rexpr(in->v.init.e, fe, ft);

          if (at && ft && tysame(at, ft)) {
            if (tys)
              gunify(f->ty, at, s->gparams, tys, s->ngparams);
            continue;
          }
          if (at && ft) {
            Type *c = recoerce(in->v.init.e, ft, fe);

            if (c && tysame(c, ft))
              continue;
          }
          if (!at || !tys || !gunify(f->ty, at, s->gparams, tys, s->ngparams)) {
            /* a slot may hold a parameter the want lent -- a
             * caller's, a row's -- not a binding the fields made:
             * where it stands in the value's way the value's word
             * outranks it, and the fields bind what the want only
             * lent (04-generics.md) */
            int   lent = 0;
            usize g;

            if (at && tys)
              for (g = 0; g < s->ngparams; g++)
                if (tys[g] && tys[g]->k == Typaram) {
                  tys[g] = 0;
                  lent = 1;
                }
            if (!(lent && gunify(f->ty, at, s->gparams, tys, s->ngparams)))
              berr(in->v.init.e, "field '%s' is %s, this is %s", f->name, btys(ft), btys(at));
          }
        }
      }
    }
    if (tys) {
      Env denv = envnone(); /* the declaration's own: a default sees
                             * the parameters before it, not the
                             * fn's names (04-generics.md) */

      denv.b = arenaalloc(s->ngparams * sizeof *denv.b);
      denv.n = 0;
      for (i = 0; i < s->ngparams; i++) {
        if (!tys[i]) {
          Ast *d = s->gparams[i]->v.gp.dflt;

          if (!d)
            berr(e, "cannot infer '%s' for '%s' from the literal", s->gparams[i]->v.gp.name,
                 s->name);
          tys[i] = rty(d, &denv);
        }
        denv.b[i].name = s->gparams[i]->v.gp.name; /* the slot
                                                    * reaches the
                                                    * count only
                                                    * once bound */
        denv.b[i].t = tys[i];
        denv.n = i + 1;
      }
      st = tysym(s, tys, s->ngparams);
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
    return rbuiltin(e, fe, want);
  case Nrange: { /* a..b: the interval's own struct, the checker's
                  * sugar -- two ends, one integer type, the value
                  * std::ops::Range<T> holds (10-iteration.md). The
                  * literal adapts the operator's way: one side
                  * names the type, the other yields */
    Type *ta = rexpr(e->v.bin.l, fe, 0);
    Type *tb = rexpr(e->v.bin.r, fe, ta); /* the other side names the ends' type */

    if (!tysame(ta, tb)) { /* a literal yields to the other side */
      Type *c = recoerce(e->v.bin.l, tb, fe);

      if (c)
        ta = c;
      else if ((c = recoerce(e->v.bin.r, ta, fe)))
        tb = c;
    }
    if (!ta || !tysame(ta, tb) || !isintty(ta))
      berr(e, "a range's ends are one integer type, these are %s and %s (10-iteration.md)",
           btys(ta), btys(tb));
    { /* the interval itself: the struct the sugar lands in */
      Type **tys = tyargs(1);

      tys[0] = ta;
      return e->ty = tysym(sym_range, tys, sym_range->ngparams);
    }
  }
  case Nspread: {            /* the pack's own rows, inside a black box: the
                              * tuple's shape rides the binding, and the re-check
                              * under the instance walks the rows for real
                              * (04-generics.md) */
    int *snap = movsnap(fe); /* the operand read here is a look, not
                              * a take: the rows the re-check spells
                              * read it for real, once, where they
                              * stand (03-move.md) */
    Type *t = rexpr(e->v.un.e, fe, 0);

    movrestore(fe, snap);
    if (t && t->k == Typaram && t->gp->v.gp.pack) {
      evalblackbox++;
      return want ? want : tyunit();
    }
    if (t && (t->k == Tytuple || t->k == Tyunit)) { /* the binding's
                                                     * own rows, the re-check's walk: the group a
                                                     * tuple literal of them -- the same expansion
                                                     * the (x, ...t) spelling takes, every row read
                                                     * where it stands (04-generics.md) */
      Ast *tup = mknear(Ntuple, e);
      Ast *sp = mknear(Nspread, e);

      sp->v.un.e = e->v.un.e;
      tup->v.list.ts = vnew(Ast *, 1);
      vappend(&tup->v.list.ts, &sp);
      *e = *tup; /* the splice: the walk below reads the tuple */
      return rexpr(e, fe, want);
    }
    berr(e, "pack spreads arrive with generics (04-generics.md)");
    return 0; /* unreachable */
  }
  default: /* the type nodes: type values arrive with reflection */
    berr(e, "this is a type, not an expression; $$ arrives with reflection (08)");
  }
  return 0; /* unreachable */
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

/* does this slice expression view storage the fn itself laid down?
 * There is no borrow checker, but the two shapes a compiler can see
 * are refused: a literal's elements, and the rows of an array that
 * lives in this frame. A slice of a slice points somewhere else
 * still, and rides on (01-types.md). */
static int
localview(Ast *e, Fenv *fe)
{
  Type *bt;

  if (!e)
    return 0;
  if (e->k == Narraylit && !e->v.arrlit.len)
    return 1; /* []T{...}: the elements are this frame's own */
  if (e->k == Nrangeindex) {
    bt = rplace(e->v.ridx.e, fe);
    if (!bt)
      bt = rexpr(e->v.ridx.e, fe, 0);
    return bt && bt->k == Tyarray;
  }
  return 0;
}

/* the leftmost name a place grows from, when it is a global: a
 * const has no address to write, a static's mut is its own
 * permission (01-types.md). What does not root in a global says
 * yes and the walk below decides */
static int
globwritable(Ast *p, Fenv *fe)
{
  while (p->k == Naccess || p->k == Nindex || p->k == Ntupidx || p->k == Nrangeindex) {
    if (p->k == Naccess)
      p = p->v.fld.e;
    else if (p->k == Nindex)
      p = p->v.n2.a;
    else if (p->k == Ntupidx)
      p = p->v.tup.e;
    else
      p = p->v.ridx.e;
  }
  if (p->k == Npath && vlen(p->v.path.segs) == 1 && !p->v.path.root) {
    Sym *s;

    if (locfind(fe, p->v.path.segs[0]->v.seg.name))
      return 1; /* a local root: the walk below decides */
    s = symfind(p->v.path.segs[0]->v.seg.name);
    if (s && s->kind == Sstatic && s->decl && s->decl->v.cst.mut)
      return 1;
    return 0; /* a const has no address to write (01-types.md); the
               * rest are not globals a place writes */
  }
  return 1;
}

int
placewritable(Ast *p, Fenv *fe)
{
  switch (p->k) {
  case Npath: {
    Local *l = locfind(fe, p->v.path.segs[0]->v.seg.name);

    if (l)
      return l->mut;
    { /* a global: a static mut's own slot; a const has none */
      Sym *s = symfind(p->v.path.segs[0]->v.seg.name);

      return s && s->kind == Sstatic && s->decl && s->decl->v.cst.mut;
    }
  }
  case Naccess: { /* the two mut levels are orthogonal (01-types.md):
                   * a.b is writable by b's own mut, never by the
                   * binding's -- and a pointer base must be *mut all
                   * the way, for it lends what it lends (README) */
    Type *bt;
    usize i;

    if (!globwritable(p, fe))
      return 0; /* the base's own global says no (01-types.md) */
    if (!derefswritable(p->v.fld.e, fe))
      return 0; /* a *T lends nothing writable */
    bt = rplace(p->v.fld.e, fe);
    if (!bt)
      bt = rexpr(p->v.fld.e, fe, 0); /* a global or computed base */
    bt = derefthrough(bt);
    if (!bt)
      return 0;
    if (bt->k == Tyslice)
      return 0; /* the two slots read, never write: @slice builds
                 * the view whole, and no half-built slice ever
                 * stands between two writes (01-types.md) */
    if (bt->k != Tystruct && bt->k != Tyunion)
      return 0;
    for (i = 0; i < bt->sym->nfields; i++)
      if (strcmp(bt->sym->fields[i].name, p->v.fld.name) == 0)
        return bt->sym->fields[i].mut; /* the field is its own slot */
    return 0;
  }
  case Nindex: {
    Type *bt;

    if (!globwritable(p, fe))
      return 0; /* the base's own global says no (01-types.md) */
    if (!derefswritable(p->v.n2.a, fe))
      return 0;
    bt = rplace(p->v.n2.a, fe);
    if (!bt)
      bt = rexpr(p->v.n2.a, fe, 0);
    bt = derefthrough(bt); /* a *mut base: the row the pointer lends,
                            * the field walk's own shape ahead
                            * (01-types.md) */
    if (!bt)
      return 0;
    if (bt->k == Tyslice || bt->k == Tyarray)
      return bt->t->k == Tymut; /* []mut T / [N]mut T */
    return 0;
  }
  case Ntupidx: { /* (T, mut U): the row is its own slot (01-types.md) */
    Type *bt;

    if (!globwritable(p, fe))
      return 0; /* the base's own global says no (01-types.md) */
    if (!derefswritable(p->v.tup.e, fe))
      return 0;
    bt = rplace(p->v.tup.e, fe);
    if (!bt)
      bt = rexpr(p->v.tup.e, fe, 0);
    bt = derefthrough(bt); /* a *mut base, the same lend: the field
                            * walk derefsthrough its own base, the row
                            * its own slot behind the pointer
                            * (01-types.md) */
    if (!bt)
      return 0;
    if (bt->k != Tytuple || p->v.tup.idx >= bt->nargs)
      return 0;
    return bt->args[p->v.tup.idx]->k == Tymut;
  }
  case Nrangeindex: {
    Type *bt;

    if (!globwritable(p, fe))
      return 0; /* the base's own global says no (01-types.md) */
    if (!derefswritable(p->v.ridx.e, fe))
      return 0;
    bt = rplace(p->v.ridx.e, fe);
    if (!bt)
      bt = rexpr(p->v.ridx.e, fe, 0);
    {
      Type *tb = derefthrough(bt);

      if (tb && tb->k == Tytuple) { /* the tail's rows: each one its
                                     * own slot, the range writable
                                     * when every row it spans is
                                     * (01-types.md) -- the bounds
                                     * the place's own walk folded,
                                     * numbers on the node */
        u64 lo = p->v.ridx.lo && p->v.ridx.lo->k == Nint ? p->v.ridx.lo->v.i.num : 0;
        u64 hi = p->v.ridx.hi && p->v.ridx.hi->k == Nint ? p->v.ridx.hi->v.i.num : tb->nargs;
        u64 k;

        if (lo > tb->nargs || hi > tb->nargs || lo > hi)
          return 0;
        for (k = lo; k < hi; k++)
          if (tb->args[k]->k != Tymut)
            return 0;
        return 1;
      }
    }
    return bt && bt->k == Tyarray && bt->t->k == Tymut;
  }
  case Nun:
    if (p->v.un.op == Tstar) { /* *p = v: p must be a *mut */
      Type *pt;
      int   spent = spentborrow(p->v.un.e);

      if (spent) /* the same spend: asking the type again freezes
                  * nothing either (01-types.md) */
        fe->nofreeze++;
      pt = rexpr(p->v.un.e, fe, 0);
      if (spent)
        fe->nofreeze--;
      return pt && pt->k == Typtr && pt->t->k == Tymut;
    }
    return 0;
  default:
    return 0; /* a temporary: not writable */
  }
}

/* $.it, the iterator's own binding (10-iteration.md): a name with a
 * dot, nothing a program declared can meet. Bound mut -- next
 * writes through it -- and past the loop's own base: a break
 * destructs the round's bindings alone, the iterator itself
 * outliving the break, its own destructors the exit's. A return
 * still finds it -- the fn's base sits below every loop's */
static void
itpush(Fenv *fb, Type *it)
{
  locpush(fb, "$.it", it, 1);
  fb->loopbs[fb->nloopbs - 1] = fb->n; /* the loop's base moves past
                                        * the iterator: the round's
                                        * bindings alone are a
                                        * break's own to destruct */
}

void
rstmt(Ast *st, Fenv *fe)
{
  switch (st->k) {
  case Nlet: {
    Type *t = st->v.let.t ? rty(st->v.let.t, &fe->env) : 0;
    Type *et;

    if (st->v.let.e && mustexit(st->v.let.e)) { /* the init never
                                                 * lands: nothing to
                                                 * bind, the binding
                                                 * the shape it
                                                 * spelled, the unit
                                                 * when it spelled
                                                 * none
                                                 * (10-iteration.md) */
      rexpr(st->v.let.e, fe, 0);                /* the arguments still check: a
                                                 * compile error is a compile error
                                                 * on every path */
      st->ty = t ? t : tyunit();
      rpat(st->v.let.pat, st->ty, fe, st->v.let.mut);
      return;
    }
    et = st->v.let.e ? rexpr(st->v.let.e, fe, t) : 0;

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
    if (st->v.let.cv && st->v.let.pat->k == Npath && vlen(st->v.let.pat->v.path.segs) == 1 &&
        !st->v.let.pat->v.path.root) {
      /* the unroll's own round value, riding the binding it spelled:
       * a name argument read against this frame finds its bytes on
       * the local (08-reflection.md) */
      Local *l = locfind(fe, st->v.let.pat->v.path.segs[0]->v.seg.name);

      if (l)
        l->cv = st->v.let.cv;
    }
    return;
  }
  case Nassign: {
    Tok   op = st->v.bin.op;
    Type *lt = rplace(st->v.bin.l, fe); /* the left is a place the
                                         * store writes, not a value
                                         * the read moves (03) --
                                         * `*p = v` and `x.f = v` walk
                                         * here now, a place's read
                                         * taking nothing */
    Type *rt;

    if (!lt)
      lt = rexpr(st->v.bin.l, fe, 0); /* not a place: the report below
                                       * names what it is */

    if (opbarelocal(st->v.bin.l)) /* the left is a place the store
                                   * writes, not a value the read
                                   * moves -- the walk's own value
                                   * read marked it, and the marking
                                   * unwinds here (03-move.md): the
                                   * right side is still to walk, and
                                   * `a = Add::add(&a, &b)` reads a
                                   * living a */
      opunmove(st->v.bin.l, fe);
    if (!isplace(st->v.bin.l))
      berr(st->v.bin.l, "assignment needs a place on the left");
    if (!placewritable(st->v.bin.l, fe)) {
      if (st->v.bin.l->k == Naccess) { /* a slice's own two slots:
                                        * read for the ABI they hand
                                        * C, written never -- @slice
                                        * is the one step that builds
                                        * a view (01-types.md) */
        Type *bt = derefthrough(st->v.bin.l->v.fld.e->ty);

        if (bt && bt->k == Tyslice && slicefield(bt, st->v.bin.l->v.fld.name))
          berr(st->v.bin.l,
               "'%s' is one of a slice's own two slots, read-only: "
               "@slice builds a view whole (01-types.md)",
               st->v.bin.l->v.fld.name);
      }
      berr(st->v.bin.l, "this place is not a mut slot (01-types.md)");
    }
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
      if (opbarelocal(st->v.bin.l)) /* the store leaves the binding
                                     * holding the value it was
                                     * given: alive again -- `a = a`
                                     * moved the right side into it,
                                     * and the assignment's own words
                                     * end with the left alive
                                     * (03-move.md) */
        opunmove(st->v.bin.l, fe);
      { /* the old value's destructor, when the type owns one
         * (03-move.md): the store runs it first -- a row of its own,
         * or the fields it inherited, spelled whole the same way a
         * scope's end spells them -- the calls pre-made, their
         * place the place itself, so the emitter drops what it
         * overwrites. */
        if (hasdrop(lt))
          dropcalls(st->v.bin.l, lt, &st->v.bin.drop, st);
      }
      return;
    }
    /* the compound forms: the operator's own rules */
    rt = rexpr(st->v.bin.r, fe, lt);
    {
      Tok   bop = op == Tpluseq    ? Tplus
                  : op == Tminuseq ? Tminus
                  : op == Tstareq  ? Tstar
                  : op == Tslasheq ? Tslash
                  : op == Tshleq   ? Tshl
                                   : Tshr;
      Type *res = 0;

      if (binop(bop, lt, rt, &res))
        return;
      if ((op == Tpluseq || op == Tminuseq || op == Tstareq || op == Tslasheq || op == Tshleq ||
           op == Tshreq) &&
          (!opscalar1(lt) || (lt && rt && tysame(lt, rt)))) { /* the
                                                               * arithmetic compound: its own trait
                                                               * now (07-operators.md) --
                                                               * AddAssign, SubAssign, MulAssign,
                                                               * DivAssign, and the shifts' own
                                                               * pair -- the left borrowed
                                                               * for the write, the right a value
                                                               * the parameter's slot takes
                                                               * whole; a mixed scalar pair is
                                                               * the language's own error
                                                               * instead, the rows all Rhs =
                                                               * Self (opscalars above) */
        const char *tr = op == Tpluseq    ? "AddAssign"
                         : op == Tminuseq ? "SubAssign"
                         : op == Tstareq  ? "MulAssign"
                         : op == Tslasheq ? "DivAssign"
                         : op == Tshleq   ? "ShlAssign"
                                          : "ShrAssign";
        const char *mth = op == Tpluseq    ? "add_assign"
                          : op == Tminuseq ? "sub_assign"
                          : op == Tstareq  ? "mul_assign"
                          : op == Tslasheq ? "div_assign"
                          : op == Tshleq   ? "shl_assign"
                                           : "shr_assign";

        opunmove(st->v.bin.l, fe); /* the left is borrowed for the
                                    * write, not moved by the entry's
                                    * read; the right moves once
                                    * whole below */
        opunmove(st->v.bin.r, fe);
        { /* the call: std::ops::<Trait>::<method>(&mut l, r) */
          Ast *f = oppath(tr, st);
          Ast *c = opnode(Ncall, st);
          Ast *x = opnode(Nexprstmt, st);

          opvpush(&f->v.path.segs, opseg(mth, st));
          c->v.call.f = f;
          c->v.call.args = vnew(Ast *, 2);
          opvpush(&c->v.call.args, opborrow(st->v.bin.l, 1, st));
          opvpush(&c->v.call.args, st->v.bin.r);
          x->v.n1.e = c;
          memset(&st->v, 0, sizeof st->v);
          st->k = Nexprstmt;
          memcpy(&st->v, &x->v, sizeof st->v);
          rstmt(st, fe); /* re-entered: the call's own walk, its
                          * dispatch the one that answers where no
                          * row exists */
          return;
        }
      }
      berr(st, "this compound assignment does not fit %s and %s", btys(lt), btys(rt));
    }
    return;
  }
  case Nreturn: {
    int dive = st->v.n1.e && mustexit(st->v.n1.e); /* return
                                                    * panic("..."):
                                                    * the value never
                                                    * exists, nothing
                                                    * to compare
                                                    * (10-iteration.md) */
    Type *t = st->v.n1.e ? rexpr(st->v.n1.e, fe, dive ? 0 : fe->fnret) : tyunit();

    if (!fe->fnret)
      berr(st, "return outside a fn");
    if (!dive && !tysame(t, fe->fnret)) {
      Type *c = st->v.n1.e ? recoerce(st->v.n1.e, fe->fnret, fe) : 0;

      if (!c || !tysame(c, fe->fnret))
        berr(st, "the fn returns %s, this is %s", btys(fe->fnret), btys(t));
    }
    if (fe->fnret->k == Tyslice && localview(st->v.n1.e, fe)) /* a
                                                               * slice the fn can see is its own
                                                               * dies with the frame (01) */
      berr(
          st->v.n1.e,
          "this slice views the fn's own storage; return the array by value instead (01-types.md)");
    st->v.n1.drops = scopedrops(fe, fe->fnbase, st); /* the early
                                                      * exit: every
                                                      * binding the
                                                      * frame owns,
                                                      * innermost
                                                      * first, the
                                                      * value home
                                                      * first of all
                                                      * (03-move.md) */
    return;
  }
  case Nbreak:
  case Ncontinue:
    if (fe->loopd <= 0)
      berr(st, "%s outside a for", st->k == Nbreak ? "break" : "continue");
    st->v.n1.drops =
        scopedrops(fe, fe->loopbs[fe->nloopbs - 1], st); /* the loop's
                                                          * own base: the pattern's bindings with
                                                          * the body's, the round that never
                                                          * reached its end (03-move.md) */
    return;
  case Nfor: {
    Ast    *body = st->v.forx.body;
    Type   *et = 0;
    Fenv    fb;
    usize   nbase;
    Frzsave sv; /* the source's own borrow, when it was one: what it
                 * froze lives the loop's life alone -- the rounds
                 * lend the owner out, the exit hands it back
                 * (01-types.md) */
    int srcthaw = 0;

    /* an Iter source consumes once, before the loop -- the sugar's
     * own walk, or the re-check's copy via's -- and the move the
     * outer frame's own: the read lands here, ahead of the fork,
     * so the copy below carries the death along -- a frame the
     * fork's own copy missed would destruct the moved-out slot a
     * second time at its exit (03-move.md) */
    if (st->v.forx.shape == FIN) /* the source a mut borrow: its
                                  * freeze unwinds at the loop's exit,
                                  * not the frame's -- the borrowed
                                  * iterator ends where the loop does
                                  * (10-iteration.md) */
      srcthaw = argborrow(st->v.forx.b, fe, &sv);
    if (st->v.forx.shape == FIN || st->v.forx.it)
      et = rexpr(st->v.forx.shape == FIN ? st->v.forx.b : st->v.forx.via, fe, 0);

    fb = fefork(fe);
    nbase = fb.n;

    /* the loop marks: moves of bindings from before the outermost
     * loop repeat every round (03-move.md) */
    fb.loopbase = fe->loopd > 0 ? fe->loopbase : fb.n;
    if (fb.nloopbs >= (int) (sizeof fb.loopbs / sizeof fb.loopbs[0]))
      berr(st, "loops nest deeper than the checker carries");
    fb.loopbs[fb.nloopbs++] = nbase; /* the innermost base: what a
                                      * break or a continue's own
                                      * destructors cover (03) */
    fb.loopd++;
    switch (st->v.forx.shape) {
    case FCOND: {
      Type *ct = rexpr(st->v.forx.a, &fb, 0); /* re-checked every round */

      if (!ct || ct->k != Tybool)
        berr(st->v.forx.a, "a for condition is a bool, this is %s", btys(ct));
      break;
    }
    case FLET: {         /* for let pat = e: e is matched every round (10) */
      if (st->v.forx.it) /* an iterator's own for-let, the sugar's
                          * making or the re-check's copy: the next
                          * call reads the binding by name (10) */
        itpush(&fb, st->v.forx.it);
      et = rexpr(st->v.forx.b, &fb, 0);
      while (et && et->k == Tymut) /* a *mut read: the emitter
                                    * strips this too (emafor) */
        et = et->t;
      rpat(st->v.forx.a, et, &fb, 0);
      break;
    }
    case FIN: { /* for pat in e: what e yields, one binding a round */
      /* the source is consumed once, before the first round -- an
       * owned array moves in here, and is not re-read every round
       * (10) -- the walk itself ran before the fork above, its
       * move the outer frame's own */
      while (et && et->k == Tymut) /* ditto */
        et = et->t;
      if (!et)
        break;
      if (et->k == Tyarray) /* an owned array yields each element
                             * itself, and is consumed -- a place
                             * of non-Copy elements is the mover's
                             * to @take (10, 03) */
        rpat(st->v.forx.a, et->t, &fb, 0);
      else if (et->k == Tyenum && et->sym == sym_option)
        rpat(st->v.forx.a, et->args[0], &fb, 0); /* ?T iterates T or ends */
      else { /* the Iter path: the sugar rides std::iter's own, the
              * desugar 10-iteration.md spells -- c.into_iter() once,
              * it.next() a round, for let Some(x) the shape it all
              * becomes (10). A slice rides it too: the library's
              * own Iter for []T hands the pointers out, the header
              * a copy the source keeps whole (10) */
        Sym    *iimp, *timp;
        Type  **tys, **ttys;
        Member *im = implfind(sym_intoiter, et, "into_iter", &iimp, &tys);
        Member *tm;
        Type   *sig, *it, *ret, *nt;
        Ast    *place, *recv, *borrow, *f, *c;

        if (et->k == Typaram) /* the bound's own signature carries a
                               * generic's method calls (04) -- the
                               * iterable's ride arrives with that
                               * milestone */
          berr(st->v.forx.b,
               "iterating the parameter '%s' arrives with a later milestone (04-generics.md)",
               et->gp->v.gp.name);
        if (!im)
          berr(st->v.forx.b, "iterating %s takes an IntoIter (10-iteration.md)", btys(et));
        sig = iimp->ngparams ? gsubst(im->ty, iimp->gparams, tys, iimp->ngparams) : im->ty;
        it = projopen(sig->t, st->v.forx.b); /* a row may answer in a
                                              * projection of its own (04) */
        if (!it)
          berr(st->v.forx.b, "the IntoIter for %s names no iterator (10-iteration.md)", btys(et));
        tm = implfind(sym_iter, it, "next", &timp, &ttys);
        if (!tm)
          berr(st->v.forx.b, "the iterator %s is no Iter (10-iteration.md)", btys(it));
        sig = timp->ngparams ? gsubst(tm->ty, timp->gparams, ttys, timp->ngparams) : tm->ty;
        ret = projopen(sig->t, st->v.forx.b);
        if (!ret || ret->k != Tyenum || ret->sym != sym_option)
          berr(st->v.forx.b, "'next' for %s must answer ?Item, not %s (10-iteration.md)", btys(it),
               btys(ret));

        st->v.forx.it = it; /* the emitter's own gate: the prologue
                             * and the exit the iterator asks */
        itpush(&fb, it);    /* $.it: past the loop's base, a break's own
                             * destructors the round's alone (03, 10) */

        /* via: c.into_iter(), the trait's own spelling -- the row
         * this find landed, the emitter's instance its own. The
         * source node itself rides as the argument: the read above
         * moved it, this call the move's destination (03) */
        f = opnode(Npath, st->v.forx.b);
        f->v.path.root = 1; /* the sugar's own words, ::spelled: a
                             * std file sits inside std::iter, where
                             * a bare std names nothing -- the root's
                             * door the one path every file reads the
                             * same (11-namespaces.md) */
        f->v.path.segs = vnew(Ast *, 4);
        opvpush(&f->v.path.segs, opseg("std", st->v.forx.b));
        opvpush(&f->v.path.segs, opseg("iter", st->v.forx.b));
        opvpush(&f->v.path.segs, opseg("IntoIter", st->v.forx.b));
        opvpush(&f->v.path.segs, opseg("into_iter", st->v.forx.b));
        c = opnode(Ncall, st->v.forx.b);
        c->v.call.f = f;
        c->v.call.args = vnew(Ast *, 1);
        opvpush(&c->v.call.args, st->v.forx.b);
        c->v.call.sym = im->sym;
        c->v.call.tys = iimp->ngparams ? tys : 0;
        c->ty = it;
        st->v.forx.via = c;

        /* the iterator's own destructors, the exit's: what it still
         * holds destructs there, break and natural end alike (03) */
        place = opnode(Npath, st->v.forx.b);
        place->v.path.segs = vnew(Ast *, 1);
        opvpush(&place->v.path.segs, opseg("$.it", st->v.forx.b));
        place->ty = it;
        dropcalls(place, it, &st->v.forx.itdrops, st->v.forx.b);

        /* the round's own call, the trait spelled whole: the
         * receiver a real borrow of the checker's own binding, the
         * pick this walk's -- the sugar itself re-walked no other
         * way than a program's own spelling (10) */
        recv = opnode(Npath, st->v.forx.b);
        recv->v.path.segs = vnew(Ast *, 1);
        opvpush(&recv->v.path.segs, opseg("$.it", st->v.forx.b));
        recv->ty = it;
        borrow = opnode(Nun, st->v.forx.b);
        borrow->v.un.op = Tamp;
        borrow->v.un.mut = 1;
        borrow->v.un.e = recv;
        f = opnode(Npath, st->v.forx.b);
        f->v.path.root = 1; /* ditto: the compiler's own naming,
                             * absolute from the root, so the walk
                             * reads the same inside std and out
                             * (11-namespaces.md) */
        f->v.path.segs = vnew(Ast *, 4);
        opvpush(&f->v.path.segs, opseg("std", st->v.forx.b));
        opvpush(&f->v.path.segs, opseg("iter", st->v.forx.b));
        opvpush(&f->v.path.segs, opseg("Iter", st->v.forx.b));
        opvpush(&f->v.path.segs, opseg("next", st->v.forx.b));
        c = opnode(Ncall, st->v.forx.b);
        c->v.call.f = f;
        c->v.call.args = vnew(Ast *, 1);
        opvpush(&c->v.call.args, borrow);
        st->v.forx.b = c; /* the source's node rides via's argument above */

        { /* the pattern dressed in Some: the payload's bindings the
           * user's own, the scrutinee -- ?Item -- picking the
           * variant out itself (09) */
          Ast *user = st->v.forx.a;

          st->v.forx.a = opnode(Nppath, user);
          f = opnode(Npath, user);
          f->v.path.segs = vnew(Ast *, 1);
          opvpush(&f->v.path.segs, opseg("Some", user));
          st->v.forx.a->v.ppath.path = f;
          st->v.forx.a->v.ppath.payload = vnew(Ast *, 1);
          opvpush(&st->v.forx.a->v.ppath.payload, user);
        }

        st->v.forx.shape = FLET;          /* the desugar's own shape: from
                                           * here an ordinary for-let, the
                                           * re-check's copy walking this way */
        nt = rexpr(st->v.forx.b, &fb, 0); /* the FLET walk's own
                                           * lines, spelled here: the
                                           * switch chose FIN */
        while (nt && nt->k == Tymut)
          nt = nt->t;
        rpat(st->v.forx.a, nt, &fb, 0);
      }
      break;
    }
    default:
      berr(st, "this for shape is not one of the three");
    }
    rblock(body, &fb, 0); /* the body's own block ends its own
                           * bindings at its closing brace, this
                           * loop's end the pattern's */
    st->v.forx.drops = scopedrops(&fb, fb.loopbs[fb.nloopbs - 1], st); /* the pattern's
                                                                        * bindings, every
                                                                        * round at its
                                                                        * end: the loop's
                                                                        * own base, the
                                                                        * same base a
                                                                        * continue's --
                                                                        * past an
                                                                        * iterator's
                                                                        * $.it, whose
                                                                        * own end the
                                                                        * exit's (03,
                                                                        * 10) */
    locpop(&fb, nbase); /* what the round bound -- and froze -- ends here */
    if (srcthaw)
      frzrestore(&sv); /* the source's own borrow: the loop's exit
                        * its own end, the owner whole behind the
                        * rounds (01-types.md, 10-iteration.md) */
    return;
  }
  case Ncfor: { /* the iteration already ran (eval.c); what it
                 * spelled walks here, statement by statement -- the
                 * inner const fors flattened where they stood, a
                 * runtime for staying itself -- with no loop around
                 * any of it: a break or a continue has no for to
                 * reach, and the check above says so
                 * (10-iteration.md) */
    Ast **un;
    usize i;

    un = cforunroll(st, fe); /* the frame's own bindings ride along:
                              * a generic's T resolves against them
                              * here, and the instance's walk holds
                              * the instance's (04-generics.md). A
                              * loop a rewrite spelled inside another
                              * round walks here too: the round is
                              * the clone the unroll handed it, so
                              * this node's unroll is its own -- the
                              * shared-tree rule that once forbade
                              * this is the clone's now
                              * (10-iteration.md) */
    if (!un)                 /* the iterable named a generic parameter, the black
                              * box: the rounds are the instance's own, this walk
                              * skips the whole loop, and the re-check under the
                              * binding walks it -- on the clone the emitter hands
                              * that walk (04-generics.md) */
      return;
    ununroll++;
    for (i = 0; i < vlen(un); i++)
      rstmt(un[i], fe);
    ununroll--;
    return;
  }
  case Nexprstmt:
    if (st->v.n1.e->k == Ncall && gatedcall(st->v.n1.e, fe)) {
      /* the mode words' own cull (01-types.md, Mode-gated functions):
       * a call that names no mode here is removed whole -- the
       * statement gone, the arguments with it, neither checked nor
       * evaluated. The shape a const if's untaken arm leaves stands
       * in its place: an empty block, nothing runs (10-iteration.md) */
      st->k = Nblock;
      st->v.blk.stmts = 0;
      st->v.blk.tail = 0;
      st->ty = tyunit();
      return;
    }
    if (st->v.n1.e->k == Ncall) {
      Sym *ts = testcall(st->v.n1.e, fe);

      if (ts) /* the artifact's own door (13-testing.md): a test the
               * statement names is refused, not removed -- the
               * removal is the mode's feature, and this call is the
               * hand's error, the fn it names not carried here */
        berr(st->v.n1.e,
             "'%s' is a test, the artifact's own fn -- the library and the"
             " executable do not carry it (13-testing.md)",
             ts->name);
    }
    rexpr(st->v.n1.e, fe, 0);
    if (st->v.n1.e->ty && hasdrop(st->v.n1.e->ty)) /* a value no
                                                    * one owns dies at
                                                    * its statement's
                                                    * end: the
                                                    * expression
                                                    * itself the
                                                    * place, by
                                                    * value (03-move.md) */
      dropcalls(st->v.n1.e, st->v.n1.e->ty, &st->v.n1.drops, st);
    return;
  default: /* if, match, blocks: expressions in statement position */
    rexpr(st, fe, 0);
    return;
  }
}

/* -- the driver ------------------------------------------------------------ */

/* one fn's body against one binding of its world: the environment,
 * the parameter types, the return -- and the const parameters'
 * values, when an instantiation's walk brings them: they ride the
 * frame's own locals, a compile-time read finding them there. The
 * declaration check calls it with T as a black-box Typaram and no
 * values; an instantiation calls it with both bound (below), and the
 * walk overwrites the body's writeback in place -- the emitter reads
 * it right after, before any other instantiation re-checks the same
 * shared tree. */
static void
runbody(Ast *it, Env env, Type **argtys, Type *ret, Val **cvals, Ast **gparams, Val **gcvals)
{
  Fenv  fe;
  Ast **ps = it->v.fn.params;
  usize n = vlen(ps), i;

  cparammark(ps, argtys, n); /* the const parameters, for the
                              * evaluator to meet by name: a read this
                              * walk holds no value for is the box, the
                              * re-check under the binding answering
                              * (08-reflection.md) */
  memset(&fe, 0, sizeof fe);
  fe.env = env;
  fe.fnret = ret;
  for (i = 0; i < n; i++) {
    locpush(&fe, ps[i]->v.param.name, argtys[i], ps[i]->v.param.mut);
    if (ps[i]->v.param.cnst) { /* the const parameters: the
                                * declaration's walk holds them
                                * empty -- every compile-time read
                                * the box -- and the instance's hands
                                * the frame their values
                                * (08-reflection.md) */
      Local *l = locfind(&fe, ps[i]->v.param.name);

      if (l) {
        l->isconst = 1;
        if (cvals && cvals[i])
          l->cv = cvals[i];
      }
    }
  }
  if (gparams) { /* the const generic parameters: values the angle
                  * brackets spell and a length names ([N]T), but the
                  * frame serves the places a value is asked of them
                  * -- the declaration's walk holds them empty, and
                  * the instance's hands them their numbers
                  * (08-reflection.md) */
    usize ng = vlen(gparams);

    for (i = 0; i < ng; i++)
      if (gparams[i]->v.gp.cnst) {
        Local *l;

        locpush(&fe, gparams[i]->v.gp.name, tyint(IN_USIZE), 0);
        l = locfind(&fe, gparams[i]->v.gp.name);
        if (l) {
          l->isconst = 1;
          if (gcvals && gcvals[i])
            l->cv = gcvals[i];
        }
      }
  }
  if (ret->k == Tyslice &&
      localview(it->v.fn.body->v.blk.tail, &fe)) /* the
                                                  * tail returns, and what it views
                                                  * dies with the frame (01) */
    berr(it->v.fn.body->v.blk.tail,
         "this slice views the fn's own storage; return the array by value instead (01-types.md)");
  { /* the body's value against the declared return: the return
     * statement's own rule, walked the tail's way around -- a
     * value that is not the declared type, or no value where one
     * is declared, is the error either door reports
     * (10-iteration.md). A dive's tail is dead code the block
     * never lands on, and rblock answers the want for it */
    Type *t = rblock(it->v.fn.body, &fe, ret);
    Ast  *tail = it->v.fn.body->v.blk.tail;

    if (!tysame(t, ret)) {
      Type *c = tail ? recoerce(tail, ret, &fe) : 0;

      if (!c || !tysame(c, ret))
        berr(tail ? tail : it->v.fn.body, "the fn returns %s, this is %s", btys(ret), btys(t));
    }
  }
  it->v.fn.drops = scopedrops(&fe, 0, it); /* the parameters' own
                                            * slots: the return the
                                            * body reaches on its
                                            * own runs them before it
                                            * leaves, an early one
                                            * carries its own
                                            * (03-move.md) */
  cparamclear();
}

/* one fn's body: the parameters bind, the return is the want, and
 * the walk is statement-first (the last expression is the value) */
void
checkbodyfn(Sym *s, Ast *it)
{
  bodyfn = s;
  runbody(it, envgparams(0, it->v.fn.gparams, vlen(it->v.fn.gparams)), s->fnty->args, s->fnty->t, 0,
          it->v.fn.gparams, 0);
}

/* one generic fn, one concrete binding: the parameters carry the
 * substituted types and the walk re-runs, the const parameters'
 * baked values riding the frame. What the declaration check proved
 * under a black-box T holds under any concrete one -- the black box
 * is the stricter world -- so the re-check's only failures are the
 * compiler's own bugs (04-generics.md) */
void
recheckfn(Sym *s, Ast *it, Type **tys, Val **cvals, Val **gcvals)
{
  Env    env;
  usize  ng, i;
  Type **ats;

  ng = s->ngparams;
  ats = vlen(it->v.fn.params) ? tyargs(vlen(it->v.fn.params)) : 0;
  env = envnone();
  env.n = ng;
  env.b = ng ? arenaalloc(ng * sizeof *env.b) : 0;
  for (i = 0; i < ng; i++) { /* T is this binding, not a parameter;
                              * a const generic's slot carries its
                              * number too, the lengths read off it */
    env.b[i].name = s->gparams[i]->v.gp.name;
    env.b[i].t = tys[i];
    env.b[i].cv = gcvals ? gcvals[i] : 0;
  }
  if (s->impl) { /* a method's re-check: Self is the impl's target
                  * under this binding, the impl in scope for its
                  * members -- the env checkbodyimpl built, narrowed
                  * to one instance */
    Type *st = s->impl->ifort ? s->impl->ifort : s->impl->ipath;

    env.impl = s->impl;
    env = envpush(&env, "Self", ng ? gsubst(st, s->gparams, tys, ng) : st);
  }
  for (i = 0; i < vlen(it->v.fn.params); i++)
    ats[i] = projopen(gsubstv(s->fnty->args[i], s->gparams, tys, gcvals, ng), it);
  bodyfn = s;
  runbody(it, env, ats, projopen(gsubstv(s->fnty->t, s->gparams, tys, gcvals, ng), it), cvals,
          s->gparams, gcvals);
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
  env = envtraitargs(&env, s); /* the head's trait parameters, bound */
  for (i = 0; i < n; i++)
    if (ms[i]->k == Nfn && ms[i]->v.fn.body) {
      Fenv  fe;
      Env   e2 = envgparams(&env, ms[i]->v.fn.gparams, vlen(ms[i]->v.fn.gparams));
      Ast **ps = ms[i]->v.fn.params;
      usize np = vlen(ps), j;
      Type *fnty = s->members[i].ty;

      for (j = 0; j < np; j++)
        if (ps[j]->v.param.cnst) /* a method's calls route through
                                  * the table half the time, and a
                                  * table slot has nowhere to hand a
                                  * baked argument over
                                  * (08-reflection.md) */
          berr(ps[j], "a const parameter on a method arrives with a later milestone "
                      "(08-reflection.md)");
      { /* the method's own angle brackets: a const length among them
         * rides the same table, and the routing it asks for arrives
         * with the same milestone (08-reflection.md) */
        Ast **gps = ms[i]->v.fn.gparams;
        usize ng = vlen(gps), g;

        for (g = 0; g < ng; g++)
          if (gps[g]->v.gp.cnst)
            berr(gps[g], "a const generic parameter on a method arrives with a later milestone "
                         "(08-reflection.md)");
      }
      memset(&fe, 0, sizeof fe);
      fe.env = e2;
      fe.fnret = fnty->t;
      for (j = 0; j < np; j++)
        locpush(&fe, ps[j]->v.param.name, fnty->args[j], ps[j]->v.param.mut);
      if (fnty->t->k == Tyslice && localview(ms[i]->v.fn.body->v.blk.tail, &fe))
        berr(ms[i]->v.fn.body->v.blk.tail, "this slice views the fn's own storage; return the "
                                           "array by value instead (01-types.md)");
      bodyfn = s->members[i].sym;
      rblock(ms[i]->v.fn.body, &fe, fnty->t);
    }
}
