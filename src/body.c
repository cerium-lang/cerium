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
 * beyond the leaf layer -- a literal length is the evaluator's own
 * here (08-reflection.md), an fn call in it waits -- pack spreads,
 * ranges, and type values ($$t, reflection). Option and Result are
 * checked as what they are -- prelude enums -- with their variants as
 * constructors and their narrowing spelled by the == None / != Err
 * forms (01-types.md).
 *
 * The environment and the helpers it walks with -- bindings,
 * borrows, joins, the diagnostics -- live in flow.c; body.h is the
 * face between the two. This file is the walk itself.
 */

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

static Type *rexpr(Ast *e, Fenv *fe, Type *want);
static Type *rexpr1(Ast *e, Fenv *fe, Type *want);

/* the fn whose body pass 4 is walking: an @compileError it holds is
 * a report only when the evaluator never ran the body -- the run
 * itself decides the branch (08-reflection.md) */
static Sym *bodyfn;

/* -- places, read (03-move.md) ------------------------------------------- */

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

/* -- expressions --------------------------------------------------------- */

static void rstmt(Ast *st, Fenv *fe);
static int  placewritable(Ast *p, Fenv *fe);

/* inside a const for's unroll walk: a const for met here sits below
 * a shared body's top level, and one unroll slot cannot serve the
 * rounds it would appear in (10-iteration.md) */
static int ununroll;

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

/* what a declared projection was is the handle's own spelling: the
 * vtable erased the impl's choice, and the spelling gives it back
 * (06-dispatch.md). The handle's args are the Mtype slots, in the
 * trait's declaration order. */
static Type *
projsubst(Type *t, Type *h)
{
  Type **as;
  usize  i;

  if (!t)
    return t;
  switch (t->k) {
  case Typroj: { /* Self::Item under this handle's trait */
    usize mi = 0;

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

/* one argument against its parameter: the check the call always
 * ran, plus the binding when the parameter names one of the
 * member's own variables. A pass-through stands for itself -- the
 * re-check binds it for real, exactly as a generic fn's recursive
 * call inside a generic body (04-generics.md). */
static void
argfit(Ast *a, Type *pt, Type *at, Ast **mg, Type **mtys, usize nm, Fenv *fe, const char *who)
{
  if (!at || !pt)
    return;
  if (tysame(at, pt)) {
    if (mtys)
      gunify(pt, at, mg, mtys, nm);
    return;
  }
  {
    Type *c = recoerce(a, pt, fe);

    if (c)
      return;
  }
  if (!mtys || !gunify(pt, at, mg, mtys, nm))
    berr(a, "'%s' wants %s here, this is %s", who, btys(pt), btys(at));
}

/* the binding, once the arguments spoke: every slot landed, every
 * bound the parameters carry answered (04-generics.md) -- the same
 * checks a generic fn's call runs over its own list. */
static void
memberdone(Ast *e, Ast **mg, Type **mtys, usize nm, const char *who)
{
  usize g, bi;

  for (g = 0; g < nm; g++)
    if (!mtys[g])
      berr(e, "cannot infer '%s' for '%s' from the call", mg[g]->v.gp.name, who);
  for (g = 0; g < nm; g++) {
    Ast **bs = mg[g]->v.gp.bounds;

    for (bi = 0; bi < vlen(bs); bi++) {
      Ast **bsegs = bs[bi]->v.path.segs;
      Sym  *tr = vlen(bsegs) == 1 ? symfind(bsegs[0]->v.seg.name) : 0;

      if (!tr || tr->kind != Strait)
        continue; /* collectbounds said it, at declaration */
      if (!implsatisfies(tr, mtys[g]))
        berr(e, "'%s' does not implement '%s'; '%s' cannot take it", btys(mtys[g]), tr->name, who);
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
static int
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

/* one signature's trial: the arguments walked against it, the
 * binding it spells picked and written back when it takes them. The
 * answer is the call's type, or 0 when it does not. cvals holds the
 * const parameters' own test -- a runtime argument there fails the
 * signature and notes it, the black box defers with the pick (08).
 * packslot: this trial feeds the pack's own parameter slot one
 * argument on its own -- the binding must be the tuple its rows
 * came in (04-generics.md); nfreeze is the caller's argument count,
 * the freezes to unwind -- the rolled trial walks a folded view */
static Type *
tryonesig(Sym *s, Ast *a, Ast **args, usize n, usize nfreeze, Fenv *fe, Ast *seg, Frzsave *svs,
          Val **cvals, int *runtime, int packslot)
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
  int    ok = n == fnty->nargs;

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
  if (ok && packslot) { /* the pack's parameter took the last
                         * argument on its own: a tuple, its rows the
                         * binding -- anything else is not this
                         * spelling (04-generics.md) */
    Type *pb = tys ? tys[s->ngparams - 1] : 0;

    if (!pb || (pb->k != Tytuple && pb->k != Tyunit))
      ok = 0;
  }
  if (ok) {
    for (i = 0; i < s->ngparams; i++) /* a const generic's slot fills
                                       * when the unifier meets its
                                       * length -- a number or a black
                                       * box; an empty one was never
                                       * met at all */
      if (!tys[i])
        berr(a, "cannot infer '%s' for '%s' from the call", s->gparams[i]->v.gp.name, s->name);
    { /* every bound, once the binding is known: does the type the
       * call landed implement the trait (04-generics.md)? The
       * impl table answers -- a bound nobody can satisfy was
       * already diagnosed where the fn was declared */
      Ast **gps = s->decl->v.fn.gparams;
      usize gi, bi;

      for (gi = 0; gi < vlen(gps); gi++) {
        Ast **bs = gps[gi]->v.gp.bounds;
        int   pk = gps[gi]->v.gp.pack;
        usize ri, rn = 0;

        if (pk && tys[gi]) /* a pack's bound is every row's own
                            * (04-generics.md): the binding holds the
                            * tuple the rows came in */
          rn = tys[gi]->k == Tytuple ? tys[gi]->nargs : 0;
        for (bi = 0; bi < vlen(bs); bi++) {
          Ast **bsegs = bs[bi]->v.path.segs;
          Sym  *tr;

          if (vlen(bsegs) != 1)
            continue; /* collectbounds diagnosed the shape */
          tr = symfind(bsegs[0]->v.seg.name);
          if (!tr || tr->kind != Strait)
            continue; /* ditto */
          if (pk) {
            for (ri = 0; ri < rn; ri++)
              if (!implsatisfies(tr, tys[gi]->args[ri]))
                berr(a, "'%s' does not implement '%s'; '%s' cannot take it",
                     btys(tys[gi]->args[ri]), tr->name, s->name);
            continue; /* the empty pack: no row, no bound to fail */
          }
          if (!implsatisfies(tr, tys[gi]))
            berr(a, "'%s' does not implement '%s'; '%s' cannot take it", btys(tys[gi]), tr->name,
                 s->name);
        }
      }
    }
    /* the emitter's pick: which overload, which instantiation. The
     * tys live in the arena, so the writeback outlives the walk
     * (04-generics.md) */
    a->v.call.sym = s;
    a->v.call.tys = s->ngparams ? tys : 0;
    a->v.call.cvals = cvals;
    a->v.call.gcvals = s->ngparams ? gcvals : 0;
    thawargs(svs, nfreeze); /* the call is done; its borrows ended with it */
    return gsubstv(fnty->t, s->gparams, tys, gcvals, s->ngparams);
  }
  thawargs(svs, nfreeze); /* this signature did not take: its freezes unwound */
  return 0;
}

/* one signature's trial against a call: the plain signature, or --
 * when the fn's last parameter takes the pack -- the pack's own two
 * spellings. A tuple landing in the pack's slot on its own binds the
 * pack to its rows; any other shape folds the tail arguments into
 * one tuple argument, the empty tail the unit (04-generics.md) */
static Type *
trysig(Sym *s, Ast *a, Ast **args, usize n, Fenv *fe, Ast *seg, Frzsave *svs, Val **cvals,
       int *runtime)
{
  Ast **ps = s->decl->v.fn.params;
  usize np = vlen(ps);
  Type *r;

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
        ret = gsubstv(s->fnty->t, s->gparams, tys, 0, s->ngparams);
        return ret;
      }
    }
    if (n == s->fnty->nargs) { /* the direct spelling: the last
                                * argument lands in the slot alone,
                                * a tuple -- its rows the binding */
      r = tryonesig(s, a, args, n, n, fe, seg, svs, cvals, runtime, 1);
      if (r)
        return r;
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
      r = tryonesig(s, a, rargs, np, n, fe, seg, svs, cvals, runtime, 0);
      if (r) { /* the fold stands: matching and emit read the
                * folded view (the spread's own writeback, 01) */
        a->v.call.args = rargs;
        return r;
      }
      return 0;
    }
    thawargs(svs, n);
    return 0;
  }
  return tryonesig(s, a, args, n, n, fe, seg, svs, cvals, runtime, 0);
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
    if (vlen(q->decl->v.fn.params) == n && fnconstparams(q))
      constmode = 1;
  cvals = constmode && n ? arenaalloc(n * sizeof *cvals) : 0;
  if (cvals)
    memset(cvals, 0, n * sizeof *cvals);
  runtime = 0;
  if (constmode) {
    for (; s; s = s->next) {
      if (!fnconstparams(s)) {
        thawargs(svs, n);
        continue; /* the plain spellings wait below */
      }
      r = trysig(s, a, args, n, fe, seg, svs, cvals, &runtime);
      if (r)
        return r;
    }
    for (s = head; s; s = s->next) { /* the plain spellings: the
                                      * runtime arguments' own
                                      * (08-reflection.md) */
      if (fnconstparams(s)) {
        thawargs(svs, n);
        continue; /* tried above */
      }
      r = trysig(s, a, args, n, fe, seg, svs, 0, 0);
      if (r)
        return r;
    }
  } else
    for (; s; s = s->next) {
      r = trysig(s, a, args, n, fe, seg, svs, 0, 0);
      if (r)
        return r;
    }
  if (runtime)
    berr(a, "the argument is not compile-time known; '%s' takes it const (08-reflection.md)", nm);
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
rbuiltin(Ast *e, Fenv *fe, Type *want)
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
    if (bodyfn && bodyfn->evaled)
      return tyunit(); /* the evaluator ran this body and did not
                        * reach here: the branch stands, and emit
                        * gives it no runtime behavior (08) */
    berr(e, "%.*s", (int) args[0]->v.s.len, args[0]->v.s.s);
  }
  if (strcmp(nm, "count") == 0) { /* the pack's own length, a
                                   * compile-time constant against
                                   * the binding (04-generics.md) */
    Type *pt;

    if (nt != 0 || na != 1 || args[0]->k != Nspread)
      berr(e, "@count takes one pack (...Ts) (04-generics.md)");
    pt = rty(args[0]->v.un.e, &fe->env);
    if (pt && pt->k == Typaram) { /* the declaration's own walk: the
                                   * number is the instance's, and
                                   * what stands on it defers (08) */
      if (!pt->gp->v.gp.pack)
        berr(args[0], "'%s' is not a pack; @count wants one (04-generics.md)", pt->gp->v.gp.name);
      evalblackbox++;
      return tyint(IN_USIZE);
    }
    if (pt && (pt->k == Tytuple || pt->k == Tyunit)) { /* the binding:
                                                        * fold to the number, every
                                                        * pass below reads a literal */
      e->k = Nint;
      e->v.i.num = pt->k == Tytuple ? pt->nargs : 0;
      return tyint(IN_USIZE);
    }
    berr(args[0], "@count takes a pack (...Ts) (04-generics.md)");
  }
  if (strcmp(nm, "typeof") == 0) { /* the value's own type, as a
                                    * reference: $$ puts it back into
                                    * a slot (08-reflection.md) */
    if (nt != 0 || na != 1)
      berr(e, "@typeof takes one value");
    rexpr(args[0], fe, 0); /* the operand is checked for its own
                            * sake; the reference names its type, and
                            * the evaluator reads that when a splice
                            * asks */
    return tytype();
  }
  if (strcmp(nm, "typeinfo") == 0) { /* the description, both slots
                                      * one case: the type named in
                                      * the argument slot, or the
                                      * value's static type
                                      * (08-reflection.md) */
    Type *t;

    if (nt == 1 && na == 0) {
      if (targs[0]->k == Nun && targs[0]->v.un.op == Tdollar2) {
        /* the deferred splice: the operand's value arrives with a
         * frame a compile-time call builds, and this walk holds no
         * frame -- the node stands for the evaluator to answer
         * there (08-reflection.md). The fn that holds it runs at
         * compile time or not at all: a runtime call would pass
         * the type's word, and the word holds nothing to describe */
        if (bodyfn && bodyfn->evaled)
          return typeinfoty();
        berr(e, "the splice names a value this walk holds no frame for -- a parameter's, a "
                "local's: name a const, or call the fn at compile time (08-reflection.md)");
      }
      t = rty(targs[0], &fe->env);
      if (t->k == Typaram)   /* a generic's own parameter, met on the
                              * declaration's walk: the description is
                              * the instance's, and the node stands for
                              * the re-check under the binding to
                              * rewrite, on the clone the emitter hands
                              * that walk (04-generics.md) */
        return typeinfoty(); /* the shape alone: this walk checks the
                              * world around it, the instance's fills
                              * the answer in */
      targs[0]->ty = t;
    } else if (nt == 0 && na == 1)
      t = rexpr(args[0], fe, 0); /* the value's own derivation, not
                                  * the slot's want */
    else
      berr(e, "@typeinfo takes one type argument or one value (08-reflection.md)");
    { /* the answer is data the checker already holds: built here,
       * the node rewritten as the literal that spells it, and the
       * walk re-entered reads its own words (08-reflection.md) */
      Ast *x = valtoexpr(typeinfoval(t, e), e);

      memset(&e->v, 0, sizeof e->v);
      e->k = x->k;
      memcpy(&e->v, &x->v, sizeof e->v);
      return rexpr(e, fe, want);
    }
  }
  if (strcmp(nm, "offset") == 0) { /* a field's own place in the type's
                                    * whole: the layout query, folded
                                    * where it stands (02-layout.md) */
    Type *t;
    char *fnm;
    usize i;

    if (nt != 1 || na != 1)
      berr(e, "@offset takes one type argument and the field's name (02-layout.md)");
    if (targs[0]->k == Nun && targs[0]->v.un.op == Tdollar2) {
      /* the deferred splice, @typeinfo's own shape: the answer typed
       * and zeroed where no runtime read reaches it (08) */
      if (bodyfn && bodyfn->evaled)
        return tyint(IN_USIZE);
      berr(e, "the splice names a value this walk holds no frame for -- a parameter's, a "
              "local's: name a const, or call the fn at compile time (08-reflection.md)");
    }
    t = rty(targs[0], &fe->env);
    if (t->k == Typaram) /* the black box again: the fold is the
                          * instance's, and the node stands for the
                          * re-check to fold it there
                          * (04-generics.md) */
      return tyint(IN_USIZE);
    targs[0]->ty = t;
    fnm = bltname(args[0], fe); /* the name: a literal's bytes, a
                                 * const for's round, a const
                                 * parameter -- whatever the
                                 * evaluator resolves, and nothing
                                 * else */
    if (!fnm)                   /* the name named a const parameter this walk holds no
                                 * value for: the fold is the instance's own, and the
                                 * node stands for the re-check to fold it there --
                                 * the same deferral the black-box type took above
                                 * (08-reflection.md) */
      return tyint(IN_USIZE);
    if (t->k != Tystruct && t->k != Tyunion)
      berr(e, "%s has no fields to offset (02-layout.md)", btys(t));
    for (i = 0; i < t->sym->nfields; i++)
      if (strcmp(t->sym->fields[i].name, fnm) == 0)
        break;
    if (i == t->sym->nfields)
      berr(args[0], "'%s' has no field '%s' (02-layout.md)", t->sym->name, fnm);
    { /* the layout's own answer, a constant the walks read as one */
      Ast *x = valtoexpr(valint(fieldoffof(t, i), tyint(IN_USIZE)), e);

      memset(&e->v, 0, sizeof e->v);
      e->k = x->k;
      memcpy(&e->v, &x->v, sizeof e->v);
      return rexpr(e, fe, want);
    }
  }
  if (strcmp(nm, "field") == 0) { /* the field's address, the name
                                   * spelled in the value's bytes:
                                   * rewritten the hand's own borrow,
                                   * every rule the access has the
                                   * rewrite's (08-reflection.md) */
    Type  *vt;
    char  *fnm;
    Field *f;
    usize  i;
    Ast   *acc;

    if (nt != 0 || na != 2)
      berr(e, "@field takes the value and the field's name (08-reflection.md)");
    vt = rplace(args[0], fe); /* a place, read: the borrow this rewrite
                               * spells is of the place's own field,
                               * and borrowing reads, never moves --
                               * a global or a computed base falls to
                               * the value walk (03-move.md) */
    if (!vt)
      vt = rexpr(args[0], fe, 0);
    while (vt && vt->k == Tymut) /* the permission, not the shape */
      vt = vt->t;
    fnm = bltname(args[1], fe); /* the name: a literal's bytes, a const
                                 * for's round (10-iteration.md is what
                                 * makes one compile-time known), a
                                 * const parameter's value (08) */
    if (!fnm) {                 /* the name named a const parameter this walk holds
                                 * no value for: the borrow the rewrite spells is the
                                 * instance's own, and the re-check under the binding
                                 * writes it -- a typed slot or the tail keeps its
                                 * shape here, an untyped let meets the answer's at
                                 * its use (08-reflection.md) */
      if (!want)
        berr(e, "the field's name is a const parameter this walk holds no value for: "
                "the instance's own -- spell the slot's type, or call the fn at compile time "
                "(08-reflection.md)");
      return want;
    }
    if (!vt || vt->k == Typaram) /* a generic's own parameter: the
                                  * fields are the instance's, and
                                  * the borrow this rewrite spells is
                                  * theirs to spell -- outside a walk
                                  * over the type's fields there is
                                  * no name to read, and inside one
                                  * the re-check does the rewrite
                                  * (04-generics.md) */
      berr(args[0], "@field of a generic parameter is the instance's: walk its type's "
                    "fields with a const for, inside the instance (04-generics.md)");
    if (vt->k != Tystruct && vt->k != Tyunion)
      berr(args[0],
           "@field reads a struct's or a union's field: %s is neither "
           "(08-reflection.md)",
           btys(vt));
    for (i = 0; i < vt->sym->nfields; i++)
      if (strcmp(vt->sym->fields[i].name, fnm) == 0)
        break;
    if (i == vt->sym->nfields)
      berr(args[1], "'%s' has no field '%s' (08-reflection.md)", vt->sym->name, fnm);
    f = &vt->sym->fields[i];
    { /* &v.name -- &mut where the field is mut, so a write through
       * the address is governed by the field's own mut, exactly as
       * v.name's is. The borrow checks, the packed rule, the place
       * itself: the access's own, unchanged */
      acc = mknear(Naccess, e);
      acc->v.fld.e = args[0];
      acc->v.fld.name = f->name;
      memset(&e->v, 0, sizeof e->v);
      e->k = Nun;
      e->v.un.op = Tamp;
      e->v.un.mut = f->mut;
      e->v.un.e = acc;
      return rexpr(e, fe, want);
    }
  }
  /* count: packs' own (04-generics.md) */
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
    if (l->isconst && l->cv && l->cv->t->k == Tyint && l->cv->t->num != IN_F32 &&
        l->cv->t->num != IN_F64) { /* a const
                                    * parameter this walk holds an integer
                                    * for: the read folds where it stands --
                                    * a const generic parameter has no
                                    * runtime slot of its own, and the
                                    * number is the instance's. A slice's
                                    * bytes ride their parameter's own
                                    * slot, as ever (08-reflection.md) */
      memset(&e->v, 0, sizeof e->v);
      e->k = Nint;
      e->v.i.num = l->cv->i;
      return l->cur;
    }
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
      Sym *c;

      if (!want || want->k != Tyfn)
        berr(e, "'%s' needs an expected fn type here", name);
      for (c = s; c; c = c->next) {
        Type **tys = c->ngparams ? tyargs(c->ngparams) : 0;
        Val  **gcvals = c->ngparams ? arenaalloc(c->ngparams * sizeof *gcvals) : 0;
        usize  i;

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

    if (op == Tdyn) { /* &dyn b / &mut dyn b: the fat handle. Which
                       * trait is meant comes from the type the
                       * handle is being made for -- the want names
                       * it (06-dispatch.md) */
      Type *t = rplace(e->v.un.e, fe);
      Type *w = want;

      while (w && w->k == Tymut) /* a slot's permission, not the
                                  * handle's own shape */
        w = w->t;
      if (!t)
        berr(e->v.un.e, "cannot make a handle of a temporary");
      if (e->v.un.mut && !placewritable(e->v.un.e, fe))
        berr(e->v.un.e, "a &mut dyn needs a mut slot (01-types.md)");
      if (touchconflict(e->v.un.e, fe, e->v.un.mut))
        berr(e->v.un.e, "this place is already borrowed (01-types.md)");
      if (!w || w->k != Tydyn)
        berr(e, "the trait a handle is made for comes from its expected type (06-dispatch.md)");
      { /* the impl the vtable carries, chosen here -- the whole
         * point of dyn: the choice travels (06-dispatch.md) */
        Sym *im = implfor(w->sym, t, 0);

        if (!im)
          berr(e->v.un.e, "no '%s' for %s", w->sym->name, btys(t));
        objectsafety(w->sym, e, w->args, w->nargs);
      }
      freeze(e->v.un.e, fe, e->v.un.mut, (int) fe->n);
      return w; /* rplace wrote the concrete type on the operand;
                 * the emitter re-finds the impl from the pair */
    }
    if (op == Tamp) { /* &x / &mut x: the place is borrowed, not read */
      Type *t = rplace(e->v.un.e, fe);

      if (!t)
        berr(e->v.un.e, "cannot borrow a temporary");
      if (e->v.un.mut && !placewritable(e->v.un.e, fe))
        berr(e->v.un.e, "a &mut needs a mut slot (01-types.md)");
      /* a shared & may stack on a live shared borrow (01-types.md: a
       * *T is not exclusive); only a &mut touches what it may not */
      if (touchconflict(e->v.un.e, fe, e->v.un.mut))
        berr(e->v.un.e, "this place is already borrowed (01-types.md)");
      freeze(e->v.un.e, fe, e->v.un.mut, (int) fe->n);
      return e->v.un.mut ? typtr(tymut(t)) : typtr(t);
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
          t = rexpr(a->v.un.e, fe, 0);
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
    if (f->k == Npath) {
      Ast **segs = f->v.path.segs;
      usize nsegs = vlen(segs);

      if (nsegs == 1) {
        char  *nm = segs[0]->v.seg.name;
        Local *l = locfind(fe, nm);
        Sym   *s;

        if (l) { /* an indirect call: a fn-typed local */
          Type    *t = l->cur;
          Frzsave *svs = n ? arenaalloc(n * sizeof *svs) : 0;
          usize    k;

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
      if (nsegs == 2) { /* Enum::Variant(...), Type::member(...), or Trait::member(&p) */
        char *nm0 = segs[0]->v.seg.name;
        char *nm1 = segs[1]->v.seg.name;
        Sym  *s = symfind(nm0);

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
            /* peel Self out of the receiver: the declared self is a
             * pattern over Self (*Self, []mut Self, ...), the
             * argument is that shape over a concrete type. The
             * receiver walks once, want-less -- the impl's signature
             * is not known until Self is */
            Type *sig0 = dm->ty->nargs ? dm->ty->args[0] : 0;
            Type *rt = rexpr(args[0], fe, 0);
            Type *self;

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
                int   bounded = 0;
                usize bi;

                for (bi = 0; bi < vlen(bs); bi++) {
                  Ast **bsegs = bs[bi]->v.path.segs;
                  Sym  *tr = vlen(bsegs) == 1 ? symfind(bsegs[0]->v.seg.name) : 0;

                  if (tr == s) {
                    bounded = 1;
                    break;
                  }
                }
                if (!bounded)
                  berr(args[0], "'%s' is not a bound on '%s'", s->name, self->gp->v.gp.name);
                t = selfsubst(dm->ty, self);
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
                  {
                    Frzsave *svs = n ? arenaalloc(n * sizeof *svs) : 0;
                    usize    k;

                    if (svs) {
                      memset(svs, 0, n * sizeof *svs);
                      for (k = 0; k < n; k++)
                        argborrow(args[k], fe, &svs[k]);
                    }
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

                      argfit(args[i], t->args[i], at, mg, mtys, nmg, fe, nm1);
                    }
                    thawargs(svs, n); /* the call is done; its borrows ended with it */
                  }
                  memberdone(e, mg, mtys, nmg, nm1);
                  return nmg ? gsubst(t->t, mg, mtys, nmg) : t->t;
                }
              }
              im = implfind(s, self, nm1, &imp, &tys);
              if (!im)
                berr(args[0], "no '%s' for %s", s->name, btys(self));
              t = tys ? gsubst(im->ty, imp->gparams, tys, imp->ngparams) : im->ty;
              e->v.call.sym = im->sym; /* the impl's member: the body that runs */
              {                        /* the member's own family, bound from the arguments */
                Ast  **mg = membergps(im, &nmg);
                Type **mtys;

                if (nmg) {
                  mtys = tyargs(nmg);
                  for (zi = 0; zi < nmg; zi++)
                    mtys[zi] = 0;
                } else
                  mtys = 0;
                {
                  Frzsave *svs = n ? arenaalloc(n * sizeof *svs) : 0;
                  usize    k;

                  if (svs) {
                    memset(svs, 0, n * sizeof *svs);
                    for (k = 0; k < n; k++)
                      argborrow(args[k], fe, &svs[k]);
                  }
                  if (n != t->nargs)
                    berr(e, "'%s::%s' takes %lu arguments, %lu given", s->name, nm1,
                         (unsigned long) t->nargs, (unsigned long) n);
                  { /* the receiver already walked: check its type
                     * where it stands, the rest against the instance */
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

                    argfit(args[i], t->args[i], at, mg, mtys, nmg, fe, nm1);
                  }
                  thawargs(svs, n); /* the call is done; its borrows ended with it */
                }
                memberdone(e, mg, mtys, nmg, nm1);
                e->v.call.tys = insttys(imp, tys, mg, mtys, nmg);
                return nmg ? gsubst(t->t, mg, mtys, nmg) : t->t;
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

                  argfit(args[i], t->args[i], at, mg, mtys, nmg, fe, nm1);
                }
              }
              thawargs(svs, n); /* the call is done; its borrows ended with it */
            }
            memberdone(e, mg, mtys, nmg, nm1);
            e->v.call.tys = insttys(0, 0, mg, mtys, nmg);
            return nmg ? gsubst(t->t, mg, mtys, nmg) : t->t;
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

        m = inherentfindt(ty, f->v.fld.name, &imp, &tys);
        if (!m)
          m = traitfindt(ty, f->v.fld.name, &imp, &tys);
        if (!m && ty->k == Typaram) {
          /* a generic fn's parameter: no impl resolves here -- the
           * bound names the trait, the declaration's signature
           * carries the walk, and the instantiation's re-check
           * picks the impl (04-generics.md) */
          Ast **bs = ty->gp->v.gp.bounds;
          usize bi, mi;

          for (bi = 0; bi < vlen(bs); bi++) {
            Ast **bsegs = bs[bi]->v.path.segs;
            Sym  *tr = vlen(bsegs) == 1 ? symfind(bsegs[0]->v.seg.name) : 0;

            if (!tr || tr->kind != Strait)
              continue;
            for (mi = 0; mi < tr->nmembers; mi++)
              if (tr->members[mi].kind == Mfn && strcmp(tr->members[mi].name, f->v.fld.name) == 0) {
                m = &tr->members[mi];
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
        if (!m)
          berr(f, "'%s' has no method '%s'", btys(ty), f->v.fld.name);
        if (m->kind != Mfn)
          berr(f, "'%s' is not a method", f->v.fld.name);
        if (declared) {
          t = selfsubst(m->ty, ty);
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
            recvadapt(f->v.fld.e, t->nargs ? t->args[0] : 0, rt, ty, fe, &sv);
            if (n + 1 != t->nargs)
              berr(e, "'%s' takes %lu arguments, %lu given", f->v.fld.name,
                   (unsigned long) (t->nargs - 1), (unsigned long) n);
            {
              usize i;

              for (i = 0; i < n; i++) {
                Type *at = rexpr(args[i], fe, t->args[i + 1]);

                argfit(args[i], t->args[i + 1], at, mg, mtys, nmg, fe, f->v.fld.name);
              }
            }
            thawargs(svs, n); /* the explicit arguments' borrows, LIFO */
            frzrestore(&sv);  /* the receiver's borrow ends with the call */
          }
          memberdone(f, mg, mtys, nmg, f->v.fld.name);
          if (!declared)
            e->v.call.tys = insttys(imp, tys, mg, mtys, nmg);
          return nmg ? gsubst(t->t, mg, mtys, nmg) : t->t;
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
      /* the read moves it when it is not Copy (03-move.md); a computed
       * base is a value already */
      if (placeroot(e, fe, pbuf, sizeof pbuf) && !iscopy(t))
        berr(e, "cannot move out of a place: %s is not Copy (@take, 03)", btys(t));
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
    Type *bt = rexpr(e->v.tup.e, fe, 0);
    char  pbuf[256];

    if (!bt)
      return 0;
    if (bt->k != Tytuple)
      berr(e, "%s is not a tuple", btys(bt));
    if (e->v.tup.idx >= bt->nargs)
      berr(e, "tuple index %lu out of range", (unsigned long) e->v.tup.idx);
    { /* the row read out of a place moves it when it is not Copy
       * (03-move.md); a computed base is a value already */
      Type *ft = bt->args[e->v.tup.idx];

      if (placeroot(e, fe, pbuf, sizeof pbuf) && !iscopy(ft))
        berr(e, "cannot move out of a place: %s is not Copy (@take, 03)", btys(ft));
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
      tf = rexpr(e->v.ifx.els, &ff,
                 want ? want
                      : tt); /* None takes its
                              * ?T from the other side: the then spelled it out (01-types.md) */
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
    Sym   *s = vlen(segs) == 1 && !e->v.slit.path->v.path.root ? symfind(segs[0]->v.seg.name) : 0;
    Type **tys = 0;
    Type  *st;

    if (vlen(segs) == 2 && !e->v.slit.path->v.path.root) {
      Sym *es = symfind(segs[0]->v.seg.name);

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
          if (!at || !tys || !gunify(f->ty, at, s->gparams, tys, s->ngparams))
            berr(in->v.init.e, "field '%s' is %s, this is %s", f->name, btys(ft), btys(at));
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
    if (p->v.ppath.named) { /* by field name, mirroring the decl --
                             * an all-rest pattern binds none, the
                             * variant still named (09-match.md) */
      Ast **ps = p->v.ppath.payload;
      usize n = vlen(ps), i;

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
    if (p->v.ppath.payload) {
      Ast **ps = p->v.ppath.payload;
      usize n = vlen(ps), i;

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

/* a match whose scrutinee @typeinfo built, routed on the value the
 * evaluator holds: the taken arm's bindings as the lets that spell
 * them, its body the block's own value, the node rewritten as that
 * block -- and the walk re-enters its own words, the @typeinfo
 * rewrite's own trick (09-match.md, 08-reflection.md) */
static Type *
cmatchroute(Ast *e, Val sv, Fenv *fe, Type *want)
{
  Ast **arms = e->v.call.args;
  usize n = vlen(arms), i;

  for (i = 0; i < n; i++) {
    Ast  *arm = arms[i];
    Ast **lets;
    usize nb, k;

    if (!patfits(arm->v.n2.a, sv))
      continue; /* another variant's round: discarded before
                 * checking (09-match.md) */
    lets = cmatchlets(arm->v.n2.a, sv, e);
    nb = vlen(lets);
    e->k = Nblock; /* the taken arm's body is the match's own now,
                    * the bindings the lets before it -- the match's
                    * node becomes the block that holds them */
    e->v.blk.stmts = vnew(Ast *, nb ? nb : 1);
    for (k = 0; k < nb; k++)
      vappend(&e->v.blk.stmts, &lets[k]);
    e->v.blk.tail = arm->v.n2.b; /* the block's value: the arm's
                                  * own, block or expression */
    return rblock(e, fe, want);
  }
  berr(e, "the match misses the value it holds (09-match.md)");
  return 0; /* unreachable */
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

  if (e->v.call.f->k == Nbuiltin && strcmp(e->v.call.f->v.blt.name, "typeinfo") == 0 &&
      !(vlen(e->v.call.f->v.blt.targs) == 1 && e->v.call.f->v.blt.targs[0]->k == Nun &&
        e->v.call.f->v.blt.targs[0]->v.un.op == Tdollar2)) {
    /* the prime scrutinee (09-match.md): the description is
     * compile-time, and the match only routes -- the taken arm's
     * bindings spelled as the lets that hold them, its body the
     * match's own, the untaken arms discarded before checking. The
     * declaration's black box defers the routing to the re-check
     * under the binding (04-generics.md). A $$ splice stays out: its
     * answer lives in the frame a compile-time call builds, and the
     * evaluator's own match reads it there (08-reflection.md) */
    int bb = evalblackbox;
    Val sv = ceval(e->v.call.f, fe->env, 0);

    if (evalblackbox != bb) {            /* the parameter, the black box: the
                                          * arms' shape is the instance's own */
      evalblackbox = bb;                 /* the flag dies with the walk that raised it */
      return want ? want : typeinfoty(); /* the shape the world
                                          * around it wants, or the
                                          * shape alone when no slot
                                          * names one -- a discarded
                                          * statement's own, a
                                          * binding's placeholder:
                                          * the instance's walk types
                                          * it for real (04) */
    }
    return cmatchroute(e, sv, fe, want);
  }
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
    at = arm->v.n2.b->k == Nblock
             ? rblock(arm->v.n2.b, &fa, want ? want : rt)
             : rexpr(
                   arm->v.n2.b, &fa,
                   want ? want
                        : rt); /* None
                                * takes its ?T from the other side: an earlier arm spelled it out */
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

static int
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

    if (!globwritable(p, fe))
      return 0; /* the base's own global says no (01-types.md) */
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
  case Ntupidx: { /* (T, mut U): the row is its own slot (01-types.md) */
    Type *bt;

    if (!globwritable(p, fe))
      return 0; /* the base's own global says no (01-types.md) */
    if (!derefswritable(p->v.tup.e, fe))
      return 0;
    bt = rplace(p->v.tup.e, fe);
    if (!bt)
      bt = rexpr(p->v.tup.e, fe, 0);
    if (!bt || bt->k != Tytuple || p->v.tup.idx >= bt->nargs)
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
    if (fe->fnret->k == Tyslice && localview(st->v.n1.e, fe)) /* a
                                                               * slice the fn can see is its own
                                                               * dies with the frame (01) */
      berr(
          st->v.n1.e,
          "this slice views the fn's own storage; return the array by value instead (01-types.md)");
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
    case FIN: {                              /* for pat in e: what e yields, one binding a round */
      Type *et = rexpr(st->v.forx.b, fe, 0); /* the source is consumed
                                              * once, before the first round -- an
                                              * owned array moves in here, and is
                                              * not re-read every round (10) */

      while (et && et->k == Tymut) /* ditto */
        et = et->t;
      if (!et)
        break;
      if (et->k == Tyslice) /* a slice lends each element out: the
                             * binding is a pointer, never a move (10) */
        rpat(st->v.forx.a, typtr(et->t), &fb, 0);
      else if (et->k == Tyarray) /* an owned array yields each element
                                  * itself, and is consumed -- a place
                                  * of non-Copy elements is the mover's
                                  * to @take (10, 03) */
        rpat(st->v.forx.a, et->t, &fb, 0);
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
    rexpr(st->v.n1.e, fe, 0);
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
  rblock(it->v.fn.body, &fe, ret);
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
    ats[i] = gsubstv(s->fnty->args[i], s->gparams, tys, gcvals, ng);
  bodyfn = s;
  runbody(it, env, ats, gsubstv(s->fnty->t, s->gparams, tys, gcvals, ng), cvals, s->gparams,
          gcvals);
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
