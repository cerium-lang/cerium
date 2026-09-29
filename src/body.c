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
#include "lex.h"
#include "sym.h"
#include "type.h"

static Type *rexpr(Ast *e, Fenv *fe, Type *want);
static Type *rexpr1(Ast *e, Fenv *fe, Type *want);

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

/* the genericity gate for a member call: a member fn with
 * parameters of its own names a family -- picking one is the
 * specialization machinery's work (04-generics.md). An impl's own
 * parameters are no longer a gate: the call binds them from the
 * receiver's type, exactly as a literal binds a struct's
 * (04-generics.md). */
static void
gatemember(Ast *at, Sym *imp, Member *m, const char *name)
{
  (void) imp;
  if (m->kind == Mfn && m->decl && vlen(m->decl->v.fn.gparams))
    berr(at, "a generic '%s' arrives with specialization (04-generics.md)", name);
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

/* a call to a named fn, overload chain and all. tys holds the
 * generic bindings while the arguments are walked. */
static Type *
callfn(Sym *s, Ast *a, Ast **args, usize n, Fenv *fe)
{
  const char *nm = s->name;
  Type      **ats;
  Ast        *seg; /* the callee's one segment, when the call
                    * spells its generic arguments out (04) */
  Frzsave *svs = n ? arenaalloc(n * sizeof *svs) : 0;
  usize    k;

  if (svs) { /* the borrow arguments' freezes go back with the call,
              * on every path out but the errors (01-types.md) */
    memset(svs, 0, n * sizeof *svs);
    for (k = 0; k < n; k++)
      argborrow(args[k], fe, &svs[k]);
  }

  ats = n ? arenaalloc(n * sizeof *ats) : 0;
  seg = 0;
  if (a->v.call.f->k == Npath && vlen(a->v.call.f->v.path.segs) == 1)
    seg = a->v.call.f->v.path.segs[0];
  for (; s; s = s->next) {
    Type  *fnty = s->fnty;
    Type **tys = s->ngparams ? tyargs(s->ngparams) : 0;
    Type **sigs; /* what the arguments are checked against: the
                  * signature's own, or its substituted form under
                  * a spelled-out binding */
    usize i;
    int   ok = n == fnty->nargs;

    if (!ok) {
      thawargs(svs, n);
      continue;
    }
    sigs = fnty->args;
    if (seg && seg->v.seg.args) { /* f<i32>(...): the binding is the
                                   * call's own words, not inference */
      if (vlen(seg->v.seg.args) != s->ngparams) {
        thawargs(svs, n);
        continue; /* an overload this spelling does not fit */
      }
      for (i = 0; i < s->ngparams; i++)
        tys[i] = rty(seg->v.seg.args[i], &fe->env);
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
          gunify(sigs[i], ats[i], s->gparams, tys, s->ngparams);
        continue;
      }
      {
        Type *c = recoerce(args[i], sigs[i], fe);

        if (c) {
          ats[i] = c;
          continue;
        }
      }
      if (!gunify(sigs[i], ats[i], s->gparams, tys, s->ngparams))
        ok = 0;
    }
    if (ok) {
      for (i = 0; i < s->ngparams; i++)
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

          for (bi = 0; bi < vlen(bs); bi++) {
            Ast **bsegs = bs[bi]->v.path.segs;
            Sym  *tr;

            if (vlen(bsegs) != 1)
              continue; /* collectbounds diagnosed the shape */
            tr = symfind(bsegs[0]->v.seg.name);
            if (!tr || tr->kind != Strait)
              continue; /* ditto */
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
      thawargs(svs, n); /* the call is done; its borrows ended with it */
      return gsubst(fnty->t, s->gparams, tys, s->ngparams);
    }
    thawargs(svs, n); /* this overload did not take: its freezes unwound */
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
        usize  i;

        if (!tys) { /* an ungeneric member: it fits or it does not */
          if (c->fnty->nargs == want->nargs && tysame(c->fnty, want)) {
            e->v.path.sym = c;
            return c->fnty;
          }
          continue;
        }
        if (c->fnty->nargs != want->nargs)
          continue;
        if (!gunify(c->fnty, want, c->gparams, tys, c->ngparams))
          continue;
        for (i = 0; i < c->ngparams; i++)
          if (!tys[i])
            berr(e, "cannot infer '%s' for '%s' from the expected type", c->gparams[i]->v.gp.name,
                 c->name);
        /* the emitter's pick, as a call's writeback (04) */
        e->v.path.sym = c;
        e->v.path.tys = tys;
        return gsubst(c->fnty, c->gparams, tys, c->ngparams);
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
        gatemember(e, imp, m, nm1);
        if (m->kind == Mfn) {
          e->v.path.sym = m->sym; /* the fn it names: its address */
          e->v.path.tys = 0;      /* a gated method is never generic */
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
              usize   i;

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

                    if (at && t->args[i] && !tysame(at, t->args[i])) {
                      Type *c = recoerce(args[i], t->args[i], fe);

                      if (!c || !tysame(c, t->args[i]))
                        berr(args[i], "'%s::%s' wants %s here, this is %s", s->name, nm1,
                             btys(t->args[i]), btys(at));
                    }
                  }
                  thawargs(svs, n); /* the call is done; its borrows ended with it */
                }
                return t->t;
              }
              im = implfind(s, self, nm1, &imp, &tys);
              if (!im)
                berr(args[0], "no '%s' for %s", s->name, btys(self));
              gatemember(e, imp, im, nm1);
              t = tys ? gsubst(im->ty, imp->gparams, tys, imp->ngparams) : im->ty;
              e->v.call.sym = im->sym; /* the impl's member: the body that runs */
              e->v.call.tys = tys;
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

                  if (at && t->args[i] && !tysame(at, t->args[i])) {
                    Type *c = recoerce(args[i], t->args[i], fe);

                    if (!c || !tysame(c, t->args[i]))
                      berr(args[i], "'%s::%s' wants %s here, this is %s", s->name, nm1,
                           btys(t->args[i]), btys(at));
                  }
                }
                thawargs(svs, n); /* the call is done; its borrows ended with it */
              }
              return t->t;
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
          gatemember(e, imp, m, nm1);
          e->v.call.sym = m->sym; /* the method's own fn: the emitter's pick */
          e->v.call.tys = 0;      /* a gated method is never generic */
          {
            Type    *t = m->ty;
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

                if (at && t->args[i] && !tysame(at, t->args[i])) {
                  Type *c = recoerce(args[i], t->args[i], fe);

                  if (!c || !tysame(c, t->args[i]))
                    berr(args[i], "'%s::%s' wants %s here, this is %s", s->name, nm1,
                         btys(t->args[i]), btys(at));
                }
              }
            }
            thawargs(svs, n); /* the call is done; its borrows ended with it */
            return t->t;
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
          gatemember(f, imp, m, f->v.fld.name);
          t = m->ty;
          if (tys) /* a pattern impl's method: Self and the pattern's
                    * variables, under the receiver's binding */
            t = gsubst(t, imp->gparams, tys, imp->ngparams);
          e->v.call.sym = m->sym; /* the method's own fn: the emitter's pick */
          e->v.call.tys = tys;
        }
      }
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

            if (at && t->args[i + 1] && !tysame(at, t->args[i + 1])) {
              Type *c = recoerce(args[i], t->args[i + 1], fe);

              if (!c || !tysame(c, t->args[i + 1]))
                berr(args[i], "'%s' wants %s here, this is %s", f->v.fld.name, btys(t->args[i + 1]),
                     btys(at));
            }
          }
        }
        thawargs(svs, n); /* the explicit arguments' borrows, LIFO */
        frzrestore(&sv);  /* the receiver's borrow ends with the call */
      }
      return t->t;
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
    if (bt->k != Tyarray && bt->k != Tyslice) {
      berr(e, "%s cannot be indexed", btys(bt));
    }
    if (!it || !isintty(it))
      berr(e->v.n2.b, "an index is an integer, this is %s", btys(it));
    if (e->v.n2.b->k == Nint && bt->k == Tyarray && e->v.n2.b->v.i.num >= bt->n)
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
    if (e->v.arrlit.len && e->v.arrlit.len->k == Nint) {
      if (e->v.arrlit.len->v.i.num < n) /* more initializers than the
                                         * length is the error; fewer
                                         * is the zero fill
                                         * (01-types.md) */
        berr(e, "[%lu] holds %lu elements, %lu given", (unsigned long) e->v.arrlit.len->v.i.num,
             (unsigned long) e->v.arrlit.len->v.i.num, (unsigned long) n);
      return tyarray(e->v.arrlit.len->v.i.num, et);
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
      for (i = 0; i < s->ngparams; i++)
        if (!tys[i]) {
          Ast *d = s->gparams[i]->v.gp.dflt;

          if (!d)
            berr(e, "cannot infer '%s' for '%s' from the literal", s->gparams[i]->v.gp.name,
                 s->name);
          tys[i] = rty(d, &fe->env); /* the declaration's own
                                      * default (04-generics.md) */
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
  case Ntupidx: { /* (T, mut U): the row is its own slot (01-types.md) */
    Type *bt;

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

/* one fn's body against one binding of its world: the environment,
 * the parameter types, the return. The declaration check calls it
 * with T as a black-box Typaram; an instantiation calls it with T
 * bound (below), and the walk overwrites the body's writeback in
 * place -- the emitter reads it right after, before any other
 * instantiation re-checks the same shared tree. */
static void
runbody(Ast *it, Env env, Type **argtys, Type *ret)
{
  Fenv  fe;
  Ast **ps = it->v.fn.params;
  usize n = vlen(ps), i;

  memset(&fe, 0, sizeof fe);
  fe.env = env;
  fe.fnret = ret;
  for (i = 0; i < n; i++)
    locpush(&fe, ps[i]->v.param.name, argtys[i], ps[i]->v.param.mut);
  if (ret->k == Tyslice &&
      localview(it->v.fn.body->v.blk.tail, &fe)) /* the
                                                  * tail returns, and what it views
                                                  * dies with the frame (01) */
    berr(it->v.fn.body->v.blk.tail,
         "this slice views the fn's own storage; return the array by value instead (01-types.md)");
  rblock(it->v.fn.body, &fe, ret);
}

/* one fn's body: the parameters bind, the return is the want, and
 * the walk is statement-first (the last expression is the value) */
void
checkbodyfn(Sym *s, Ast *it)
{
  runbody(it, envgparams(0, it->v.fn.gparams, vlen(it->v.fn.gparams)), s->fnty->args, s->fnty->t);
}

/* one generic fn, one concrete binding: the parameters carry the
 * substituted types and the walk re-runs. What the declaration check
 * proved under a black-box T holds under any concrete one -- the
 * black box is the stricter world -- so the re-check's only failures
 * are the compiler's own bugs (04-generics.md) */
void
recheckfn(Sym *s, Ast *it, Type **tys)
{
  Env    env;
  usize  ng, i;
  Type **ats;

  ng = s->ngparams;
  ats = vlen(it->v.fn.params) ? tyargs(vlen(it->v.fn.params)) : 0;
  env = envnone();
  env.n = ng;
  env.b = ng ? arenaalloc(ng * sizeof *env.b) : 0;
  for (i = 0; i < ng; i++) { /* T is this binding, not a parameter */
    env.b[i].name = s->gparams[i]->v.gp.name;
    env.b[i].t = tys[i];
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
    ats[i] = gsubst(s->fnty->args[i], s->gparams, tys, ng);
  runbody(it, env, ats, gsubst(s->fnty->t, s->gparams, tys, ng));
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
      if (fnty->t->k == Tyslice && localview(ms[i]->v.fn.body->v.blk.tail, &fe))
        berr(ms[i]->v.fn.body->v.blk.tail, "this slice views the fn's own storage; return the "
                                           "array by value instead (01-types.md)");
      rblock(ms[i]->v.fn.body, &fe, fnty->t);
    }
}
