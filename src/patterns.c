/* patterns.c -- pass 4's pattern half: what a pattern fits against
 * the type it must fit, binding what it names, and the match that
 * arms them. A pattern only fits -- exhaustiveness is the match's
 * own (09-match.md). The walk in body.c hands a match over, and the
 * let and for statements their patterns. */

#include "ast.h"
#include "body.h"
#include "check.h" /* nshead: a path's head names the namespace (11-namespaces.md) */
#include "eval.h"
#include "sym.h"
#include "type.h"
#include "vec.h"

/* -- patterns (09-match.md) ---------------------------------------------- */

/* check a pattern against the type it must fit, binding what it
 * names. Exhaustiveness is the caller's -- a pattern only fits. */
void
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
    Ns             *ns;
    usize           nsegs, k;

    nsegs = vlen(segs);
    k = nshead(segs, nsegs, &ns, p->v.ppath.path->v.path.root);
    if (k == nsegs) /* the whole path a namespace walk: a
                     * namespace names no pattern (11-namespaces.md) */
      berr(p, "a namespace names no pattern; name what is in it (11-namespaces.md)");
    segs += k; /* the namespaces walked fall away (11-namespaces.md) */
    nsegs -= k;

    if (nsegs == 1 && !p->v.ppath.payload && !p->v.ppath.named &&
        (!t || t->k != Tyenum || !varfind(t->sym, segs[0]->v.seg.name))) {
      /* a bare name that names no variant of the scrutinee's enum:
       * the binding form -- the parser sends every ident-headed
       * pattern here, variant or not */
      p->ty = t;
      locpush(fe, segs[0]->v.seg.name, t, mut);
      return;
    }
    if (nsegs == 2) { /* Enum::Variant */
      en = segs[0]->v.seg.name;
      vn = segs[1]->v.seg.name;
      s = k ? nsitem(ns, en) : symfind(en); /* the enum lands where
                                             * the walk did, when one
                                             * walked (11) */
      if (s && k && !s->pub)                /* a pattern reads the enum the same
                                             * door a value does: qualified across
                                             * namespaces, the private stays home
                                             * (11-namespaces.md) */
        berr(p, "'%s' is private to %s (11-namespaces.md)", s->name, nsname(ns));
      if (!s || s->kind != Stype || s->tykind != TYenum)
        berr(p, "'%s' is not an enum", en);
      if (t && (t->k != Tyenum || t->sym != s))
        berr(p, "this pattern fits %s, not %s's values", btys(t), s->name);
      { /* the resolved form is the short name: the walk rewrote the
         * pattern so every pass after -- the evaluator, the emitter
         * -- reads it the way a short-form one reads, the
         * scrutinee's own enum picking the variant out, no
         * namespace's help asked twice (11-namespaces.md) */
        Ast  *last = segs[1];
        Ast **one = vnew(Ast *, 1);

        vappend(&one, &last);
        p->v.ppath.path->v.path.segs = one;
        p->v.ppath.path->v.path.root = 0;
      }
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
    Ast  *sp = n && ps[n - 1]->k == Nspread ? ps[n - 1] : 0;
    usize nh = sp ? n - 1 : n;

    if (sp && t && t->k == Tyunit && !nh) { /* no row at all: the rest
                                             * binds (), the empty
                                             * tuple's own spelling
                                             * (01-types.md) */
      sp->ty = tyunit();
      if (sp->v.un.e)
        rpat(sp->v.un.e, sp->ty, fe, mut);
      return;
    }
    if (sp) { /* the rest pattern: the rows before it one a one, the
               * rows after gathered into the tuple the binding holds,
               * () the spelling when no row is left. The parser saw
               * the rest last (09-match.md) */
      Type *rt;

      if (!t || t->k != Tytuple || t->nargs < nh)
        berr(p, "this pattern fits at least %lu things, the type is %s", (unsigned long) nh,
             btys(t));
      rt = t->nargs > nh ? tytuple(t->args + nh, t->nargs - nh) : tyunit();
      sp->ty = rt; /* the tail's own shape, one source: the emit and the
                    * evaluator read it here */
      for (i = 0; i < nh; i++)
        rpat(ps[i], t->args[i], fe, mut);
      if (sp->v.un.e) /* a binding, or the tail's own pattern -- the
                       * whole walk again, the rows' own shape under
                       * it */
        rpat(sp->v.un.e, rt, fe, mut);
      return;
    }
    if (t && t->k == Tyunit && !n) /* the unit's own pattern: () the
                                    * empty tuple's one spelling
                                    * (01-types.md) */
      return;
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
  case Nspread: /* the rest pattern: the binding it names, when it
                 * names one -- the bare ... binds nothing */
    return p->v.un.e ? 1 : 0;
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

/* does this scrutinee place read through a pointer deref? The whole
 * it matches sits in the borrow's own storage, not a binding's slot
 * -- an arm's binding hands the value out of a place its holder does
 * not own, and moving a non-Copy one out is the deref's own refusal
 * (03-move.md, 09-match.md) */
static int
scrderef(Ast *e)
{
  while (e->k == Naccess || e->k == Nindex || e->k == Ntupidx || e->k == Nrangeindex) {
    if (e->k == Naccess)
      e = e->v.fld.e;
    else if (e->k == Nindex)
      e = e->v.n2.a;
    else if (e->k == Ntupidx)
      e = e->v.tup.e;
    else
      e = e->v.ridx.e;
  }
  return e->k == Nun && e->v.un.op == Tstar;
}

Type *
rmatch(Ast *e, Fenv *fe, Type *want)
{
  Ast **arms = e->v.call.args;
  usize n = vlen(arms), i;
  Type *st = rplace(e->v.call.f, fe); /* the scrutinee, read as a place */
  int   binds = 0;
  int   borrowed; /* the whole behind a deref: its arms bind
                   * in place, and nothing they bind may
                   * leave the borrow (03-move.md) */
  Type           *rt = 0;
  struct Variant *vs[32];
  usize           nv = 0;
  int             whatever = 0;
  int             joined = 0;
  Fenv            acc;

  borrowed = scrderef(e->v.call.f);

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
      markmoved(e->v.call.f, fe, root); /* the flag store rides the
                                         * spent place's read
                                         * (03-move.md, Guarded drops) */
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
    int   dive = mustexit(arm->v.n2.b); /* the arm never lands: its
                                         * value is nothing, it agrees
                                         * with any other arm
                                         * (10-iteration.md) */

    rpat(arm->v.n2.a, st, &fa, 0);
    if (borrowed) { /* the bindings this arm pushed sit where the
                     * borrow holds them: a borrow of one, a Copy
                     * read of one, both fine -- a move of a non-Copy
                     * one is the deref's own refusal (03-move.md) */
      usize k;

      for (k = nbase; k < fa.n; k++)
        fa.ls[k].inpl = 1;
    }
    at = arm->v.n2.b->k == Nblock
             ? rblock(arm->v.n2.b, &fa, dive ? 0 : (want ? want : rt))
             : rexpr(arm->v.n2.b, &fa,
                     dive ? 0 : (want ? want : rt)); /* None
                                                      * takes its ?T from the other side: an earlier
                                                      * arm spelled it out */
    if (!dive) /* a diverging arm seeds nothing and disagrees with
                * nothing: there is no value to compare */
    {
      if (!rt)
        rt = at;
      else if (at && !tysame(rt, at))
        berr(arm->v.n2.b, "the arms disagree: %s and %s", btys(rt), btys(at));
    }
    locpop(&fa, nbase); /* the arm's bindings -- and thaws -- end here */
    if (dive)
      continue; /* never reaches the join */
    if (!joined) {
      acc = fa; /* the first reachable arm seeds the join */
      joined = 1;
    } else
      fejoin(&acc, &acc, &fa);
  }
  if (joined)
    *fe = acc;                               /* every arm checked against the same pre-state */
  return rt ? rt : (want ? want : tyunit()); /* every arm leaving:
                                              * what follows is unreachable
                                              * anyway -- and all of them
                                              * leaving leaves no arm's
                                              * shape to name, the world's
                                              * own want standing in
                                              * (10-iteration.md) */
}

/* -- blocks, closures, statements ----------------------------------------- */
