/* bodymove.c -- the drop calls and the closure ground.
 *
 * A binding leaving scope calls what its type owns: dropcalls
 * spells a place's row of drops -- the pre-made calls mkdropcall
 * builds, the match mkdropmatch assembles when the row is a
 * variant's -- and scopedrops runs the ones a scope grew, in
 * reverse (03-move.md). A block is the unit: rblock walks one
 * statement at a time against the flow the environment carries,
 * and a closure literal is a block with captures -- rclosure
 * builds the fn it compiles to and the env its captures ride in
 * (05-traits.md, the closure face). The statements themselves --
 * the let, the assign, the flow control -- stay behind in
 * body.c; what moved here is what moves.
 */

#include <stdio.h>
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

/* -- blocks, closures, statements ------------------------------------------ */

/* one pre-made drop call: the row's own fn spelled std::Drop::drop,
 * its argument the place -- the sym the emitter's handle, the path
 * what a dump says, the bindings what a generic row instantiates
 * (03-move.md, 07-operators.md). NULL when the type owns no row. */
static Ast *
mkdropcall(Ast *place, Type *ty, Ast *at)
{
  Sym    *imp;
  Type  **tys;
  Member *m = implfind(sym_drop, ty, "drop", &imp, &tys);
  Ast    *f, *c;

  if (!m)
    return 0;
  f = opnode(Npath, at);
  f->v.path.root = 1; /* the destructor's own words, ::spelled: the
                       * compiler's naming reads from the root, so a
                       * Drop inside std itself leaves the same way
                       * one outside does (11-namespaces.md) */
  f->v.path.segs = vnew(Ast *, 3);
  opvpush(&f->v.path.segs, opseg("std", at));
  opvpush(&f->v.path.segs, opseg("Drop", at));
  opvpush(&f->v.path.segs, opseg("drop", at));
  c = opnode(Ncall, at);
  c->v.call.f = f;
  c->v.call.args = vnew(Ast *, 1);
  opvpush(&c->v.call.args, place);
  c->v.call.sym = m->sym;
  c->v.call.tys = tys && imp->ngparams ? tys : 0;
  c->ty = tyunit();
  return c;
}

/* an enum's own destructor is a match the checker spells whole: one
 * arm a variant, the payload's bindings destructed at the arm's end
 * -- the emitter's match its own, the tag it reads the value's
 * shape, not a "was this moved?" flag (03-move.md, Static
 * insertion). The bindings' names carry a dot: no lexer spells one,
 * so nothing a program declared can meet them. */
void dropcalls(Ast *place, Type *t, Ast ***out, Ast *at);

static Ast *
mkdropmatch(Ast *place, Type *t, Ast *at)
{
  Sym  *es = t->sym;
  Ast  *m = opnode(Nmatch, at);
  Ast **arms = vnew(Ast *, (usize) es->nvariants);
  usize i;

  for (i = 0; i < (usize) es->nvariants; i++) {
    Variant *v = &es->variants[i];
    Ast     *arm = opnode(Narm, at);
    Ast     *pat = opnode(Nppath, at);
    Ast     *body = opnode(Nblock, at);
    Ast     *head = opnode(Npath, at);
    char   **nms;
    Type   **tys;
    usize    nb = 0, j;

    head->v.path.segs = vnew(Ast *, 2);
    opvpush(&head->v.path.segs, opseg(es->name, at));
    opvpush(&head->v.path.segs, opseg(v->name, at));
    pat->v.ppath.path = head;
    nms = vnew(char *, (usize) (v->payload ? v->npayload : v->nfields) + 1);
    tys = vnew(Type *, (usize) (v->payload ? v->npayload : v->nfields) + 1);
    if (v->payload && v->named) { /* the struct-shaped variant: a
                                   * field a name, mirroring the
                                   * declaration (09-match.md) */
      pat->v.ppath.named = 1;
      pat->v.ppath.payload = vnew(Ast *, (usize) v->nfields);
      for (j = 0; j < (usize) v->nfields; j++) {
        Ast  *pf = opnode(Npfield, at);
        Ast  *bp = opnode(Npath, at);
        char *nm = arenaalloc(16);

        sprintf(nm, ".v%lu", (unsigned long) j);
        bp->v.path.segs = vnew(Ast *, 1);
        opvpush(&bp->v.path.segs, opseg(nm, at));
        pf->v.init.name = v->fields[j].name;
        pf->v.init.e = bp;
        opvpush(&pat->v.ppath.payload, pf);
        if (t->nargs == (usize) es->ngparams /* the variant's field
                                              * under the instance */
            && hasdrop(gsubst(v->fields[j].ty, es->gparams, t->args, t->nargs))) {
          bp->ty = gsubst(v->fields[j].ty, es->gparams, t->args, t->nargs);
          nms[nb] = nm;
          tys[nb] = bp->ty;
          nb++;
        }
      }
    } else if (v->payload) { /* positional: a binding a position */
      pat->v.ppath.payload = vnew(Ast *, (usize) v->npayload);
      for (j = 0; j < (usize) v->npayload; j++) {
        Ast  *bp = opnode(Npath, at);
        char *nm = arenaalloc(16);
        Type *pt = gsubst(v->payload[j], es->gparams, t->args, t->nargs);

        sprintf(nm, ".v%lu", (unsigned long) j);
        bp->v.path.segs = vnew(Ast *, 1);
        opvpush(&bp->v.path.segs, opseg(nm, at));
        opvpush(&pat->v.ppath.payload, bp);
        if (hasdrop(pt)) {
          bp->ty = pt;
          nms[nb] = nm;
          tys[nb] = pt;
          nb++;
        }
      }
    }
    for (j = 0; j < nb; j++) { /* the payload's own destructors, the
                                * arm's block end (03-move.md): a
                                * binding the arm pattern made, its
                                * place the name itself */
      Ast *bp = opnode(Npath, at);

      bp->v.path.segs = vnew(Ast *, 1);
      opvpush(&bp->v.path.segs, opseg(nms[j], at));
      bp->ty = tys[j];
      dropcalls(bp, tys[j], &body->v.blk.drops, at);
    }
    arm->v.n2.a = pat;
    arm->v.n2.b = body;
    opvpush(&arms, arm);
  }
  m->v.call.f = place;
  m->v.call.args = arms;
  m->ty = tyunit();
  return m;
}

/* the destructors a place owns, spelled whole (03-move.md): a row of
 * its own answers whole, the fields' -- the elements', the payload's
 * -- is the no-row case, structural. A union forgets: nothing stored
 * in one is destructed, ever. The calls go out pre-made, the
 * emitter sends them; the array's own order is the order they run. */
void
dropcalls(Ast *place, Type *t, Ast ***out, Ast *at)
{
  usize i;

  if (!t)
    return;
  if (!*out) /* opvpush will not begin a vector on its own */
    *out = vnew(Ast *, 4);
  {
    Ast *c = mkdropcall(place, t, at);

    if (c) { /* the type's own row: it answers for everything inside */
      opvpush(out, c);
      return;
    }
  }
  switch (t->k) {
  case Tystruct: {
    Sym *s = t->sym;

    for (i = s->nfields; i > 0; i--) { /* reverse: the later field
                                        * dies first, a binding's own
                                        * order (03-move.md) */
      Type *ft = s->fields[i - 1].ty;
      Ast  *fp;

      if (t->nargs == (usize) s->ngparams) /* the field under the
                                            * instance (04) */
        ft = gsubst(ft, s->gparams, t->args, t->nargs);
      if (!hasdrop(ft))
        continue;
      fp = opnode(Naccess, at);
      fp->v.fld.e = place;
      fp->v.fld.name = s->fields[i - 1].name;
      fp->ty = ft;
      dropcalls(fp, ft, out, at);
    }
    return;
  }
  case Tyarray: { /* every element, statically spelled: the length is
                   * the declaration's own, a compile-time thing */
    Type *et = t->t;

    while (et && et->k == Tymut) /* a mut element's permission layer */
      et = et->t;
    for (i = t->n; i > 0; i--) {
      Ast *ip;

      if (!hasdrop(et))
        return;
      ip = opnode(Nindex, at);
      ip->v.n2.a = place;
      ip->v.n2.b = opnode(Nint, at);
      ip->v.n2.b->v.i.num = i - 1;
      ip->ty = et;
      dropcalls(ip, et, out, at);
    }
    return;
  }
  case Tytuple:
    for (i = t->nargs; i > 0; i--) {
      Type *et = t->args[i - 1];
      Ast  *ip;

      if (!hasdrop(et))
        continue;
      ip = opnode(Ntupidx, at);
      ip->v.tup.e = place;
      ip->v.tup.idx = i - 1;
      ip->ty = et;
      dropcalls(ip, et, out, at);
    }
    return;
  case Tyenum: /* a match: the tag decides, each arm its own */
    opvpush(out, mkdropmatch(place, t, at));
    return;
  default: /* scalars, pointers, slices, unions: nothing to destruct */
    return;
  }
}

/* the destructors a scope's bindings owe, innermost first: a
 * binding the move killed owes nothing -- its destructor went with
 * the value (03-move.md) -- and a const parameter is a compile-time
 * thing, no slot to destruct. */
Ast **
scopedrops(Fenv *fe, usize from, Ast *at)
{
  Ast **out = 0;
  usize i;

  for (i = fe->n; i > from; i--) {
    Local *l = &fe->ls[i - 1];
    Ast   *p;

    if (l->dead || l->isconst || !hasdrop(l->ty))
      continue;
    p = opnode(Npath, at); /* the binding's own place: a plain name,
                            * its slot the emitter's own */
    p->v.path.segs = vnew(Ast *, 1);
    opvpush(&p->v.path.segs, opseg(l->name, at));
    p->ty = l->ty;
    dropcalls(p, l->ty, &out, at);
  }
  return out;
}

Type *
rblock(Ast *b, Fenv *fe, Type *want)
{
  usize nbase = fe->n;
  Ast **ss = b->v.blk.stmts;
  usize n = vlen(ss), i;
  Type *t;
  int   dive;

  if (b->v.blk.tail &&
      b->v.blk.tail->k == Ncall) { /* the tail a
                                    * mode-gated call spells, removed here when every row of its
                                    * chain is closed and every row answers (): the block's own
                                    * value the unit the call never made -- the value no one
                                    * reads, the one shape past the statement that still compiles
                                    * away (01-types.md, Mode-gated functions). A non-unit answer
                                    * stays: its value is a use, and the use the mode refuses */
    Sym *gs = gatedcall(b->v.blk.tail, fe);

    if (gs) {
      Sym *c;

      for (c = gs; c; c = c->next)
        if (fnsigof(c)->t->k != Tyunit)
          break;
      if (!c) {
        b->v.blk.tail = 0;
        b->ty = tyunit();
      }
    }
  }
  dive = (n && mustexit(ss[n - 1])) ||
         (b->v.blk.tail && mustexit(b->v.blk.tail)); /* the
                                                      * last statement leaves,
                                                      * or the tail itself is
                                                      * a call that never lands
                                                      * -- a panic's own shape
                                                      * (10-iteration.md): the
                                                      * block never lands, its
                                                      * value the shape the
                                                      * world asked for */

  for (i = 0; i < n; i++)
    rstmt(ss[i], fe);
  if (dive) { /* the tail is dead code: checked with nothing wanted,
               * the errors still errors, the value no value */
    if (b->v.blk.tail)
      rexpr(b->v.blk.tail, fe, 0);
    locpop(fe, nbase); /* no drops: the tail left on its own way out,
                        * and that way carries them (03-move.md) */
    return want ? want : tyunit();
  }
  t = b->v.blk.tail ? rexpr(b->v.blk.tail, fe, want) : tyunit();
  b->v.blk.drops = scopedrops(fe, nbase, b); /* the closing brace the
                                              * block reached on its
                                              * own: the bindings
                                              * die here, innermost
                                              * first (03-move.md) */
  locpop(fe, nbase);
  return t;
}

Type *
rclosure(Ast *c, Fenv *fe)
{
  Ast  **ps = c->v.clos.params;
  Ast  **cs = c->v.clos.caps;
  usize  np = vlen(ps), nc = vlen(cs), i, j;
  Type **ts = np ? tyargs(np) : 0;
  Type  *ret = c->v.clos.ret ? rty(c->v.clos.ret, &fe->env) : tyunit();
  Field *cf = nc ? arenaalloc(nc * sizeof *cf) : 0;
  Fenv   fb;

  for (i = 0; i < nc; i++) { /* the capture list: each name a local of
                              * the world above, spelled once here into
                              * a field of the literal's own env
                              * (01-types.md) */
    Ast   *a = cs[i];
    Local *l;
    Ast   *p;

    for (j = 0; j < i; j++)
      if (strcmp(cs[j]->v.cap.name, a->v.cap.name) == 0)
        berr(a, "the capture '%s' is named twice", a->v.cap.name);
    l = locfind(fe, a->v.cap.name);
    if (!l)
      berr(a, "'%s' names no local to capture", a->v.cap.name);
    if (l->dead)
      berr(a, "'%s' has been moved", a->v.cap.name);
    p = opnode(Npath, a); /* the name as the world above spelled it:
                           * the emitter reads this place building the
                           * env -- a byref capture stores the address,
                           * a value one the bytes (05-traits.md) */
    p->v.path.segs = vnew(Ast *, 1);
    opvpush(&p->v.path.segs, opseg(a->v.cap.name, a));
    p->ty = l->ty;
    a->v.cap.place = p;
    cf[i].name = a->v.cap.name;
    cf[i].attrs = 0;
    if (a->v.cap.byref) { /* the pointer the address-of operators
                           * spell: & a *T, &mut a *mut T, and the
                           * body reads the name as that pointer, its
                           * writes through the pointer's own *mut
                           * rules (01-types.md). The borrow it holds
                           * is the slice's own bargain: taken at the
                           * capture, alive from there, and a closure
                           * that outlives what it points at is the
                           * same undefined behaviour a slice's is
                           * (01-types.md) */
      if (a->v.cap.mut && !placewritable(p, fe))
        berr(a, "a &mut capture needs a writable place (01-types.md)");
      if (touchconflict(p, fe, a->v.cap.mut))
        berr(a, "the capture '%s' crosses a live borrow (01-types.md)", a->v.cap.name);
      cf[i].ty = typtr(a->v.cap.mut ? tymut(l->ty) : l->ty);
      cf[i].mut = 0; /* the env's slot holds the pointer, and never
                      * changes -- the name it is read by is the
                      * pointer's own shape (01-types.md) */
      continue;
    }
    if (!iscopy(l->ty)) /* by value: a Copy one is copied in, any
                         * other moved -- the name unusable after, the
                         * move spelled here (03-move.md) */
      l->dead = 1;
    cf[i].ty = l->ty;
    cf[i].mut = a->v.cap.mut; /* the env's own slot writable, as a mut
                               * anywhere is (01-types.md) */
  }
  for (i = 0; i < np; i++) {
    if (!ps[i]->v.param.t)
      berr(ps[i], "a closure parameter carries its type");
    ts[i] = rty(ps[i]->v.param.t, &fe->env);
  }
  fb = fefork(fe);
  fb.ls = 0; /* the body sees its captures and its parameters alone:
              * the env's own fields are the world it knows, the world
              * above them no name at a time (01-types.md) */
  fb.n = 0;
  fb.fnret = ret;
  for (i = 0; i < nc; i++) /* the captures first, the env's own slots:
                            * a byref name is the pointer it is read
                            * as, a value one its own type, its mut the
                            * capture's own word (01-types.md) */
    locpush(&fb, cs[i]->v.cap.name, cf[i].ty, cs[i]->v.cap.byref ? 0 : cs[i]->v.cap.mut);
  for (i = 0; i < np; i++)
    locpush(&fb, ps[i]->v.param.name, ts[i], ps[i]->v.param.mut);
  fb.fnbase = fb.n; /* a closure's return leaves its own frame only:
                     * the captures and the world above them stay
                     * (03-move.md) -- the env's fields are the
                     * caller's, destructed where the literal's own
                     * scope ends, the binding that holds it */
  {                 /* the same check a fn's body walks: the closure's own tail
                     * against its declared return (10-iteration.md) */
    Type *t = rblock(c->v.clos.body, &fb, ret);
    Ast  *tail = c->v.clos.body->v.blk.tail;

    if (!tysame(t, ret)) {
      Type *c2 = tail ? recoerce(tail, ret, &fb) : 0;

      if (!c2 || !tysame(c2, ret))
        berr(tail ? tail : c->v.clos.body, "the closure returns %s, this is %s", btys(ret),
             btys(t));
    }
  }
  c->v.clos.drops = scopedrops(&fb, nc, c); /* the parameters' own slots
                                             * and the body's bindings:
                                             * the capture slots at 0
                                             * stay -- their destructors
                                             * are the caller's, run
                                             * where the env itself
                                             * dies (03-move.md) */
  c->v.clos.sig = tyfn(ts, np, ret);        /* the fn the literal spells: what
                                             * a captureless one is, what a
                                             * capturing one's call rides
                                             * (01-types.md, 05-traits.md) */
  if (!nc)
    return c->v.clos.sig; /* no captures, no env: the value is the fn
                           * pointer it spells, as it always was
                           * (01-types.md) */
  {                       /* the env: one struct, a capture a field, the literal's value its
                           * address. Its own drops ride the binding that holds it -- the
                           * fields die where the closure does (03-move.md) */
    static usize nenv;
    Sym         *env = arenaalloc(sizeof *env);
    int          once = 0, mutslot = 0;
    Sym         *fam;

    memset(env, 0, sizeof *env);
    env->name = arenaalloc(16);
    sprintf(env->name, "$env.%lu", (unsigned long) ++nenv);
    env->kind = Stype;
    env->tykind = TYstruct;
    env->fields = cf;
    env->nfields = nc;
    env->decl = c; /* the literal the env belongs to: the call sugar
                    * reaches the fn it names through here
                    * (05-traits.md) */
    {              /* the family the body needs, the least demanding row that
                    * works (01-types.md): a capture the body moved out of the
                    * env spells FnOnce, the binding that holds the closure
                    * dying with the call that spent it; a mut by-value
                    * capture is the one write through self there is, FnMut;
                    * everything else -- reads, and writes a captured
                    * pointer's own *mut carries -- an Fn (01-types.md) */
      usize ci;

      for (ci = 0; ci < nc; ci++)
        if (fb.ls[ci].dead)
          once = 1;
        else if (!cs[ci]->v.cap.byref && cs[ci]->v.cap.mut)
          mutslot = 1;
      c->v.clos.once = once;
    }
    fam = once ? sym_fnonce : mutslot ? sym_fnmut : sym_fn;
    { /* the env's own impl of the family, a row the table holds: the
       * bound a fn spells over the closure reads it, the projection
       * F::Output opens on it. No file spelled it -- the literal did,
       * here, its own words the only ones an env this private can
       * hear (05-traits.md) */
      Sym    *im = arenaalloc(sizeof *im);
      Type  **ta = tyargs(fam->ngparams);
      Member *ms = arenaalloc(2 * sizeof *ms); /* Output, then the
                                                * family's own method:
                                                * what a handle's
                                                * table carries
                                                * (06-dispatch.md) */
      Type *envt = tysym(env, 0, 0);

      memset(im, 0, sizeof *im);
      im->kind = Simpl;
      im->ownns = nscuring(); /* the file the literal stands in: a
                               * method's mangle carries it (11) */
      im->decl = c;
      im->ifort = tysym(env, 0, 0); /* the for-type: the env itself */
      {                             /* the trait's own words: the arguments the pack spells, one
                                     * a parameter, gathered whole into the pack's own slot
                                     * (04-generics.md) -- a bound's spelling lands the same */
        ta[fam->ngparams - 1] = tytuple(ts, np);
      }
      im->ipath = tysym(fam, ta, fam->ngparams);
      memset(ms, 0, 2 * sizeof *ms);
      ms[0].kind = Mtype; /* Output: the answer the body returns, the
                           * projection's own supply (05-traits.md) */
      ms[0].name = "Output";
      ms[0].val = ret;
      ms[1].kind = Mfn; /* the family's own method: the literal's fn
                         * itself, the row a handle's vtable names
                         * (06-dispatch.md). No Sym -- the emitter
                         * reads the literal's own name off the decl;
                         * the trait's candidates skip it, the sugar
                         * the only door (05-traits.md) */
      ms[1].name = fam == sym_fn ? "call" : fam == sym_fnmut ? "call_mut" : "call_once";
      ms[1].decl = c;
      { /* the signature the trait's own check would spell: the
         * pack's rows one for one, Self the env (04-generics.md) --
         * the ABI the fat call passes already matches, the env
         * pointer the first word (05-traits.md) */
        Type **fa = tyargs(np + 1);
        usize  ai;

        fa[0] = fam == sym_fn ? typtr(envt) : fam == sym_fnmut ? typtr(tymut(envt)) : envt;
        for (ai = 0; ai < np; ai++)
          fa[ai + 1] = ts[ai];
        ms[1].ty = tyfn(fa, np + 1, ret);
      }
      im->nmembers = 2;
      im->members = ms;
      if (!chk_impls)
        chk_impls = vnew(Sym *, 16);
      vappend(&chk_impls, &im); /* the table the bounds walk reads:
                                 * a literal met this late answers
                                 * the asks that follow it (04) */
      chk_nimpls = vlen(chk_impls);
    }
    return tysym(env, 0, 0);
  }
}
