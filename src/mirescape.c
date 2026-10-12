/* mirescape.c -- client C (#185): the frame-escape check, then the
 * callee's signature's promise.
 *
 * specs/01-types.md names two errors the compiler never said a word
 * about -- returning the address of a local, returning a slice that
 * outlives what it borrows -- and README hedged with "not proven"
 * while every one of them compiled. The first half (landed) is the
 * boundary itself: a value that points into the fn's own frame may
 * not cross it, not by the return and not by a store through a
 * pointer. This file keeps it and adds the half the first named as
 * unseen: the call. Whether a callee keeps what it was handed is
 * its signature's promise, and the promise is readable -- the callee
 * is a fn this same mid layer holds -- so each fn carries a summary,
 * a bit a parameter: does it flow to the return, does it reach a
 * store through a pointer. A caller reads the summary at the call:
 * a frame pointer handed to a parameter that leaks is the same
 * escape the return always was, and a pointer-carrying answer built
 * from an argument follows the argument's own provenance instead of
 * the coarse "any argument, any answer" the first half made do with.
 *
 * The summary is the same walk, seeded differently: one bit a slot
 * carries became a mask -- bit 0 the frame, bit i+1 the parameter
 * i -- and a run with the parameters seeded asks, at the sinks,
 * which of them the fn's own course sends across. The summaries are
 * computed over every fn until they stop growing (a callee's summary
 * feeds its callers', recursion included), and the check then runs
 * per fn as before, the calls reading the settled summaries. A
 * callee the layer does not hold -- a handle's vtable row, a body
 * the compilation never spelled -- keeps the coarse rule, and leaks
 * nothing: being wrong there costs a refused program today, and the
 * first half's words were looser.
 *
 * Still unseen, said plainly: a byval capture of a frame pointer
 * copied into a closure's env and handed on -- though a closure has
 * no spelling that leaves its fn today, so the hole waits for the
 * spelling to exist (05-traits.md). Nothing here changes what a
 * program answers: what it changes is which programs are programs
 * (01-types.md, and specs/README's Not-proven narrows with it). */

#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "check.h" /* cerrat: the report says where, the tree's own
                    * place */
#include "die.h"
#include "lex.h" /* lexsetpath: the deferred check reports in the
                  * file that wrote the fn */
#include "mir.h"
#include "type.h"
#include "vec.h"

/* the state one slot of the frame carries: a mask of the values it
 * may hold -- bit 0 a pointer into this frame's own storage, bit
 * i+1 a value derived from the parameter i. The check asks only
 * bit 0; the summary asks the others, and the two runs are the same
 * walk with different seeds */
typedef unsigned Esc;

#define EFRAME 1u /* this frame's own storage, the bit the check refuses */

/* the run's own mode: the summary records where the seeds reach, the
 * check refuses where the frame bit lands. One walk, two questions */
static int summode;
static Esc sret, sleak; /* the summary run's answers: the bits that
                         * reached the return, the bits that reached
                         * a store through a pointer */

/* -- the ground: places and their roots -------------------------------- */

static Mirlocal *
locidx(Mirfn *f, char *name, usize *ix)
{
  usize i;

  for (i = 0; i < vlen(f->locals); i++)
    if (strcmp(f->locals[i]->name, name) == 0) {
      if (ix)
        *ix = i;
      return f->locals[i];
    }
  return 0;
}

/* the root of a place chain: the name the chain stands on, whether a
 * deref stands between -- *p.field's root is p and the storage is
 * the caller's -- and whether the chain steps through the root's own
 * value: a pointer-typed root's p.field is the pointee's field, the
 * caller's storage and not the frame the pointer sits in
 * (01-types.md) */
static int
placerootx(Ast *e, char **name, int *steps)
{
  int deref = 0;

  *name = 0;
  *steps = 0;
  while (e) {
    if (e->k == Naccess) {
      (*steps)++;
      e = e->v.fld.e;
    } else if (e->k == Nindex) {
      (*steps)++;
      e = e->v.n2.a;
    } else if (e->k == Nrangeindex) {
      (*steps)++;
      e = e->v.ridx.e;
    } else if (e->k == Ntupidx) {
      (*steps)++;
      e = e->v.tup.e;
    } else if (e->k == Nun && e->v.un.op == Tstar) {
      deref = 1;
      e = e->v.un.e;
    } else
      break;
  }
  if (e && e->k == Npath && vlen(e->v.path.segs) == 1 && !e->v.path.root &&
      !e->v.path.segs[0]->v.seg.args)
    *name = e->v.path.segs[0]->v.seg.name;
  return deref;
}

/* a local's chain steps through its own value when the root is a
 * pointer and the walk went inside: p.f, p[i] -- the storage the
 * place names is the pointee's, not this frame's */
static int
thruptr(Mirlocal *l, int steps)
{
  if (!l || !steps || !l->ty)
    return 0;
  return l->ty->k == Typtr || l->ty->k == Tyslice || l->ty->k == Tydyn;
}

/* does this type carry a pointer: the types whose values a frame
 * pointer could hide in */
static int
carryptr(Type *t)
{
  if (!t)
    return 1; /* unknown: the conservative answer */
  while (t->k == Tymut)
    t = t->t;
  return t->k == Typtr || t->k == Tyslice || t->k == Tydyn || t->k == Tystruct || t->k == Tyenum ||
         t->k == Tyunion || t->k == Tytuple || t->k == Tyfn || t->k == Tytrait;
}

/* -- the summaries: the callee's signature's promise -------------------- */

/* a callee's promise, read off the fns this layer holds: which
 * parameters flow to the return (bit i), which reach a store through
 * a pointer. A Sym names every instance of a fn -- the instances are
 * the emitter's own numbering, and the call's tree does not carry it
 * -- so the promise is the union over them: sound, and the instances
 * of one fn rarely disagree */
static void
calsum(Sym *s, unsigned *ret, unsigned *leak, int *known, int *recvptr)
{
  usize i, nf = mirnfns();

  *ret = *leak = 0;
  *known = 0;
  *recvptr = 0;
  if (!s)
    return;
  for (i = 0; i < nf; i++) {
    Mirfn *g = mirfnat(i);

    if (g->s == s) {
      *known = 1;
      *ret |= g->retbits;
      *leak |= g->leakbits;
      if (*recvptr || !vlen(g->locals) || !g->locals[0]->param || !g->locals[0]->ty)
        continue;
      { /* the receiver's own shape, the first instance's word: a
         * pointer-typed self is the sugar's borrow of the receiver,
         * any other self the copy of it (05-traits.md) */
        Type *t = g->locals[0]->ty;

        while (t->k == Tymut)
          t = t->t;
        *recvptr = t->k == Typtr;
      }
    }
  }
}

/* -- the provenance: what this value is a mask of ----------------------- */

static Esc df(Mirfn *f, Ast *e, Esc *st);

/* a body that answers with a value: the tail the walk falls off */
static Ast *
armval(Ast *b)
{
  if (!b)
    return 0;
  if (b->k == Nblock)
    return armval(b->v.blk.tail);
  return b;
}

/* a row of children, the mask of each, all of them together */
static Esc
anyof(Mirfn *f, Ast **es, usize n, Esc *st)
{
  usize i;
  Esc   r = 0;

  for (i = 0; i < n; i++)
    r |= df(f, es[i], st);
  return r;
}

/* the address of a place, as a value: of this frame's own storage,
 * the frame bit; of a pointee -- through a pointer root, a deref --
 * the pointer the root slot held, the offset one value with it
 * (01-types.md) */
static Esc
addrmask(Mirfn *f, Ast *place, Esc *st)
{
  char     *nm;
  int       steps, deref = placerootx(place, &nm, &steps);
  usize     ix;
  Mirlocal *l;

  if (!nm || !(l = locidx(f, nm, &ix)))
    return 0;
  if (deref || thruptr(l, steps))
    return st[ix];
  return EFRAME;
}

/* the value a place holds, asked only of a pointer-carrying read */
static Esc
placeval(Mirfn *f, Ast *place, Esc *st)
{
  char     *nm;
  int       steps, deref = placerootx(place, &nm, &steps);
  usize     ix;
  Mirlocal *l;

  if (deref || !nm || !(l = locidx(f, nm, &ix)))
    return 0;
  if (thruptr(l, steps))
    return 0; /* loaded out of the pointee, not out of the slot */
  return st[ix];
}

static Esc
df(Mirfn *f, Ast *e, Esc *st)
{
  usize i;

  if (!e)
    return 0;
  switch (e->k) {
  case Nun:
    if (e->v.un.op == Tamp)
      return addrmask(f, e->v.un.e, st); /* the address itself */
    if (e->v.un.op == Tstar) {           /* the value at what a slot points to:
                                          * the slot's own mask says where
                                          * it points; the read's type says
                                          * whether what it loaded can
                                          * carry a pointer */
      if (!carryptr(e->ty))
        return 0;
      return placeval(f, e->v.un.e, st);
    }
    return 0; /* arith on scalars */
  case Naccess:
  case Nindex:
  case Ntupidx:
    if (!carryptr(e->ty))
      return 0; /* a scalar field or element: nothing to carry */
    return placeval(f, e, st);
  case Nrangeindex: { /* a view: of a slice- or pointer-typed local,
                       * the tracked mask; of the frame's own storage,
                       * the frame */
    char     *nm;
    int       steps, deref = placerootx(e, &nm, &steps);
    Mirlocal *l;

    if (deref || !nm || !(l = locidx(f, nm, 0)))
      return 0;
    if (thruptr(l, steps))
      return 0; /* a view of the pointee, the caller's */
    if (l->ty && (l->ty->k == Tyslice || l->ty->k == Typtr))
      return placeval(f, e, st); /* a value that points, itself */
    return EFRAME;               /* this frame's own storage -- a
                                  * by-value parameter's included: the
                                  * slot is the frame's, the caller's
                                  * copy in it (01-types.md) */
  }
  case Npath: { /* a name: a local's or a parameter's tracked mask;
                 * anything else's -- a static, a fn, a variant --
                 * nobody's frame */
    char     *nm;
    usize     ix;
    Mirlocal *l;

    if (vlen(e->v.path.segs) != 1 || e->v.path.root || e->v.path.segs[0]->v.seg.args)
      return 0;
    nm = e->v.path.segs[0]->v.seg.name;
    if (!(l = locidx(f, nm, &ix)))
      return 0;
    return st[ix];
  }
  case Ntuple:
    return anyof(f, e->v.list.ts, vlen(e->v.list.ts), st);
  case Narraylit:
    if (!e->v.arrlit.len)
      return EFRAME; /* the unsized literal: a slice whose backing the
                      * emitter raises in this frame (01-types.md) */
    return anyof(f, e->v.arrlit.es, vlen(e->v.arrlit.es), st);
  case Nstructlit:
    return anyof(f, e->v.slit.inits, vlen(e->v.slit.inits), st);
  case Ncall: { /* the callee's promise, read at the call: an
                 * argument's mask rides the parameters that flow to
                 * the answer, and a frame pointer handed to a
                 * parameter that leaks is the return's own escape,
                 * said here. A callee the layer does not hold keeps
                 * the first half's coarse rule -- every argument into
                 * a pointer-carrying answer -- and leaks nothing. The
                 * method sugar folds its receiver into the callee
                 * node: the first argument is not among the args, and
                 * its mask is the receiver's own -- the address the
                 * sugar borrows when the self is a pointer, the value
                 * it copies when the self is not (05-traits.md) */
    unsigned retb = 0, leakb = 0, res = 0;
    int      known = 0, recvptr = 0;
    usize    na = vlen(e->v.call.args);
    Ast     *recv = e->v.call.f && e->v.call.f->k == Naccess ? e->v.call.f->v.fld.e : 0;

    calsum(e->v.call.sym, &retb, &leakb, &known, &recvptr);
    if (known && na + (recv ? 1 : 0) > 31)
      known = 0; /* more arguments than a mask has bits: the coarse
                  * rule, not a silent shift off the end */
    if (recv) {
      Esc am = recvptr ? addrmask(f, recv, st) : placeval(f, recv, st);

      if (am) {
        if (!known)
          res |= am;
        else {
          if (leakb & 1u) {
            if (summode)
              sleak |= am;
            else if (am & EFRAME)
              cerrat(recv,
                     "this value points into the fn's own frame; the call keeps it past the frame's"
                     " end (01-types.md)");
          }
          if (retb & 1u)
            res |= am;
        }
      }
    }
    for (i = 0; i < na; i++) {
      Esc am = df(f, e->v.call.args[i], st);

      if (!am)
        continue;
      if (!known) {
        res |= am;
        continue;
      }
      if (recv) { /* the sugar's receiver rode bit 0; the spelled
                   * arguments ride the rest, one further along */
        if (leakb & (1u << (i + 1))) {
          if (summode)
            sleak |= am;
          else if (am & EFRAME)
            cerrat(e->v.call.args[i],
                   "this value points into the fn's own frame; the call keeps it past the frame's"
                   " end (01-types.md)");
        }
        if (retb & (1u << (i + 1)))
          res |= am;
      } else {
        if (leakb & (1u << i)) {
          if (summode)
            sleak |= am;
          else if (am & EFRAME)
            cerrat(e->v.call.args[i],
                   "this value points into the fn's own frame; the call keeps it past the frame's"
                   " end (01-types.md)");
        }
        if (retb & (1u << i))
          res |= am;
      }
    }
    if (!carryptr(e->ty))
      return 0; /* a scalar answer cannot hold a pointer at all */
    return res;
  }
  case Nbuiltin:
    if (!carryptr(e->ty))
      return 0;
    return anyof(f, e->v.blt.args, vlen(e->v.blt.args), st);
  case Nclosure: { /* the env: a byref capture holds an outer place's
                    * address, a byval capture copies the outer value
                    * and the mask it carried (05-traits.md) */
    Ast **cs = e->v.clos.caps;
    Esc   r = 0;

    for (i = 0; i < vlen(cs); i++)
      if (cs[i]->v.cap.byref)
        r |= addrmask(f, cs[i]->v.cap.place, st);
      else
        r |= df(f, cs[i]->v.cap.place, st);
    return r;
  }
  case Nif:
  case Ncif: /* the value is an arm's, never the cond's: the cond only
              * decides which one, a pointer it holds flows nowhere */
    return df(f, armval(e->v.ifx.then), st) | df(f, armval(e->v.ifx.els), st);
  case Nmatch: { /* the arms' values answer; the scrutinee only selects */
    Esc r = 0;

    for (i = 0; i < vlen(e->v.call.args); i++)
      r |= df(f, armval(e->v.call.args[i]->v.n2.b), st);
    return r;
  }
  case Nblock:
    return df(f, armval(e), st);
  case Ntry:
    return df(f, e->v.n1.e, st);
  default:
    return 0; /* the scalars, the literals' own values: a string's
               * bytes ride the data segment, not a frame */
  }
}

/* -- the transfer: what a statement does to the state ------------------ */

/* is this local's name among the pattern's bindings */
static int
boundin(Mirlocal *l, Ast *p)
{
  usize i;

  if (!p)
    return 0;
  switch (p->k) {
  case Npath:
    return vlen(p->v.path.segs) == 1 && !p->v.path.segs[0]->v.seg.args &&
           strcmp(p->v.path.segs[0]->v.seg.name, l->name) == 0;
  case Nppath:
    if (!p->v.ppath.payload && !p->v.ppath.named) /* the binding form */
      return vlen(p->v.ppath.path->v.path.segs) == 1 &&
             strcmp(p->v.ppath.path->v.path.segs[0]->v.seg.name, l->name) == 0;
    if (p->v.ppath.payload)
      for (i = 0; i < vlen(p->v.ppath.payload); i++)
        if (boundin(l, p->v.ppath.payload[i]->k == Npfield ? p->v.ppath.payload[i]->v.n1.e
                                                           : p->v.ppath.payload[i]))
          return 1;
    return 0;
  case Npstruct:
    if (p->v.pstruct.fields)
      for (i = 0; i < vlen(p->v.pstruct.fields); i++)
        if (boundin(l, p->v.pstruct.fields[i]->v.n1.e))
          return 1;
    return 0;
  case Ntuple:
  case Npor:
    for (i = 0; i < vlen(p->v.list.ts); i++)
      if (boundin(l, p->v.list.ts[i]))
        return 1;
    return 0;
  default:
    return 0;
  }
}

/* an assignment's two halves: a binding's mask set, or a store through
 * a pointer -- the second of the two ways a value leaves, and the
 * report's own to make (the check) or the summary's own to keep (the
 * run that asks which parameters get there) */
static void
assign(Mirfn *f, Ast *a, Esc *s)
{
  char *nm;
  int   steps, deref = placerootx(a->v.bin.l, &nm, &steps);
  usize ix;
  Esc   v = df(f, a->v.bin.r, s);

  if (!v)
    return;
  if (!deref && nm && locidx(f, nm, &ix)) {
    if (thruptr(f->locals[ix], steps) || f->locals[ix]->param) {
      /* written through the pointer the root holds -- the sugar's
       * own p.f = v, no * spelled (01-types.md) -- into the pointee,
       * a storage this fn does not own: the value handed there is
       * the same leak a spelled deref's is. A parameter's own slot
       * is not a place a store may name; the pointee's is, and it
       * answers the same way */
      if (summode)
        sleak |= v;
      else if (v & EFRAME)
        cerrat(a, "this value points into the fn's own frame; storing it through a pointer hands"
                  " it past the frame's end (01-types.md)");
      return;
    }
    s[ix] = v; /* a slot's own aggregate holds the mask now */
    return;
  }
  if (deref) {
    if (summode)
      sleak |= v;
    else if (v & EFRAME)
      cerrat(a, "this value points into the fn's own frame; storing it through a pointer hands it"
                " past the frame's end (01-types.md)");
  }
}

/* one instruction against the state: the lets and the assigns are
 * the two that move a pointer; the rest only spell the flow */
static void
transfer(Mirfn *f, Ast *e, Esc *s)
{
  usize i;

  if (!e)
    return;
  if (e->k == Nlet) {
    Esc v = df(f, e->v.let.e, s);

    if (v)
      for (i = 0; i < vlen(f->locals); i++) /* the pattern's own
                                             * bindings, the whole
                                             * value each: a
                                             * destructuring's rows
                                             * are not told apart */
        if (!f->locals[i]->param && boundin(f->locals[i], e->v.let.pat))
          s[i] = v;
    return;
  }
  if (e->k == Nassign)
    assign(f, e, s);
  if (e->k == Nexprstmt) {
    (void) df(f, e->v.n1.e, s); /* the expression's own course: a call
                                 * in statement position is a sink's
                                 * to see -- the leak question is about
                                 * the arguments, an answer nobody
                                 * keeps (#185) */
    return;
  }
}

/* -- the walk: a block at a time, to a fixpoint ------------------------- */

/* one fn's course, run once: the seed says which slots start holding
 * what (the check: nothing -- the parameters carry the caller's
 * values, the bindings nothing yet; the summary: one bit a
 * parameter), and the mode says what the sinks do (refuse, or
 * record which seeds got there) */
static void
escrun(Mirfn *f, Esc *seed)
{
  Esc  **in;  /* the state a block starts with */
  Esc  **out; /* the state its own instructions leave */
  int   *inq;
  usize  nb = vlen(f->blocks), nl = vlen(f->locals), b, i, j, k, head, wi;
  usize *work;

  if (!nb)
    return; /* no blocks: nothing was spelled. No locals is fine --
             * a frameless fn can still answer a literal the emitter
             * raises in its frame, and the sink below is the one
             * that sees it */
  in = calloc(nb, sizeof *in);
  out = calloc(nb, sizeof *out);
  inq = calloc(nb, sizeof *inq);
  if (!in || !out || !inq)
    die("mir: no memory for the escape check");
  for (b = 0; b < nb; b++) {
    in[b] = nl ? calloc(nl, sizeof **in) : (Esc *) 1;
    out[b] = nl ? calloc(nl, sizeof **out) : (Esc *) 1;
    if ((nl && !in[b]) || (nl && !out[b]))
      die("mir: no memory for the escape check");
  }
  if (seed && nl)
    for (k = 0; k < nl; k++)
      in[0][k] = seed[k];
  work = vnew(usize, 16);
  wi = 0; /* the entry */
  vappend(&work, &wi);
  inq[0] = 1;
  while (vlen(work)) {
    Mirblock *bb;

    head = work[vlen(work) - 1];
    (void) vpop(work);
    inq[head] = 0;
    bb = f->blocks[head];
    memcpy(out[head], in[head], nl * sizeof **out);
    for (i = 0; i < vlen(bb->insns); i++)
      transfer(f, bb->insns[i]->e, out[head]);
    if (bb->term == Mterm_return && bb->te) { /* the sink the fn spells
                                               * itself: the value it
                                               * hands back, read with
                                               * the state its own
                                               * block left */
      Ast *v = bb->te->k == Nreturn ? bb->te->v.n1.e : bb->te;
      Esc  r = df(f, v, out[head]);

      if (summode)
        sret |= r;
      else if (r & EFRAME)
        cerrat(v, "this value points into the fn's own frame; it dies with the return"
                  " (01-types.md)");
    }
    for (j = 0; j < vlen(bb->succ); j++) {
      usize s = bb->succ[j];

      for (k = 0; k < nl; k++)
        if (out[head][k] & ~in[s][k]) {
          in[s][k] |= out[head][k];
          if (!inq[s]) {
            vappend(&work, &s);
            inq[s] = 1;
          }
        }
    }
  }
  for (b = 0; b < nb; b++) {
    if (nl) {
      free(in[b]);
      free(out[b]);
    }
  }
  free(in);
  free(out);
  free(inq);
}

/* -- the two phases: the summaries, then the check ---------------------- */

/* one fn's summary, from the promises its callees carry now: the
 * parameter bits that reach the return, the parameter bits that
 * reach a store through a pointer or a leaking call. Zero when the
 * fn has nothing a mask can name */
static void
sumrun(Mirfn *f)
{
  Esc     *seed;
  usize    nl = vlen(f->locals), k, np = 0;
  unsigned pmask;

  f->retbits = 0;
  f->leakbits = 0;
  if (!vlen(f->blocks))
    return;
  for (k = 0; k < nl; k++)
    if (f->locals[k]->param)
      np++;
  if (!np || np > 31)
    return; /* nothing to promise, or more parameters than a mask has
             * bits: the callers' coarse rule answers */
  seed = nl ? calloc(nl, sizeof *seed) : 0;
  if (nl && !seed)
    die("mir: no memory for the escape summaries");
  {
    usize pi = 0;

    for (k = 0; k < nl; k++)
      if (f->locals[k]->param)
        seed[k] = 1u << ++pi; /* bit i+1 the parameter i: bit 0 the
                               * frame's own, never a seed's */
  }
  summode = 1;
  sret = sleak = 0;
  escrun(f, seed);
  summode = 0;
  pmask = np == 31 ? ~0u : ((1u << np) - 1);
  f->retbits = (sret >> 1) & pmask;   /* the bits the return met */
  f->leakbits = (sleak >> 1) & pmask; /* the bits a store through a
                                       * pointer met -- the frame bit
                                       * among them is this fn's own
                                       * check's, not a promise */
  if (seed)
    free(seed);
}

/* the frame-escape check over the whole program, the MIR whole: every
 * fn's promise computed first -- a callee's summary feeding its
 * callers', recursion included, until nothing grows -- and the
 * per-fn check after, the calls reading the settled promises and the
 * reports said in the file that wrote each fn */
void
mirescapeall(void)
{
  usize nf = mirnfns(), i;

  for (;;) { /* every promise starts empty (the tables' own zero) and
              * grows with what the callees promise, until still */ /* the callees' promises feed the callers': grow until
              * still. Each pass only adds -- the walk's answers are
              * monotone in what the callees promise -- so the loop
              * ends, recursion included. No fn is skipped: an empty
              * promise today can grow when a callee's does, and the
              * passes are cheap beside the emit that already ran */
    int      changed = 0;
    unsigned r0, l0;

    for (i = 0; i < nf; i++) {
      Mirfn *f = mirfnat(i);

      r0 = f->retbits;
      l0 = f->leakbits;
      sumrun(f);
      if (f->retbits != r0 || f->leakbits != l0)
        changed = 1;
    }
    if (!changed)
      break;
  }
  for (i = 0; i < nf; i++) { /* the check, one fn at a time, its
                              * file's own words for its own reports */
    Mirfn *f = mirfnat(i);

    if (f->path)
      lexsetpath(f->path);
    escrun(f, 0);
  }
}
