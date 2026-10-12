/* mirescape.c -- client C's first half (#185): the frame-escape check.
 *
 * specs/01-types.md names two errors the compiler never said a word
 * about -- returning the address of a local, returning a slice that
 * outlives what it borrows -- and README hedged with "not proven"
 * while every one of them compiled. This is the check: a value that
 * points into the fn's own frame may not cross the boundary, not by
 * the return and not by a store through a pointer -- the two ways a
 * value leaves (01-types.md).
 *
 * It runs on the mid layer's own ground, the blocks and the locals
 * mirbuild registered, and it is flow-sensitive: a binding's answer
 * is tracked forward through the blocks, a join unions what its ways
 * brought, a loop converges. The frame's whole life is one fn's --
 * it dies at the return, unconditionally -- so there are no regions
 * yet: "points into this frame" is one bit a slot carries
 * (todos/todo-20261012-00.md, client C's later half holds the
 * outlives algebra).
 *
 * What it cannot see, said plainly: a pointer handed to a call (the
 * callee's own frame is the callee's to check; whether it stores
 * what it was given is its signature's promise, not this walk's),
 * and a byval capture of a frame pointer copied into a closure's
 * env -- the closure's own check reads the env, not the frame the
 * pointer came from. Both are the interprocedural half, C's next.
 * Nothing here changes what a program answers: what it changes is
 * which programs are programs (01-types.md, and README's Not-proven
 * narrows with it). */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "check.h" /* cerrat: the report says where, the tree's own
                    * place */
#include "die.h"
#include "mir.h"
#include "type.h"
#include "vec.h"

/* the state one slot of the frame carries: does its value point into
 * this frame's storage. Two bits would do; one is all the frame's
 * own life needs */
typedef unsigned char Esc;

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

/* a place's own storage: rooted at a local of this fn, the storage is
 * this frame's -- a parameter's slot included, the caller's value in
 * it; rooted through a deref, the caller's; a static's or a
 * namespace's, nobody's frame here */
static int
frameplace(Mirfn *f, Ast *place)
{
  char     *nm;
  int       steps, deref = placerootx(place, &nm, &steps);
  Mirlocal *l;

  if (deref || !nm || !(l = locidx(f, nm, 0)))
    return 0;
  return !thruptr(l, steps); /* the chain steps inside a pointer-typed
                              * root: the pointee's, the caller's */
}

/* the value a place holds, asked only of a pointer-carrying read: a
 * local's tracked bit, a parameter's own (the caller's value, never
 * this frame's), anything else's nothing. The caller gates on the
 * read's own type -- a scalar loaded out of a frame slot carries
 * nothing anywhere (01-types.md) */
static Esc
placeval(Mirfn *f, Ast *place, Esc *st)
{
  char     *nm;
  int       steps, deref = placerootx(place, &nm, &steps);
  usize     ix;
  Mirlocal *l;

  if (deref || !nm || !(l = locidx(f, nm, &ix)) || l->param)
    return 0;
  if (thruptr(l, steps))
    return 0; /* loaded out of the pointee, not out of the slot */
  return st[ix];
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

/* -- the provenance: does this value point into the frame -------------- */

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

/* a row of children, the one question asked of each: any of them
 * points into the frame */
static Esc
anyof(Mirfn *f, Ast **es, usize n, Esc *st)
{
  usize i;

  for (i = 0; i < n; i++)
    if (df(f, es[i], st))
      return 1;
  return 0;
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
      return frameplace(f, e->v.un.e); /* the address itself: of this
                                        * frame's storage, or not */
    if (e->v.un.op == Tstar) {         /* the value at what a slot points to:
                                        * the slot's own bit says where it
                                        * points; the read's type says whether
                                        * what it loaded can carry a pointer */
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
                       * the tracked bit; of the frame's own storage,
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
    return 1;                    /* this frame's own storage -- a
                                  * by-value parameter's included: the
                                  * slot is the frame's, the caller's
                                  * copy in it (01-types.md) */
  }
  case Npath: { /* a name: a local's tracked bit; a parameter's, the
                 * caller's; anything else's -- a static, a fn, a
                 * variant -- nobody's frame */
    char     *nm;
    usize     ix;
    Mirlocal *l;

    if (vlen(e->v.path.segs) != 1 || e->v.path.root || e->v.path.segs[0]->v.seg.args)
      return 0;
    nm = e->v.path.segs[0]->v.seg.name;
    if (!(l = locidx(f, nm, &ix)) || l->param)
      return 0;
    return st[ix];
  }
  case Ntuple:
    return anyof(f, e->v.list.ts, vlen(e->v.list.ts), st);
  case Narraylit:
    if (!e->v.arrlit.len)
      return 1; /* the unsized literal: a slice whose backing the
                 * emitter raises in this frame (01-types.md) */
    return anyof(f, e->v.arrlit.es, vlen(e->v.arrlit.es), st);
  case Nstructlit:
    return anyof(f, e->v.slit.inits, vlen(e->v.slit.inits), st);
  case Ncall: { /* the result: this walk cannot follow the call, so
                 * the conservative ask -- a pointer-carrying answer
                 * built from an argument that points into the frame
                 * is a pointer into the frame; a scalar answer
                 * cannot hold one at all */
    if (!carryptr(e->ty))
      return 0;
    if (df(f, e->v.call.f, st))
      return 1;
    return anyof(f, e->v.call.args, vlen(e->v.call.args), st);
  }
  case Nbuiltin:
    if (!carryptr(e->ty))
      return 0;
    return anyof(f, e->v.blt.args, vlen(e->v.blt.args), st);
  case Nclosure: { /* the env: a byref capture holds an outer place,
                    * this frame's if the place is; a byval capture
                    * copies the outer value, the bit it carried
                    * (05-traits.md) */
    Ast **cs = e->v.clos.caps;

    for (i = 0; i < vlen(cs); i++)
      if (cs[i]->v.cap.byref) {
        if (frameplace(f, cs[i]->v.cap.place))
          return 1;
      } else if (df(f, cs[i]->v.cap.place, st))
        return 1;
    return 0;
  }
  case Nif:
  case Ncif: /* the value is an arm's, never the cond's: the cond only
              * decides which one, a pointer it holds flows nowhere */
    return df(f, armval(e->v.ifx.then), st) | df(f, armval(e->v.ifx.els), st);
  case Nmatch: /* the arms' values answer; the scrutinee only selects */
    for (i = 0; i < vlen(e->v.call.args); i++)
      if (df(f, armval(e->v.call.args[i]->v.n2.b), st))
        return 1;
    return 0;
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

/* an assignment's two halves: a binding's bit set, or a store through
 * a pointer -- the second of the two ways a value leaves, and the
 * report's own to make */
static void
assign(Mirfn *f, Ast *st, Esc *s)
{
  char *nm;
  int   steps, deref = placerootx(st->v.bin.l, &nm, &steps);
  usize ix;
  Esc   v = df(f, st->v.bin.r, s);

  if (!v)
    return;
  if (!deref && nm && locidx(f, nm, &ix) && !f->locals[ix]->param) {
    if (thruptr(f->locals[ix], steps))
      return;  /* written through the pointer, into the pointee */
    s[ix] = 1; /* a slot's own aggregate holds a frame pointer now */
    return;
  }
  if (deref)
    cerrat(st, "this value points into the fn's own frame; storing it through a pointer hands it"
               " past the frame's end (01-types.md)");
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
    if (df(f, e->v.let.e, s))
      for (i = 0; i < vlen(f->locals); i++) /* the pattern's own
                                             * bindings, the whole
                                             * value each: a
                                             * destructuring's rows
                                             * are not told apart */
        if (!f->locals[i]->param && boundin(f->locals[i], e->v.let.pat))
          s[i] = 1;
    return;
  }
  if (e->k == Nassign)
    assign(f, e, s);
}

/* -- the walk: a block at a time, to a fixpoint ------------------------- */

void
mirescape(Mirfn *f)
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
  work = vnew(usize, 16);
  wi = 0; /* the entry: every slot starts clean -- the parameters hold
           * the caller's values, the bindings nothing yet */
  vappend(&work, &wi);
  inq[0] = 1;
  while (vlen(work)) {
    Mirblock *bb;

    head = work[vlen(work) - 1];
    (void) vpop(work);
    inq[head] = 0;
    bb = f->blocks[head];
    memcpy(out[head], in[head], nl);
    for (i = 0; i < vlen(bb->insns); i++)
      transfer(f, bb->insns[i]->e, out[head]);
    if (bb->term == Mterm_return && bb->te) { /* the sink the fn spells
                                               * itself: the value it
                                               * hands back, read with
                                               * the state its own
                                               * block left */
      Ast *v = bb->te->k == Nreturn ? bb->te->v.n1.e : bb->te;

      if (getenv("MIRESC"))
        fprintf(stderr, "sink %s: te-kind=%d df=%d\n", f->name ? f->name : "?", v ? v->k : -1,
                df(f, v, out[head]));
      if (df(f, v, out[head]))
        cerrat(v, "this value points into the fn's own frame; it dies with the return"
                  " (01-types.md)");
    }
    for (j = 0; j < vlen(bb->succ); j++) {
      usize s = bb->succ[j];

      for (k = 0; k < nl; k++)
        if (out[head][k] && !in[s][k]) {
          in[s][k] = 1;
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
