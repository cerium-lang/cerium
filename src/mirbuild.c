/* mirbuild.c -- the tree to the mid layer: one fn's course, block by
 * block.
 *
 * The walk is deliberately thin. What it spells is the flow -- which
 * block hands to which, and where the calls stand in them -- and what
 * it leaves alone is every rvalue: an expression's node is held whole
 * rather than taken apart, because stage A takes no client and the
 * client that needs the values apart is the one that should say how.
 * (todos/todo-20261012-00.md: build it, take no client.)
 *
 * It runs beside the emitter's own walk and not instead of it: the
 * instances are the emitter's to know -- a generic fn's body is
 * re-checked and emitted per instantiation (04-generics.md) -- so a
 * build that ran ahead of it would meet a tree the bindings had not
 * yet reached. Nothing here writes to the tree. */

#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "body.h" /* iscopy: a value's own shape, rd or mv */
#include "die.h"
#include "lex.h"
#include "mir.h"
#include "sym.h"
#include "type.h"
#include "vec.h"

/* the state one fn's walk stands in: a file-wide singleton, for the
 * build walks one fn to its end before the next begins */
typedef struct
{
  Mirfn *f;
  usize  cur;      /* the block being filled */
  usize *loophdr;  /* the loops the walk is inside: a continue's way
                    * back, innermost last */
  usize *loopexit; /* and a break's way out, the same order */
} Mb;

static Mb mb;

/* -- the facts: what one instruction touches (client D's ground) -----
 *
 * A binding's identity is its slot in the frame -- the name alone is
 * not it, a shadow takes a name a second time -- so the walk keeps
 * the names it can see, innermost last, the way the checker's own
 * locfind does. The touches gather in a buffer that empties at the
 * next instruction or terminator: the reads that feed a nested call
 * land on the call's own insn, and what is left when the statement's
 * own insn is made -- the writes a pattern bound, the move a return
 * takes -- lands there. Nothing a fact says is new: every word of it
 * the checker already knows, spelled here where the flow can read it
 * (03-move.md, 01-types.md). */

typedef struct
{
  char *name;
  usize ix; /* the slot in f->locals the binding owns */
} Mfname;

static Mfname  *mfseen;  /* the bindings visible, innermost last */
static usize   *mfmarks; /* the scopes' depths, a block an arm a round */
static Mirfact *mfbuf;   /* what the walk has gathered since the last
                          * instruction */

/* the slot a name spells right now, -1 when the walk sees no
 * binding of it: params, captures, and the patterns met so far, the
 * shadowing discipline the checker's locfind reads */
static int
mfshadow(char *name)
{
  usize i;

  for (i = vlen(mfseen); i > 0; i--)
    if (strcmp(mfseen[i - 1].name, name) == 0)
      return (int) mfseen[i - 1].ix;
  return -1;
}

static void
mfmark(void)
{
  usize d = vlen(mfseen);

  if (!mfmarks)
    mfmarks = vnew(usize, 8);
  vappend(&mfmarks, &d);
}

static void
mfunmark(void)
{
  usize d = vlen(mfmarks) ? mfmarks[vlen(mfmarks) - 1] : 0;

  if (vlen(mfmarks))
    (void) vpop(mfmarks);
  while (vlen(mfseen) > d)
    (void) vpop(mfseen);
}

/* a binding the walk met: the slot mirlocalnew just took, the name
 * now visible until its scope ends */
static void
mfbind(char *name)
{
  Mfname n;

  if (!mfseen)
    mfseen = vnew(Mfname, 16);
  n.name = name;
  n.ix = vlen(mb.f->locals) - 1;
  vappend(&mfseen, &n);
}

static void
mffact(usize local, char *path, Mirfactk k)
{
  Mirfact t;

  memset(&t, 0, sizeof t);
  if (path[0]) { /* the chain copied in: the buffer it was built in
                  * is the caller's own stack, gone before any client
                  * reads the fact */
    t.path = arenaalloc(strlen(path) + 1);
    strcpy(t.path, path);
  } else
    t.path = ""; /* the whole slot */
  t.local = local;
  t.k = k;
  if (!mfbuf)
    mfbuf = vnew(Mirfact, 8);
  vappend(&mfbuf, &t);
}

/* what the walk gathered lands on the instruction it feeds; the
 * buffer empties, the next statement starting clean */
static void
mfattach(Mirinsn *n)
{
  if (vlen(mfbuf)) {
    n->facts = mfbuf;
    mfbuf = 0;
  }
}

static void
mfattachterm(Mirblock *b)
{
  usize i;

  if (!vlen(mfbuf))
    return;
  if (!b->facts)
    b->facts = vnew(Mirfact, 4);
  for (i = 0; i < vlen(mfbuf); i++) /* the block one way out: what the
                                     * walk gathered rides it, after
                                     * whatever an earlier way left */
    vappend(&b->facts, &mfbuf[i]);
  mfbuf = 0;
}

/* the chain a place spells, top down: the field path under it ".a.b"
 * -- an index, a row, a tuple's, a slice touching the whole root,
 * the freeze's own rule (01-types.md) -- and the binding it bottoms
 * on, -1 when it bottoms on no binding of this frame (a temporary's
 * row, a deref across a pointer, a name the walk has not bound). The
 * bottom node comes back for the caller that still owes it a walk */
static int
mfroot(Ast *e, char *path, usize psz, Ast **base)
{
  usize n = 0;

  path[0] = 0;
  while (e->k == Naccess || e->k == Nindex || e->k == Nrangeindex || e->k == Ntupidx) {
    if (e->k == Naccess && strlen(e->v.fld.name) < 126 && n + strlen(e->v.fld.name) + 2 < psz) {
      char cat[128], old[256];

      sprintf(cat, ".%s", e->v.fld.name);
      strncpy(old, path, sizeof old - 1);
      old[(sizeof old) - 1] = 0;
      sprintf(path, "%s%s", cat, old); /* the root first: the chain's
                                        * own reading order */
      n = strlen(path);
    } else { /* an index, a row, a slice: the whole root */
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
  if (base)
    *base = e;
  if (e->k == Nun && e->v.un.op == Tstar)
    return -1; /* *p: across the pointer is a promise, not a proof */
  if (e->k != Npath || vlen(e->v.path.segs) != 1 || e->mvd)
    return -1; /* the move flag's own read: not the binding's */
  return mfshadow(e->v.path.segs[0]->v.seg.name);
}

/* the values a chain carries on its way down -- the subscripts, the
 * range's ends, the pointer a deref reads -- walked before the
 * place itself is written. The value arms need none of this: the
 * chain bottoms on no binding there and the whole node walks; only
 * the assign's left, whose read the store replaces, spells its
 * subscripts out here */
static void mfwalkchain(Ast *e);

static Mirblock *
curb(void)
{
  return mb.f->blocks[mb.cur];
}

static usize
newblock(void)
{
  usize n;

  n = vlen(mb.f->blocks);
  mirblocknew(mb.f);
  return n;
}

/* one block handing to another. A block the walk already ended -- a
 * return, a break, the arm a match took -- gets no second way out and
 * no successor named after it: the successor would contradict the
 * terminator the block already carries. Today a return, a break and a
 * continue each open a fresh block behind them, so this is a guard
 * rather than a path the walk takes; it is here because the opposite
 * order writes a successor onto a block that says it returns */
static void
handto(usize from, usize to, Mirtermk k, Ast *te)
{
  Mirblock *b = mb.f->blocks[from];

  if (b->termset)
    return;
  vappend(&b->succ, &to);
  mfattachterm(b); /* the goto's own: an arm's trailing writes when
                    * its body spelled no instruction */
  mirtermset(b, k, te);
}

/* -- the expressions: the calls in one, the rvalue left whole -------- */

static void mex(Ast *e);
static void mstmts(Ast **stmts);
static void mstmt(Ast *st);
static void mlocs(Ast *p); /* the names a pattern binds, as slots */

/* a place consumed as a value: the chain's root and path, the touch
 * the value's own shape spells -- a Copy reads, anything else moves
 * (03-move.md). A chain bottoming on no binding is no fact: the
 * whole node walks, the values under it read */
static void
mftake(Ast *e, Mirfactk bor)
{
  char p[256];
  Ast *base;
  int  ix = mfroot(e, p, sizeof p, &base);

  if (ix >= 0) {
    if (e->k == Nrangeindex) /* a view's own bargain: the root
                              * borrowed wherever it stands
                              * (01-types.md) */
      mffact((usize) ix, p, Mfbor);
    else if (bor != Mfrd) /* the caller's own word: a borrow */
      mffact((usize) ix, p, bor);
    else /* a value's own shape: a Copy reads, anything else moves */
      mffact((usize) ix, p, e->ty && !iscopy(e->ty) ? Mfmv : Mfrd);
  } else
    mex(base);
}

/* the destructors a place runs on its way out: a block's closing
 * brace, a return's early exit, an assignment's old value, a round's
 * own bindings. Pre-made calls the checker spelled and the emitter
 * sends (03-move.md) -- not statements, and not the tail either, so a
 * walk that read neither would lose every drop call in the program
 * and let the graph drop the destructors themselves. Every call the
 * walk meets under one is a drop's own, marked so -- a row holds
 * nothing a program wrote, only calls the checker made and the match
 * a variant's destructor spells (03-move.md) */
static int indrop;

static void
mdrops(Ast **d)
{
  usize i;

  for (i = 0; i < vlen(d); i++) {
    indrop++;
    mex(d[i]);
    indrop--;
  }
}

/* the calls an expression holds, in the order they stand. A node this
 * walk does not open leaves nothing behind -- a kind the tree carries
 * and this switch does not name is a kind whose calls go unrecorded,
 * which is a hole the next stage's completeness owes a word, not a
 * wrong shape */
static void
mex(Ast *e)
{
  usize i;

  if (!e)
    return;
  switch (e->k) {
  case Ncall: {
    Ast *f = e->v.call.f;
    int  dyn = 0;

    if (f && f->k == Naccess && f->v.fld.e && f->v.fld.e->ty && f->v.fld.e->ty->k == Tydyn)
      dyn = 1; /* the handle's own: the row a vtable names, chosen
                * where the handle was and not here
                * (06-dispatch.md) */
    if (f && f->k == Naccess)
      /* method sugar: the receiver a whole place, the method's own
       * name no field of it (06-dispatch.md) -- the value's shape
       * says rd or mv, a checker-wrapped & says bor */
      mftake(f->v.fld.e, Mfrd);
    else
      mex(f);
    for (i = 0; i < vlen(e->v.call.args); i++)
      mex(e->v.call.args[i]);
    mfattach(mirinsnnew(curb(), indrop ? Minsn_drop : Minsn_call, e, e->v.call.sym, dyn));
    return;
  }
  case Nbuiltin: /* @len, @ptr, @cast: the compiler's own word, and no
                  * fn of the program's behind the name. Its arguments
                  * are blt's and not call's -- a builtin's slots are
                  * a name, the type arguments and the value ones --
                  * and the values are where a call may stand: @cast
                  * carries one whole expression */
    for (i = 0; i < vlen(e->v.blt.args); i++)
      mex(e->v.blt.args[i]);
    return;
  case Nbin:
    mex(e->v.bin.l);
    mex(e->v.bin.r);
    return;
  case Nun:
    if (e->v.un.op == Tamp) { /* &x / &mut x: the place is borrowed,
                               * not read (01-types.md) */
      char p[256];
      Ast *base;
      int  ix = mfroot(e->v.un.e, p, sizeof p, &base);

      if (ix >= 0)
        mffact((usize) ix, p, e->v.un.mut ? Mfborm : Mfbor);
      else
        mex(base);
      return;
    }
    mex(e->v.n1.e);
    return;
  case Ntry:
    mex(e->v.n1.e);
    return;
  case Naccess:
  case Nindex:
  case Ntupidx: /* a place's own row, read or moved by the value the
                 * chain's top spells (03-move.md) */
    mftake(e, Mfrd);
    return;
  case Nrangeindex: /* a view's own two ends: the root borrowed
                     * (01-types.md), the ends values */
    mftake(e, Mfbor);
    return;
  case Nrange: /* the interval's own two ends: `lo()..hi()` runs both,
                * once, before the first round -- the walk records them
                * wherever the iterable stands, so the graph does not
                * drop a fn only a range end names */
    mex(e->v.bin.l);
    mex(e->v.bin.r);
    return;
  case Nspread: /* the pack's own operand: a call may stand in the rows
                 * a spread hands over */
    mex(e->v.un.e);
    return;
  case Npath: /* a name: what it names may be a fn whose address this
               * expression takes rather than calls -- &dyn add, a
               * slot a fn's own pointer is put in (05-traits.md). A
               * binding reads or moves by its own shape; the move
               * flag's read (mvd) touches nothing of the binding's */
    if (e->v.path.sym && e->v.path.sym->kind == Sfn) {
      mfattach(mirinsnnew(curb(), Minsn_ref, e, e->v.path.sym, 0));
      return;
    }
    if (!e->mvd && !e->v.path.sym && vlen(e->v.path.segs) == 1) {
      int ix = mfshadow(e->v.path.segs[0]->v.seg.name);

      if (ix >= 0)
        mffact((usize) ix, "", e->ty && !iscopy(e->ty) ? Mfmv : Mfrd);
    }
    return;
  case Nclosure: { /* a literal: the fn it becomes is reached by being
                    * made, and made it is wherever this stands. The
                    * captures are the outer frame's own touches: a
                    * byref one the borrow it takes, a value one the
                    * move it spells (05-traits.md) */
    usize ci;

    for (ci = 0; ci < vlen(e->v.clos.caps); ci++) {
      Ast *c = e->v.clos.caps[ci];

      if (c->v.cap.byref)
        mftake(c->v.cap.place, c->v.cap.mut ? Mfborm : Mfbor);
      else
        mftake(c->v.cap.place, Mfrd); /* Copy reads, the rest move */
    }
    mfattach(mirinsnnew(curb(), Minsn_ref, e, 0, 0));
    return;
  }
  case Ntuple:
    for (i = 0; i < vlen(e->v.list.ts); i++)
      mex(e->v.list.ts[i]);
    return;
  case Narraylit:
    for (i = 0; i < vlen(e->v.arrlit.es); i++)
      mex(e->v.arrlit.es[i]);
    return;
  case Nstructlit:
    for (i = 0; i < vlen(e->v.slit.inits); i++)
      mex(e->v.slit.inits[i]);
    return;
  case Ninit:
    mex(e->v.n1.e);
    return;
  case Nblock: /* a value's own block: the statements run where the
                * enclosing block stands -- stage A does not cut one
                * for it */
    mfmark();
    mstmts(e->v.blk.stmts);
    mex(e->v.blk.tail);
    mdrops(e->v.blk.drops);
    mfunmark();
    return;
  case Nif:
  case Ncif:
  case Nmatch:
    /* the flow is the same whether the value is kept or dropped: an
     * if or a match a program writes in a statement's place reaches
     * this walk wrapped in an expr (the checker's own shape), and
     * the blocks it asks for are the ones the statement form asks
     * for. Cutting them here keeps the branch and the switch from
     * vanishing into the block that holds them */
    mstmt(e);
    return;
  default:
    return; /* a leaf, or a kind whose inside is not a call's */
  }
}

/* the subscripts and the deref a written place carries: the values
 * the store reads on its way to the slot (the assign's left, whose
 * own read the store replaces) */
static void
mfwalkchain(Ast *e)
{
  while (e && (e->k == Naccess || e->k == Nindex || e->k == Nrangeindex || e->k == Ntupidx)) {
    if (e->k == Naccess)
      e = e->v.fld.e;
    else if (e->k == Ntupidx)
      e = e->v.tup.e;
    else if (e->k == Nindex) {
      mex(e->v.n2.b);
      e = e->v.n2.a;
    } else {
      mex(e->v.ridx.lo);
      mex(e->v.ridx.hi);
      e = e->v.ridx.e;
    }
  }
  if (e && e->k == Nun && e->v.un.op == Tstar)
    mex(e->v.un.e); /* the pointer the store reads through */
}

/* the store's own shape: the addressing reads first -- the pointer a
 * deref crosses, the subscripts a rooted place carries -- then the
 * new value's reads, the old value's drops, the write that lands
 * last -- the order the emitter sends (03-move.md), the order a
 * dataflow reads */
static void
mfwrite(Ast *l, Ast *r, Ast **olddrops)
{
  char p[256];
  Ast *base;
  int  ix;

  ix = mfroot(l, p, sizeof p, &base);
  if (ix >= 0)
    mfwalkchain(l); /* the subscripts under the rooted place */
  else if (base)
    mex(base); /* *p = v: the pointer's place read; the store itself
                * crosses it */
  mex(r);
  mdrops(olddrops);
  if (ix >= 0)
    mffact((usize) ix, p, Mfwr);
}

/* -- the statements: where the blocks come from --------------------- */

/* the kinds that stand where a statement stands. A match's arm and an
 * if's branch may hold either -- a value, or a statement of its own --
 * and which one decides whether the walk cuts a block */
static int
stmtkind(Nk k)
{
  switch (k) {
  case Nlet:
  case Nassign:
  case Nreturn:
  case Nbreak:
  case Ncontinue:
  case Nfor:
  case Ncfor:
  case Nexprstmt:
  case Nblock:
  case Nif:
  case Ncif:
  case Nmatch:
    return 1;
  default:
    return 0;
  }
}

static void
mbody(Ast *b) /* an arm's or a branch's body: a block, a statement, or
               * the value the arm answers with */
{
  if (!b)
    return;
  if (stmtkind(b->k))
    mstmt(b);
  else
    mex(b);
}

static void
mif(Ast *st)
{
  usize thenb, elseb, join, endthen;

  mex(st->v.ifx.cond); /* the cond's reads: the branch's own facts */
  thenb = newblock();
  join = newblock();
  elseb = st->v.ifx.els ? newblock() : join; /* no else: the join is
                                              * the way the branch
                                              * falls, and a block of
                                              * its own would be an
                                              * empty one */
  vappend(&curb()->succ, &thenb);
  vappend(&curb()->succ, &elseb);
  mfattachterm(curb());
  mirtermset(curb(), Mterm_branch, st);
  mb.cur = thenb;
  mbody(st->v.ifx.then);
  endthen = mb.cur;
  handto(endthen, join, Mterm_goto, 0);
  if (st->v.ifx.els) {
    mb.cur = elseb;
    mbody(st->v.ifx.els);
    handto(mb.cur, join, Mterm_goto, 0);
  }
  mb.cur = join;
}

static void
mmatch(Ast *st)
{
  usize  i, join;
  usize *armb;

  mex(st->v.call.f); /* the scrutinee's read: the switch's own */
  mfattachterm(curb());
  armb = vnew(usize, 4);
  for (i = 0; i < vlen(st->v.call.args); i++) {
    usize b = newblock();

    vappend(&armb, &b);
  }
  join = newblock();
  for (i = 0; i < vlen(armb); i++)
    vappend(&curb()->succ, &armb[i]);
  mirtermset(curb(), Mterm_switch, st);
  for (i = 0; i < vlen(st->v.call.args); i++) {
    /* the arm's bindings scoped to the arm: the pattern's names walk
     * with the body they answer, not ahead of every arm -- a later
     * arm's binding would shadow an earlier arm's reads else */
    mb.cur = armb[i];
    mfmark();
    mlocs(st->v.call.args[i]->v.n2.a);
    mbody(st->v.call.args[i]->v.n2.b);
    mfunmark();
    handto(mb.cur, join, Mterm_goto, 0);
  }
  mb.cur = join;
}

static void
mfor(Ast *st)
{
  usize hdr, body, exit;

  hdr = newblock();
  body = newblock();
  exit = newblock();
  handto(mb.cur, hdr, Mterm_goto, st);
  mfmark(); /* the round's bindings: scoped to the loop, the whole
             * of it, a continue's way back included */
  if (st->v.forx.shape != FCOND)
    mlocs(st->v.forx.a); /* the round's own bindings (10-iteration.md) */
  mb.cur = hdr;
  mex(st->v.forx.a);   /* the condition, or the round's pattern's source */
  mex(st->v.forx.b);   /* the round's let, or the iterable -- FIN's
                        * own walks put the next call in the body
                        * (10-iteration.md) */
  mex(st->v.forx.via); /* the into_iter the loop's sugar spelled:
                        * run once before the first round, and no
                        * statement of the body's carries it */
  vappend(&curb()->succ, &body);
  vappend(&curb()->succ, &exit);
  mfattachterm(curb());
  mirtermset(curb(), Mterm_branch, st);
  vappend(&mb.loophdr, &hdr);
  vappend(&mb.loopexit, &exit);
  mb.cur = body;
  mbody(st->v.forx.body);
  mstmts(st->v.forx.unroll);  /* the const rounds' own copies */
  mdrops(st->v.forx.drops);   /* the round's own bindings */
  mdrops(st->v.forx.itdrops); /* and the iterator's, at the loop's end */
  mfunmark();
  handto(mb.cur, hdr, Mterm_goto, st); /* a round's end is the
                                        * header's again */
  (void) vpop(mb.loophdr);
  (void) vpop(mb.loopexit);
  mb.cur = exit;
  /* a const-for's unroll is the rounds already spelled, one copy a
   * round -- but a copy whose baked numbers are that round's, and a
   * const-generic instance a number names is a different fn from the
   * one the next round's names (08-reflection.md). So the copies are
   * walked too, not left to the shape above */
}

static void
mstmt(Ast *st)
{
  if (!st)
    return;
  switch (st->k) {
  case Nlet:
    mex(st->v.let.e);
    mlocs(st->v.let.pat); /* the binding's slots, whatever the pattern */
    mb.f->nbind++;
    mfattach(mirinsnnew(curb(), Minsn_expr, st, 0, 0));
    return;
  case Nassign:
    mfwrite(st->v.bin.l, st->v.bin.r, st->v.bin.drop);
    mfattach(mirinsnnew(curb(), Minsn_expr, st, 0, 0));
    return;
  case Nexprstmt:
    mex(st->v.n1.e);
    mdrops(st->v.n1.drops);
    mfattach(mirinsnnew(curb(), Minsn_expr, st, 0, 0));
    return;
  case Nreturn:
    mex(st->v.n1.e);
    mdrops(st->v.n1.drops);
    mfattachterm(curb());
    mirtermset(curb(), Mterm_return, st);
    mb.cur = newblock(); /* what follows is reachable only in the
                          * tree's own shape */
    return;
  case Nbreak:
    mdrops(st->v.n1.drops);
    if (vlen(mb.loopexit)) {
      handto(mb.cur, mb.loopexit[vlen(mb.loopexit) - 1], Mterm_goto, st);
      mb.cur = newblock();
    }
    return;
  case Ncontinue:
    mdrops(st->v.n1.drops);
    if (vlen(mb.loophdr)) {
      handto(mb.cur, mb.loophdr[vlen(mb.loophdr) - 1], Mterm_goto, st);
      mb.cur = newblock();
    }
    return;
  case Nblock:
    mfmark();
    mstmts(st->v.blk.stmts);
    mex(st->v.blk.tail);
    mdrops(st->v.blk.drops);
    mfunmark();
    return;
  case Nif:
  case Ncif:
    mif(st);
    return;
  case Nmatch:
    mmatch(st);
    return;
  case Nfor:
  case Ncfor:
    mfor(st);
    return;
  default: /* a statement the walk does not cut a block for */
    mfattach(mirinsnnew(curb(), Minsn_expr, st, 0, 0));
    return;
  }
}

static void
mstmts(Ast **stmts)
{
  usize i;

  for (i = 0; i < vlen(stmts); i++)
    mstmt(stmts[i]);
}

/* -- the face mir.h gives the walk ---------------------------------- */

/* the names a pattern binds, registered as the frame's slots: a
 * place rooted at one is this frame's storage, whatever the pattern
 * spelled around it (09-match.md). A pattern's nodes carry the
 * binding's type -- the checker wrote it there (rpat). Each binding
 * is a write: the value lands in the slot the pattern made */
static void
mlocs(Ast *p)
{
  usize i;

  if (!p)
    return;
  switch (p->k) {
  case Npath: /* the binding form: one name, no arguments. The
               * checker wrote the binding's own type here (rpat);
               * a bare variant never carries one -- the mark that
               * tells the two apart, no walk of this own */
    if (vlen(p->v.path.segs) == 1 && !p->v.path.segs[0]->v.seg.args && p->ty) {
      mirlocalnew(mb.f, p->v.path.segs[0]->v.seg.name, p->ty, 0);
      mfbind(p->v.path.segs[0]->v.seg.name);
      mffact(vlen(mb.f->locals) - 1, "", Mfwr);
    }
    return;
  case Nppath: {
    Ast **segs = p->v.ppath.path->v.path.segs;

    if (!p->v.ppath.payload && !p->v.ppath.named && vlen(segs) == 1 && p->ty) {
      /* the parser sends every ident-headed pattern here: bare, no
       * payload and no braces, it is the binding form -- and the
       * type the checker wrote says which, a unit variant carrying
       * none (09) */
      mirlocalnew(mb.f, segs[0]->v.seg.name, p->ty, 0);
      mfbind(segs[0]->v.seg.name);
      mffact(vlen(mb.f->locals) - 1, "", Mfwr);
      return;
    }
    if (p->v.ppath.payload)
      for (i = 0; i < vlen(p->v.ppath.payload); i++)
        mlocs(p->v.ppath.payload[i]->k == Npfield ? p->v.ppath.payload[i]->v.n1.e
                                                  : p->v.ppath.payload[i]);
    return;
  }
  case Npstruct:
    if (p->v.pstruct.fields)
      for (i = 0; i < vlen(p->v.pstruct.fields); i++)
        mlocs(p->v.pstruct.fields[i]->v.n1.e);
    return;
  case Ntuple:
    for (i = 0; i < vlen(p->v.list.ts); i++)
      mlocs(p->v.list.ts[i]);
    return;
  case Npor:
    for (i = 0; i < vlen(p->v.list.ts); i++)
      mlocs(p->v.list.ts[i]);
    return;
  default: /* _ , a literal, a variant alone: nothing binds */
    return;
  }
}

/* the value a body answers with, the fn's own when the walk falls off
 * its end: a block's tail, an arm's, whatever expression stands where
 * the fn's value comes from -- the escape check reads it at the
 * return it never spelled */
static Ast *
tailval(Ast *b)
{
  if (!b)
    return 0;
  if (b->k == Nblock)
    return tailval(b->v.blk.tail);
  return b; /* a statement here is the unit's own shape; the check
             * walks what it is given and asks nothing of a unit */
}

void
mirfnbuild(char *name, Sym *s, Ast *decl)
{
  Ast **ps;
  Type *sig; /* the fn it spells: the parameters' own types live
              * here, the Nparam node carrying only its spelling */
  usize i, np;
  Ast  *body;

  if (decl->k == Nfn) {
    body = decl->v.fn.body;
    ps = decl->v.fn.params;
    sig = s ? s->fnty : 0;
  } else if (decl->k == Nclosure) {
    body = decl->v.clos.body;
    ps = decl->v.clos.params;
    sig = decl->v.clos.sig;
  } else
    return; /* not a fn's shape: nothing to spell */
  if (!body)
    return; /* a declaration, or the ";" form (01-types.md) */
  mb.f = mirfnnew(name, s, decl);
  mb.loophdr = vnew(usize, 4);
  mb.loopexit = vnew(usize, 4);
  mfseen = 0; /* this fn's names alone: the captures a closure sees,
               * the bindings the body met -- the world above a
               * closure is the env's, not its frame's */
  mfmarks = 0;
  mfbuf = 0;
  np = vlen(ps);
  for (i = 0; i < np; i++) { /* the parameters' slots: this frame's own
                              * storage, the caller's value in it
                              * (01-types.md) */
    mirlocalnew(mb.f, ps[i]->v.param.name, sig && i < sig->nargs ? sig->args[i] : 0, 1);
    mfbind(ps[i]->v.param.name);
  }
  if (decl->k == Nclosure)
    for (i = 0; i < vlen(decl->v.clos.caps); i++) { /* a capture's
                                                     * name, read from
                                                     * the env: byval a
                                                     * copy of the
                                                     * outer value, byref
                                                     * an outer place
                                                     * (05-traits.md) */
      Ast *c = decl->v.clos.caps[i];

      mirlocalnew(mb.f, c->v.cap.name, c->ty, 1);
      mfbind(c->v.cap.name);
    }
  mb.cur = newblock();
  mbody(body);
  mdrops(decl->k == Nfn ? decl->v.fn.drops : decl->v.clos.drops); /* the parameters' own
                                                                   * slots, run at the
                                                                   * return the body
                                                                   * reaches on its own */
  mfattachterm(curb());
  mirtermset(curb(), Mterm_return, tailval(body)); /* the body's end: the fn's own
                                                    * way out, whether or not the
                                                    * tree spelled a return there --
                                                    * the tail named so the check
                                                    * below reads the value */
  mb.f->path = lexpath();                          /* the file the fn was written in: the
                                                    * escape check runs once the MIR is whole,
                                                    * and its reports are this file's own
                                                    * (#185) */
}
