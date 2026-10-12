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
#include "die.h"
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
  mirtermset(b, k, te);
}

/* -- the expressions: the calls in one, the rvalue left whole -------- */

static void mexpr(Ast *e);
static void mstmts(Ast **stmts);
static void mstmt(Ast *st);

/* the calls an expression holds, in the order they stand. A node this
 * walk does not open leaves nothing behind -- a kind the tree carries
 * and this switch does not name is a kind whose calls go unrecorded,
 * which is a hole the next stage's completeness owes a word, not a
 * wrong shape */
static void
mexpr(Ast *e)
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
    mexpr(f);
    for (i = 0; i < vlen(e->v.call.args); i++)
      mexpr(e->v.call.args[i]);
    mirinsnnew(curb(), Minsn_call, e, e->v.call.sym, dyn);
    return;
  }
  case Nbuiltin: /* @len, @ptr, @cast: the compiler's own word, no
                  * fn of the program's behind it (01-types.md) */
    for (i = 0; i < vlen(e->v.call.args); i++)
      mexpr(e->v.call.args[i]);
    return;
  case Nbin:
    mexpr(e->v.bin.l);
    mexpr(e->v.bin.r);
    return;
  case Nun:
  case Ntry:
    mexpr(e->v.n1.e);
    return;
  case Naccess:
    mexpr(e->v.fld.e);
    return;
  case Nindex:
  case Ntresult:
    mexpr(e->v.n2.a);
    mexpr(e->v.n2.b);
    return;
  case Ntupidx:
    mexpr(e->v.tup.e);
    return;
  case Ntuple:
    for (i = 0; i < vlen(e->v.list.ts); i++)
      mexpr(e->v.list.ts[i]);
    return;
  case Narraylit:
    for (i = 0; i < vlen(e->v.arrlit.es); i++)
      mexpr(e->v.arrlit.es[i]);
    return;
  case Nstructlit:
    for (i = 0; i < vlen(e->v.slit.inits); i++)
      mexpr(e->v.slit.inits[i]);
    return;
  case Ninit:
    mexpr(e->v.n1.e);
    return;
  case Nblock: /* a value's own block: the statements run where the
                * enclosing block stands -- stage A does not cut one
                * for it */
    mstmts(e->v.blk.stmts);
    mexpr(e->v.blk.tail);
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
    mexpr(b);
}

static void
mif(Ast *st)
{
  usize thenb, elseb, join, endthen;

  mexpr(st->v.ifx.cond);
  thenb = newblock();
  join = newblock();
  elseb = st->v.ifx.els ? newblock() : join; /* no else: the join is
                                              * the way the branch
                                              * falls, and a block of
                                              * its own would be an
                                              * empty one */
  vappend(&curb()->succ, &thenb);
  vappend(&curb()->succ, &elseb);
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

  mexpr(st->v.call.f); /* the scrutinee */
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
    mb.cur = armb[i];
    mbody(st->v.call.args[i]->v.n2.b);
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
  mb.cur = hdr;
  mexpr(st->v.forx.a); /* the condition, or the round's pattern's source */
  mexpr(st->v.forx.b); /* the round's let, or the iterable -- FIN's
                        * own walks put the next call in the body
                        * (10-iteration.md) */
  vappend(&curb()->succ, &body);
  vappend(&curb()->succ, &exit);
  mirtermset(curb(), Mterm_branch, st);
  vappend(&mb.loophdr, &hdr);
  vappend(&mb.loopexit, &exit);
  mb.cur = body;
  mbody(st->v.forx.body);
  handto(mb.cur, hdr, Mterm_goto, st); /* a round's end is the
                                        * header's again */
  (void) vpop(mb.loophdr);
  (void) vpop(mb.loopexit);
  mb.cur = exit;
  /* a const-for's unroll is the same rounds already spelled, one copy
   * a round (08-reflection.md): the body above is the shape, and the
   * copies hold no call the shape does not */
}

static void
mstmt(Ast *st)
{
  if (!st)
    return;
  switch (st->k) {
  case Nlet:
    mexpr(st->v.let.e);
    mb.f->nbind++;
    mirinsnnew(curb(), Minsn_expr, st, 0, 0);
    return;
  case Nassign:
    mexpr(st->v.bin.l);
    mexpr(st->v.bin.r);
    mirinsnnew(curb(), Minsn_expr, st, 0, 0);
    return;
  case Nexprstmt:
    mexpr(st->v.n1.e);
    mirinsnnew(curb(), Minsn_expr, st, 0, 0);
    return;
  case Nreturn:
    mexpr(st->v.n1.e);
    mirtermset(curb(), Mterm_return, st);
    mb.cur = newblock(); /* what follows is reachable only in the
                          * tree's own shape */
    return;
  case Nbreak:
    if (vlen(mb.loopexit)) {
      handto(mb.cur, mb.loopexit[vlen(mb.loopexit) - 1], Mterm_goto, st);
      mb.cur = newblock();
    }
    return;
  case Ncontinue:
    if (vlen(mb.loophdr)) {
      handto(mb.cur, mb.loophdr[vlen(mb.loophdr) - 1], Mterm_goto, st);
      mb.cur = newblock();
    }
    return;
  case Nblock:
    mstmts(st->v.blk.stmts);
    mexpr(st->v.blk.tail);
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
    mirinsnnew(curb(), Minsn_expr, st, 0, 0);
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

void
mirfnbuild(char *name, Sym *s, Ast *decl)
{
  Ast *body;

  if (!mir_wanted)
    return;
  if (decl->k == Nfn)
    body = decl->v.fn.body;
  else if (decl->k == Nclosure)
    body = decl->v.clos.body;
  else
    return; /* not a fn's shape: nothing to spell */
  if (!body)
    return; /* a declaration, or the ";" form (01-types.md) */
  mb.f = mirfnnew(name, s, decl);
  mb.loophdr = vnew(usize, 4);
  mb.loopexit = vnew(usize, 4);
  mb.cur = newblock();
  mbody(body);
  mirtermset(curb(), Mterm_return, 0); /* the body's end: the fn's own
                                        * way out, whether or not the
                                        * tree spelled a return there */
}
