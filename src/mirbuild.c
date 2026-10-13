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
static void mlocs(Ast *p); /* the names a pattern binds, as slots */

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
    mexpr(d[i]);
    indrop--;
  }
}

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
    mirinsnnew(curb(), indrop ? Minsn_drop : Minsn_call, e, e->v.call.sym, dyn);
    return;
  }
  case Nbuiltin: /* @len, @ptr, @cast: the compiler's own word, and no
                  * fn of the program's behind the name. Its arguments
                  * are blt's and not call's -- a builtin's slots are
                  * a name, the type arguments and the value ones --
                  * and the values are where a call may stand: @cast
                  * carries one whole expression */
    for (i = 0; i < vlen(e->v.blt.args); i++)
      mexpr(e->v.blt.args[i]);
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
    mexpr(e->v.n2.a);
    mexpr(e->v.n2.b);
    return;
  case Nrangeindex: /* a view's own two ends: a call may stand in
                     * either -- `a[f()..g()]` runs both */
    mexpr(e->v.ridx.e);
    mexpr(e->v.ridx.lo);
    mexpr(e->v.ridx.hi);
    return;
  case Nrange: /* the interval's own two ends: `lo()..hi()` runs both,
                * once, before the first round -- the walk records them
                * wherever the iterable stands, so the graph does not
                * drop a fn only a range end names */
    mexpr(e->v.bin.l);
    mexpr(e->v.bin.r);
    return;
  case Nspread: /* the pack's own operand: a call may stand in the rows
                 * a spread hands over */
    mexpr(e->v.un.e);
    return;
  case Ntupidx:
    mexpr(e->v.tup.e);
    return;
  case Npath: /* a name: what it names may be a fn whose address this
               * expression takes rather than calls -- &dyn add, a
               * slot a fn's own pointer is put in (05-traits.md) */
    if (e->v.path.sym && e->v.path.sym->kind == Sfn)
      mirinsnnew(curb(), Minsn_ref, e, e->v.path.sym, 0);
    return;
  case Nclosure: /* a literal: the fn it becomes is reached by being
                  * made, and made it is wherever this stands */
    mirinsnnew(curb(), Minsn_ref, e, 0, 0);
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
    mdrops(e->v.blk.drops);
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
    mlocs(st->v.call.args[i]->v.n2.a); /* the arm's bindings */
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
  if (st->v.forx.shape != FCOND)
    mlocs(st->v.forx.a); /* the round's own bindings (10-iteration.md) */
  mb.cur = hdr;
  mexpr(st->v.forx.a);   /* the condition, or the round's pattern's source */
  mexpr(st->v.forx.b);   /* the round's let, or the iterable -- FIN's
                          * own walks put the next call in the body
                          * (10-iteration.md) */
  mexpr(st->v.forx.via); /* the into_iter the loop's sugar spelled:
                          * run once before the first round, and no
                          * statement of the body's carries it */
  vappend(&curb()->succ, &body);
  vappend(&curb()->succ, &exit);
  mirtermset(curb(), Mterm_branch, st);
  vappend(&mb.loophdr, &hdr);
  vappend(&mb.loopexit, &exit);
  mb.cur = body;
  mbody(st->v.forx.body);
  mstmts(st->v.forx.unroll);           /* the const rounds' own copies */
  mdrops(st->v.forx.drops);            /* the round's own bindings */
  mdrops(st->v.forx.itdrops);          /* and the iterator's, at the loop's end */
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
    mexpr(st->v.let.e);
    mlocs(st->v.let.pat); /* the binding's slots, whatever the pattern */
    mb.f->nbind++;
    mirinsnnew(curb(), Minsn_expr, st, 0, 0);
    return;
  case Nassign:
    mexpr(st->v.bin.l);
    mexpr(st->v.bin.r);
    mdrops(st->v.bin.drop);
    mirinsnnew(curb(), Minsn_expr, st, 0, 0);
    return;
  case Nexprstmt:
    mexpr(st->v.n1.e);
    mdrops(st->v.n1.drops);
    mirinsnnew(curb(), Minsn_expr, st, 0, 0);
    return;
  case Nreturn:
    mexpr(st->v.n1.e);
    mdrops(st->v.n1.drops);
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
    mstmts(st->v.blk.stmts);
    mexpr(st->v.blk.tail);
    mdrops(st->v.blk.drops);
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

/* the names a pattern binds, registered as the frame's slots: a
 * place rooted at one is this frame's storage, whatever the pattern
 * spelled around it (09-match.md). A pattern's nodes carry the
 * binding's type -- the checker wrote it there (rpat) */
static void
mlocs(Ast *p)
{
  usize i;

  if (!p)
    return;
  switch (p->k) {
  case Npath: /* the binding form: one name, no arguments */
    if (vlen(p->v.path.segs) == 1 && !p->v.path.segs[0]->v.seg.args)
      mirlocalnew(mb.f, p->v.path.segs[0]->v.seg.name, p->ty, 0);
    return;
  case Nppath: {
    Ast **segs = p->v.ppath.path->v.path.segs;

    if (!p->v.ppath.payload && !p->v.ppath.named && vlen(segs) == 1) {
      /* the parser sends every ident-headed pattern here: bare, no
       * payload and no braces, it is the binding form (09) */
      mirlocalnew(mb.f, segs[0]->v.seg.name, p->ty, 0);
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
  np = vlen(ps);
  for (i = 0; i < np; i++) /* the parameters' slots: this frame's own
                            * storage, the caller's value in it
                            * (01-types.md) */
    mirlocalnew(mb.f, ps[i]->v.param.name, sig && i < sig->nargs ? sig->args[i] : 0, 1);
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
    }
  mb.cur = newblock();
  mbody(body);
  mdrops(decl->k == Nfn ? decl->v.fn.drops : decl->v.clos.drops); /* the parameters' own
                                                                   * slots, run at the
                                                                   * return the body
                                                                   * reaches on its own */
  mirtermset(curb(), Mterm_return, tailval(body));                /* the body's end: the fn's own
                                                                   * way out, whether or not the
                                                                   * tree spelled a return there --
                                                                   * the tail named so the check
                                                                   * below reads the value */
  mb.f->path = lexpath(); /* the file the fn was written in: the
                           * escape check runs once the MIR is whole,
                           * and its reports are this file's own
                           * (#185) */
}
