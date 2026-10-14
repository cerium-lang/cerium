/* mirredecide.c -- client D's flip: the drop rows the checker spelled
 * whole -- every binding that owns one, plain, the dead and the
 * guarded among them -- decided here against what the blocks say.
 * A row for a slot moved on every path comes out of the vector; a
 * row for a slot moved down one path hides under the guard the flag
 * reads; a row for a slot alive stands as spelled. The guard's own
 * words are the checker's (mkdropguard), the emitter's text not a
 * byte moved -- the shadow proved the two agree before the flip,
 * and the flip changes who says it, not what is said
 * (todos/todo-20261012-00.md, 03-move.md). */

#include <string.h>

#include "ast.h"
#include "body.h" /* mkdropguard: the guard's own words */
#include "die.h"
#include "mir.h"
#include "sym.h"
#include "vec.h"

/* the states the walk left on the fn */
static Mvst *
st(Mirfn *f)
{
  return f->mv;
}

/* the row a tree entry is, by its own node: the call a plain row
 * is, the match an enum's row spells -- the move walk recorded each
 * where it stands */
static Mvrow *
rowfind(Mirfn *f, Ast *e)
{
  Mvst *s = st(f);
  usize i;

  if (!s || !e)
    return 0;
  for (i = 0; i < vlen(s->rows); i++)
    if (s->rows[i].e == e)
      return &s->rows[i];
  return 0;
}

/* the state the row meets, snapshotted at its own point by the walk:
 * moved comes out, maybe hides under the guard, alive stands */
static u8
rowstate(Mirfn *f, Mvrow *r)
{
  (void) f;

  return r ? r->state : 0; /* nowhere found: alive, the row stands */
}

/* one vector of rows, decided in place. The rows a binding owes
 * stand together in the vector (the scope spelled them one binding
 * at a time), so a maybe gathers its own run and the run hides
 * under one guard -- one ownership, one flag (03-move.md) */
static void
rdvec(Mirfn *f, Ast ***vecp)
{
  Ast **v;
  usize n, i, j, k;

  if (!vecp || !*vecp)
    return;
  v = *vecp;
  n = vlen(v);
  i = 0;
  while (i < n) {
    Mvrow *r = rowfind(f, v[i]);

    if (!r || rowstate(f, r) == 0) { /* alive, or no voice of the
                                      * flow's: stands as spelled */
      i++;
      continue;
    }
    if (rowstate(f, r) == 2) { /* moved on every path: the row comes
                                * out, its destructor long gone with
                                * the value */
      vremove(v, i);
      n--;
      continue;
    }
    /* maybe: moved down one path, the guard the flag reads */
    j = i + 1;
    while (j < n) {
      Mvrow *r2 = rowfind(f, v[j]);

      if (!r2 || r2->slot != r->slot)
        break;
      j++;
    }
    v[i] = mkdropguard(f->locals[r->slot] ? f->locals[r->slot]->name : "?", v + i, j - i, v[i]);
    for (k = j - 1; k > i; k--)
      vremove(v, k);
    n -= j - i - 1;
    i++;
  }
}

/* -- the collector: every vector of rows the fn's tree holds -------
 *
 * The same shapes the build walks: statements and the blocks they
 * hold, the arms a match spells, the rounds a for runs -- a closure
 * literal is another fn's own tree and not walked here (its Mirfn
 * decides it), a value's own block is a statement's shape in an
 * expression's place. */

static void rdstmt(Mirfn *f, Ast *st);
static void rdexpr(Mirfn *f, Ast *e);

static void
rdstmts(Mirfn *f, Ast **ss)
{
  usize i;

  for (i = 0; i < vlen(ss); i++)
    rdstmt(f, ss[i]);
}

static void
rdbody(Mirfn *f, Ast *b)
{
  if (!b)
    return;
  switch (b->k) {
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
    rdstmt(f, b);
    return;
  default:
    rdexpr(f, b);
  }
}

static void
rdstmt(Mirfn *f, Ast *st)
{
  if (!st)
    return;
  switch (st->k) {
  case Nlet:
    rdexpr(f, st->v.let.e);
    return;
  case Nassign:
    rdexpr(f, st->v.bin.l);
    rdexpr(f, st->v.bin.r);
    /* the old value's row is the assignment's own contract -- drop
     * what you overwrite, spelled whole, never gated (03-move.md,
     * the checker's opunmove beside it): not a scope's row, not
     * this pass's to decide */
    return;
  case Nexprstmt: /* a discard's row is the statement's own --
                   * the value no one owns dies here, spelled whole,
                   * never gated (03-move.md) */
    rdexpr(f, st->v.n1.e);
    return;
  case Nreturn:
  case Nbreak:
  case Ncontinue:
    rdexpr(f, st->v.n1.e);
    rdvec(f, &st->v.n1.drops); /* the early exit's scope rows */
    return;
  case Nblock:
    rdstmts(f, st->v.blk.stmts);
    rdexpr(f, st->v.blk.tail);
    rdvec(f, &st->v.blk.drops);
    return;
  case Nif:
  case Ncif:
    rdexpr(f, st->v.ifx.cond);
    rdbody(f, st->v.ifx.then);
    rdbody(f, st->v.ifx.els);
    return;
  case Nmatch: {
    usize i;

    rdexpr(f, st->v.call.f);
    for (i = 0; i < vlen(st->v.call.args); i++)
      rdbody(f, st->v.call.args[i]->v.n2.b);
    return;
  }
  case Nfor:
  case Ncfor:
    rdexpr(f, st->v.forx.a);
    rdexpr(f, st->v.forx.b);
    rdexpr(f, st->v.forx.via);
    rdbody(f, st->v.forx.body);
    rdstmts(f, st->v.forx.unroll);
    rdvec(f, &st->v.forx.drops);
    rdvec(f, &st->v.forx.itdrops);
    return;
  default:
    rdexpr(f, st);
  }
}

/* an expression's own shapes: a value's block holds statements, an
 * if or a match in a value's place the flow of one -- everything
 * else is a leaf or a composite whose rows are not its own. A
 * closure literal is another fn's tree, not this one's */
static void
rdexpr(Mirfn *f, Ast *e)
{
  usize i;

  if (!e)
    return;
  switch (e->k) {
  case Nblock:
    rdstmts(f, e->v.blk.stmts);
    rdexpr(f, e->v.blk.tail);
    rdvec(f, &e->v.blk.drops);
    return;
  case Nif:
  case Ncif:
    rdexpr(f, e->v.ifx.cond);
    rdbody(f, e->v.ifx.then);
    rdbody(f, e->v.ifx.els);
    return;
  case Nmatch: {
    rdexpr(f, e->v.call.f);
    for (i = 0; i < vlen(e->v.call.args); i++)
      rdbody(f, e->v.call.args[i]->v.n2.b);
    return;
  }
  case Ncall:
    rdexpr(f, e->v.call.f);
    for (i = 0; i < vlen(e->v.call.args); i++)
      rdexpr(f, e->v.call.args[i]);
    return;
  case Nbuiltin:
    for (i = 0; i < vlen(e->v.blt.args); i++)
      rdexpr(f, e->v.blt.args[i]);
    return;
  case Nbin:
    rdexpr(f, e->v.bin.l);
    rdexpr(f, e->v.bin.r);
    return;
  case Nun:
  case Ntry:
    rdexpr(f, e->v.n1.e);
    return;
  case Naccess:
    rdexpr(f, e->v.fld.e);
    return;
  case Nindex:
    rdexpr(f, e->v.n2.a);
    rdexpr(f, e->v.n2.b);
    return;
  case Nrangeindex:
    rdexpr(f, e->v.ridx.e);
    rdexpr(f, e->v.ridx.lo);
    rdexpr(f, e->v.ridx.hi);
    return;
  case Nrange:
    rdexpr(f, e->v.bin.l);
    rdexpr(f, e->v.bin.r);
    return;
  case Nspread:
    rdexpr(f, e->v.un.e);
    return;
  case Ntupidx:
    rdexpr(f, e->v.tup.e);
    return;
  case Ntuple:
    for (i = 0; i < vlen(e->v.list.ts); i++)
      rdexpr(f, e->v.list.ts[i]);
    return;
  case Narraylit:
    for (i = 0; i < vlen(e->v.arrlit.es); i++)
      rdexpr(f, e->v.arrlit.es[i]);
    return;
  case Nstructlit:
    for (i = 0; i < vlen(e->v.slit.inits); i++)
      rdexpr(f, e->v.slit.inits[i]);
    return;
  case Ninit:
    rdexpr(f, e->v.init.e);
    return;
  default:
    return;
  }
}

/* one fn's rows, decided: the body walked, every vector asked, the
 * parameters' own row last -- the return the body reaches runs it */
void
mirredecide(Mirfn *f)
{
  Ast   *body;
  Ast ***declvec;

  if (!f->decl)
    return;
  if (f->decl->k == Nfn) {
    body = f->decl->v.fn.body;
    declvec = &f->decl->v.fn.drops;
  } else if (f->decl->k == Nclosure) {
    body = f->decl->v.clos.body;
    declvec = &f->decl->v.clos.drops;
  } else
    return;
  if (!body)
    return;
  rdstmt(f, body); /* the body's own shape: a block walks itself */
  rdvec(f, declvec);
}
