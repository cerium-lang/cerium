/* mirfmove.c -- client D, the move walk's shadow: what the tree's
 * checker says about a binding's aliveness, asked again over the
 * blocks, and any disagreement between the two is a bug in one of
 * them -- this cut refuses to compile on it, loudly, so the test
 * suite's every program is the compare (todos/todo-20261012-00.md).
 *
 * The state is a slot's, one of three: alive, maybe (moved down one
 * path, a join could not say which), moved (moved on every path that
 * reaches). The transfer reads the facts b1 spelled: a move kills
 * the slot, a write re-arms it, a read of a moved slot is a use
 * after move -- the same bargain the tree's Local.dead and
 * Local.cmv hold, asked where the flow can see it. Loops need no
 * loop machinery of their own: the header joins the round's way
 * back with everything the entry brought, and the fixpoint is the
 * answer -- a binding the round moves joins to maybe at its own
 * header, the shape the tree spells "would be moved every round".
 *
 * What this cut deliberately is not: the decision maker. The drops
 * the tree's scopes owe are still the tree's to spell (the mid
 * layer builds beside the emitter, after the checker walked); this
 * is the shadow that proves the two agree, round after round, until
 * the flip -- the cut that moves the decision itself -- has nothing
 * left to surprise it.
 *
 * A maybe is not argued with. A read or a move of a maybe the tree
 * accepts (its own join could not say), so this one does not
 * either; what it refuses is the hard disagreement -- a read or a
 * move of a moved slot the tree let stand, a drop row spelled for a
 * slot this walk says already gone. */

#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "die.h"
#include "lex.h"
#include "mir.h"
#include "sym.h"

/* a slot's state. TOP is the not-yet-reached: the join's own unit,
 * a block no path has come to yet -- its state is no state, not a
 * maybe, which says a path came and moved and another did not */
enum
{
  MALIVE = 0,
  MMAYBE = 1,
  MMOVED = 2,
  MTOP = 3
};

/* the join of two states: the same word the same, the tree's own
 * bargain -- moved and moved is moved (dead down every path), a
 * moved beside an alive is maybe (the join could not say which),
 * and a maybe met is a maybe kept */
static u8
mfjoin(u8 a, u8 b)
{
  if (a == MTOP)
    return b;
  if (b == MTOP)
    return a;
  return a == b ? a : MMAYBE;
}

/* one row of facts, in the order the tree spelled them, against the
 * state they meet. A disagreement is the shadow's own say: the tree
 * checked this program and let it stand, this walk says otherwise --
 * one of the two is wrong, and the message names the place.
 *
 * The rows of one taking are not two takings: a spread hands its
 * rows one a one under the same slot, and the instruction that asked
 * holds them all -- a move the same instruction already made is the
 * spelling of that one taking, not a second one (03-move.md,
 * 04-generics.md). A drop row's own moves ask the state the
 * instruction entered with: a row spelled for a slot already gone is
 * the disagreement, and nothing else about the row moves the state
 * -- the binding's end is not another taking of it. */
static void
mfacts(Mirfn *f, u8 *st, u8 *was, Mirfact *fs, Ast *at, int drops)
{
  usize i;

  for (i = 0; i < vlen(fs); i++) {
    Mirfact *t = &fs[i];
    char *nm = f->locals[t->local] && f->locals[t->local]->name ? f->locals[t->local]->name : "?";

    if (t->k == Mfmv) {
      if (drops == 1) { /* a plain row: the slot alive on every path
                         * that reaches -- a maybe says the tree missed
                         * the guard the row owes, a moved slot owes no
                         * row at all (03-move.md) */
        if (was[t->local] != MALIVE)
          die("mir: the move walk disagrees at @%u:%u -- a plain drop row spelled for '%s', "
              "%s, the tree let it stand",
              at ? at->line : 0, at ? at->col : 0, nm,
              was[t->local] == MMAYBE ? "moved down one path and owed a guard"
                                      : "already moved on every path");
        continue;
      }
      if (drops == 2) { /* a guarded row: the flag's own way in, the
                         * not-moved path the branch takes -- a maybe
                         * is its shape, a moved slot still owes
                         * nothing */
        if (was[t->local] == MMOVED)
          die("mir: the move walk disagrees at @%u:%u -- a drop row spelled for '%s', already "
              "moved on every path, the tree let it stand",
              at ? at->line : 0, at ? at->col : 0, nm);
        continue;
      }
      if (st[t->local] == MMOVED) {
        usize q;
        int   own = 0;

        for (q = 0; q < i && !own; q++) /* this instruction's own
                                         * earlier move: the rows of
                                         * one taking */
          if (fs[q].k == Mfmv && fs[q].local == t->local)
            own = 1;
        if (!own)
          die("mir: the move walk disagrees at @%u:%u -- '%s' moved twice on one path, the tree "
              "let it stand",
              at ? at->line : 0, at ? at->col : 0, nm);
      } else
        st[t->local] = MMOVED;
    } else if (t->k == Mfrd) {
      if (st[t->local] == MMOVED) {
        usize q;
        int   own = 0;

        for (q = 0; q < i && !own; q++) /* a read beside its own
                                         * taking's rows: the same
                                         * instruction's spelling */
          if (fs[q].k == Mfmv && fs[q].local == t->local)
            own = 1;
        if (!own)
          die("mir: the move walk disagrees at @%u:%u -- '%s' read after its move, the tree let it "
              "stand",
              at ? at->line : 0, at ? at->col : 0, nm);
      }
    } else if (t->k == Mfwr)
      st[t->local] = MALIVE; /* the re-arm: a value landed, the slot
                              * owes the world again (03) */
    /* bor / borm: the pointer's own read, the slot's value not
     * taken -- the borrow walk's question, not this one's */
  }
}

/* one fn, to the fixpoint: the entry's slots alive, every block's
 * facts applied to what its predecessors left, the ways around a
 * loop joined until nothing moves */
static void
mfrun(Mirfn *f)
{
  u8   **in, **out;
  int   *inq, *guarded;
  usize  nl = vlen(f->locals), nb = vlen(f->blocks);
  usize *work;
  usize  b, i, j, k, head;

  if (!nb || !nl)
    return; /* no blocks or no slots: nothing to ask */
  in = calloc(nb, sizeof *in);
  out = calloc(nb, sizeof *out);
  inq = calloc(nb, sizeof *inq);
  if (!in || !out || !inq)
    die("mir: no memory for the move walk");
  for (b = 0; b < nb; b++) {
    in[b] = calloc(nl, sizeof **in);
    out[b] = calloc(nl, sizeof **out);
    if (!in[b] || !out[b])
      die("mir: no memory for the move walk");
    memset(in[b], MTOP, nl); /* no path yet: the join's own unit */
  }
  memset(in[0], MALIVE, nl);             /* the entry: the caller's values in
                                          * the parameters, nothing bound yet */
  guarded = calloc(nb, sizeof *guarded); /* a block a drop's guard hands
                                          * to: the flag's own way in,
                                          * the not-moved path */
  if (!guarded)
    die("mir: no memory for the move walk");
  for (b = 0; b < nb; b++) {
    Mirblock *bb = f->blocks[b];

    if (bb->term == Mterm_branch && bb->termset && bb->te && bb->te->k == Nif &&
        bb->te->v.ifx.cond && bb->te->v.ifx.cond->mvd == 2 && vlen(bb->succ))
      guarded[bb->succ[0]] = 1;
  }
  work = vnew(usize, 16);
  head = 0;
  vappend(&work, &head);
  inq[0] = 1;
  while (vlen(work)) {
    Mirblock *bb;

    head = work[vlen(work) - 1];
    (void) vpop(work);
    inq[head] = 0;
    bb = f->blocks[head];
    memcpy(out[head], in[head], nl);
    for (i = 0; i < vlen(bb->insns); i++)
      mfacts(f, out[head], in[head], bb->insns[i]->facts, bb->insns[i]->e,
             bb->insns[i]->k == Minsn_drop ? (guarded[head] ? 2 : 1) : 0);
    mfacts(f, out[head], in[head], bb->facts, bb->te, 0); /* the terminator's own:
                                                           * a cond's read, a
                                                           * return's move */
    for (j = 0; j < vlen(bb->succ); j++) {
      usize s = bb->succ[j];

      for (k = 0; k < nl; k++) {
        u8 was = in[s][k];

        in[s][k] = mfjoin(in[s][k], out[head][k]);
        if (in[s][k] != was && !inq[s]) {
          vappend(&work, &s);
          inq[s] = 1;
        }
      }
    }
  }
  for (b = 0; b < nb; b++) { /* the guard a drop row owes, asked at its
                              * own branch: the flag's read the tree
                              * spells (mvd) names the slot, and the
                              * state the branch decides on is maybe --
                              * a plain alive or a moved would say the
                              * guard spelled for a row that owed it
                              * not (03-move.md, Guarded drops) */
    Mirblock *bb = f->blocks[b];
    Ast      *cond;

    if (!bb->termset || bb->term != Mterm_branch || !bb->te || bb->te->k != Nif)
      continue;
    cond = bb->te->v.ifx.cond;
    if (!cond || cond->mvd != 2 || cond->k != Npath || vlen(cond->v.path.segs) != 1)
      continue; /* a program's own branch, no drop's guard */
    for (k = 0; k < nl; k++)
      if (f->locals[k] && f->locals[k]->name &&
          strcmp(f->locals[k]->name, cond->v.path.segs[0]->v.seg.name) == 0) {
        if (in[b][k] != MMAYBE)
          die("mir: the move walk disagrees at @%u:%u -- a drop's guard spelled for '%s', "
              "%s on every path that reaches, a guard owed where the join could not say",
              bb->te->line, bb->te->col, f->locals[k]->name,
              in[b][k] == MALIVE ? "alive" : "moved");
        break;
      }
  }
  for (b = 0; b < nb; b++) {
    free(in[b]);
    free(out[b]);
  }
  free(in);
  free(out);
  free(inq);
  free(guarded);
}

/* the whole program's, once the mid layer is built: every fn, every
 * instance, every closure -- reached or not, for the shadow asks
 * the ones the emitter drops too */
void
mirfmoveall(void)
{
  usize i;

  for (i = 0; i < mirnfns(); i++)
    mfrun(mirfnat(i));
}
