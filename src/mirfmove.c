/* mirfmove.c -- client D's move walk: the aliveness of every slot,
 * forward over the blocks to a fixpoint. A slot is alive, maybe
 * (moved down one path, a join could not say which), or moved (on
 * every path that reaches); the join is the tree's own bargain, and
 * a loop needs no loop machinery of its own -- the header joins the
 * round's way back, and the fixpoint is the answer.
 *
 * The states and the row map land on the fn for the re-decide pass
 * (mirredecide.c): the rows the checker spelled whole, decided
 * against what the blocks say. The use-after-move dies below stay
 * for every compile -- a read or a move of a moved slot the tree
 * let stand is a bug in one of the two walks, named where it
 * stands.
 *
 * A maybe is not argued with: a read or a move of a maybe the tree
 * accepts (its own join could not say), so this one does not
 * either. The rows of one taking are not two takings: a spread
 * hands its rows one a one under the same slot, and the instruction
 * that asked holds them all -- a move the same instruction already
 * made is the spelling of that one taking, not a second one
 * (03-move.md, 04-generics.md). */

#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "die.h"
#include "lex.h"
#include "mir.h"
#include "sym.h"
#include "vec.h"

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
 * state they meet. The hard disagreement refuses the compile: the
 * tree checked this program and let it stand, this walk says
 * otherwise -- one of the two is wrong, and the message names the
 * place */
static void
mfacts(Mirfn *f, u8 *st, Mirfact *fs, Ast *at)
{
  usize i;

  for (i = 0; i < vlen(fs); i++) {
    Mirfact *t = &fs[i];
    char *nm = f->locals[t->local] && f->locals[t->local]->name ? f->locals[t->local]->name : "?";

    if (t->k == Mfmv) {
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
 * loop joined until nothing moves. The states stay on the fn (mv)
 * for the re-decide pass, the row map with them -- a row's own
 * instruction names the block it stands in, the slot its first
 * move touches */
void
mirfmoverun(Mirfn *f)
{
  Mvst  *mv;
  u8   **in, **out;
  int   *inq;
  usize  nl = vlen(f->locals), nb = vlen(f->blocks);
  usize *work;
  usize  b, i, j, k, head;

  if (!nb || !nl) {
    if (!f->mv) { /* still a cache, an empty one: the re-decide asks
                   * rows by node and finds none either way */
      mv = calloc(1, sizeof *mv);
      if (!mv)
        die("mir: no memory for the move walk");
      f->mv = mv;
    }
    return; /* no blocks or no slots: nothing to ask */
  }
  if (f->mv) { /* the walk again: the old states freed whole */
    mv = f->mv;
    if (mv->in)
      for (b = 0; b < nb; b++)
        free(mv->in[b]);
    free(mv->in);
    free(mv);
  }
  in = calloc(nb, sizeof *in);
  out = calloc(nb, sizeof *out);
  inq = calloc(nb, sizeof *inq);
  mv = calloc(1, sizeof *mv);
  if (!in || !out || !inq || !mv)
    die("mir: no memory for the move walk");
  for (b = 0; b < nb; b++) {
    in[b] = calloc(nl, sizeof **in);
    out[b] = calloc(nl, sizeof **out);
    if (!in[b] || !out[b])
      die("mir: no memory for the move walk");
    memset(in[b], MTOP, nl); /* no path yet: the join's own unit */
  }
  memset(in[0], MALIVE, nl); /* the entry: the caller's values in
                              * the parameters, nothing bound yet */
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
    for (i = 0; i < vlen(bb->insns); i++) {
      Mirinsn *n = bb->insns[i];

      if (n->k == Minsn_drop) { /* a row's own spelling: the
                                 * re-decide's input, not this
                                 * walk's question -- the row map
                                 * takes it, below */
        Mvrow r;
        usize q;

        r.e = n->e;
        r.b = head;
        r.slot = (usize) -1;
        for (q = 0; q < vlen(n->facts); q++)
          if (n->facts[q].k == Mfmv) {
            r.slot = n->facts[q].local;
            break;
          }
        if (r.slot != (usize) -1) {
          if (!mv->rows)
            mv->rows = vnew(Mvrow, 8);
          vappend(&mv->rows, &r);
        }
        continue;
      }
      mfacts(f, out[head], n->facts, n->e);
    }
    mfacts(f, out[head], bb->facts, bb->te); /* the terminator's own:
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
  for (b = 0; b < nb; b++)
    free(out[b]);
  free(out);
  free(inq);
  mv->in = in;
  mv->nl = nl;
  f->mv = mv;
}

/* the whole program's, once the text is out: every fn, every
 * instance, every closure -- the use-after-move ask, the states
 * cached for whoever asks next */
void
mirfmoveall(void)
{
  usize i;

  for (i = 0; i < mirnfns(); i++)
    mirfmoverun(mirfnat(i));
}
