/* mir.c -- the mid layer's own tables, and the print that shows it.
 *
 * mirbuild.c fills these; this file holds them, hands out the pieces,
 * and spells a fn's course on a stream. Nothing here reads the tree
 * beyond an occasional line and column to place a line by, and nothing
 * here changes it. The walk is the build's; the text is this file's. */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "die.h"
#include "mir.h"
#include "sym.h"
#include "vec.h"

int mir_wanted; /* -i: build it. Else the walk stays empty and the
                 * emitter's text is the only product (mir.h) */

static Mirfn **fns; /* every fn the walk built, in the order it met
                     * them: the project's own first, the library's
                     * behind them, an instance where its call stood */

void
mirbegin(void)
{
  fns = vnew(Mirfn *, 64); /* a pass's worth: -i's own build, or a
                            * client's later */
}

usize
mirnfns(void)
{
  return vlen(fns);
}

/* a fn the walk is about to spell. The blocks come from mirbuild.c,
 * which pushes them as it goes; this hands out the shell */
Mirfn *
mirfnnew(char *name, Sym *s, Ast *decl)
{
  Mirfn *f;

  f = (Mirfn *) calloc(1, sizeof *f);
  if (!f)
    die("mir: no memory for a fn");
  f->name = name;
  f->s = s;
  f->decl = decl;
  f->blocks = vnew(Mirblock *, 8);
  vappend(&fns, &f);
  return f;
}

Mirblock *
mirblocknew(Mirfn *f)
{
  Mirblock *b;

  b = (Mirblock *) calloc(1, sizeof *b);
  if (!b)
    die("mir: no memory for a block");
  b->insns = vnew(Mirinsn *, 8);
  b->succ = vnew(usize, 2);
  b->term = Mterm_goto;
  vappend(&f->blocks, &b);
  return b;
}

void
mirinsnnew(Mirblock *b, Mirinsnk k, Ast *e, Sym *s, int dyn)
{
  Mirinsn *i;

  i = (Mirinsn *) calloc(1, sizeof *i);
  if (!i)
    die("mir: no memory for an instruction");
  i->k = k;
  i->e = e;
  i->s = s;
  i->dyn = dyn;
  vappend(&b->insns, &i);
}

/* the block's terminator, written once: a return, a break, the join a
 * branch hands to. A second ask -- the statement after a return, the
 * block a branch left -- is the shape's completeness and not a second
 * way out, so it is refused rather than overwriting the first */
void
mirtermset(Mirblock *b, Mirtermk k, Ast *te)
{
  if (b->termset)
    return;
  b->term = k;
  b->te = te;
  b->termset = 1;
}

/* -- the print ------------------------------------------------------- */

static const char *
termname(Mirtermk k)
{
  switch (k) {
  case Mterm_goto:
    return "goto";
  case Mterm_branch:
    return "branch";
  case Mterm_switch:
    return "switch";
  case Mterm_return:
    return "return";
  case Mterm_diverge:
    return "diverge";
  }
  return "?";
}

static void
pat(FILE *o, Ast *e)
{
  if (e)
    fprintf(o, " @%u:%u", e->line, e->col);
}

/* the name a call was spelled with, for the ones no Sym answers: a
 * variant's construction -- Some(v) reads as a call and names no fn --
 * and a call the checker left unattached. A vtable's row has no name
 * to give either, and says so for itself */
static const char *
callname(Mirinsn *n)
{
  Ast  *f;
  Ast **segs;

  if (n->s && n->s->name)
    return n->s->name;
  if (n->dyn)
    return "a vtable's row";
  if (!n->e || n->e->k != Ncall)
    return "?";
  f = n->e->v.call.f;
  if (!f || f->k != Npath)
    return "?";
  segs = f->v.path.segs;
  if (!vlen(segs))
    return "?";
  return segs[vlen(segs) - 1]->v.seg.name;
}

static void
printfn(FILE *o, Mirfn *f)
{
  usize i, j, nins;

  nins = 0;
  for (i = 0; i < vlen(f->blocks); i++)
    nins += vlen(f->blocks[i]->insns);
  fprintf(o, "fn %s%s%s: %lu blocks, %lu insns, %lu lets\n", f->name ? f->name : "?",
          f->s ? " " : "", f->s ? (f->s->name ? f->s->name : "?") : "",
          (unsigned long) vlen(f->blocks), (unsigned long) nins, (unsigned long) f->nbind);
  for (i = 0; i < vlen(f->blocks); i++) {
    Mirblock *b = f->blocks[i];

    fprintf(o, "  bb%lu:\n", (unsigned long) i);
    for (j = 0; j < vlen(b->insns); j++) {
      Mirinsn *n = b->insns[j];

      if (n->k == Minsn_call) {
        fprintf(o, "    call %s", callname(n));
        pat(o, n->e);
        fputc('\n', o);
      } else {
        fprintf(o, "    expr %s", n->e ? nkname(n->e->k) : "?");
        pat(o, n->e);
        fputc('\n', o);
      }
    }
    fprintf(o, "    %s", termname(b->term));
    for (j = 0; j < vlen(b->succ); j++)
      fprintf(o, " bb%lu", (unsigned long) b->succ[j]);
    if (!b->termset)
      fputs(" /* not ended: the walk fell through */", o);
    else
      pat(o, b->te);
    fputc('\n', o);
  }
}

void
mirprint(FILE *o)
{
  usize i;

  fprintf(o, "mir: %lu fns\n", (unsigned long) vlen(fns));
  for (i = 0; i < vlen(fns); i++)
    printfn(o, fns[i]);
}
