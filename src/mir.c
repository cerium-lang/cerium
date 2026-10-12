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
#include "layout.h" /* attrfind: an #[extern(C)] fn is a root of its
                     * own (01-types.md) */
#include "mir.h"
#include "sym.h"
#include "vec.h"

static Mirfn **fns;  /* every fn the walk built, in the order it met
                      * them: the project's own first, the library's
                      * behind them, an instance where its call stood */
static char **roots; /* the names the graph starts from (mir.h) */
void
mirbegin(void)
{
  fns = vnew(Mirfn *, 64); /* a pass's worth: the -i print's, or
                            * the reachability walk's below */
  roots = vnew(char *, 16);
}

Mirfn *
mirfnat(usize i)
{
  return fns[i];
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
  f->locals = vnew(Mirlocal *, 8);
  vappend(&fns, &f);
  return f;
}

/* one slot of the frame, registered as the walk meets it
 * (mirescape.c's ground) */
Mirlocal *
mirlocalnew(Mirfn *f, char *name, Type *ty, int param)
{
  Mirlocal *l;

  l = (Mirlocal *) calloc(1, sizeof *l);
  if (!l)
    die("mir: no memory for a local");
  l->name = name;
  l->ty = ty;
  l->param = param;
  vappend(&f->locals, &l);
  return l;
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
  if (!f->live) /* no root reached it, no reached fn called it: the
                 * emitter leaves its text out */
    fputs(" -- unreached", o);
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

/* -- reachability, the first client ---------------------------------- */

void
mirroot(char *name)
{
  if (name)
    vappend(&roots, &name);
}

/* a fn is reached: once, and pushed on the worklist whose walk ends
 * when it stops growing */
static void
mark(Mirfn ***work, Mirfn *f)
{
  if (f->live)
    return;
  f->live = 1;
  vappend(work, &f); /* work is the worklist's own address: vappend
                      * takes where the vec lives, not the vec */
}

/* every fn whose Sym is this one: a generic's instances are many fns
 * under one Sym, and marking them all keeps an instance a call named
 * even where this walk cannot tell which (04-generics.md) */
static void
markbySym(Mirfn ***work, Sym *s)
{
  usize i;

  if (!s) { /* no Sym at all: a closure's own fn. Which one a call
             * means is not a thing this walk can say, so it is all
             * of them rather than one too few */
    for (i = 0; i < vlen(fns); i++)
      if (!fns[i]->s)
        mark(work, fns[i]);
    return;
  }
  for (i = 0; i < vlen(fns); i++)
    if (fns[i]->s == s)
      mark(work, fns[i]);
}

/* a name the library mangles under: `ceri`, the segment's length in
 * digits, then "std". The literal is the emitter's own -- projhead
 * asks nschild(nsroot(), "std") and writes the same four bytes -- and
 * it is the namespace's name rather than the directory's, so -S
 * naming another path leaves it alone. Everything else -- the
 * program's own rows, a closure's -- is not the library's, and both
 * this walk and the emitter's question treat it the same way: it
 * stands */
static int
isstdname(const char *nm)
{
  usize j;

  if (!nm || strncmp(nm, "ceri", 4))
    return 0;
  j = 4;
  while (nm[j] >= '0' && nm[j] <= '9')
    j++;
  if (strncmp(nm + j, "std", 3))
    return 0;
  return !(nm[j + 3] >= 'a' && nm[j + 3] <= 'z');
}

/* every fn the tree spells under this name: a plain fn's own name
 * reaches the walk with no Sym on the path (the checker writes one
 * back for an instance it made and not otherwise), so the name is
 * what is left to match on */
static void
markbyname(Mirfn ***work, const char *nm)
{
  usize i;

  if (!nm)
    return;
  for (i = 0; i < vlen(fns); i++)
    if (fns[i]->s && fns[i]->s->name && strcmp(fns[i]->s->name, nm) == 0)
      mark(work, fns[i]);
}

/* the name a fn was spelled with, however the spelling reached it: a
 * path's last segment, or the name a method call reads off its
 * receiver -- the loop's sugar spells into_iter and next the second
 * way (10-iteration.md), and no Sym comes with them */
static const char *
fname(Ast *e)
{
  Ast **segs;

  if (!e)
    return 0;
  if (e->k == Naccess)
    return e->v.fld.name;
  if (e->k != Npath)
    return 0;
  segs = e->v.path.segs;
  if (!vlen(segs))
    return 0;
  return segs[vlen(segs) - 1]->v.seg.name;
}

void
mirreach(void)
{
  Mirfn **work;
  usize   head, i, j, k;

  work = vnew(Mirfn *, 64);
  head = 0;
  for (i = 0; i < vlen(roots); i++) /* the roots, by the name the
                                     * linker sees */
    for (j = 0; j < vlen(fns); j++)
      if (fns[j]->name && strcmp(fns[j]->name, roots[i]) == 0)
        mark(&work, fns[j]);
  for (j = 0; j < vlen(fns); j++) /* the program's own rows, every
                                   * one of them: a fn a project
                                   * wrote may be named without being
                                   * called -- its address taken, a
                                   * handle made of it -- and a walk
                                   * that waited for a call to reach
                                   * it would never follow the calls
                                   * *it* makes, so the library's rows
                                   * those name would go with it */
    if (!isstdname(fns[j]->name))
      mark(&work, fns[j]);
  for (j = 0; j < vlen(fns); j++) /* an #[extern(C)] fn: the name C
                                   * spells is called from outside
                                   * this unit, seen or not (01) */
    if (fns[j]->decl && attrfind(fns[j]->decl->attrs, "extern"))
      mark(&work, fns[j]);
  while (head < vlen(work)) {
    Mirfn *f = work[head++];

    for (i = 0; i < vlen(f->blocks); i++) {
      Mirblock *b = f->blocks[i];

      for (j = 0; j < vlen(b->insns); j++) {
        Mirinsn *n = b->insns[j];

        if (n->k == Minsn_ref) {
          /* named, not called: a fn's address taken, or a closure's
           * literal standing as a value. The Sym when the name gave
           * one; a literal is matched by the node it became */
          if (n->s)
            markbySym(&work, n->s);
          else if (n->e && n->e->k == Nclosure) {
            for (k = 0; k < vlen(fns); k++)
              if (fns[k]->decl == n->e)
                mark(&work, fns[k]);
          } else
            markbyname(&work, fname(n->e));
          continue;
        }
        if (n->k != Minsn_call)
          continue;
        if (n->dyn) /* the row a vtable names: the roots carry it,
                     * for the handle that points at it was made
                     * somewhere this walk does not follow
                     * (06-dispatch.md) */
          continue;
        if (n->s)
          markbySym(&work, n->s);
        else if (n->e && n->e->k == Ncall)
          /* a call the checker left no Sym on -- the loop's sugar
           * spells into_iter and next itself (10-iteration.md) --
           * matched the only other way a name can: by the path the
           * callee was written as */
          markbyname(&work, fname(n->e->v.call.f));
      }
    }
  }
}

int
mirreachable(Sym *s, char *name)
{
  usize i;

  if (!vlen(fns)) /* no walk ran: nothing to say no to */
    return 1;
  /* the library's rows are the only ones this walk may take away --
   * the same say isstdname gives the walk above, so a fn the graph
   * kept is a fn the emitter writes */
  if (!isstdname(name))
    return 1;
  for (i = 0; i < vlen(fns); i++) /* the name first: an instance's
                                   * own, and one Sym stands over
                                   * many of them */
    if (fns[i]->name && name && strcmp(fns[i]->name, name) == 0)
      return fns[i]->live;
  for (i = 0; i < vlen(fns); i++) /* then the Sym: this fn under any
                                   * of the names its instances took */
    if (fns[i]->s && fns[i]->s == s)
      return fns[i]->live;
  return 1; /* a fn this walk never met: the emitter's own say, not
             * the graph's */
}
