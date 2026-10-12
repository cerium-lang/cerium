/* mir.h -- the mid layer: a fn's course, blocks and a terminator each.
 *
 * The tree is what the parser wrote and the checker walked; the ssa is
 * qbe's, one module for the whole unit (12-projects.md). Between them
 * sits this: one shape per fn, control flow spelled out as blocks that
 * hand to blocks, so a pass that asks "what does this reach" or "what
 * does this outlive" has somewhere to stand -- the tree answers the one
 * only by walking it again, and the ssa answers it too late to say
 * where.
 *
 * The name is MIR because another layer sits above it: tree, then hir
 * when a macro system brings one, then mir, then qbe's ssa. A macro
 * wants a tree that can still be expanded -- names resolved, sugar off,
 * still a tree -- and that is hir; what we need is the layer where the
 * flow is spelled out, and that is this one.
 *
 * Stage A builds the shape and takes no client: the blocks, the
 * terminators, and the calls, an rvalue left as the tree node it came
 * from. Reachability needs no more than the calls; the lifetime check
 * and the borrow migration will need the rvalues, and they arrive with
 * the clients that ask for them. One IR, not one per client
 * (todos/todo-20261012-00.md). */

#ifndef MIR_H
#define MIR_H

#include <stdio.h>

#include "sym.h" /* Sym, Member, Variant -- and Ast and Type through it */

typedef struct Mirfn    Mirfn;
typedef struct Mirblock Mirblock;
typedef struct Mirinsn  Mirinsn;

/* what one instruction is. Stage A: a call, or the tree node a value's
 * read or write stands in -- the rvalue is not ours yet, and the two
 * clients that need it (the lifetime check, the borrow migration) will
 * spell it when they arrive */
typedef enum
{
  Minsn_expr, /* the tree's own node, held whole */
  Minsn_call  /* a call: the one thing a graph's edges are made of */
} Mirinsnk;

/* how a block hands to the next. Goto is also what a fallthrough
 * leaves behind, and a block no walk reaches keeps it */
typedef enum
{
  Mterm_goto,
  Mterm_branch, /* if: the cond decided which of two */
  Mterm_switch, /* match: an arm a way out, in the arms' own order */
  Mterm_return,
  Mterm_diverge /* a walk that does not come back. No stage A walk
                 * sets it: which block is unreachable is a question
                 * about the graph and not about the tree, and
                 * reachability -- the first client -- is the one
                 * that answers it */
} Mirtermk;

struct Mirinsn
{
  Mirinsnk k;
  Ast     *e; /* the node it came from: the whole expression while the
               * rvalue is the tree's (stage A) */
  Sym *s;     /* Minsn_call: the overload the checker picked, so this
               * walk need not guess by name. NULL for a vtable's
               * row, and for a variant's construction -- Some(v)
               * reads as a call and names no fn -- both of which the
               * print spells from the tree instead */
  int dyn;    /* Minsn_call: reached through a handle, the row a
               * vtable picks at a run (06-dispatch.md) */
};

struct Mirblock
{
  Mirinsn **insns;
  Mirtermk  term;
  int       termset; /* a terminator written: a return, a break, the
                      * end of a branch. 0: the walk fell through it,
                      * and the next block is its successor */
  Ast   *te;         /* the node the terminator came from, or NULL */
  usize *succ;       /* the blocks it hands to, in order */
};

struct Mirfn
{
  char *name;      /* the symbol the linker sees: an instance's own
                    * name, a closure's the emitter's */
  Sym       *s;    /* NULL: a closure, no Sym of the tables */
  Ast       *decl; /* Nfn, or the closure's literal */
  Mirblock **blocks;
  usize      nbind; /* the lets the walk stood over: the locals' count
                     * until the locals are ours */
};

/* the program's. Built beside the emitter's walk -- the instances are
 * the emitter's to know (04-generics.md), so this one is too -- and
 * read by the clients that follow. mirbegin before, mirprint after */
extern int mir_wanted; /* -i: build it. Else the walk stays empty and
                        * the emitter's text is the only product */
void  mirbegin(void);
void  mirfnbuild(char *name, Sym *s, Ast *decl);
void  mirprint(FILE *o);
usize mirnfns(void);

/* the pieces mirbuild.c puts together. mir.c owns the tables, so the
 * build asks it for each one rather than growing its own */
Mirfn    *mirfnnew(char *name, Sym *s, Ast *decl);
Mirblock *mirblocknew(Mirfn *f);
void      mirinsnnew(Mirblock *b, Mirinsnk k, Ast *e, Sym *s, int dyn);
void      mirtermset(Mirblock *b, Mirtermk k, Ast *te);

#endif /* MIR_H */
