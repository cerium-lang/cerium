/* emit.h -- the codegen entry, and the state its walk shares. */

#ifndef EMIT_H
#define EMIT_H

#include <stdio.h>

#include "ast.h"
#include "check.h" /* Srcfile: the project's files, each its own
                    * namespace (12-projects.md) */
#include "sym.h"
#include "type.h"
#include "vec.h" /* usize */

/* the whole project as .ssa text -- one function per non-generic fn
 * with a body, data segments as they grow in. A file's fns read
 * from its own namespace, each walk switched to it. release, -r's
 * own, leaves the runtime checks out (01-types.md). proj is the
 * project's own name, the first segment of every mangled symbol
 * (12-projects.md, Symbols). test rides -x: the door is a generated
 * runner over every #[test] fn instead of the main wrapper
 * (13-testing.md) */
void emitfile(FILE *out, Srcfile **files, usize nfiles, int release, const char *proj, int test);

/* the emitter's own state, one per compilation: the output, the
 * temporaries, the data lines grown under the fns, the zero blocks
 * by size, the bindings in scope, and the loops a break or a
 * continue jumps out of. The body walk in emit.c and the data
 * segment in emitdata.c share it. ELoc, a binding's own row, is
 * emit.c's. */
typedef struct ELoc ELoc;
typedef struct Em   Em;
struct Em
{
  FILE  *o;
  usize  tmp;    /* one a temporary: %t.N */
  char **allocs; /* the entry's stack asks, held for the front: a
                  * slot asked for inside a loop is bytes taken every
                  * round, and a long loop walks the frame off the
                  * guard page -- so every ask goes out at @start,
                  * once, and the rounds share the slot (Clang's
                  * alloca discipline) */
  char **datas;  /* the data lines, printed after the fns */
  usize *zerosz; /* the zero blocks already grown, by size */
  char **zeros;  /* their symbols, parallel */
  usize  nzeros;
  ELoc  *locs; /* the bindings in scope */
  usize  nlocs;
  usize  ormark; /* (usize)-1 when no or-pattern is being emitted:
                  * the reuse scan is off then -- a same-named slot
                  * an earlier pattern made is a different binding,
                  * not a shared one. Under an or-pattern it is the
                  * first pre-bound slot: the alternatives bind the
                  * same names, and a later one reuses the slot the
                  * pre-binding made (09-match.md) -- else the join
                  * would read a slot only one path defined */
  usize lbl;     /* one a block label: @L.N */
  int   openend; /* the fn body's last text ends in the call the abort
                  * never returns from: qbe still wants its terminator
                  * -- emitfn's dead ret (10-iteration.md) */
  struct
  {
    char *brk;  /* where break lands */
    char *cont; /* where continue lands */
  } loops[32];  /* the fors in effect, innermost last */
  int nloops;
};

/* One generic fn, one binding of its parameters -- the types, and
 * the const parameters' baked values with them. Types are interned,
 * so the key is the Sym with the Type pointers themselves; the
 * values compare by what they hold. The same instantiation is
 * emitted once, and its name is what every call site says
 * (08-reflection.md: the const arguments are baked into each). */
typedef struct Inst Inst;
struct Inst
{
  Sym   *s;
  Type **tys;   /* s->ngparams of them, the call sites' binding */
  Val  **cvals; /* the const parameters' values, the parameters' own
                 * order -- NULL when the fn marks none (08) */
  Val **gcvals; /* the const generic parameters' numbers, the angle
                 * brackets' own order -- what a [N]T binding tells
                 * apart from another the types alone cannot (08) */
  char *name;
  int   mark; /* the drain this entry is queued for */
  Inst *from; /* the instance whose re-check found this call: the
               * chain a pack's recursion unfolds down, the depth
               * the cap reads (04-generics.md) */
};

/* the data segment and the vtables, their own file: emitdata.c. A
 * zero block per size, a const's leaves spelled in, a fn const's
 * symbol, a trait's vtables printed at the end (06-dispatch.md). */
char *zeroblk(Em *em, usize sz);
char *constsym(Em *em, Sym *s);
char *vtname(Sym *tr, Type *ty);
void  printvts(FILE *o);
int   vtssettled(void);
void  vtsreset(void);

/* the pass machine's own, emitfile's say but every file's read:
 * the data symbol counter, one a $flt.N or a $str.N across the
 * whole compilation, and the pass in effect -- scratch is 1, the
 * text is 2 (the driver's loop between the drains, 06) */
extern usize dsn;
extern int   ipass;

/* the mangle and the instance walks the data segment rides on --
 * emit.c's own, called out of emitdata.c: the mangled names a
 * symbol's data lines take, and the instance a monomorphized
 * call site holds. */
usize segput(char *buf, usize o, usize n, const char *s);
usize tymns(Ns *ns, char *buf, usize o, usize n);
usize tymang2(Type *t, char *buf, usize o, usize n);
Inst *instensure(Sym *s, Type **tys, Val **cvals, Val **gcvals);
char *fsymname(Sym *s, Ast *it);

#endif
