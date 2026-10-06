/* check.h -- the type checker's entry points.
 *
 * checkinit once, then checkproject per compilation: the files one
 * project, each walking all four passes in its own context -- the
 * namespace it sits in, the uses it bound, the file its diagnostics
 * name (12-projects.md). checkdump prints what resolving made of each
 * item, one (file ...) block each -- the -T contract, like -t and -a
 * before it.
 */

#ifndef CHECK_H
#define CHECK_H

#include "ast.h"
#include "sym.h" /* Sym, Ns, Use: the entries below speak all three */

/* one file of a project: the items it parsed, the namespace its path
 * spells, its own use bindings, and pass 1's Syms parallel to the
 * items (12-projects.md). A single-file compilation is the same
 * shape: one Srcfile, the root's -- the typedef itself sym.h's own,
 * a Sym's file rides it (04-generics.md) */
struct Srcfile
{
  Ast       **items;
  Ns         *ns;
  Use       **uses; /* the checker fills it, before the bindings */
  Sym       **syms; /* pass 1's, the checker fills it */
  const char *path; /* its diagnostics' name */
};

void checkinit(void);
void checkproject(Srcfile **files, usize nfiles);            /* the files whole:
                                                              * std's own walk first
                                                              * (the sysroot's), the
                                                              * project's after, the
                                                              * prelude injected into
                                                              * every one of them
                                                              * (12-projects.md) */
Sym **declare(Ast **items, Ns *ns);                          /* pass 1: every name
                                                              * in the table, the
                                                              * Syms back, parallel
                                                              * to the items */
void resolveitems(Ast **items, Sym **syms);                  /* pass 2: what each
                                                              * declaration is --
                                                              * after the file's
                                                              * uses bind in
                                                              * checkproject */
usize nshead(Ast **segs, usize nsegs, Ns **nsp, int rooted); /* resolve.c's
                                                              * namespace-head
                                                              * strip, shared by
                                                              * pass 4: how many
                                                              * leading segments
                                                              * walk a namespace --
                                                              * the file's own
                                                              * first, the root's,
                                                              * then what a use
                                                              * brought in; ::
                                                              * reads the root
                                                              * only (11-namespaces.md) */
void checkdump(Srcfile **files, usize nfiles);
void checkbodyfn(Sym *s, Ast *it);   /* pass 4, one fn (body.c) */
void checkbodyimpl(Sym *s, Ast *it); /* pass 4, one impl's member fns */
void recheckfn(Sym *s, Ast *it, Type **tys, Val **cvals,
               Val **gcvals); /* one instantiation, for the emitter --
                               * the const parameters' baked values
                               * with the types, the const generic
                               * parameters' numbers beside them
                               * (04, 08) */

/* resolve.c's type resolver, shared by pass 4: one type node, one
 * path in type position, against the names in scope */
Type  *rty(Ast *t, Env *env);
Type  *rpath(Ast *p, Env *env);
Type **dflttail(Sym *s, Type **args, usize nargs, Env *outer, Type *self,
                Ast *at); /* the missing tail of a
                           * declaration's arguments,
                           * from its own defaults: a
                           * bound call fills a trait's
                           * generics this way, its Self
                           * the parameter under the
                           * bound (04, 07) */
Type *fnsigof(Sym *s);    /* a fn's signature, on demand (eval.c: a
                           * forward reference from a const) */

/* resolve.c's trait-head bindings, shared by pass 4: the trait's
 * own parameters, as the impl's head named them */
Env envtraitargs(Env *e, Sym *s);

/* body.c's projection opener, shared by the emitter: a projection
 * over a concrete Self, opened to the impl's answer -- an
 * instantiation's own close (04-generics.md, 05-traits.md) */
Type *projopen(Type *t, Ast *at);

/* flow.c's impl table walk: the impl of a trait for a type, the
 * question a handle's construction asks (06-dispatch.md). The
 * emitter asks it again, printing the vtable */
Sym *implfor(Sym *trait, Type *t, Type ***tysp);

/* resolve.c's pattern order, shared by flow.c's call-site picks:
 * does every type matching a also match b? The same order that
 * checks overlap at declaration orders the matches a call finds
 * (04-generics.md). And the bounds half of the joint order: a's
 * bounds naming every trait b's do. rowspec is the order's whole
 * row: a trait impl's arguments beside the type it is for, the
 * variables binding across both. */
int specializes(Type *a, Type *b);
int boundsincl(Sym *a, Sym *b);
int rowspec(Sym *a, Sym *b);

/* a diagnostic at a node: path:line:col: message, then exit(1) --
 * the same shape the lexer's and the parser's errors take */
void cerrat(Ast *a, const char *fmt, ...) __attribute__((__noreturn__));

#endif
