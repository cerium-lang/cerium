/* check.h -- the type checker's entry points.
 *
 * checkinit once, then checkfile per compilation (the whole project
 * is one unit, 12-projects.md). checkdump prints what resolving
 * made of each item -- the -T contract, like -t and -a before it.
 */

#ifndef CHECK_H
#define CHECK_H

#include "ast.h"
#include "sym.h" /* Sym, Env: the entries below speak both */

void checkinit(void);
void checkfile(Ast **items);
void checkdecls(Ast **items); /* pass 1 + 2 alone -- std's embedded
                               * source walks this without the user's
                               * passes 3 and 4 */
void checkdump(Ast **items);
void checkbodyfn(Sym *s, Ast *it);           /* pass 4, one fn (body.c) */
void checkbodyimpl(Sym *s, Ast *it);         /* pass 4, one impl's member fns */
void recheckfn(Sym *s, Ast *it, Type **tys); /* one instantiation, for
                                              * the emitter (04-generics.md) */

/* resolve.c's type resolver, shared by pass 4: one type node, one
 * path in type position, against the names in scope */
Type *rty(Ast *t, Env *env);
Type *rpath(Ast *p, Env *env);
Type *fnsigof(Sym *s); /* a fn's signature, on demand (eval.c: a
                        * forward reference from a const) */

/* resolve.c's trait-head bindings, shared by pass 4: the trait's
 * own parameters, as the impl's head named them */
Env envtraitargs(Env *e, Sym *s);

/* flow.c's impl table walk: the impl of a trait for a type, the
 * question a handle's construction asks (06-dispatch.md). The
 * emitter asks it again, printing the vtable */
Sym *implfor(Sym *trait, Type *t, Type ***tysp);

/* resolve.c's pattern order, shared by flow.c's call-site picks:
 * does every type matching a also match b? The same order that
 * checks overlap at declaration orders the matches a call finds
 * (04-generics.md). And the bounds half of the joint order: a's
 * bounds naming every trait b's do. */
int specializes(Type *a, Type *b);
int boundsincl(Sym *a, Sym *b);

/* a diagnostic at a node: path:line:col: message, then exit(1) --
 * the same shape the lexer's and the parser's errors take */
void cerrat(Ast *a, const char *fmt, ...);

#endif
