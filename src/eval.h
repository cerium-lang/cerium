/* eval.h -- the compile-time evaluator (08-reflection.md).
 *
 * The leaf layer: an expression whose inputs are compile-time known
 * is evaluated here, with ordinary semantics. It runs where the
 * checker's passes have not reached -- a const's initializer, an
 * array's length -- so it carries its own narrow type derivation:
 * a literal takes the type expected of it, a const reference brings
 * its declared one, and the two sides of an operator must agree. */

#ifndef EVAL_H
#define EVAL_H

#include "ast.h"
#include "sym.h"

void cevalsym(Sym *s);                      /* a const's value, now or never: the
                                             * initializer must be compile-time known
                                             * (01-types.md), and so must a static's --
                                             * its storage is runtime, its first value is
                                             * not */
u64 cevallong(Ast *e, Env env, Type *want); /* an integer's value,
                                             * where a type's own parts need one: an
                                             * array's length, a variant's discriminant
                                             * (08-reflection.md) */

#endif
