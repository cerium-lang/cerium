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
#include "type.h"

/* a value the walk carries: a scalar in its two words, an aggregate
 * as its elements -- one Val each, the nesting recursive. The
 * aggregate half is a const's memoized value beside its Sym's
 * scalar one, and emit folds it into the data segment a body's read
 * copies from (08-reflection.md) */
typedef struct Val Val;
struct Val
{
  Type  *t;     /* what the checker would say; the derivation's answer */
  u64    i;     /* an integer's or a bool's bits, two's complement */
  double f;     /* a float's value */
  Val   *elems; /* an aggregate's elements, or NULL: the scalars' mark */
};

void cevalsym(Sym *s);                      /* a const's value, now or never: the
                                             * initializer must be compile-time known
                                             * (01-types.md), and so must a static's --
                                             * its storage is runtime, its first value is
                                             * not */
u64 cevallong(Ast *e, Env env, Type *want); /* an integer's value,
                                             * where a type's own parts need one: an
                                             * array's length, a variant's discriminant
                                             * (08-reflection.md) */
Ast **cforunroll(Ast *st);                  /* a const for's statements: the
                                             * iteration ran, each round's value bound
                                             * as a let that spells it, the body shared
                                             * -- what the checker walks and the
                                             * emitter emits where the loop stood, no
                                             * loop surviving (10-iteration.md) */

#endif
