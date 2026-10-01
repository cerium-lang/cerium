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

/* a value the walk carries: a scalar in its own two words, an
 * aggregate as its elements, a type as a reference -- one Val each,
 * the nesting recursive. The aggregate half carries a tag beside the
 * elements: an enum's discriminant, a union's active row. A scalar's
 * own bits never sit in the tag, so no consumer has to ask which
 * kind of bits it is holding -- i is a value, tag is an aggregate's
 * own metadata (08-reflection.md) */
typedef struct Val Val;
struct Val
{
  Type  *t;    /* what the checker would say; the derivation's answer */
  u64    i;    /* an integer's or a bool's bits, two's complement */
  double f;    /* a float's value */
  u64    tag;  /* an enum's discriminant, a union's active row; zero
                * everywhere else */
  Type *tyval; /* a type value: t is Tytype, this is the type it
                * holds -- ^^^ lifted it up, $$ splices it back
                * (08-reflection.md) */
  Val *elems;  /* an aggregate's elements in their own order -- an
                * array's, a struct's fields, a tuple's rows, an
                * enum's payloads, a union's active row alone -- or
                * NULL: the scalars' mark */
  usize len;   /* a slice's length: on the value, not the type, for
                * []T names none -- the elements are borrowed, and
                * the count is the value's own (08-reflection.md) */
};

/* a union's no-active-row: the zeroed whole, every row reading zero
 * (01-types.md). A row number is always below the field count, so
 * this one is never a row's */
#define VNONROW ((u64) ~(u64) 0)

void cevalsym(Sym *s);                      /* a const's value, now or never: the
                                             * initializer must be compile-time known
                                             * (01-types.md), and so must a static's --
                                             * its storage is runtime, its first value is
                                             * not */
Type *tysplice(Ast *e, Env *env);           /* a $$ operand's value: the type it
                                             * holds, for the slot the splice names
                                             * (08-reflection.md) */
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
Val typeinfoval(Type *t, Ast *at);          /* a type's own description, as
                                             * std::meta's model holds it: the
                                             * checker's body pass builds it too,
                                             * where a runtime fn asks at a place
                                             * the answer is already known
                                             * (08-reflection.md) */
Ast *valtoexpr(Val v, Ast *at);             /* a value the walk holds, back to
                                             * the expression that spells it: the
                                             * body pass's rewrites read it
                                             * (08-reflection.md) */

#endif
