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
void checkdump(Ast **items);
void checkbodyfn(Sym *s, Ast *it);   /* pass 4, one fn (body.c) */
void checkbodyimpl(Sym *s, Ast *it); /* pass 4, one impl's member fns */

/* resolve.c's type resolver, shared by pass 4: one type node, one
 * path in type position, against the names in scope */
Type *rty(Ast *t, Env *env);
Type *rpath(Ast *p, Env *env);

/* a diagnostic at a node: path:line:col: message, then exit(1) --
 * the same shape the lexer's and the parser's errors take */
void cerrat(Ast *a, const char *fmt, ...);

#endif
