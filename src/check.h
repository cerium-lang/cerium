/* check.h -- the type checker's entry points.
 *
 * checkinit once, then checkfile per compilation (the whole project
 * is one unit, 12-projects.md). checkdump prints what resolving
 * made of each item -- the -T contract, like -t and -a before it.
 */

#ifndef CHECK_H
#define CHECK_H

#include "ast.h"

void checkinit(void);
void checkfile(Ast **items);
void checkdump(Ast **items);

/* a diagnostic at a node: path:line:col: message, then exit(1) --
 * the same shape the lexer's and the parser's errors take */
void cerrat(Ast *a, const char *fmt, ...);

#endif
