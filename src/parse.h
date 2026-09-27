/* parse.h -- the recursive descent's public face. */

#ifndef PARSE_H
#define PARSE_H

#include "ast.h"

/* parse one item; the lexer must be at its first token */
Ast *parseitem(void);

#endif
