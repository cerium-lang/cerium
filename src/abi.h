/* abi.h -- the aggregate calling convention. Cerium's own convention
 * is the platform's C convention (01-types.md); where a value
 * crosses a call, an aggregate wears a :type and qbe lowers the
 * rest -- register eightbytes, stack order, an sret. */

#ifndef ABI_H
#define ABI_H

#include <stdio.h>

#include "ast.h"

int   isagg(Type *t);            /* lives in memory: its value is an address */
int   isabb(Type *t);            /* the convention carries it: an aggregate past the niches */
int   qbety(Type *ret, Ast *at); /* qbe's letter for a scalar */
char *sigty(Type *t, Ast *at);   /* what a value wears where it crosses a call */
char *typereg(Type *t);          /* an aggregate's ":t.N", making it and all it holds */
void  abidecls(FILE *out);       /* the registry's declarations, the order qbe reads */

#endif
