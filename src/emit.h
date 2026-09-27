/* emit.h -- the codegen entry. */

#ifndef EMIT_H
#define EMIT_H

#include <stdio.h>

#include "ast.h"

/* the whole compilation unit as .ssa text -- one function per
 * non-generic fn with a body, data segments as they grow in */
void emitfile(FILE *out, Ast **items);

#endif
