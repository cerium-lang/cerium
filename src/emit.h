/* emit.h -- the codegen entry. */

#ifndef EMIT_H
#define EMIT_H

#include <stdio.h>

#include "ast.h"
#include "check.h" /* Srcfile: the project's files, each its own
                    * namespace (12-projects.md) */

/* the whole project as .ssa text -- one function per non-generic fn
 * with a body, data segments as they grow in. A file's fns read
 * from its own namespace, each walk switched to it. release, -r's
 * own, leaves the runtime checks out (01-types.md). proj is the
 * project's own name, the first segment of every mangled symbol
 * (12-projects.md, Symbols) */
void emitfile(FILE *out, Srcfile **files, usize nfiles, int release, const char *proj);

#endif
