/* cfg.h -- the platform cull, the checker's pass zero.
 *
 * resolve.c calls it at checkproject's head, before pass 1 declares
 * a single name: every file's items rebuilt without the ones the
 * platform culls (12-projects.md).
 */

#ifndef CFG_H
#define CFG_H

#include "check.h" /* Srcfile: the files a project is */

void cfgcull(Srcfile **files, usize nfiles);

#endif
