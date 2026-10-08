/* qbe.h -- the backend's door: qbe's passes behind one call. */

#ifndef QBE_H
#define QBE_H

#include <stdio.h>

/* .ssa text in, .s text out, qbe's passes between -- in the
 * process, the pipe and the subprocess the tool's shell shape
 * had gone. A malformed .ssa dies inside qbe's own words */
void qberun(FILE *inf, FILE *out);

#endif
