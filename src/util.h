/* util.h -- what every file shares: the one way to die. */

#ifndef UTIL_H
#define UTIL_H

/* "xyz: ..." on stderr, then exit(1). Internal errors -- the ones
 * that mean a compiler bug -- say so; anything else is a diagnostic
 * from the pass that found it. */
void die(const char *fmt, ...);

/* a compiler bug: "xyz: internal error: ..." */
void dieinternal(const char *fmt, ...);

#endif
