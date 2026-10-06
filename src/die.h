/* die.h -- the one way out. */

#ifndef DIE_H
#define DIE_H

/* "cerium: ..." on stderr, then exit(1). The attribute is for the
 * compilers that read it: the exit is the whole body, and a caller
 * that treats the return as reachable is a warning away from a lie
 * -- Apple clang's possibly-uninitialized the first to say it. */
void die(const char *fmt, ...) __attribute__((__noreturn__));

#endif
