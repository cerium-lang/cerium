/* die.c -- the one way out. */

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include "die.h"

void
die(const char *fmt, ...)
{
  va_list ap;

  fputs("xyz: ", stderr);
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  fputc('\n', stderr);
  exit(1);
}
