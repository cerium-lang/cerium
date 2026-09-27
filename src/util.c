/* util.c -- the shared diagnostics. */

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include "util.h"

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

void
dieinternal(const char *fmt, ...)
{
  va_list ap;

  fputs("xyz: internal error: ", stderr);
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  fputc('\n', stderr);
  abort();
}
