/* main.c -- xyz, stage 0: the driver.
 *
 * One mode so far: "xyz -t file" dumps the token stream, one token a
 * line. The golden tests diff against exactly this output, which makes
 * the format a contract -- changing it rewrites every .golden.
 *
 *   LINE:COL KIND [VALUE]
 *     Tint    -- the digits, decimal
 *     Tflt    -- %g
 *     Tident  -- the name
 *     Tstr    -- BYTELEN "escaped bytes" [c] [r] [ml]
 *     Tbyte   -- 'escaped byte'
 *
 * The dump escapes are the lexer's own closed set (\n \t \r \0 \' \"
 * \\ \xNN), so what a literal spelled and what it holds read the same.
 */

#include <stdio.h>
#include <string.h>

#include "lex.h"

static void
dumpu64(u64 v) /* C89 printf has no %llu; the digits are spelled out */
{
  char d[20]; /* 2^64-1 is 20 digits */
  int n = 0;

  do {
    d[n++] = (char) ('0' + (int) (v % 10));
    v /= 10;
  } while (v != 0);
  while (n > 0)
    putchar(d[--n]);
}

static void
dumpbody(const char *s, usize n)
{
  usize i;

  for (i = 0; i < n; i++) {
    unsigned char c = (unsigned char) s[i];

    switch (c) {
    case '\n':
      printf("\\n");
      continue;
    case '\t':
      printf("\\t");
      continue;
    case '\r':
      printf("\\r");
      continue;
    case 0:
      printf("\\0");
      continue;
    case '\'':
      printf("\\'");
      continue;
    case '"':
      printf("\\\"");
      continue;
    case '\\':
      printf("\\\\");
      continue;
    }
    if (c < 0x20 || c > 0x7e)
      printf("\\x%02x", c);
    else
      putchar(c);
  }
}

static void
dumpflags(unsigned f)
{
  if (f & STRF_C)
    printf(" c");
  if (f & STRF_RAW)
    printf(" r");
  if (f & STRF_ML)
    printf(" ml");
}

static void
dumptok(Token *t)
{
  printf("%u:%u %s", t->line, t->col, tokname(t->t));
  switch (t->t) {
  case Tint:
    putchar(' ');
    dumpu64(t->v.num);
    break;
  case Tflt:
    printf(" %g", t->v.flt);
    break;
  case Tident:
    printf(" %s", t->v.str.s);
    break;
  case Tstr:
    printf(" %lu \"", (unsigned long) t->v.str.len);
    dumpbody(t->v.str.s, t->v.str.len);
    putchar('"');
    dumpflags(t->v.str.flags);
    break;
  case Tbyte:
    printf(" '");
    dumpbody(t->v.str.s, 1);
    putchar('\'');
    break;
  default: /* the tokens with no value */
    break;
  }
  putchar('\n');
}

static int
dumptoks(const char *path)
{
  lexinit(path);
  do {
    next();
    dumptok(lexcur());
  } while (lexcur()->t != Teof);
  return 0;
}

int
main(int argc, char **argv)
{
  if (argc == 3 && strcmp(argv[1], "-t") == 0)
    return dumptoks(argv[2]);
  fprintf(stderr, "usage: xyz -t file\n");
  return 1;
}
