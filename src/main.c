/* main.c -- xyz, stage 0: the driver.
 *
 * Two modes:
 *   xyz -t file -- the token stream, one token a line (the lexer's
 *                  golden tests, tests/ok, diff against this)
 *   xyz -a file -- the AST as S-expressions (the parser's golden
 *                  tests, tests/parse, diff against this)
 *
 * Both formats are contracts -- changing either rewrites goldens.
 * The escapes in both are the lexer's own closed set (\n \t \r \0 \'
 * \" \\ \xNN), so what a literal spelled and what it holds read the
 * same in either dump.
 */

#include <stdio.h>
#include <string.h>

#include "ast.h"
#include "lex.h"
#include "parse.h"

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
    dumpstr(t->v.str.s, t->v.str.len);
    putchar('"');
    dumpflags(t->v.str.flags);
    break;
  case Tbyte:
    printf(" '");
    dumpstr(t->v.str.s, 1);
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

static int
dumpast_file(const char *path)
{
  lexinit(path);
  printf("(file");
  for (;;) {
    Ast *it;

    while (peek() != Teof) {
      it = parseitem();
      putchar('\n');
      dumpast(it);
    }
    break;
  }
  printf(")\n");
  return 0;
}

int
main(int argc, char **argv)
{
  if (argc == 3 && strcmp(argv[1], "-t") == 0)
    return dumptoks(argv[2]);
  if (argc == 3 && strcmp(argv[1], "-a") == 0)
    return dumpast_file(argv[2]);
  fprintf(stderr, "usage: xyz -t file | xyz -a file\n");
  return 1;
}
