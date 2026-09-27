/* main.c -- xyz, stage 0: the driver.
 *
 * Five modes:
 *   xyz -t file -- the token stream, one token a line (the lexer's
 *                  golden tests, tests/lex/ok, diff against this)
 *   xyz -a file -- the AST as S-expressions (the parser's golden
 *                  tests, tests/parse/ok, diff against this)
 *   xyz -T file -- what checking made of each item: resolved types,
 *                  fields, variants, impl heads (the checker's golden
 *                  tests, tests/check/ok, diff against this)
 *   xyz -s file -- the .ssa text, qbe's input (codegen's)
 *   xyz -c file -o out -- the pipeline: emit, run qbe on it, link
 *                  with the system cc (QBE_BIN and CC override the
 *                  binaries, both by environment)
 *
 * The first four formats are contracts -- changing one rewrites
 * goldens. The escapes in all are the lexer's own closed set (\n \t
 * \r \0 \' \" \\ \xNN), so what a literal spelled and what it holds
 * read the same in any dump.
 */

#define _POSIX_C_SOURCE                                                                            \
  200809L /* getopt, popen, mkstemp, snprintf;                                                     \
           * c89 hides them all */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "ast.h"
#include "check.h"
#include "emit.h"
#include "lex.h"
#include "parse.h"
#include "vec.h"

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
  Ast **items = vnew(Ast *, 16);
  usize i;

  lexinit(path);
  while (peek() != Teof) {
    Ast *it = parseitem();

    vappend(&items, &it); /* nothing prints until the whole file
                           * parses -- a rejection must not leave
                           * "(file" stranded on stdout */
  }
  printf("(file");
  for (i = 0; i < vlen(items); i++) {
    putchar('\n');
    dumpast(items[i]);
  }
  printf(")\n");
  return 0;
}

/* one file's way to checked items: lexed, parsed, all four passes */
static Ast **
checked(const char *path)
{
  Ast **items = vnew(Ast *, 16);

  lexinit(path);
  while (peek() != Teof) {
    Ast *it = parseitem();

    vappend(&items, &it);
  }
  checkinit();
  checkfile(items); /* a rejection dies before any output, like -a */
  return items;
}

static int
dumpcheck_file(const char *path)
{
  Ast **items = checked(path);

  checkdump(items);
  return 0;
}

static int
usage(void)
{
  fprintf(stderr, "usage: xyz -t file | xyz -a file | xyz -T file | xyz -s file"
                  " | xyz -c file -o out\n");
  return 1;
}

static int
emitssa_file(const char *path)
{
  emitfile(stdout, checked(path));
  return 0;
}

/* the pipeline: .ssa text through a pipe into qbe, the .s it writes
 * into the system cc, the executable named by -o. qbe reads stdin
 * as "-", so nothing touches the disk but the one .s and the out. */
static int
compile(const char *path, const char *out)
{
  const char *qbebin = getenv("QBE_BIN");
  const char *cc = getenv("CC");
  char        cmd[512], base[] = "/tmp/xyzXXXXXX", ssa[64];
  Ast       **items;
  FILE       *p;
  int         fd;

  if (!qbebin || !*qbebin)
    qbebin = "qbe/qbe";
  if (!cc || !*cc)
    cc = "cc";
  fd = mkstemp(base); /* the X's must end the template, so the .s is
                       * added after -- cc links an assembler file by
                       * its suffix */
  if (fd < 0) {
    fprintf(stderr, "xyz: cannot make a temporary file\n");
    return 1;
  }
  close(fd);
  unlink(base);
  snprintf(ssa, sizeof ssa, "%s.s", base);
  snprintf(cmd, sizeof cmd, "%s -o %s -", qbebin, ssa);
  p = popen(cmd, "w");
  if (!p) {
    fprintf(stderr, "xyz: cannot run %s\n", qbebin);
    unlink(ssa);
    return 1;
  }
  items = checked(path);
  emitfile(p, items);
  if (pclose(p) != 0) {
    fprintf(stderr, "xyz: %s rejected the .ssa\n", qbebin);
    unlink(ssa);
    return 1;
  }
  snprintf(cmd, sizeof cmd, "%s %s -o %s", cc, ssa, out);
  if (system(cmd) != 0) {
    fprintf(stderr, "xyz: %s failed to link\n", cc);
    unlink(ssa);
    return 1;
  }
  unlink(ssa);
  return 0;
}

int
main(int argc, char **argv)
{
  const char *file = 0;
  const char *out = 0;
  int         mode = 0;
  int         c;

  while ((c = getopt(argc, argv, "a:c:o:s:t:T:")) != -1) {
    switch (c) {
    case 'a':
    case 'c':
    case 's':
    case 't':
    case 'T':
      if (mode) /* one mode, one file */
        return usage();
      mode = c;
      file = optarg;
      break;
    case 'o':
      out = optarg;
      break;
    default: /* '?': getopt already said why */
      return usage();
    }
  }
  if (!mode || optind != argc) /* a file and nothing after it */
    return usage();
  if ((mode == 'c') != (out != 0)) /* -c wants -o, nothing else does */
    return usage();
  switch (mode) {
  case 't':
    return dumptoks(file);
  case 'a':
    return dumpast_file(file);
  case 's':
    return emitssa_file(file);
  case 'c':
    return compile(file, out);
  default:
    return dumpcheck_file(file);
  }
}
