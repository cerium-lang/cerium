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
 * The last three read a project: one file, or a directory -- every
 * .xyz under it a file of it, each in the namespace its path spells
 * (12-projects.md). -T prints one (file ...) block per file; -t and
 * -a stay single-file, a directory's tokens and AST its files' own.
 *
 * The first four formats are contracts -- changing one rewrites
 * goldens. The escapes in all are the lexer's own closed set (\n \t
 * \r \0 \' \" \\ \xNN), so what a literal spelled and what it holds
 * read the same in any dump.
 */

#define _POSIX_C_SOURCE                                                                            \
  200809L /* getopt, popen, mkstemp, snprintf, dirent, stat;                                       \
           * c89 hides them all */

#include <dirent.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#include "ast.h"
#include "check.h"
#include "emit.h"
#include "lex.h"
#include "parse.h"
#include "sym.h" /* the namespace tree the walk builds into */
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

/* one file's items: the lexer is one global, so the file binds it,
 * parses out, and the next takes over */
static Ast **
parsefile(const char *path)
{
  Ast **items = vnew(Ast *, 16);

  lexinit(path);
  while (peek() != Teof) {
    Ast *it = parseitem();

    vappend(&items, &it);
  }
  return items;
}

static int
isdir(const char *path)
{
  struct stat st;

  return stat(path, &st) == 0 && S_ISDIR(st.st_mode);
}

static int
cmpname(const void *a, const void *b) /* the walk's order: the names
                                       * themselves, whatever the
                                       * file system hands over */
{
  return strcmp(*(char *const *) a, *(char *const *) b);
}

static char *
dirjoin(const char *dir, const char *name) /* "dir/name", the
                                            * arena's: a project's
                                            * paths live as long */
{
  char *p = arenaalloc(strlen(dir) + 1 + strlen(name) + 1);

  sprintf(p, "%s/%s", dir, name);
  return p;
}

/* a directory walked: every .xyz under it a file of the project,
 * each in the namespace its path spells -- a subdirectory a
 * sub-namespace, X.xyz beside an X/ the two halves of one
 * (12-projects.md). The entries sort, so the walk is the same
 * whatever readdir hands over; the .xyz files of a directory come
 * ahead of its subdirectories, a paired file's declarations the
 * first into the namespace it shares with its directory */
static void
walkdir(const char *dir, Ns *ns, Srcfile ***filesp)
{
  DIR           *d = opendir(dir);
  struct dirent *e;
  char         **files = vnew(char *, 8); /* the .xyz's */
  char         **dirs = vnew(char *, 8);  /* the subdirectories */
  usize          i, n;

  if (!d) {
    fprintf(stderr, "xyz: cannot read %s\n", dir);
    exit(1);
  }
  while ((e = readdir(d))) {
    char *nm;
    usize len;

    if (strcmp(e->d_name, ".") == 0 || strcmp(e->d_name, "..") == 0)
      continue;
    len = strlen(e->d_name);
    if (!isdir(dirjoin(dir, e->d_name))) {
      if (len < 5 || strcmp(e->d_name + len - 4, ".xyz") != 0)
        continue; /* not source: the directory's own, goldens,
                   * whatever else lives beside the code */
      nm = arenaalloc(len + 1);
      strcpy(nm, e->d_name);
      vappend(&files, &nm);
    } else {
      nm = arenaalloc(len + 1);
      strcpy(nm, e->d_name);
      vappend(&dirs, &nm);
    }
  }
  closedir(d);
  qsort(files, vlen(files), sizeof *files, cmpname);
  qsort(dirs, vlen(dirs), sizeof *dirs, cmpname);

  n = vlen(files);
  for (i = 0; i < n; i++) {
    usize    blen = strlen(files[i]) - 4; /* past the .xyz */
    char    *base = arenaalloc(blen + 1);
    Ns      *fns = ns;
    Srcfile *sf;
    usize    j;

    memcpy(base, files[i], blen);
    base[blen] = 0;
    for (j = 0; j < vlen(dirs); j++) /* the beside pair: X.xyz and
                                      * X/ are one namespace, X's
                                      * own (12-projects.md) */
      if (strcmp(dirs[j], base) == 0) {
        fns = nschild(ns, base);
        if (!fns)
          fns = nsmk(ns, base);
        break;
      }
    sf = arenaalloc(sizeof *sf);
    memset(sf, 0, sizeof *sf);
    sf->path = dirjoin(dir, files[i]);
    sf->ns = fns;
    sf->items = parsefile(sf->path);
    vappend(filesp, &sf);
  }
  n = vlen(dirs);
  for (i = 0; i < n; i++) {
    Ns *sub = nschild(ns, dirs[i]);

    if (strcmp(dirs[i], "std") == 0) { /* the standard library's own
                                        * name, reserved against the
                                        * project's directories
                                        * (11-namespaces.md): the
                                        * tree the sysroot's walk
                                        * already made is the one a
                                        * project's std/ would land
                                        * in, and no project may
                                        * write into it */
      fprintf(stderr,
              "xyz: %s: 'std' is reserved for the standard library"
              " (11-namespaces.md)\n",
              dirjoin(dir, dirs[i]));
      exit(1);
    }
    if (!sub)
      sub = nsmk(ns, dirs[i]);
    walkdir(dirjoin(dir, dirs[i]), sub, filesp);
  }
}

/* the project: one file -- the root's own single file -- or a
 * directory, every .xyz under it (12-projects.md). The table stands
 * already: the walk builds the project's tree into it */
static Srcfile **
loadproject(const char *path, usize *nfilesp)
{
  Srcfile **files = vnew(Srcfile *, 8);

  if (!isdir(path)) {
    Srcfile *sf = arenaalloc(sizeof *sf);

    memset(sf, 0, sizeof *sf);
    sf->path = path;
    sf->ns = nsroot();
    sf->items = parsefile(path);
    vappend(&files, &sf);
  } else {
    usize n = strlen(path); /* the shell's own trailing slash -- the
                             * glob's, or the user's -- kept out of the
                             * paths the diagnostics carry */
    char *dir;

    while (n > 1 && path[n - 1] == '/')
      n--;
    dir = arenaalloc(n + 1);
    memcpy(dir, path, n);
    dir[n] = 0;
    walkdir(dir, nsroot(), &files);
  }
  *nfilesp = vlen(files);
  return files;
}

static const char *argv0; /* the driver's own path, for the sysroot's
                           * search below */

/* where the standard library lives: the environment names it, the
 * executable's own directory the usual install shape (the checkout's
 * too), the working directory the last resort (12-projects.md). The
 * std below it is source like any other -- the compiler reads it,
 * nothing is embedded */
static const char *
sysrootpath(void)
{
  const char *env = getenv("XYZ_SYSROOT");
  static char buf[512];

  if (env && *env)
    return env;
  if (argv0 && strchr(argv0, '/')) { /* beside the executable */
    char *slash = strrchr(argv0, '/');
    usize n = (usize) (slash - argv0);

    if (n + 5 < sizeof buf) { /* "/std" and the NUL */
      memcpy(buf, argv0, n);
      memcpy(buf + n, "/std", 5);
      if (isdir(buf))
        return buf;
    }
  }
  return "std"; /* the working directory's own */
}

/* the standard library's walk, the compilation's first files: its
 * directory read like a project, each file in the namespace its path
 * spells -- the std namespace made here, ahead of the project's
 * tree, the name the project's own walk refuses (11-namespaces.md)
 */
static void
stdwalk(Srcfile ***filesp)
{
  const char *root = sysrootpath();
  Ns         *ns;

  if (!isdir(root)) {
    fprintf(stderr,
            "xyz: the standard library is not found at %s"
            " -- XYZ_SYSROOT names where it lives (12-projects.md)\n",
            root);
    exit(1);
  }
  ns = nschild(nsroot(), "std");
  if (!ns)
    ns = nsmk(nsroot(), "std");
  walkdir(root, ns, filesp);
}

/* the project's way to checked files: lexed, parsed, all four
 * passes -- a rejection dies before any output, like -a. The table
 * and the hand pair stand before any walk; std's files come ahead
 * of the project's own, *nstd of them, and -T's dump starts past
 * them (12-projects.md) */
static Srcfile **
checked(const char *path, usize *np, usize *nstdp)
{
  Srcfile **files = vnew(Srcfile *, 8);
  Srcfile **user;
  usize     nu, i;

  checkinit();
  stdwalk(&files);
  *nstdp = vlen(files);
  user = loadproject(path, &nu);
  for (i = 0; i < nu; i++)
    vappend(&files, &user[i]);
  *np = vlen(files);
  checkproject(files, *np);
  return files;
}

static int
dumpcheck_project(const char *path)
{
  usize     n, nstd;
  Srcfile **files = checked(path, &n, &nstd);

  checkdump(files + nstd, n - nstd); /* the user's files only: std's
                                      * are the language's own, no
                                      * golden holds them */
  return 0;
}

static int
usage(void)
{
  fprintf(stderr, "usage: xyz -t file | xyz -a file | xyz -T file | xyz -s file"
                  " | xyz -c file -o out\n"
                  "       the last three read a directory as a project\n");
  return 1;
}

static int
emitssa_project(const char *path)
{
  usize n, nstd; /* nstd ignored: -s prints the whole unit, std's
                  * panic included, like -c's own */
  Srcfile **files = checked(path, &n, &nstd);

  emitfile(stdout, files, n);
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
  {
    usize n, nstd; /* nstd ignored: the whole unit is emitted, std's
                    * panic included */
    Srcfile **files = checked(path, &n, &nstd);

    emitfile(p, files, n);
  }
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

  argv0 = argv[0]; /* the sysroot's search reads it below */
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
  if ((mode == 't' || mode == 'a') && isdir(file)) {
    fprintf(stderr, "xyz: -t and -a read one file; a directory is a"
                    " project (-T, -s, -c)\n");
    return 1;
  }
  switch (mode) {
  case 't':
    return dumptoks(file);
  case 'a':
    return dumpast_file(file);
  case 's':
    return emitssa_project(file);
  case 'c':
    return compile(file, out);
  default:
    return dumpcheck_project(file);
  }
}
