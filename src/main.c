/* main.c -- cerium, stage 0: the driver.
 *
 * Five modes:
 *   cerium -t file -- the token stream, one token a line (the lexer's
 *                  golden tests, tests/lex/ok, diff against this)
 *   cerium -a file -- the AST as S-expressions (the parser's golden
 *                  tests, tests/parse/ok, diff against this)
 *   cerium -T file -- what checking made of each item: resolved types,
 *                  fields, variants, impl heads (the checker's golden
 *                  tests, tests/check/ok, diff against this)
 *   cerium -s file -- the .ssa text, qbe's input (codegen's)
 *   cerium -c file -o out -- the pipeline: emit, run qbe on it, link
 *                  with the system cc (QBE_BIN and CC override the
 *                  binaries, both by environment)
 *
 * -r rides the last two: release, the runtime checks left out
 * (01-types.md) -- debug is the default, the checks with it.
 *
 * The last three read a project: one file, or a directory -- every
 * .ce under it a file of it, each in the namespace its path spells
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
#define _XOPEN_SOURCE                                                                              \
  700 /* realpath -- the library's own guard is XSI's,                                             \
       * a POSIX call behind a POSIX-shaped door */

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

/* the project's own name: its directory's -- a single file's parent
 * too, for a file is a project of one and the directory it stands
 * in the project it grows into, the symbols steady across the
 * growth. The directory is taken however the path spells it: a
 * bare file and a `.` the shell's own, a `..` and a link what
 * they stand in, one directory one name. The name folds onto the
 * identifier's alphabet (a my-app and a my_app share a symbol's
 * word -- accepted, for a directory is not a declaration); std is
 * refused outright, the name being the library's own. What the
 * directory says, every mangled symbol says first
 * (12-projects.md, Symbols) */
static const char *
projectname(const char *path)
{
  static char buf[256];
  char       *dir, *r;
  const char *src;
  usize       n = strlen(path), e, i, j;

  while (n > 1 && path[n - 1] == '/')
    n--;              /* the shell's own trailing slash, out of the way */
  if (!isdir(path)) { /* a file: its parent's name */
    while (n > 0 && path[n - 1] != '/')
      n--;
    if (n > 0)
      n--; /* past the slash */
  }
  dir = arenaalloc(n + 1);
  memcpy(dir, path, n);
  dir[n] = 0;
  r = realpath(dir[0] ? dir : ".", NULL);
  if (r) {
    src = r;
    e = strlen(r);
  } else {
    src = dir; /* no such directory to resolve: the spelling
                * itself, the best the path said */
    e = n;
  }
  while (e > 0 && src[e - 1] == '/')
    e--; /* the root's own slash, a name it has none of */
  j = e;
  while (j > 0 && src[j - 1] != '/')
    j--; /* the name itself: src[j..e) */
  for (i = j, j = 0; i < e && j < sizeof buf - 1; i++) {
    char c = src[i];

    buf[j++] =
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_'
            ? c
            : '_';
  }
  if (!j)
    buf[j++] = '_'; /* the directory said nothing a symbol can say */
  buf[j] = 0;
  if (strcmp(buf, "std") == 0) {
    fprintf(stderr,
            "cerium: %s: 'std' is reserved for the standard library"
            " -- the project's own name is the library's (12-projects.md)\n",
            path);
    exit(1);
  }
  return buf;
}

/* a directory walked: every .ce under it a file of the project,
 * each in the namespace its path spells -- a subdirectory a
 * sub-namespace, X.ce beside an X/ the two halves of one
 * (12-projects.md). The entries sort, so the walk is the same
 * whatever readdir hands over; the .ce files of a directory come
 * ahead of its subdirectories, a paired file's declarations the
 * first into the namespace it shares with its directory */
static void
walkdir(const char *dir, Ns *ns, Srcfile ***filesp)
{
  DIR           *d = opendir(dir);
  struct dirent *e;
  char         **files = vnew(char *, 8); /* the .ce's */
  char         **dirs = vnew(char *, 8);  /* the subdirectories */
  usize          i, n;

  if (!d) {
    fprintf(stderr, "cerium: cannot read %s\n", dir);
    exit(1);
  }
  while ((e = readdir(d))) {
    char *nm;
    usize len;

    if (strcmp(e->d_name, ".") == 0 || strcmp(e->d_name, "..") == 0)
      continue;
    len = strlen(e->d_name);
    if (!isdir(dirjoin(dir, e->d_name))) {
      if (len < 4 || strcmp(e->d_name + len - 3, ".ce") != 0)
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
    usize    blen = strlen(files[i]) - 3; /* past the .ce */
    char    *base = arenaalloc(blen + 1);
    Ns      *fns = ns;
    Srcfile *sf;
    usize    j;

    memcpy(base, files[i], blen);
    base[blen] = 0;
    for (j = 0; j < vlen(dirs); j++) /* the beside pair: X.ce and
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
              "cerium: %s: 'std' is reserved for the standard library"
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
 * directory, every .ce under it (12-projects.md). The table stands
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
  const char *env = getenv("CERIUM_SYSROOT");
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
            "cerium: the standard library is not found at %s"
            " -- CERIUM_SYSROOT names where it lives (12-projects.md)\n",
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
 * them (12-projects.md). The project's name lands here too: every
 * mode that reads a project reads it, the std refusal with it */
static const char *projname; /* checked's own say, the emitters' read */

static Srcfile **
checked(const char *path, usize *np, usize *nstdp)
{
  Srcfile **files = vnew(Srcfile *, 8);
  Srcfile **user;
  usize     nu, i;

  projname = projectname(path);
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
  fprintf(stderr, "usage: cerium -t file | cerium -a file | cerium -T file | cerium -s file"
                  " | cerium -c file -o out | cerium -x file -o out\n"
                  "       the last four read a directory as a project\n"
                  "       -r rides -s and -c: release, the runtime checks out\n"
                  "       -x builds the test artifact, debug shape: every #[test] fn a"
                  " runner walks (13-testing.md)\n");
  return 1;
}

static int
emitssa_project(const char *path, int release)
{
  usize n, nstd; /* nstd ignored: -s prints the whole unit, std's
                  * panic included, like -c's own */
  Srcfile **files = checked(path, &n, &nstd);

  emitfile(stdout, files, n, release, projname, 0);
  return 0;
}

/* the pipeline: .ssa text through a pipe into qbe, the .s it writes
 * into the system cc, the executable named by -o. qbe reads stdin
 * as "-", so nothing touches the disk but the one .s and the out. */
static int
compile(const char *path, const char *out, int release, int test)
{
  const char *qbebin = getenv("QBE_BIN");
  const char *cc = getenv("CC");
  char        cmd[512], base[] = "/tmp/ceriumXXXXXX", ssa[64];
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
    fprintf(stderr, "cerium: cannot make a temporary file\n");
    return 1;
  }
  close(fd);
  unlink(base);
  snprintf(ssa, sizeof ssa, "%s.s", base);
  snprintf(cmd, sizeof cmd, "%s -o %s -", qbebin, ssa);
  p = popen(cmd, "w");
  if (!p) {
    fprintf(stderr, "cerium: cannot run %s\n", qbebin);
    unlink(ssa);
    return 1;
  }
  {
    usize n, nstd; /* nstd ignored: the whole unit is emitted, std's
                    * panic included */
    Srcfile **files = checked(path, &n, &nstd);

    emitfile(p, files, n, release, projname, test);
  }
  if (pclose(p) != 0) {
    fprintf(stderr, "cerium: %s rejected the .ssa\n", qbebin);
    unlink(ssa);
    return 1;
  }
  snprintf(cmd, sizeof cmd, "%s %s -o %s", cc, ssa, out);
  if (system(cmd) != 0) {
    fprintf(stderr, "cerium: %s failed to link\n", cc);
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
  int         release = 0;
  int         c;

  argv0 = argv[0]; /* the sysroot's search reads it below */
  while ((c = getopt(argc, argv, "a:c:o:rs:t:T:x:")) != -1) {
    switch (c) {
    case 'a':
    case 'c':
    case 's':
    case 't':
    case 'T':
    case 'x':
      if (mode) /* one mode, one file */
        return usage();
      mode = c;
      file = optarg;
      break;
    case 'o':
      out = optarg;
      break;
    case 'r':
      release = 1;
      break;
    default: /* '?': getopt already said why */
      return usage();
    }
  }
  if (!mode || optind != argc) /* a file and nothing after it */
    return usage();
  if ((mode == 'c' || mode == 'x') != (out != 0)) /* -c and -x want -o,
                                                   * nothing else does */
    return usage();
  if (release && mode != 's' && mode != 'c') /* the checks are
                                              * codegen's own: the
                                              * dumps never carried
                                              * them (01-types.md).
                                              * A test build keeps
                                              * them -- debug shape
                                              * is its own (13) */
    return usage();
  chk_rel = release; /* the mode's own word, held for every #[build]
                      * door the passes open (01-types.md) */
  if ((mode == 't' || mode == 'a') && isdir(file)) {
    fprintf(stderr, "cerium: -t and -a read one file; a directory is a"
                    " project (-T, -s, -c)\n");
    return 1;
  }
  switch (mode) {
  case 't':
    return dumptoks(file);
  case 'a':
    return dumpast_file(file);
  case 's':
    return emitssa_project(file, release);
  case 'c':
    return compile(file, out, release, 0);
  case 'x':
    return compile(file, out, release, 1);
  default:
    return dumpcheck_project(file);
  }
}
