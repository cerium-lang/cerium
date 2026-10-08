/* test.c -- the test word, the one word a user says (13-testing.md).
 *
 * cerium test [dir] is the whole hand: the artifact built aside --
 * a passing file, the project's tree untouched -- run, and gone.
 * The report is the runner's own on stdout, the exit code through:
 * the compiler's first word that runs a product it built, the step
 * taken here and nowhere else -- -c and -x keep the older shape,
 * the artifact left for the shell.
 *
 * No dir is the empty project (12-projects.md): the sysroot alone,
 * no file of a project's own, the library's rows the whole
 * artifact -- the door an install owns without a project to point
 * at, the runner's report reading them std:: by name.
 */

#define _POSIX_C_SOURCE 200809L /* mkstemp, the wait words */

#include <stdio.h>
#include <stdlib.h>
#include <sys/wait.h>
#include <unistd.h>

#include "test.h"

/* the artifact run, its exit code through. The wait's own two
 * answers: an exit the code itself; a signal no report ever wrote
 * -- the runner's own death, not a test's, every test a fork that
 * dies alone -- said on stderr and answered a failure */
static int
runout(const char *out)
{
  int st = system(out);

  if (st == -1) {
    fprintf(stderr, "cerium: cannot run %s\n", out);
    return 1;
  }
  if (WIFEXITED(st))
    return WEXITSTATUS(st);
  fprintf(stderr, "cerium: the runner died on signal %d, no report\n", WTERMSIG(st));
  return 1;
}

int
ceriumtest(const char *path)
{
  char base[] = "/tmp/ceriumtestXXXXXX";
  int  fd, r;

  fd = mkstemp(base); /* a passing name only: the X's end the
                       * template, nothing stays on disk, the
                       * artifact landing by the name when it links */
  if (fd < 0) {
    fprintf(stderr, "cerium: cannot make a temporary file\n");
    return 1;
  }
  close(fd);
  unlink(base);
  if (compile(path, base, 0, 1) != 0) { /* debug shape, -x's own: the
                                         * checks the artifact keeps
                                         * (13-testing.md) */
    unlink(base);
    return 1;
  }
  r = runout(base);
  unlink(base);
  return r;
}
