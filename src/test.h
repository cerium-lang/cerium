/* test.h -- the test word: the artifact built and run, one word
 * (13-testing.md). */

#ifndef TEST_H
#define TEST_H

/* cerium test [dir]: the artifact of the project the dir names --
 * no dir the empty project, the sysroot alone, the library's rows
 * the whole artifact -- built aside and run, the report on stdout,
 * the exit code through. The compiler's first word that runs a
 * product it built. path may be null, the empty project's shape */
int ceriumtest(const char *path);

/* main.c's own pipeline -- -c's and -x's, the test word's too: the
 * .ssa through qbe, the .s through the system cc, the out the
 * product. Declared here for the test word's borrow; main.c is
 * where it lives */
int compile(const char *path, const char *out, int release, int test);

#endif
