/* prelude.c -- the Syms the language itself reads by pointer.
 *
 * Option, Result, Copy, Drop -- the sugar's four -- std::meta's
 * TypeInfo, std's panic, the door every runtime check fails into
 * (01-types.md), and std::fmt's two, the ends an E?() main has
 * (12-projects.md). Nothing is declared here anymore: std's own
 * source holds them (std/option.ce, std/result.ce, std/copy.ce,
 * std/drop.ce, std/meta/meta.ce, std/panic.ce, std/fmt/), and
 * checkproject takes the Syms back from the tree the walks fill,
 * after pass 1 (12-projects.md). What remains is the storage and
 * the one lazy type -- the value's own derivation reads it, the
 * checker's slots ask it again and again.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "check.h"
#include "sym.h"
#include "type.h"
#include "vec.h"

Sym *sym_option, *sym_result;
Sym *sym_copy, *sym_drop;
Sym *sym_fn, *sym_fnmut, *sym_fnonce; /* ops' call family: the sugar's
                                       * own traits, the fn pointer's
                                       * built-in row (05-traits.md) */
Sym *sym_typeinfo;                    /* std::meta's reflection model, set from the
                                       * sysroot's own source by checkproject
                                       * (08-reflection.md) */
Sym *sym_panic;                       /* std's one runtime fn, the checks' failure door
                                       * -- the emitter reads it by pointer
                                       * (01-types.md) */
Sym *sym_fmt;                         /* std::fmt's Fmt, the Err half's own words: an
                                       * E?() main's error type is checked against it
                                       * where the ending is declared (12-projects.md) */
Sym *sym_iter, *sym_intoiter;         /* std::iter's two, for-in's own words
                                       * (10-iteration.md): the desugar's two
                                       * rows, found by the walk at each sugar */
Sym *sym_range;                       /* std::ops's own, the interval a ..
                                       * lands in (10-iteration.md) */
Sym *sym_exit;                        /* std's exit (entry.ce), the ending answered as
                                       * the platform takes it -- run_err hands it the
                                       * ending; the prelude asks only that it stands */
Sym *sym_entry_unit, *sym_entry_i32,
    *sym_entry_err; /* std::entry's
                     * three runs, one per ending a main has: the
                     * wrapper the compiler arranges picks the one the
                     * project's main answers (12-projects.md) */

/* std's own face, taken back from the tree the declares-all pass
 * filled (12-projects.md): the language's citizens -- Option and
 * Result, every ?T and every E?T reading them by pointer (01, 03,
 * 05) -- std::meta's TypeInfo, what every @typeinfo answers with
 * (08-reflection.md), and std's panic, the door every runtime check
 * fails into (01-types.md). The entry fns, one per ending a main
 * has, private to std: the wrapper alone calls them, this the one
 * door in (12-projects.md, 11-namespaces.md). The operator traits,
 * the sugar's own (07-operators.md), and the two the compiler calls
 * on its own -- Copy at a move, Drop at a scope's end (03) -- ride
 * the same face: the rewrite spells the operators' paths, so those
 * names never enter a scope, and the two ride no prelude either --
 * a file that impls one names it. A sysroot without any of them is
 * a broken one -- said here, whole, not at the first sugar that
 * reaches for one */
void
stdface(void)
{
  Ns *std = nsopen("std");

  sym_option = nsitem(std, "Option");
  sym_result = nsitem(std, "Result");
  sym_typeinfo = nsitem(nsopen("std::meta"), "TypeInfo");
  sym_panic = nsitem(std, "panic");
  sym_entry_unit = nsitem(std, "run_unit");
  sym_entry_i32 = nsitem(std, "run_i32");
  sym_entry_err = nsitem(std, "run_err");
  sym_exit = nsitem(std, "exit"); /* the ending an E?() main has,
                                   * run_err's own arm, a program
                                   * free to call it itself
                                   * (entry.ce, 12-projects.md) */
  sym_fmt = nsitem(nsopen("std::fmt"), "Fmt");
  sym_iter = nsitem(nsopen("std::iter"), "Iter"); /* for-in's two, the
                                                   * desugar's own rows
                                                   * (10-iteration.md) */
  sym_intoiter = nsitem(nsopen("std::iter"), "IntoIter");
  sym_range = nsitem(nsopen("std::ops"), "Range"); /* the interval a ..
                                                    * lands in, the
                                                    * checker's own sugar
                                                    * (10-iteration.md) */
  {
    static const char *const ops[] = {"Add",       "Sub",    "Mul", "Div",      "Rem",  "BitAnd",
                                      "BitOr",     "BitXor", "Shl", "Shr",      "Neg",  "ShlAssign",
                                      "ShrAssign", "Ord",    "Eq",  "Ordering", "Copy", "Drop"};
    Ns                      *ons = nsopen("std::ops");
    usize                    oi;

    sym_copy = nsitem(ons, "Copy");
    sym_drop = nsitem(ons, "Drop");
    sym_fn = nsitem(ons, "Fn"); /* the family the call sugar reads
                                 * (05-traits.md): a bound names it,
                                 * a fn pointer answers it for its
                                 * own signature -- the compiler's
                                 * own knowledge, no impl spelled */
    sym_fnmut = nsitem(ons, "FnMut");
    sym_fnonce = nsitem(ons, "FnOnce");
    for (oi = 0; oi < sizeof ops / sizeof ops[0]; oi++)
      if (!ons || !nsitem(ons, ops[oi])) {
        fprintf(stderr,
                "cerium: the standard library is incomplete: %s is missing from"
                " std::ops (07-operators.md)\n",
                ops[oi]);
        exit(1);
      }
  }
  if (!sym_option || !sym_result || !sym_copy || !sym_drop || !sym_typeinfo || !sym_panic ||
      !sym_fmt || !sym_exit || !sym_entry_unit || !sym_entry_i32 || !sym_entry_err || !sym_fn ||
      !sym_fnmut || !sym_fnonce || !sym_iter || !sym_intoiter || !sym_range) {
    fprintf(stderr, "cerium: the standard library is incomplete: Option, Result, Copy, Drop,"
                    " meta::TypeInfo, panic, fmt's Fmt, iter's Iter and IntoIter, ops' Range,"
                    " exit, entry's three runs, ops' Fn family -- one is missing from the"
                    " sysroot (12-projects.md)\n");
    exit(1);
  }
}

/* TypeInfo itself, the type every @typeinfo answers with: one
 * instance, cached */
Type *
typeinfoty(void)
{
  static Type *t;

  if (!t)
    t = tysym(sym_typeinfo, 0, 0);
  return t;
}
