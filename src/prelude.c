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
