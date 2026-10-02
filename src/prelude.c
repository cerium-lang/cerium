/* prelude.c -- the Syms the language itself reads by pointer.
 *
 * Option, Result, Copy, Drop -- the sugar's four -- std::meta's
 * TypeInfo, and std's panic, the door every runtime check fails
 * into (01-types.md). Nothing is declared here anymore: std's own
 * source holds them (std/option.xyz, std/result.xyz, std/copy.xyz,
 * std/drop.xyz, std/meta/meta.xyz, std/panic.xyz), and
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
Sym *sym_typeinfo; /* std::meta's reflection model, set from the
                    * sysroot's own source by checkproject
                    * (08-reflection.md) */
Sym *sym_panic;    /* std's one runtime fn, the checks' failure door
                    * -- the emitter reads it by pointer
                    * (01-types.md) */

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
