/* prelude.c -- the declarations the language itself writes.
 *
 * Decision made in the M2 plan: the compiler knows these symbols
 * first (option A), and the real std/ source replaces them file by
 * file once the passes that check it exist. What M2a needs is the
 * pair the sugar builds on:
 *
 *   enum Option<T> { None, Some(T) }
 *   enum Result<T, E> { Ok(T), Err(E) }
 *
 * They are declared through symdecl, in the same namespace the
 * file's items enter, so 'Option' is a taken name before the first
 * line is read -- the same answer a std source file would give.
 * Copy/Drop and the operator traits (07-operators.md) join here as
 * their passes arrive; the compiler-provided impls for the built-in
 * types are pass-3 work, not declarations.
 */

#include <string.h>

#include "ast.h"
#include "sym.h"
#include "type.h"

Sym *sym_option, *sym_result;

/* an Ngparam without a lexer behind it: the prelude's type
 * parameters carry no position, and nothing prints one */
static Ast *
mkgp(const char *name)
{
  Ast *g = arenaalloc(sizeof *g);

  memset(g, 0, sizeof *g);
  g->k = Ngparam;
  g->v.gp.name = (char *) name; /* a literal: permanent, never freed */
  return g;
}

void
prelude(void)
{
  Ast *ot = mkgp("T"); /* Option's T */
  Ast *rt = mkgp("T"); /* Result's own T */
  Ast *re = mkgp("E");
  Ast **ogps, **rgps;
  struct Variant *ov, *rv;
  Type **some, *ok, *err;

  /* Option<T> */
  ogps = arenaalloc(1 * sizeof *ogps);
  ogps[0] = ot;
  some = tyargs(1); /* Some(T)'s payload */
  some[0] = typaram(ot);
  ov = arenaalloc(2 * sizeof *ov);
  memset(ov, 0, 2 * sizeof *ov);
  ov[0].name = "None";
  ov[1].name = "Some";
  ov[1].disc = 1;
  ov[1].payload = some;
  ov[1].npayload = 1;
  sym_option = symdecl("Option", Stype, 0, ogps, 1);
  sym_option->tykind = TYenum;
  sym_option->variants = ov;
  sym_option->nvariants = 2;

  /* Result<T, E> */
  rgps = arenaalloc(2 * sizeof *rgps);
  rgps[0] = rt;
  rgps[1] = re;
  ok = typaram(rt);
  err = typaram(re);
  rv = arenaalloc(2 * sizeof *rv);
  memset(rv, 0, 2 * sizeof *rv);
  rv[0].name = "Ok";
  rv[0].payload = tyargs(1); /* filled below: [T] */
  rv[0].payload[0] = ok;
  rv[0].npayload = 1;
  rv[1].name = "Err";
  rv[1].disc = 1;
  rv[1].payload = tyargs(1);
  rv[1].payload[0] = err;
  rv[1].npayload = 1;
  sym_result = symdecl("Result", Stype, 0, rgps, 2);
  sym_result->tykind = TYenum;
  sym_result->variants = rv;
  sym_result->nvariants = 2;
}
