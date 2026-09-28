/* sym.c -- the one namespace, and the declarations in it.
 *
 * A hash table on the name, open addressing like the type table:
 * names are few (one file's worth), collisions rare, and a miss is
 * the common case. syminit() must run before any symdecl. A
 * duplicate name is not this file's error to report -- symdecl
 * returns NULL and leaves the diagnostic to the caller, which knows
 * the position. The prelude (prelude.c) declares through the same
 * door the file's own items do, which is why a program cannot name a
 * type Option: the name is taken, as it would be by another file.
 */

#include <string.h>

#include "ast.h"
#include "die.h"
#include "sym.h"

/* Self, everywhere it resolves: one parameter, so every trait's
 * members, every impl's, and the prelude's Drop share the one
 * identity (built by syminit, before the prelude) */
Ast *sym_selfgp;

/* djb2 */
static usize
shash(const char *s)
{
  unsigned h = 5381u;

  while (*s)
    h = h * 33u + (unsigned char) *s++;
  return h;
}

static Sym **tbl;
static usize tblcap, tbln;

static void
grow(void)
{
  usize newcap = tblcap * 2u;
  Sym **nt = arenaalloc(newcap * sizeof *nt);
  usize i;

  memset(nt, 0, newcap * sizeof *nt);
  for (i = 0; i < tblcap; i++) {
    if (tbl[i]) {
      usize j = shash(tbl[i]->name) & (newcap - 1u);

      while (nt[j])
        j = (j + 1u) & (newcap - 1u);
      nt[j] = tbl[i];
    }
  }
  tbl = nt;
  tblcap = newcap;
}

static usize
probe(const char *name) /* the slot: occupied by this name, or empty */
{
  usize i = shash(name) & (tblcap - 1u);

  while (tbl[i] && strcmp(tbl[i]->name, name) != 0)
    i = (i + 1u) & (tblcap - 1u);
  return i;
}

void
syminit(void)
{
  tblcap = 1024;
  tbl = arenaalloc(tblcap * sizeof *tbl);
  memset(tbl, 0, tblcap * sizeof *tbl);

  sym_selfgp = arenaalloc(sizeof *sym_selfgp);
  memset(sym_selfgp, 0, sizeof *sym_selfgp);
  sym_selfgp->k = Ngparam;
  sym_selfgp->v.gp.name = "Self";
}

Sym *
symfind(const char *name)
{
  return tbl[probe(name)];
}

/* declare: NULL when the name is taken -- the caller reports, for it
 * holds the position. gparams may be a vec or a bare array, indexed
 * [0, ngparams). A function may be overloaded (04-generics.md), so
 * a second Sfn under the same name chains instead of colliding --
 * the pass that resolves a call sorts them out. */
Sym *
symdecl(const char *name, int kind, Ast *decl, Ast **gparams, usize ngparams)
{
  Sym  *s;
  usize i;

  if (tbln * 4u >= tblcap * 3u)
    grow();
  i = probe(name);
  if (tbl[i]) {
    if (tbl[i]->kind == Sfn && kind == Sfn) {
      Sym *l = tbl[i];

      while (l->next)
        l = l->next;
      s = arenaalloc(sizeof *s);
      memset(s, 0, sizeof *s);
      s->name = (char *) name;
      s->kind = kind;
      s->decl = decl;
      s->gparams = gparams;
      s->ngparams = ngparams;
      l->next = s;
      return s;
    }
    return 0;
  }
  s = arenaalloc(sizeof *s);
  memset(s, 0, sizeof *s);
  s->name = (char *) name; /* the arena copies nothing: names come
                            * from the tree or from literals, and
                            * neither is ever freed */
  s->kind = kind;
  s->decl = decl;
  s->gparams = gparams;
  s->ngparams = ngparams;
  tbl[i] = s;
  tbln++;
  return s;
}

/* a variant by name, declaration order. Shared by the checker's
 * patterns and the emitter's construction and matching. */
struct Variant *
symvarfind(Sym *s, const char *name)
{
  usize i;

  for (i = 0; i < s->nvariants; i++)
    if (strcmp(s->variants[i].name, name) == 0)
      return &s->variants[i];
  return 0;
}

/* the enum a variant's short name belongs to: the prelude's four,
 * the ones ?T's sugar rides on, are the only variant names an
 * expression may spell bare (01-types.md) */
Sym *
symvariantowner(char *name)
{
  if (sym_option && symvarfind(sym_option, name))
    return sym_option;
  if (sym_result && symvarfind(sym_result, name))
    return sym_result;
  return 0;
}
