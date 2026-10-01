/* sym.c -- the namespace tree, and the declarations in it.
 *
 * A hash table per namespace on the name, open addressing like the
 * type table: names are few, collisions rare, and a miss is the
 * common case. syminit() must run before any symdecl. A duplicate
 * name is not this file's error to report -- symdecl returns NULL
 * and leaves the diagnostic to the caller, which knows the
 * position. The prelude (prelude.c) declares through the same
 * door the file's own items do, which is why a program cannot name a
 * type Option: the name is taken, as it would be by another file.
 */

#include <stdio.h>
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

/* one table's shape, over any namespace's own */
static void
grow(Sym ***tblp, usize *capp)
{
  usize newcap = *capp * 2u;
  Sym **nt = arenaalloc(newcap * sizeof *nt);
  usize i;

  memset(nt, 0, newcap * sizeof *nt);
  for (i = 0; i < *capp; i++) {
    if ((*tblp)[i]) {
      usize j = shash((*tblp)[i]->name) & (newcap - 1u);

      while (nt[j])
        j = (j + 1u) & (newcap - 1u);
      nt[j] = (*tblp)[i];
    }
  }
  *tblp = nt;
  *capp = newcap;
}

static usize
probe(Sym **tbl, usize cap, const char *name) /* the slot: occupied by
                                               * this name, or empty */
{
  usize i = shash(name) & (cap - 1u);

  while (tbl[i] && strcmp(tbl[i]->name, name) != 0)
    i = (i + 1u) & (cap - 1u);
  return i;
}

/* -- the use environment --------------------------------------------------
 * What a `use` brought into scope: an item, or a namespace itself.
 * The bare name's lookup reads the root's table first, then these
 * (11-namespaces.md) -- the names few and read hot through a body,
 * but a vec keeps the walk simple and the pass that fills it cold. */

static Use **uenv;

void
useclear(void)
{
  uenv = vnew(Use *, 16);
}

Use *
usebind(const char *name, Sym *sym, Ns *ns, Ast *at)
{
  usize i, n = vlen(uenv);
  Use  *u;

  for (i = 0; i < n; i++)
    if (strcmp(uenv[i]->name, name) == 0)
      return uenv[i]; /* the name held: the caller reports, for it
                       * holds the position of its own */
  u = arenaalloc(sizeof *u);
  memset(u, 0, sizeof *u);
  u->name = (char *) name;
  u->sym = sym;
  u->ns = ns;
  u->at = at;
  vappend(&uenv, &u);
  return 0;
}

Use *
usefind(const char *name)
{
  usize i, n = vlen(uenv);

  for (i = 0; i < n; i++)
    if (strcmp(uenv[i]->name, name) == 0)
      return uenv[i];
  return 0;
}

Use *
usefindns(const char *name) /* the binding a path's head names: a
                             * namespace, or nothing */
{
  Use *u = usefind(name);

  return u && u->ns ? u : 0;
}

/* -- the namespace tree --------------------------------------------------
 * The root is the project's own (11-namespaces.md); std::meta is
 * the one the embedded prelude fills. The tree is small and read
 * cold -- the sub-namespaces a plain vec, the walk a strcmp each. */

static Ns nstroot; /* the root itself, not arena'd: it is the tree */

/* the namespace whose file the checker is in: a bare name reads its
 * table first (11-namespaces.md) -- the embedded source's items
 * reach their own neighbours this way, the user's file sitting in
 * the root and reading it. checkdecls/checkfile set it; the root
 * reads as itself, the way a single-file program always has */
static Ns *curns;

void
nscur(Ns *ns)
{
  curns = ns;
}

Ns *
nsroot(void)
{
  return &nstroot;
}

Ns *
nsmk(Ns *parent, const char *name)
{
  Ns *ns = arenaalloc(sizeof *ns);

  memset(ns, 0, sizeof *ns);
  ns->name = (char *) name; /* a literal, or the tree's: never freed */
  ns->parent = parent;
  ns->cap = 16;
  ns->tbl = arenaalloc(ns->cap * sizeof *ns->tbl);
  memset(ns->tbl, 0, ns->cap * sizeof *ns->tbl);
  ns->subs = vnew(Ns *, 4);
  vappend(&parent->subs, &ns);
  return ns;
}

Ns *
nschild(Ns *ns, const char *name)
{
  usize i, n = vlen(ns->subs);

  for (i = 0; i < n; i++)
    if (strcmp(ns->subs[i]->name, name) == 0)
      return ns->subs[i];
  return 0;
}

Sym *
nsitem(Ns *ns, const char *name)
{
  return ns->tbl[probe(ns->tbl, ns->cap, name)];
}

Sym **
nstable(Ns *ns, usize *np) /* every slot filled, for a glob's walk:
                            * the table's own order, whatever it is */
{
  Sym **out = vnew(Sym *, ns->n ? ns->n : 1);
  usize i;

  for (i = 0; i < ns->cap; i++)
    if (ns->tbl[i])
      vappend(&out, &ns->tbl[i]);
  *np = vlen(out);
  return out;
}

/* the namespace's full path, std::meta spelled out -- an error's
 * naming, built in the arena */
char *
nsname(Ns *ns)
{
  char *p = ns->parent ? nsname(ns->parent) : 0;
  char *n = arenaalloc((p ? strlen(p) : 0) + strlen(ns->name) + 1);

  sprintf(n, "%s%s%s", p ? p : "", p && *p ? "::" : "", ns->name);
  return n;
}

void
syminit(void)
{
  memset(&nstroot, 0, sizeof nstroot);
  nstroot.name = "";
  nstroot.cap = 1024;
  nstroot.tbl = arenaalloc(nstroot.cap * sizeof *nstroot.tbl);
  memset(nstroot.tbl, 0, nstroot.cap * sizeof *nstroot.tbl);
  nstroot.subs = vnew(Ns *, 4);
  useclear();

  sym_selfgp = arenaalloc(sizeof *sym_selfgp);
  memset(sym_selfgp, 0, sizeof *sym_selfgp);
  sym_selfgp->k = Ngparam;
  sym_selfgp->v.gp.name = "Self";
}

Sym *
symfind(const char *name) /* the namespace being checked first, then
                           * the root's own, then what a `use` brought
                           * in (11-namespaces.md) */
{
  Sym *s;
  Use *u;

  if (curns && curns != &nstroot) {
    s = nsitem(curns, name);
    if (s)
      return s;
  }
  s = nstroot.tbl[probe(nstroot.tbl, nstroot.cap, name)];
  if (s)
    return s;
  u = usefind(name);
  return u ? u->sym : 0;
}

/* declare into a namespace: NULL when the name is taken -- the
 * caller reports, for it holds the position. gparams may be a vec
 * or a bare array, indexed [0, ngparams). A function may be
 * overloaded (04-generics.md), so a second Sfn under the same name
 * chains instead of colliding -- the pass that resolves a call
 * sorts them out. */
Sym *
nsdecl(Ns *ns, const char *name, int kind, Ast *decl, Ast **gparams, usize ngparams)
{
  Sym  *s;
  usize i;

  if (ns->n * 4u >= ns->cap * 3u)
    grow(&ns->tbl, &ns->cap);
  i = probe(ns->tbl, ns->cap, name);
  if (ns->tbl[i]) {
    if (ns->tbl[i]->kind == Sfn && kind == Sfn) {
      Sym *l = ns->tbl[i];

      while (l->next)
        l = l->next;
      s = arenaalloc(sizeof *s);
      memset(s, 0, sizeof *s);
      s->name = (char *) name;
      s->kind = kind;
      s->ownns = ns;
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
  s->ownns = ns;
  s->decl = decl;
  s->gparams = gparams;
  s->ngparams = ngparams;
  ns->tbl[i] = s;
  ns->n++;
  return s;
}

/* the root's own: every declare of the single-file era reads this
 * door, and finds its namespace the day the directories arrive */
Sym *
symdecl(const char *name, int kind, Ast *decl, Ast **gparams, usize ngparams)
{
  return nsdecl(&nstroot, name, kind, decl, gparams, ngparams);
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
