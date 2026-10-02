/* prelude.c -- the declarations the language itself writes.
 *
 * Two kinds live here. The hand-built pair below is M2's: Option,
 * Result, Copy, Drop -- declared through symdecl, in the same
 * namespace the file's items enter, so 'Option' is a taken name
 * before the first line is read. The rest is std's own source,
 * embedded: every file the registry below names parses like any
 * other source (its text arrives from tools/embed.sh as
 * src/prelude_text.h) before the user's file does, and its items
 * resolve under the clean symbol table. What M5h loads this way is
 * std::meta's reflection model (08-reflection.md): the names live in
 * std::meta, reached by path or by use, the bare name retired with
 * the shim it rode (11-namespaces.md). 12a adds std's runtime half
 * alongside: std::panic, a plain fn the checks' fabricated calls
 * name (01-types.md) -- embedded the same way, walked the same.
 */

#include <string.h>

#include "ast.h"
#include "check.h"
#include "lex.h"
#include "parse.h"
#include "prelude_text.h"
#include "sym.h"
#include "type.h"
#include "vec.h"

Sym *sym_option, *sym_result;
Sym *sym_copy, *sym_drop;
Sym *sym_typeinfo; /* std::meta's reflection model, from the embedded
                    * source (08-reflection.md) */

/* TypeInfo itself, the type every @typeinfo answers with: one
 * instance, cached -- the value's own derivation reads it, the
 * checker's slots ask it again and again */
Type *
typeinfoty(void)
{
  static Type *t;

  if (!t)
    t = tysym(sym_typeinfo, 0, 0);
  return t;
}

/* std's every file, the registry: the table prelude_text.h spelled
 * (embed.sh's own naming), the namespace its items live in, its
 * path the diagnostics carry. The order is the walk's own -- meta
 * first, the tree's std::meta made before panic's std reuses the
 * std it holds. */
static struct
{
  const char        *name; /* the table prelude_text.h spelled */
  const char *const *text;
  const char        *ns;    /* where the file's items live (12-projects.md) */
  const char        *path;  /* the diagnostics' own spelling of it */
  Ast              **items; /* preludeparse's own, held for preludefile */
  Srcfile           *sf;    /* preludefile's: the later passes walk it */
} stdsrcs[] = {
    {"meta", prelude_src_meta, "std::meta", "std/meta.xyz", 0, 0},
    {"panic", prelude_src_panic, "std", "std/panic.xyz", 0, 0},
};

static Srcfile **stdtab; /* preludefile's own, the registry's files in
                          * its walk order */

/* the Srcfile table the checker's pass 4 and the emitter walk ahead
 * of the project's own (12-projects.md) */
Srcfile **
stdfiles(usize *np)
{
  *np = stdtab ? vlen(stdtab) : 0;
  return stdtab;
}

void
preludeparse(void) /* first: the lexer is one global, so the embedded
                    * sources read out before the user's file opens */
{
  usize f;

  for (f = 0; f < sizeof stdsrcs / sizeof stdsrcs[0]; f++) {
    Ast  *it;
    usize len = 0, i, p = 0;
    char *text;

    /* join the table's lines into one NUL-terminated text: each line
     * is a literal within C90's 509-char minimum, the whole is not
     * (-Woverlength-strings), so embed.sh writes the table and the
     * joining happens here, in the arena */
    for (i = 0; stdsrcs[f].text[i]; i++)
      len += strlen(stdsrcs[f].text[i]);
    text = arenaalloc(len + 1);
    for (i = 0; stdsrcs[f].text[i]; i++) {
      usize n = strlen(stdsrcs[f].text[i]);

      memcpy(text + p, stdsrcs[f].text[i], n + 1); /* with the NUL */
      p += n;
    }
    stdsrcs[f].items = vnew(Ast *, 16);
    lexsrc(text);
    while (peek() != Teof) {
      it = parseitem();
      vappend(&stdsrcs[f].items, &it);
    }
  }
}

void
preludefile(void) /* from checkinit, after syminit and the hand pair:
                   * declare and resolve, and the impls among the
                   * items leave with their members resolved too --
                   * held for checkproject's pass-3 table, std's
                   * is_same the first to ride this (05-traits.md). The
                   * items land in the namespace the registry names,
                   * each file in its own context the way every file's
                   * resolve below runs (11-namespaces.md), and the
                   * Srcfile the later passes walk is made here: std
                   * is the project's first files (12-projects.md) */
{
  usize f;

  stdtab = vnew(Srcfile *, sizeof stdsrcs / sizeof stdsrcs[0]);
  for (f = 0; f < sizeof stdsrcs / sizeof stdsrcs[0]; f++) {
    Ns *ns = nsopen(stdsrcs[f].ns); /* the tree's own branch,
                                     * each segment made or found */
    Use    **uses = usenew();
    Sym    **syms;
    Srcfile *sf = arenaalloc(sizeof *sf);

    nscur(ns);
    usecur(uses); /* its own, empty: the embedded sources use
                   * nothing, and nothing leaks either way */
    syms = declare(stdsrcs[f].items, ns);
    resolveitems(stdsrcs[f].items, syms);
    collectstdimpls(stdsrcs[f].items, syms);
    memset(sf, 0, sizeof *sf);
    sf->items = stdsrcs[f].items;
    sf->ns = ns;
    sf->uses = uses;
    sf->syms = syms;
    sf->path = stdsrcs[f].path;
    stdsrcs[f].sf = sf;
    vappend(&stdtab, &sf);
  }
  sym_typeinfo = nsitem(nsopen("std::meta"), "TypeInfo"); /* @typeinfo's
                                                           * answers are its
                                                           * variants; the name lives
                                                           * in std::meta now, no
                                                           * shim below the bare one
                                                           * (11-namespaces.md) */
}

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

/* a marker trait: no members, nothing to resolve (03-move.md) */
static Sym *
mkmarker(const char *name)
{
  return symdecl(name, Strait, 0, 0, 0);
}

void
prelude(void)
{
  Ast     *ot = mkgp("T"); /* Option's T */
  Ast     *rt = mkgp("T"); /* Result's own T */
  Ast     *re = mkgp("E");
  Ast    **ogps, **rgps;
  Variant *ov, *rv;
  Type   **some, *ok, *err;

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

  /* the exclusion pair (04-generics.md): disjointness proofs and
   * Copy field checks read these; what they mean is 03-move.md's.
   * Copy is the empty marker; Drop declares the one fn (05) */
  sym_copy = mkmarker("Copy");
  sym_drop = mkmarker("Drop");
  {
    Member *dm = arenaalloc(sizeof *dm);
    Type  **ps = tyargs(1);

    memset(dm, 0, sizeof *dm);
    dm->name = "drop";
    dm->kind = Mfn;
    ps[0] = typaram(sym_selfgp); /* mut self: Self */
    dm->ty = tyfn(ps, 1, tyunit());
    sym_drop->members = dm;
    sym_drop->nmembers = 1;
  }
}
