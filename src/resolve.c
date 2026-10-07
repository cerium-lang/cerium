/* resolve.c -- pass 1 declares every name, pass 2 resolves every type.
 *
 * A project is files, each its own namespace (11-namespaces.md,
 * 12-projects.md): pass 1 walks every file's items and declares each
 * name into its file's namespace -- a function may be overloaded
 * (04-generics.md), so same-name fn Syms chain instead of colliding
 * -- and pass 2 resolves every type a declaration carries: fields,
 * params, returns, alias targets, enum payloads, impl heads. The
 * split is what lets a field name a type declared further down the
 * file, or in another one.
 *
 * Alias resolution is demand-driven: reaching an alias that has not
 * resolved yet resolves it first, which is where the cycle check
 * lives (01-types.md -- "type A = B; type B = A" never terminates).
 * Instantiating an alias re-walks its target with the arguments
 * bound -- an alias is a name, not a shape pattern, so nothing is
 * specialized and nothing recurses.
 *
 * What pass 2 deliberately leaves alone: function bodies and the
 * initializers of consts and statics (the next milestone), trait and
 * impl member signatures (they need Self and the impl table), and
 * use items (namespaces are their own feature).
 */

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "cfg.h" /* cfgcull: pass zero, the platform's own cull */
#include "check.h"
#include "die.h"
#include "eval.h"
#include "layout.h" /* attrfind: #[noreturn] rides declare (10) */
#include "lex.h"
#include "sym.h"
#include "type.h"

/* the checker's file-local types -- typedef'd in one place so use
 * sites drop the struct, the pattern ast.h and type.h set. Bind and
 * Env moved to sym.h: pass 4 (body.c) builds the same environments
 * for fn bodies. */
typedef struct TSub    TSub;
typedef struct SpecSub SpecSub;

/* -- diagnostics ------------------------------------------------------- */

void
cerrat(Ast *a, const char *fmt, ...)
{
  va_list ap;

  fprintf(stderr, "%s:%u:%u: ", lexpath(), a->line, a->col);
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  fputc('\n', stderr);
  exit(1);
}

/* -- names in scope while a type resolves ------------------------------ */
/* The types live in sym.h now; these are their constructors, shared
 * with pass 4. */

Type *
envfind(Env *env, char *name)
{
  usize i;

  for (i = env->n; i > 0; i--) /* innermost binding first */
    if (strcmp(env->b[i - 1].name, name) == 0)
      return env->b[i - 1].t;
  return 0;
}

Bind *
envbind(Env *env, char *name) /* the binding whole, its value with it:
                               * a const generic parameter's carries
                               * the number the re-check answers with
                               * (08-reflection.md) */
{
  usize i;

  for (i = env->n; i > 0; i--) /* innermost binding first */
    if (strcmp(env->b[i - 1].name, name) == 0)
      return &env->b[i - 1];
  return 0;
}

Env
envnone(void)
{
  Env env;

  env.b = 0;
  env.n = 0;
  env.strait = 0;
  env.impl = 0;
  return env;
}

/* one more binding, on a fresh array -- envs are small and rare */
Env
envpush(Env *e, char *name, Type *t)
{
  Env r;

  r.strait = e->strait;
  r.impl = e->impl;
  r.n = e->n + 1;
  r.b = arenaalloc(r.n * sizeof *r.b);
  if (e->n)
    memcpy(r.b, e->b, e->n * sizeof *r.b);
  r.b[e->n].name = name;
  r.b[e->n].t = t;
  r.b[e->n].cv = 0;
  return r;
}

/* Self's type: sym_selfgp is built by syminit, shared with the
 * prelude's Drop (sym.c) */
static Type *
selfty(void)
{
  return typaram(sym_selfgp);
}

/* the outer bindings, then these parameters' own on top of them --
 * a member fn's own generics shadow the impl's. outer may be NULL:
 * a top-level declaration has nothing outside it */
Env
envgparams(Env *outer, Ast **gps, usize n)
{
  Env   o, r;
  usize i;

  if (!outer) {
    o = envnone();
    outer = &o;
  }
  r.strait = outer->strait;
  r.impl = outer->impl;
  r.n = outer->n + n;
  r.b = r.n ? arenaalloc(r.n * sizeof *r.b) : 0;
  if (outer->n)
    memcpy(r.b, outer->b, outer->n * sizeof *r.b);
  for (i = 0; i < n; i++) {
    r.b[outer->n + i].name = gps[i]->v.gp.name;
    r.b[outer->n + i].t = typaram(gps[i]);
    r.b[outer->n + i].cv = 0;
  }
  return r;
}

/* -- type resolution --------------------------------------------------- */

Type *rty(Ast *t, Env *env); /* shared with pass 4 (check.h) */
Type *rpath(Ast *p, Env *env);

/* the scalar type names are keywords in type position: they resolve
 * before the environment and the symbol table are consulted, and no
 * declaration can take one (01-types.md). NULL when name is not a
 * scalar. */
static Type *
prim(const char *name)
{
  static const struct
  {
    const char *n;
    int         num;
  } ps[] = {
      {"i8", IN_I8},       {"i16", IN_I16},     {"i32", IN_I32}, {"i64", IN_I64}, {"i128", IN_I128},
      {"u8", IN_U8},       {"u16", IN_U16},     {"u32", IN_U32}, {"u64", IN_U64}, {"u128", IN_U128},
      {"isize", IN_ISIZE}, {"usize", IN_USIZE}, {"f32", IN_F32}, {"f64", IN_F64},
  };
  usize i;

  if (strcmp(name, "bool") == 0)
    return tybool();
  if (strcmp(name, "voidptr") == 0)
    return tyvoidptr();
  for (i = 0; i < sizeof ps / sizeof ps[0]; i++)
    if (strcmp(ps[i].n, name) == 0)
      return tyint(ps[i].num);
  return 0;
}

/* resolve an alias's target once, abstractly -- the cycle check. A
 * caller that instantiates re-walks the target with its arguments
 * bound, so the stored form is for the check and the dump. */
static Type *
aliastarget(Sym *s)
{
  Env env;

  if (s->aliasty)
    return s->aliasty;
  if (s->resolving)
    cerrat(s->decl, "type alias '%s' resolves through itself", s->name);
  s->resolving = 1;
  env = envgparams(0, s->gparams, s->ngparams);
  s->aliasty = rty(s->decl->v.td.t, &env);
  s->resolving = 0;
  return s->aliasty;
}

/* instantiate an alias with the given arguments. Defaults fill the
 * tail, and a default may name the parameters before it. */
static Type *
aliasinst(Sym *s, Type **args, usize nargs, Ast *at)
{
  Env    env;
  usize  i, n = s->ngparams;
  Type **all;
  Ast   *target;

  if (nargs > n)
    cerrat(at, "'%s' takes %lu type argument%s, not %lu", s->name, (unsigned long) n,
           n == 1 ? "" : "s", (unsigned long) nargs);
  all = nargs == n ? args : tyargs(n);
  env.b = arenaalloc(n * sizeof *env.b);
  env.n = 0;
  for (i = 0; i < n; i++) {
    if (i < nargs) {
      all[i] = args[i];
    } else {
      Ast *d = s->gparams[i]->v.gp.dflt;

      if (!d)
        cerrat(at, "missing type argument '%s'", s->gparams[i]->v.gp.name);
      all[i] = rty(d, &env); /* sees only the parameters before it */
    }
    env.b[i].name = s->gparams[i]->v.gp.name;
    env.b[i].t = all[i];
    env.n = i + 1;
  }
  aliastarget(s); /* the cycle check, and the abstract form */
  target = s->decl->v.td.t;
  return rty(target, &env);
}

/* the missing tail of a declaration's arguments, from its own
 * defaults. A default belongs to the declaration (04-generics.md):
 * it names the parameters before it -- and a trait's Self, which is
 * the impl's own type where an impl head is resolving one, the
 * trait's parameter itself elsewhere. outer carries an impl head's
 * own generics; a struct's defaults have nothing outside them. A
 * bound call shares this: the path spelled no arguments, so Rhs is
 * the Self of that call -- the parameter under the bound (07). */
Type **
dflttail(Sym *s, Type **args, usize nargs, Env *outer, Type *self, Ast *at)
{
  Type **full = tyargs(s->ngparams);
  Env    env = outer ? *outer : envnone();
  usize  base, i;

  if (s->kind == Strait)
    env = envpush(&env, "Self", self ? self : selfty());
  base = env.n;
  { /* the parameters' own slots, one at a time: the count reaches
     * a slot only once its binding is in */
    Bind *b = arenaalloc((base + s->ngparams) * sizeof *b);

    if (env.n)
      memcpy(b, env.b, base * sizeof *b);
    env.b = b;
    env.n = base;
  }
  for (i = 0; i < s->ngparams; i++) {
    if (i < nargs) {
      full[i] = args[i];
    } else {
      Ast *d = s->gparams[i]->v.gp.dflt;

      if (!d)
        cerrat(at, "missing type argument '%s'", s->gparams[i]->v.gp.name);
      full[i] = rty(d, &env); /* sees only the parameters before it */
    }
    env.b[base + i].name = s->gparams[i]->v.gp.name;
    env.b[base + i].t = full[i];
    env.n = base + i + 1;
  }
  return full;
}

/* the generic arguments of a path segment, resolved; $$ and ^^ wait
 * for the compile-time evaluator. A bound's pins stand behind the
 * arguments (04-generics.md): pins allowed walks the positional
 * alone, and any other position rejects one -- a type's arguments
 * are its own parameters, and an associated type is a member's
 * answer, not an argument's. */
static Type **
rargs(Ast *seg, Env *env, usize *np, int pins)
{
  Ast  **as = seg->v.seg.args;
  usize  na = vlen(as);
  Type **ts;
  usize  i, j, n;

  n = 0;
  for (i = 0; i < na; i++) {
    if (as[i]->k == Nassoc) {
      if (!pins)
        cerrat(as[i], "an associated type is pinned only in a bound (04-generics.md)");
      continue;
    }
    n++;
  }
  if (!n) {
    *np = 0;
    return 0;
  }
  ts = tyargs(n);
  for (i = 0, j = 0; i < na; i++)
    if (as[i]->k != Nassoc)
      ts[j++] = rty(as[i], env); /* a $$ among them: rty's own case
                                  * splices it (08-reflection.md) */
  *np = n;
  return ts;
}

/* the pack's binding, the whole tuple: the elements a bound or an
 * impl head spells one for one gather into the pack's own slot --
 * zero elements the empty tuple -- the count back to the
 * declaration's own (04-generics.md). No pack, the arguments one a
 * slot, as spelled */
static Type **
packargs(Sym *s, Type **args, usize n)
{
  Type **full;
  usize  i, floor;

  if (!s->ngparams || !s->gparams[s->ngparams - 1]->v.gp.pack)
    return args;
  floor = s->ngparams - 1;
  full = tyargs(s->ngparams);
  for (i = 0; i < floor; i++)
    full[i] = args[i];
  full[floor] = tytuple(args ? args + floor : 0, n - floor);
  return full;
}

/* a member item's name -- fn, typedef, and const are the three */
static char *
itemname(Ast *it)
{
  switch (it->k) {
  case Nfn:
    return it->v.fn.name;
  case Ntypedef:
    return it->v.td.name;
  case Nconst:
  case Nstatic:
    return it->v.cst.name;
  default:
    die("internal: itemname: unhandled kind %s", nkname(it->k));
  }
  return 0; /* unreachable */
}

/* one type into a rotating static buffer, for diagnostics */
static char *
tysprint1(Type *t)
{
  static char     bufs[4][256];
  static unsigned which;
  char           *b = bufs[which++ & 3u];

  return tysprint(b, sizeof bufs[0], t);
}

/* a member by name, or NULL (traits and impls) */
static Member *
memberfind(Sym *s, const char *name)
{
  usize i;

  for (i = 0; i < s->nmembers; i++)
    if (strcmp(s->members[i].name, name) == 0)
      return &s->members[i];
  return 0;
}

/* the namespace head of a path: how many leading segments walk
 * namespaces, and the one they land in. The first segment names a
 * sub-namespace the way a bare name is looked up -- the file's own
 * first, the root's, then one a `use` brought in: meta::TypeInfo
 * reads the std::meta a `use std::meta` holds, and repr::Foo inside
 * net reads net's own repr (11-namespaces.md). :: is absolute: the
 * root's tree only, nothing nearer. 0 when the first segment names
 * no namespace -- the path is not a namespaced one, and the caller's
 * own reading stands. The whole path a namespace walk leaves nothing
 * behind: the caller decides what that must mean. */
usize
nshead(Ast **segs, usize nsegs, Ns **nsp, int rooted)
{
  Ns   *ns = nsroot();
  usize k = 0;

  while (k < nsegs) {
    Ns *sub =
        (k == 0 && !rooted) ? nssubfind(segs[k]->v.seg.name) : nschild(ns, segs[k]->v.seg.name);

    if (!sub)
      break;
    ns = sub;
    k++;
  }
  *nsp = ns;
  return k;
}

/* a path in type position */
static void resolvetrait(Sym *s); /* the family's members, read early
                                   * where a projection needs them
                                   * (04-generics.md) */
Type *
rpath(Ast *p, Env *env)
{
  Ast  **segs = p->v.path.segs;
  usize  nsegs = vlen(segs);
  Ast   *seg;
  Sym   *s;
  char  *name;
  Type **args;
  usize  nargs;

  s = 0;
  { /* the namespace head: std::meta::... walking the namespaces to
     * the type's own segment, the one segment left
     * (11-namespaces.md) */
    Ns   *ns;
    usize k = nshead(segs, nsegs, &ns, p->v.path.root);

    if (k) {
      if (k == nsegs)
        cerrat(p, "a namespace names no type; the path ends inside it (11-namespaces.md)");
      seg = segs[k];
      name = seg->v.seg.name;
      s = nsitem(ns, name); /* the type lands where the walk did --
                             * the bare name's fallthrough below is
                             * not this path's */
      if (s) {              /* a qualified type crosses namespaces: a private
                             * item stays its own namespace's, the value path's
                             * own rule (11-namespaces.md) -- a re-export's
                             * target is pub by the use's own check, and need
                             * not the question again */
        if (!s->pub)
          cerrat(p, "'%s' is private to %s (11-namespaces.md)", name, nsname(ns));
      } else {
        s = nsreexpfind(ns, name); /* a pub use's binding: the target
                                    * is the type (11-namespaces.md) */
        if (!s)
          cerrat(p, "unknown type '%s' in %s", name, nsname(ns));
      }
      if (nsegs - k != 1)
        cerrat(p, "a type is the namespace path's end; nothing follows it (11-namespaces.md)");
    }
  }

  if (nsegs == 2 && !p->v.path.root && strcmp(segs[0]->v.seg.name, "Self") == 0 &&
      (env->strait || env->impl)) {
    /* Self::X, the associated type (05-traits.md): a projection in
     * a trait's own declaration, the supplied type in an impl */
    char   *nm = segs[1]->v.seg.name;
    Sym    *owner = env->strait ? env->strait : env->impl;
    Member *m = memberfind(owner, nm);

    if (segs[1]->v.seg.args)
      cerrat(p, "'%s' takes no type arguments", nm);
    if (!m || m->kind != Mtype)
      cerrat(p, "'%s' is not an associated type of '%s'", nm, owner->name);
    if (env->impl)
      return m->val; /* what the impl supplied, resolved before any fn */
    return typroj(env->strait, selfty(), nm);
  }
  if (nsegs == 2 && !p->v.path.root && !segs[0]->v.seg.args) {
    /* It::Item outside: the associated type as the impl supplied it
     * (05-traits.md). One hit resolves; two traits carrying the
     * same name on one type is an ambiguity neither wins */
    char *nm0 = segs[0]->v.seg.name;
    char *nm1 = segs[1]->v.seg.name;
    Type *pt = envfind(env, nm0);
    Sym  *s = symfind(nm0);

    if (pt && pt->k == Typaram) {
      /* T::Item, the parameter's own bounds naming the trait that
       * carries Item (04-generics.md): a projection, answered
       * under a binding -- the instantiation's re-check, or the
       * impl's row -- never here */
      Ast **bs = pt->gp->v.gp.bounds;
      usize bi, mi;
      Sym  *hitsym = 0;
      Ast  *hitpin = 0; /* the bound's own pin for it, when it spelled
                         * one: the answer is the pinned type itself,
                         * no projection left to open (04-generics.md) */

      if (segs[1]->v.seg.args)
        cerrat(p, "'%s' takes no type arguments", nm1);
      for (bi = 0; bi < vlen(bs); bi++) {
        Sym *tr = bs[bi]->v.path.sym; /* the bound's own cache (04) */

        if (!tr) { /* a signature read ahead of its declaration's own
                    * walk -- a lazy pass-2 read in another file. The
                    * name reads here; the cache holds once the walk
                    * lands (04-generics.md) */
          Ast **bsegs = bs[bi]->v.path.segs;

          tr = vlen(bsegs) == 1 ? symfind(bsegs[0]->v.seg.name) : 0;
        }
        if (!tr || tr->kind != Strait)
          continue;
        if (!tr->traitdone)
          resolvetrait(tr); /* early: the projection reads the
                             * family's members now, the lazy read a
                             * file away (04-generics.md) */
        for (mi = 0; mi < tr->nmembers; mi++)
          if (tr->members[mi].kind == Mtype && strcmp(tr->members[mi].name, nm1) == 0) {
            if (hitsym)
              cerrat(p, "'%s' carries '%s' twice; the bounds cannot be told apart", nm0, nm1);
            hitsym = tr;
            {
              Ast **pas = bs[bi]->v.path.segs[0]->v.seg.args;
              usize pa, pna = vlen(pas);

              for (pa = 0; pa < pna; pa++)
                if (pas[pa]->k == Nassoc && strcmp(pas[pa]->v.assoc.name, nm1) == 0) {
                  hitpin = pas[pa];
                  break;
                }
            }
          }
      }
      if (!hitsym)
        cerrat(p, "'%s' is not an associated type of a bound on '%s'", nm1, nm0);
      /* the pin unresolved -- a bound read ahead of its own walk --
       * falls to the projection, the instance's opening the answer
       * either way */
      return hitpin && hitpin->v.assoc.rt ? hitpin->v.assoc.rt : typroj(hitsym, pt, nm1);
    }
    if (pt && pt->k != Typaram && pt->sym) {
      /* T::Item under the binding: the parameter is a type now,
       * and the outer form's own answer reads it -- the impl's
       * row, the same lookup a spelled name takes (05-traits.md) */
      Member *hit = 0;
      usize   i, j;

      for (i = 0; i < chk_nimpls; i++) {
        Sym *im = chk_impls[i];

        if (!im->ifort || !im->ipath || im->ifort->sym != pt->sym || im->ngparams)
          continue; /* a pattern impl's reads under its instance (04) */
        for (j = 0; j < im->nmembers; j++)
          if (im->members[j].kind == Mtype && strcmp(im->members[j].name, nm1) == 0) {
            if (hit)
              cerrat(p, "'%s' carries '%s' twice; the trait cannot be told apart", nm0, nm1);
            hit = &im->members[j];
          }
      }
      if (hit) {
        if (segs[1]->v.seg.args)
          cerrat(p, "'%s' takes no type arguments", nm1);
        return hit->val;
      }
    }
    if (s && s->kind == Strait)
      cerrat(p,
             "'%s::%s' names the trait's own projection; the type implementing it "
             "spells it (05-traits.md)",
             nm0, nm1);
    if (s && s->kind == Stype) {
      Member *hit = 0;
      usize   i, j;

      for (i = 0; i < chk_nimpls; i++) {
        Sym *im = chk_impls[i];

        if (!im->ifort || !im->ipath || im->ifort->sym != s || im->ngparams)
          continue; /* a pattern impl's reads under its instance (04) */
        for (j = 0; j < im->nmembers; j++)
          if (im->members[j].kind == Mtype && strcmp(im->members[j].name, nm1) == 0) {
            if (hit)
              cerrat(p, "'%s' carries '%s' twice; the trait cannot be told apart", nm0, nm1);
            hit = &im->members[j];
          }
      }
      if (hit) {
        if (segs[1]->v.seg.args)
          cerrat(p, "'%s' takes no type arguments", nm1);
        return hit->val;
      }
    }
  }
  if (!s) { /* the bare name's own path: the whole single-file
             * namespace, the prelude's std half still answering a
             * bare name through symfind's fallthrough
             * (11-namespaces.md) */
    if (nsegs != 1)
      cerrat(p, "a qualified type name needs its namespace (not yet)");
    seg = segs[0];
    name = seg->v.seg.name;
    {
      Type *t = prim(name);

      if (t) {
        if (seg->v.seg.args)
          cerrat(p, "'%s' takes no type arguments", name);
        return t;
      }
    }
    if (!p->v.path.root) {
      Type *t = envfind(env, name);

      if (t) {
        if (t->k == Typaram && t->gp->v.gp.cnst)
          cerrat(p, "'%s' is a value parameter, not a type", name);
        return t;
      }
    }
    s = symfind(name);
    if (!s)
      cerrat(p, "unknown type '%s'", name);
  }
  if (s->kind == Sfn || s->kind == Sconst || s->kind == Sstatic)
    cerrat(p, "'%s' is not a type", name);
  args = rargs(seg, env, &nargs, 0);
  if (s->kind == Strait) {
    if (nargs)
      cerrat(p, "a trait's arguments belong to its impl head (05-traits.md)");
    return tysym(s, args, nargs);
  }
  if (s->kind == Stype && s->tykind == TYalias)
    return aliasinst(s, args, nargs, p);
  if (nargs > s->ngparams)
    cerrat(p, "'%s' takes %lu type argument%s, not %lu", name, (unsigned long) s->ngparams,
           s->ngparams == 1 ? "" : "s", (unsigned long) nargs);
  /* defaults fill the missing tail, the alias rule again */
  if (nargs < s->ngparams) {
    args = dflttail(s, args, nargs, 0, 0, p);
    nargs = s->ngparams;
  }
  return tysym(s, args, nargs);
}

/* does a constant fit the tag type? (01-types.md, enums) */
static int
fits(u64 v, Type *t)
{
  switch (t->num) {
  case IN_I8:
    return v <= 0x7fu;
  case IN_I16:
    return v <= 0x7fffu;
  case IN_I32:
    return v <= 0x7fffffffu;
  case IN_I64:
    return (v >> 63) == 0; /* v <= 2^63-1, without a long long literal */
  case IN_U8:
    return v <= 0xffu;
  case IN_U16:
    return v <= 0xffffu;
  case IN_U32:
    return v <= 0xffffffffu;
  case IN_U64:
  case IN_U128:
  case IN_ISIZE:
  case IN_USIZE:
    return 1; /* a u64 discriminant always fits */
  default:
    return 0;
  }
}

/* a signature spelled in pass 2 may read a trait's member table
 * before pass 3 builds it (a handle's associated types align against
 * the declaration order); the read builds it then, once (05) */
static void resolvetrait(Sym *s);
static void filectx(Srcfile *sf); /* a file's whole context, entered --
                                   * bound whole on the first read (below) */

Type *
rty(Ast *t, Env *env)
{
  switch (t->k) {
  case Nunit:
    return tyunit();
  case Ntopt:
    return tyopt(rty(t->v.n1.e, env));
  case Ntresult: { /* E?T is Result<T, E>: a is E, b is T */
    Type *e = rty(t->v.n2.a, env);
    Type *v = rty(t->v.n2.b, env);

    return tyres(v, e);
  }
  case Ntptr: {
    Type *c = rty(t->v.un.e, env);

    return typtr(t->v.un.mut ? tymut(c) : c);
  }
  case Ntmut: /* a tuple row's writable slot (01-types.md) */
    return tymut(rty(t->v.un.e, env));
  case Nspread:  /* (...Ts): the grouping of the spread -- the same
                  * pack, spelled the impl target's way
                  * (04-generics.md) */
  case Ntpack: { /* ...Ts: the pack's rows. The declaration's walk
                  * reads the parameter itself, a Typaram whose gp
                  * carries the pack; a re-check's env holds the
                  * binding, the tuple the instance feeds it -- both
                  * spell the same value, the tuple (04-generics.md) */
    Type *p = rty(t->v.un.e, env);

    if (p && p->k == Typaram) {
      if (!p->gp->v.gp.pack)
        cerrat(t, "'%s' is not a pack; the ... wants one (04-generics.md)", p->gp->v.gp.name);
      return p;
    }
    if (p && (p->k == Tytuple || p->k == Tyunit)) /* the binding, the
                                                   * rows it stands for */
      return p;
    cerrat(t, "the ... names a pack: a generic parameter's (04-generics.md)");
    return 0; /* unreachable */
  }
  case Ntarray: {
    Type *elem = rty(t->v.arrlit.t, env);
    Ast  *len = t->v.arrlit.len;

    if (t->v.arrlit.mut)
      elem = tymut(elem);
    if (!len)
      return tyslice(elem);
    { /* [N]T: a const parameter names the length symbolically (the
       * generic's own binding); any other const expression is the
       * length, evaluated (08-reflection.md) */
      if (len->k == Npath && vlen(len->v.path.segs) == 1 && !len->v.path.root) {
        char *nm = len->v.path.segs[0]->v.seg.name;
        Type *p = envfind(env, nm);

        if (p && p->k == Typaram) { /* the declaration's own walk: N
                                     * the parameter, the length the
                                     * instance's binding answers */
          if (!p->gp->v.gp.cnst)
            cerrat(len, "'%s' is not a const parameter", nm);
          return tyarrayp(p->gp, elem);
        } /* a re-check's env binds N to its number: the walk below
           * reads it off the binding (08-reflection.md) */
      }
      return tyarray(cevallong(len, *env, tyint(IN_USIZE)), elem);
    }
  }
  case Nttuple: {
    usize  n = vlen(t->v.list.ts);
    Type **ts = n ? tyargs(n) : 0;
    usize  i;

    if (n == 1 && t->v.list.ts[0]->k == Ntpack) /* (...Ts): the
                                                 * impl target's
                                                 * spelling -- the
                                                 * pack itself, the
                                                 * rows standing as
                                                 * the tuple's own
                                                 * (04-generics.md) */
      return rty(t->v.list.ts[0], env);
    if (n > 1 && t->v.list.ts[0]->k == Ntpack) /* (...Ts, U): the
                                                * pack among rows is
                                                * no type -- it
                                                * stands for the
                                                * whole tuple or
                                                * nothing
                                                * (04-generics.md) */
      cerrat(t, "the pack stands for the whole tuple: (...Ts) alone (04-generics.md)");
    for (i = 0; i < n; i++)
      ts[i] = rty(t->v.list.ts[i], env);
    return tytuple(ts, n);
  }
  case Ntfn: {
    usize  n = vlen(t->v.fnty.args);
    Type **ts = n ? tyargs(n) : 0;
    usize  i;

    for (i = 0; i < n; i++)
      ts[i] = rty(t->v.fnty.args[i], env);
    /* a missing return is () -- the two spellings intern to one */
    return tyfn(ts, n, t->v.fnty.ret ? rty(t->v.fnty.ret, env) : tyunit());
  }
  case Ntdyn: {
    Ast *path = t->v.tdyn.e;
    Ast *seg;
    Sym *s;

    if (path->k != Npath || vlen(path->v.path.segs) != 1)
      cerrat(path, "expected a trait after dyn");
    seg = path->v.path.segs[0];
    s = symfind(seg->v.seg.name);
    if (!s)
      cerrat(path, "unknown trait '%s'", seg->v.seg.name);
    if (s->kind != Strait)
      cerrat(path, "'%s' is not a trait", seg->v.seg.name);
    if (!s->traitdone)
      resolvetrait(s); /* early: this signature needs the table now */
    {                  /* the handle's own words: the positional, a pack trait
                        * gathering them whole into its own slot (04-generics.md),
                        * then the associated types the Mtype slots hold, aligned to
                        * the trait's declaration order -- what a handle exposes of
                        * them (06) */
      Ast  **as = t->v.tdyn.assocs;
      Ast  **ps = t->v.tdyn.args;
      usize  na = vlen(as), nps = vlen(ps), i, j;
      usize  ng = s->ngparams, nty = 0, mi;
      Type **ga = 0;
      Type **args;

      if (!ng && nps)
        cerrat(t, "'%s' takes no arguments", s->name);
      if (ng) {
        int    pack = s->gparams[ng - 1]->v.gp.pack;
        Type **pa = nps ? tyargs(nps) : 0;

        for (i = 0; i < nps; i++)
          pa[i] = rty(ps[i], env);
        if (pack) {
          if (nps + 1 < ng) /* the pack's own prefix stands spelled
                             * or defaulted nowhere: a handle's words
                             * give the whole head (04-generics.md) */
            cerrat(t, "'%s' wants its %lu leading arguments, %lu spelled", s->name,
                   (unsigned long) (ng - 1), (unsigned long) nps);
          ga = packargs(s, pa, nps);
        } else {
          if (nps != ng)
            cerrat(t, "'%s' takes %lu arguments, %lu spelled", s->name, (unsigned long) ng,
                   (unsigned long) nps);
          ga = pa;
        }
      }
      for (i = 0; i < s->nmembers; i++)
        if (s->members[i].kind == Mtype)
          nty++;
      for (i = 0; i < na; i++) { /* every spelling names one of them */
        Member *m = 0;

        for (j = 0; j < s->nmembers; j++)
          if (s->members[j].kind == Mtype && strcmp(s->members[j].name, as[i]->v.init.name) == 0) {
            m = &s->members[j];
            break;
          }
        if (!m)
          cerrat(as[i], "'%s' has no associated type '%s'", s->name, as[i]->v.init.name);
      }
      args = tyargs(ng + nty);
      for (i = 0; i < ng; i++)
        args[i] = ga[i];
      mi = ng;
      for (i = 0; i < s->nmembers; i++) { /* each slot what its spelling gave */
        Member *m = &s->members[i];

        if (m->kind != Mtype)
          continue;
        args[mi] = 0;
        for (j = 0; j < na; j++)
          if (strcmp(as[j]->v.init.name, m->name) == 0) {
            args[mi] = rty(as[j]->v.init.e, env);
            break;
          }
        if (!args[mi])
          cerrat(t, "'%s' is not given; a handle leaves nothing open (06-dispatch.md)", m->name);
        mi++;
      }
      return tydyn(s, args, ng + nty, t->v.tdyn.mut);
    }
  }
  case Nttype:
    return tytype();
  case Nun:
    if (t->v.un.op == Tdollar2) /* the splice: the operand's value is
                                 * the type this slot takes
                                 * (08-reflection.md) */
      return tysplice(t->v.un.e, env);
    if (t->v.un.op == Tcaret2) /* a lift is a value: it crosses the
                                * other way (08-reflection.md) */
      cerrat(t, "a lift is a value, a type slot takes a type or a splice (08-reflection.md)");
    cerrat(t, "expected a type");
    return 0; /* unreachable */
  case Npath:
    return rpath(t, env);
  default:
    cerrat(t, "expected a type");
  }
  return 0; /* unreachable */
}

/* -- pass 2, per declaration kind -------------------------------------- */

/* a fn signature, against an env that already holds the outer
 * bindings -- the fn's own generics shadow them (04-generics.md) */
static Type *
resolvefnsig(Ast *it, Env *env)
{
  Env    e = envgparams(env, it->v.fn.gparams, vlen(it->v.fn.gparams));
  usize  n = vlen(it->v.fn.params);
  Type **ps = n ? tyargs(n) : 0;
  usize  i, j;

  { /* a const generic parameter names a length, and usize is the one
     * shape a length takes -- the numbers the binding rides are
     * usize's own (08-reflection.md) */
    Ast **gps = it->v.fn.gparams;
    usize ng = vlen(gps), g;

    for (g = 0; g < ng; g++)
      if (gps[g]->v.gp.cnst) {
        Type *t = rty(gps[g]->v.gp.t, &e);

        if (!t || t->k != Tyint || t->num != IN_USIZE)
          cerrat(gps[g], "a const generic parameter names a length: usize is the type it takes "
                         "(08-reflection.md)");
      }
  }
  for (i = 0; i < n; i++) {
    Ast *p = it->v.fn.params[i];

    for (j = 0; j < i; j++)
      if (strcmp(p->v.param.name, it->v.fn.params[j]->v.param.name) == 0)
        cerrat(p, "duplicate parameter '%s'", p->v.param.name);
    ps[i] = rty(p->v.param.t, &e);
  }
  return tyfn(ps, n, it->v.fn.ret ? rty(it->v.fn.ret, &e) : tyunit());
}

static void
resolvefn(Sym *s)
{
  s->fnty = resolvefnsig(s->decl, 0);
}

/* a signature read on demand -- the evaluator meets a forward
 * reference (a const initializer calling a fn below it) before this
 * pass reached it; resolvefnsig reads the declaration alone, so the
 * lazy form is the same answer. The declaration's own names resolve
 * in its own file: a lazy read may arrive from anywhere, and the
 * file's context is entered here the way resolvetrait enters its
 * own, whole and given back at the door (12-projects.md) */
Type *
fnsigof(Sym *s)
{
  if (!s->fnty) {
    Ns         *svns = nscuring();
    Use       **svuses = useuring();
    const char *svpath = lexpath();

    if (s->ownsf)
      filectx(s->ownsf);
    s->fnty = resolvefnsig(s->decl, 0);
    if (s->ownsf) {
      nscur(svns);
      usecur(svuses);
      lexsetpath(svpath);
    }
  }
  return s->fnty;
}

/* the fields of a struct or union, and of a named enum payload */
static Field *
resolvefields(Ast **fs, Env *env, usize n, int isunion)
{
  Field *fields = n ? arenaalloc(n * sizeof *fields) : 0;
  usize  i, j;

  memset(fields, 0, n * sizeof *fields);
  for (i = 0; i < n; i++) {
    Ast *f = fs[i];

    for (j = 0; j < i; j++)
      if (strcmp(f->v.variant.name, fs[j]->v.variant.name) == 0)
        cerrat(f, "duplicate field '%s'", f->v.variant.name);
    if (isunion && f->v.variant.mut)
      cerrat(f, "a union field cannot be mut");
    fields[i].name = f->v.variant.name;
    fields[i].mut = f->v.variant.mut;
    fields[i].attrs = f->attrs; /* flattened out with the rest, for
                                 * the reflection's walk (08) */
    fields[i].ty = rty(f->v.variant.t, env);
  }
  return fields;
}

static void
resolvestruct(Sym *s)
{
  Ast *it = s->decl;
  Env  env = envgparams(0, it->v.ty.gparams, vlen(it->v.ty.gparams));

  s->nfields = vlen(it->v.ty.fields);
  s->fields = resolvefields(it->v.ty.fields, &env, s->nfields, s->tykind == TYunion);
}

static void
resolveenum(Sym *s)
{
  Ast  *it = s->decl;
  Env   env = envgparams(0, it->v.en.gparams, vlen(it->v.en.gparams));
  usize n = vlen(it->v.en.variants);
  u64   next = 0;
  usize i, j;

  if (it->v.en.tag) {
    s->tagty = rty(it->v.en.tag, &env);
    if (s->tagty->k != Tyint || s->tagty->num >= IN_F32)
      cerrat(it->v.en.tag, "the enum tag must be an integer type");
  }
  s->nvariants = n;
  s->variants = n ? arenaalloc(n * sizeof *s->variants) : 0;
  memset(s->variants, 0, n * sizeof *s->variants);
  for (i = 0; i < n; i++) {
    Ast     *v = it->v.en.variants[i];
    Variant *dv = &s->variants[i];

    for (j = 0; j < i; j++)
      if (strcmp(v->v.variant.name, it->v.en.variants[j]->v.variant.name) == 0)
        cerrat(v, "duplicate variant '%s'", v->v.variant.name);
    dv->name = v->v.variant.name;
    dv->attrs = v->attrs; /* flattened out, the Field pattern (08) */
    dv->hasdisc = v->v.variant.hasdisc;
    dv->disc = v->v.variant.hasdisc ? cevallong(v->v.variant.discexpr, env, tyint(IN_USIZE)) : next;
    next = dv->disc + 1;
    if (s->tagty && !fits(dv->disc, s->tagty))
      cerrat(v, "discriminant %lu does not fit the tag type", (unsigned long) dv->disc);
    for (j = 0; j < i; j++)
      if (s->variants[j].disc == dv->disc)
        cerrat(v, "discriminant %lu appears twice", (unsigned long) dv->disc);
    dv->named = v->v.variant.named;
    if (v->v.variant.named) {
      usize nf = vlen(v->v.variant.payload);

      dv->fields = resolvefields(v->v.variant.payload, &env, nf, 0);
      dv->nfields = nf;
    } else {
      usize np = vlen(v->v.variant.payload);
      usize k;

      dv->payload = np ? tyargs(np) : 0;
      dv->npayload = np;
      for (k = 0; k < np; k++)
        dv->payload[k] = rty(v->v.variant.payload[k], &env);
    }
  }
}

/* the trait named in an impl head -- the one place a trait carries
 * arguments: they are its own parameters, bound by this impl
 * (05-traits.md). Defaults fill the tail, the alias rule again. */
static Type *
rtraitpath(Ast *p, Env *env, Type *self)
{
  Ast  **segs = p->v.path.segs;
  Ast   *seg;
  Sym   *s;
  Type **args;
  usize  nargs;

  if (vlen(segs) != 1 || p->v.path.root)
    cerrat(p, "expected a trait name after impl");
  seg = segs[0];
  s = symfind(seg->v.seg.name);
  if (!s)
    cerrat(p, "unknown trait '%s'", seg->v.seg.name);
  if (s->kind != Strait)
    cerrat(p, "'%s' is not a trait", seg->v.seg.name);
  args = rargs(seg, env, &nargs, 0); /* the impl head: a trait's
                                      * arguments its own parameters,
                                      * the members the impl's words
                                      * answer -- no pin here */
  if (s->ngparams && s->gparams[s->ngparams - 1]->v.gp.pack) {
    /* the pack: the impl spells the elements one for one, any
     * number -- zero included -- and they gather into the pack's
     * own slot, the whole tuple (04-generics.md). The prefix the
     * same missing word as anywhere */
    usize k;

    for (k = nargs; k + 1 < s->ngparams; k++)
      cerrat(p, "missing type argument '%s'", s->gparams[k]->v.gp.name);
    return tysym(s, packargs(s, args, nargs), s->ngparams);
  }
  if (nargs > s->ngparams)
    cerrat(p, "'%s' takes %lu type argument%s, not %lu", s->name, (unsigned long) s->ngparams,
           s->ngparams == 1 ? "" : "s", (unsigned long) nargs);
  if (nargs < s->ngparams) { /* the defaults, Self the impl's own
                              * type (04-generics.md) */
    args = dflttail(s, args, nargs, env, self, p);
    nargs = s->ngparams;
  }
  return tysym(s, args, nargs);
}

static void
resolveimpl(Sym *s)
{
  Ast *it = s->decl;
  Env  env = envgparams(0, it->v.impl.gparams, vlen(it->v.impl.gparams));
  { /* the impl's own angle brackets: a const length among them wants
     * its [N]T routed through the table, and that routing arrives
     * with a later milestone (08-reflection.md) */
    Ast **gps = it->v.impl.gparams;
    usize ng = vlen(gps), g;

    for (g = 0; g < ng; g++)
      if (gps[g]->v.gp.cnst)
        cerrat(gps[g], "a const generic parameter on an impl arrives with a later milestone "
                       "(08-reflection.md)");
  }

  if (it->v.impl.fort) {                   /* a trait impl: the path names the trait */
    s->ifort = rty(it->v.impl.fort, &env); /* Self, for the defaults */
    s->ipath = rtraitpath(it->v.impl.path, &env, s->ifort);
  } else { /* inherent: the path is the type */
    s->ipath = rty(it->v.impl.path, &env);
  }
}

/* -- pass 3: trait and impl members, coherence -------------------------- */

/* a trait's members, in declaration order. Self is a parameter
 * here -- it names whatever implements the trait, and the fn
 * signatures carry it (and Self::Item projections) until an impl
 * substitutes them away. The members' own names resolve in the
 * trait's own file: a lazy read may arrive from any file's pass --
 * a signature half a project away reaching for the table -- and a
 * bare name belongs where the trait lives, its namespace and its
 * uses, not to whichever file asked (12-projects.md). The caller's
 * context is kept whole and given back at the door */
static void
resolvetrait(Sym *s)
{
  Ast        *it = s->decl;
  Env         env = envgparams(0, it->v.ty.gparams, vlen(it->v.ty.gparams));
  Ast       **ms = it->v.ty.members;
  usize       n = vlen(ms);
  usize       i, j;
  Ns         *svns;
  Use       **svuses;
  const char *svpath;

  if (s->traitdone)
    return; /* built early by a signature read, or already built */
  s->traitdone = 1;
  svns = nscuring();
  svuses = useuring();
  svpath = lexpath();
  if (s->ownsf)
    filectx(s->ownsf); /* the trait's own file, its context whole */
  env.strait = s;
  env = envpush(&env, "Self", selfty());
  s->nmembers = n;
  s->members = n ? arenaalloc(n * sizeof *s->members) : 0;
  memset(s->members, 0, n * sizeof *s->members);
  for (i = 0; i < n; i++) {
    Ast    *m = ms[i];
    Member *dm = &s->members[i];

    for (j = 0; j < i; j++)
      if (strcmp(itemname(m), itemname(ms[j])) == 0)
        cerrat(m, "duplicate member '%s'", itemname(m));
    dm->name = itemname(m);
    dm->decl = m;
    switch (m->k) {
    case Nfn:
      dm->kind = Mfn;
      dm->ty = resolvefnsig(m, &env);
      break;
    case Ntypedef:
      if (m->v.td.t)
        cerrat(m, "a trait's associated type is declared bare (defaults are not a feature)");
      dm->kind = Mtype;
      break;
    case Nconst:
      dm->kind = Mconst;
      dm->ty = rty(m->v.cst.t, &env);
      break;
    default:
      cerrat(m, "a trait member is a fn, a type, or a const");
    }
  }
  if (s->ownsf) {
    nscur(svns);
    usecur(svuses);
    lexsetpath(svpath);
  }
}

/* an impl's members. Two rounds: the type and const members first,
 * so a fn's Self::Item reads what the impl supplied; the fns after.
 * An inherent impl carries no associated types -- it supplies
 * constants and methods only (05-traits.md). */
/* one impl's member fns: the same env resolveimplmembers built --
 * Self is the impl's type, the impl's own generics are in scope */

/* a trait impl's head named the trait's own parameters: impl Add
 * for Vec3 is Add<Vec3>, and a member's Rhs is that argument
 * (07-operators.md). The bindings sit innermost, over the impl's own */
Env
envtraitargs(Env *e, Sym *s)
{
  Sym  *tr;
  usize i, n;

  if (!s->ifort || !s->ipath || s->ipath->k != Tytrait)
    return *e;
  tr = s->ipath->sym;
  n = s->ipath->nargs < tr->ngparams ? s->ipath->nargs : tr->ngparams;
  for (i = 0; i < n; i++) {
    Env r = envpush(e, tr->gparams[i]->v.gp.name, s->ipath->args[i]);

    *e = r;
  }
  return *e;
}

static void
resolveimplmembers(Sym *s)
{
  Ast  *it = s->decl;
  Env   env = envgparams(0, it->v.impl.gparams, vlen(it->v.impl.gparams));
  Ast **ms = it->v.impl.members;
  usize n = vlen(ms);
  int   round;

  env.impl = s;
  env = envpush(&env, "Self", s->ifort ? s->ifort : s->ipath);
  env = envtraitargs(&env, s);
  s->nmembers = n;
  s->members = n ? arenaalloc(n * sizeof *s->members) : 0;
  memset(s->members, 0, n * sizeof *s->members);
  {
    usize i;

    for (i = 0; i < n; i++) {
      s->members[i].name = itemname(ms[i]);
      s->members[i].decl = ms[i];
    }
  }
  for (round = 0; round < 2; round++) {
    usize i, j;

    for (i = 0; i < n; i++) {
      Ast    *m = ms[i];
      Member *dm = &s->members[i];
      int     isfn = m->k == Nfn;

      if ((round == 0) == isfn)
        continue;
      for (j = 0; j < i; j++)
        if (strcmp(dm->name, s->members[j].name) == 0 && (round == 0) == (ms[j]->k != Nfn))
          cerrat(m, "duplicate member '%s'", dm->name);
      switch (m->k) {
      case Nfn:
        dm->kind = Mfn;
        dm->ty = resolvefnsig(m, &env);
        { /* the method's own fn Sym: outside the namespace -- the
           * call sites write it back, the emitter names by it. The
           * impl's parameters ride along as the Sym's own, a generic
           * method's after them -- the instantiation key is the
           * concatenation, exactly as a generic fn's is its own list
           * (04-generics.md) */
          Sym  *fs = arenaalloc(sizeof *fs);
          usize ng = vlen(it->v.impl.gparams);
          usize nm = vlen(m->v.fn.gparams);

          memset(fs, 0, sizeof *fs);
          fs->name = dm->name;
          fs->kind = Sfn;
          fs->ownns = s->ownns; /* the impl's file's namespace: the
                                 * method's mangle carries it (11) */
          fs->decl = m;
          fs->fnty = dm->ty;
          fs->impl = s;
          if (ng + nm) {
            Ast **gg = arenaalloc((ng + nm) * sizeof *gg);
            usize g;

            for (g = 0; g < ng; g++)
              gg[g] = it->v.impl.gparams[g];
            for (g = 0; g < nm; g++)
              gg[ng + g] = m->v.fn.gparams[g];
            fs->gparams = gg;
            fs->ngparams = ng + nm;
          }
          dm->sym = fs;
        }
        break;
      case Ntypedef:
        if (!s->ifort)
          cerrat(m, "an inherent impl supplies no associated types (05-traits.md)");
        if (!m->v.td.t)
          cerrat(m, "an impl supplies the type: 'type %s = T'", dm->name);
        dm->kind = Mtype;
        dm->val = rty(m->v.td.t, &env);
        break;
      case Nconst:
        dm->kind = Mconst;
        dm->ty = rty(m->v.cst.t, &env);
        break;
      default:
        cerrat(m, "an impl member is a fn, a type, or a const");
      }
    }
  }
}

/* one parameter list's bounds, read here where they were written:
 * the trait each names -- a bound is a trait's name (04-generics.md)
 * -- and the type arguments it spelled, cached on the bound's own
 * node. The consumers run under whoever called them -- a call's
 * binding check, the generic branch's signature, a projection's
 * hunt -- and a name would read that caller's context
 * (11-namespaces.md); the cache is what keeps the bound's own
 * file's reading. The pins a bound spells ride the same cache: each
 * resolved here, standing behind the arguments (04-generics.md).
 * The tail the bound left unspelled is not cached: its defaults
 * belong to whoever asks, their Self their own (07-operators.md). */
static void
boundresolve(Ast **gps, Env *env)
{
  usize i, j;

  for (i = 0; i < vlen(gps); i++) {
    Ast **bs = gps[i]->v.gp.bounds;

    for (j = 0; j < vlen(bs); j++) {
      Ast  *b = bs[j];
      Ast **segs = b->v.path.segs;
      Ast  *seg;
      Sym  *s;
      usize n, k, na;
      int   pack = 0; /* the trait's last parameter is a pack: the
                       * arguments gather into its slot, the whole
                       * tuple (04-generics.md) */

      if (vlen(segs) != 1)
        cerrat(b, "a bound is a trait's name");
      seg = segs[0];
      s = symfind(seg->v.seg.name);
      if (!s)
        cerrat(b, "unknown trait '%s'", seg->v.seg.name);
      if (s->kind != Strait)
        cerrat(b, "a bound names a trait, and '%s' is not one", seg->v.seg.name);
      n = 0; /* the pins stand behind every argument: the count
              * walks the positional alone (04-generics.md) */
      na = vlen(seg->v.seg.args);
      for (k = 0; k < na; k++)
        if (seg->v.seg.args[k]->k != Nassoc)
          n++;
      if (s->ngparams && s->gparams[s->ngparams - 1]->v.gp.pack) {
        /* the pack: a bound feeds it types one for one, any number
         * its own -- zero included, spelled <> -- the binding the
         * whole tuple (04-generics.md). The count takes the prefix
         * alone; the tail the bound left unspelled is the same
         * missing word as anywhere */
        usize k2;

        for (k2 = n; k2 + 1 < s->ngparams; k2++)
          if (!s->gparams[k2]->v.gp.dflt)
            cerrat(b, "the bound spells no '%s', and it has no default", s->gparams[k2]->v.gp.name);
        pack = 1;
      } else if (n > s->ngparams)
        cerrat(b, "'%s' takes %lu type argument%s, not %lu", s->name, (unsigned long) s->ngparams,
               s->ngparams == 1 ? "" : "s", (unsigned long) n);
      else if (n < s->ngparams) { /* the tail the bound left unspelled:
                                   * whoever asks fills it from the
                                   * trait's defaults, so every parameter
                                   * past the spelled ones must carry one
                                   * (04-generics.md) */
        usize k2;

        for (k2 = n; k2 < s->ngparams; k2++)
          if (!s->gparams[k2]->v.gp.dflt)
            cerrat(b, "the bound spells no '%s', and it has no default", s->gparams[k2]->v.gp.name);
      }
      if (!s->traitdone)
        resolvetrait(s);         /* early: the pins read the trait's members
                                  * now, the same early a projection takes
                                  * (04-generics.md) */
      for (k = 0; k < na; k++) { /* each pin names one of the trait's
                                  * associated types -- the dyn
                                  * handle's own rule (06) -- and
                                  * rides the node resolved, the
                                  * arguments' own cache */
        Ast *a = seg->v.seg.args[k];

        if (a->k != Nassoc)
          continue;
        {
          Member *m = memberfind(s, a->v.assoc.name);

          if (!m || m->kind != Mtype)
            cerrat(a, "'%s' has no associated type '%s'", s->name, a->v.assoc.name);
          a->v.assoc.rt = rty(a->v.assoc.t, env);
        }
      }
      b->v.path.sym = s;
      b->v.path.tys =
          pack ? packargs(s, n ? rargs(seg, env, &n, 1) : 0, n) : (n ? rargs(seg, env, &n, 1) : 0);
    }
  }
}

/* the bounds one declaration's parameters carry, and the member
 * fns' own under it -- free fns and plain types here too, for their
 * consumers read them under the caller's context (04-generics.md).
 * The members' scope mirrors what resolves their signatures: an
 * impl's Self is its own type with the trait's head arguments over
 * the impl's generics (05-traits.md), a trait's Self the trait's
 * own parameter. */
static void
resolvebounds(Ast *it, Sym *s)
{
  Ast **gps;
  Ast **ms = 0;
  Env   mem;

  switch (it->k) {
  case Nfn:
    gps = it->v.fn.gparams;
    break;
  case Nstruct:
  case Nunion:
    gps = it->v.ty.gparams;
    break;
  case Nenum:
    gps = it->v.en.gparams;
    break;
  case Ntypedef:
    gps = it->v.td.gparams;
    break;
  case Ntrait:
  case Nimpl: {
    Env   own = envgparams(0, it->k == Ntrait ? it->v.ty.gparams : it->v.impl.gparams,
                         it->k == Ntrait ? vlen(it->v.ty.gparams) : vlen(it->v.impl.gparams));
    usize i;

    gps = it->k == Ntrait ? it->v.ty.gparams : it->v.impl.gparams;
    boundresolve(gps, &own);
    mem = own;
    if (it->k == Ntrait) {
      mem.strait = s;
      mem = envpush(&mem, "Self", selfty());
      ms = it->v.ty.members;
    } else {
      mem.impl = s;
      mem = envpush(&mem, "Self", s->ifort ? s->ifort : s->ipath);
      mem = envtraitargs(&mem, s);
      ms = it->v.impl.members;
    }
    for (i = 0; i < vlen(ms); i++)
      if (ms[i]->k == Nfn) {
        Env e2 = envgparams(&mem, ms[i]->v.fn.gparams, vlen(ms[i]->v.fn.gparams));

        boundresolve(ms[i]->v.fn.gparams, &e2);
      }
    return;
  }
  default:
    return;
  }
  {
    Env env = envgparams(0, gps, vlen(gps));

    boundresolve(gps, &env);
  }
}

/* -- comparing a trait impl against its trait -------------------------- */

/* what a trait's declaration resolves against an impl: Self becomes
 * the impl's type, the trait's own parameters the head's arguments,
 * and each projection the type the impl supplied */
struct TSub
{
  Ast  **gp;     /* the trait's parameters */
  Type **ty;     /* the head's arguments, parallel */
  usize  n;      /* their count */
  Type  *selfty; /* the impl's type, what Self becomes */
  Sym   *is;     /* the impl: Typroj name -> the supplied type */
};

static Type *
tsubst(Type *t, TSub *sub)
{
  usize i;

  switch (t->k) {
  case Typaram:
    if (t->gp == sym_selfgp)
      return sub->selfty;
    for (i = 0; i < sub->n; i++)
      if (t->gp == sub->gp[i])
        return sub->ty[i];
    return t;
  case Typroj: {
    Member *m = memberfind(sub->is, t->name);

    if (m && m->kind == Mtype && t->sym == sub->is->ipath->sym)
      return m->val;
    return typroj(t->sym, tsubst(t->t, sub), t->name);
  }
  case Typtr:
    return typtr(tsubst(t->t, sub));
  case Tyslice:
    return tyslice(tsubst(t->t, sub));
  case Tymut: /* *mut Self must reach the Self through the wrapper */
    return tymut(tsubst(t->t, sub));
  case Tyarray:
    return t->gp ? t : tyarray(t->n, tsubst(t->t, sub));
  case Tytuple: {
    Type **ts = t->nargs ? tyargs(t->nargs) : 0;

    for (i = 0; i < t->nargs; i++)
      ts[i] = tsubst(t->args[i], sub);
    return tytuple(ts, t->nargs);
  }
  case Tyfn: {
    /* the pack's row spelled out: a declared parameter that is the
     * pack itself, bound the whole tuple, becomes the elements one
     * for one -- the impl's own spelling of the same signature
     * (04-generics.md). An unbound pack stays a slot: the trait's
     * own declaration reads itself */
    Type **ts;
    usize  n = 0, j;

    for (i = 0; i < t->nargs; i++) { /* the count first */
      Type *a = t->args[i];

      if (a->k == Typaram && a->gp->v.gp.pack)
        for (j = 0; j < sub->n; j++)
          if (a->gp == sub->gp[j] && sub->ty[j] && sub->ty[j]->k == Tytuple) {
            n += sub->ty[j]->nargs;
            goto next;
          }
      n++;
    next:;
    }
    ts = n ? tyargs(n) : 0;
    n = 0;
    for (i = 0; i < t->nargs; i++) {
      Type *a = tsubst(t->args[i], sub);

      if (a->k == Tytuple && t->args[i]->k == Typaram && t->args[i]->gp->v.gp.pack)
        for (j = 0; j < a->nargs; j++)
          ts[n++] = a->args[j];
      else
        ts[n++] = a;
    }
    return tyfn(ts, n, tsubst(t->t, sub));
  }
  case Tystruct:
  case Tyunion:
  case Tyenum:
  case Tytrait: {
    Type **ts = t->nargs ? tyargs(t->nargs) : 0;

    for (i = 0; i < t->nargs; i++)
      ts[i] = tsubst(t->args[i], sub);
    return tysym(t->sym, ts, t->nargs);
  }
  case Tydyn: {
    Type **ts = t->nargs ? tyargs(t->nargs) : 0;

    for (i = 0; i < t->nargs; i++)
      ts[i] = tsubst(t->args[i], sub);
    return tydyn(t->sym, ts, t->nargs, t->mut);
  }
  default: /* the interned leaves carry nothing to substitute */
    return t;
  }
}

/* a trait impl supplies every member the trait declares, no more,
 * and each fn with the declared signature -- Self and the
 * projections substituted away (05-traits.md) */
static void
checkimplcomplete(Sym *s)
{
  Sym  *ts = s->ipath->sym;
  TSub  sub;
  usize i;

  sub.gp = ts->gparams;
  sub.ty = s->ipath->args;
  sub.n = s->ipath->nargs;
  sub.selfty = s->ifort;
  sub.is = s;
  for (i = 0; i < ts->nmembers; i++) {
    Member *tm = &ts->members[i];
    Member *im = memberfind(s, tm->name);

    if (!im)
      cerrat(s->decl, "'%s' is missing from the impl", tm->name);
    if (im->kind != tm->kind)
      cerrat(im->decl, "'%s' is %s in the trait but %s in the impl", tm->name,
             tm->kind == Mfn     ? "a fn"
             : tm->kind == Mtype ? "an associated type"
                                 : "a const",
             im->kind == Mfn     ? "a fn"
             : im->kind == Mtype ? "an associated type"
                                 : "a const");
    if (tm->kind == Mtype)
      continue; /* the supplied type is the supply */
    {           /* a method's own parameters are its own names: the trait's K
                 * and the impl's K are different variables that must land the
                 * same positions. Rename the impl's onto the trait's, and the
                 * comparison sees one alphabet (04-generics.md) */
      Type *want = tsubst(tm->ty, &sub);
      Type *got = im->ty;

      if (tm->kind == Mfn && tm->decl) {
        Ast **tg = tm->decl->v.fn.gparams;
        Ast **ig = im->decl->v.fn.gparams;
        usize nt = vlen(tg);

        if (nt != vlen(ig))
          cerrat(im->decl, "'%s' takes %lu parameters of its own, the trait declares %lu", tm->name,
                 (unsigned long) vlen(ig), (unsigned long) nt);
        if (nt) {
          Type **tt = tyargs(nt);
          usize  g;

          for (g = 0; g < nt; g++)
            tt[g] = typaram(tg[g]);
          got = gsubst(got, ig, tt, nt);
        }
      }
      if (!tysame(want, got))
        cerrat(im->decl, "'%s' must be %s, not %s", tm->name, tysprint1(want), tysprint1(got));
    }
  }
  for (i = 0; i < s->nmembers; i++)
    if (!memberfind(ts, s->members[i].name))
      cerrat(s->members[i].decl, "the trait declares no '%s'", s->members[i].name);
}

/* -- impl overlap (04-generics.md) -------------------------------------- */

/* is a a strictly more specific pattern than b -- does every type
 * matching a also match b? A variable in b binds; a repeated one
 * must land on the same thing twice; mut under a slot orders the
 * way the spec's table lists (mutable matches the subset). */
struct SpecSub
{
  Ast  *gp[16]; /* the variables b has bound, so far */
  Type *ty[16]; /* what each one landed on, parallel */
  usize n;      /* their count */
};

static int spec1(Type *a, Type *b, SpecSub *s);

/* the child of a slot kind, Tymut unwrapped, and whether it had one */
static Type *
slotchild(Type *t, int *mut)
{
  *mut = t && t->k == Tymut;
  return *mut ? t->t : t;
}

static int
spec1(Type *a, Type *b, SpecSub *s)
{
  usize i;

  if (b->k == Typaram) {
    for (i = 0; i < s->n; i++)
      if (s->gp[i] == b->gp)
        return tysame(s->ty[i], a); /* the second landing of a variable */
    if (s->n >= 16)
      return 0; /* too many variables to order here */
    s->gp[s->n] = b->gp;
    s->ty[s->n] = a;
    s->n++;
    return 1;
  }
  if (a->k != b->k)
    return 0;
  switch (b->k) {
  case Typtr:
  case Tyslice:
  case Tyarray: {
    Type *ac, *bc;
    int   am, bm;

    if (b->k == Tyarray) {
      if (a->gp || b->gp)
        return a->gp == b->gp && spec1(a->t, b->t, s); /* the same const parameter */
      if (a->n != b->n)
        return 0;
    }
    ac = slotchild(a->t, &am);
    bc = slotchild(b->t, &bm);
    if (am < bm)
      return 0; /* a's slot is immutable where b's is not */
    return spec1(ac, bc, s);
  }
  case Tystruct:
  case Tyunion:
  case Tyenum:
  case Tytrait:
  case Tydyn:
    if (a->sym != b->sym || a->nargs != b->nargs || a->mut != b->mut)
      return 0;
    for (i = 0; i < b->nargs; i++)
      if (!spec1(a->args[i], b->args[i], s))
        return 0;
    return 1;
  case Tytuple:
  case Tyfn:
    if (a->nargs != b->nargs)
      return 0;
    for (i = 0; i < b->nargs; i++)
      if (!spec1(a->args[i], b->args[i], s))
        return 0;
    return b->k == Tyfn ? spec1(a->t, b->t, s) : 1;
  case Tyint:
    return a->num == b->num;
  case Typroj:
    return a->sym == b->sym && a->name && b->name && strcmp(a->name, b->name) == 0 &&
           spec1(a->t, b->t, s);
  case Tybool:
  case Tyunit:
  case Tyvoidptr:
  case Tytype:
    return 1; /* same kind, nothing left to compare */
  default:
    return 0;
  }
}

int
specializes(Type *a, Type *b)
{
  SpecSub s;

  memset(&s, 0, sizeof s);
  return spec1(a, b, &s);
}

/* provably disjoint: no type can match both. Conservative --
 * different kinds, different declarations, different numbers */
static int
disjoint(Type *a, Type *b)
{
  usize i;

  if (a->k == Typaram || b->k == Typaram)
    return 0; /* a variable matches anything */
  if (a->k != b->k)
    return 1;
  switch (b->k) {
  case Tyint:
    return a->num != b->num;
  case Typtr:
  case Tyslice: {
    Type *ac, *bc;
    int   am, bm;

    ac = slotchild(a->t, &am);
    bc = slotchild(b->t, &bm);
    return disjoint(ac, bc);
  }
  case Tyarray: {
    Type *ac, *bc;
    int   am, bm;

    if (a->gp || b->gp)
      return a->gp == b->gp ? disjoint(a->t, b->t) : 0; /* a variable can bind */
    if (a->n != b->n)
      return 1;
    ac = slotchild(a->t, &am);
    bc = slotchild(b->t, &bm);
    return disjoint(ac, bc);
  }
  case Tystruct:
  case Tyunion:
  case Tyenum:
  case Tytrait:
  case Tydyn:
    if (a->sym != b->sym)
      return 1;
    if (a->nargs != b->nargs)
      return 0; /* resolve already shaped them; stay unproven */
    for (i = 0; i < b->nargs; i++)
      if (disjoint(a->args[i], b->args[i]))
        return 1;
    return 0;
  case Tytuple:
    if (a->nargs != b->nargs)
      return 1;
    for (i = 0; i < b->nargs; i++)
      if (disjoint(a->args[i], b->args[i]))
        return 1;
    return 0;
  default:
    return 0;
  }
}

/* the traits an impl's parameter bounds name, at most sixteen -- a
 * bound is a trait's name (04-generics.md), read where it was
 * written: the bound's own cache, filled at declaration
 * (07-operators.md) */
static usize
collectbounds(Sym *s, Sym **out)
{
  Ast **gps = s->decl->v.impl.gparams;
  usize n = 0, i, j;

  for (i = 0; i < vlen(gps); i++) {
    Ast **bs = gps[i]->v.gp.bounds;

    for (j = 0; j < vlen(bs); j++)
      if (n < 16)
        out[n++] = bs[j]->v.path.sym;
  }
  return n;
}

static int
boundscontain(Sym **bs, usize n, Sym *t)
{
  usize i;

  for (i = 0; i < n; i++)
    if (bs[i] == t)
      return 1;
  return 0;
}

/* a ⊇ b: every trait b's bounds name, a's name too. No dedup -- a
 * bound written twice counts twice, which only ever overstates.
 * The table itself is pass 3's cache: the walks re-read it per
 * call, the emitter's picks among them, and a bound's name
 * resolves in the file that wrote it (04-generics.md) */
int
boundsincl(Sym *a, Sym *b)
{
  usize i;

  for (i = 0; i < b->nibounds; i++)
    if (!boundscontain(a->ibounds, a->nibounds, b->ibounds[i]))
      return 0;
  return 1;
}

/* Copy against Drop, either way round -- the exclusion table
 * (04-generics.md), with its two entries and the one row */
static int
boundsexclude(Sym *a, Sym *b)
{
  return (boundscontain(a->ibounds, a->nibounds, sym_copy) &&
          boundscontain(b->ibounds, b->nibounds, sym_drop)) ||
         (boundscontain(a->ibounds, a->nibounds, sym_drop) &&
          boundscontain(b->ibounds, b->nibounds, sym_copy));
}

/* a trait impl's whole identity: the trait's own arguments beside
 * the type it is for, one picture -- the call picks rows by their
 * arguments (07-operators.md), so the declaration's order and
 * disjointness read them too. Is a's row a specialization of b's,
 * the variables binding across both halves? */
int
rowspec(Sym *a, Sym *b)
{
  SpecSub s;
  usize   i;

  memset(&s, 0, sizeof s);
  for (i = 0; i < a->ipath->nargs; i++)
    if (!spec1(a->ipath->args[i], b->ipath->args[i], &s))
      return 0;
  return spec1(a->ifort ? a->ifort : a->ipath, b->ifort ? b->ifort : b->ipath, &s);
}

/* two rows, provably disjoint: a call the two cannot both take --
 * the trait's arguments or the for-type itself, whichever already
 * disagrees. A variable in either place matches anything, so rows
 * that meet only through one stay unproven (04-generics.md). The
 * arguments pair only where both rows carry the same count of
 * them -- a trait's rows all take the trait's own count (the
 * defaults filling the tail), an inherent row its type's -- and
 * rows that disagree on the count can only meet through their
 * types, which the last line reads. */
static int
rowdisjoint(Sym *a, Sym *b)
{
  usize i;

  if (a->ipath->nargs == b->ipath->nargs)
    for (i = 0; i < a->ipath->nargs; i++)
      if (disjoint(a->ipath->args[i], b->ipath->args[i]))
        return 1;
  return disjoint(a->ifort ? a->ifort : a->ipath, b->ifort ? b->ifort : b->ipath);
}

/* a later impl against an earlier one: provably disjoint, or
 * strictly ordered by specificity, or rejected on the spot
 * (04-generics.md -- overlap is checked at declaration) */
static void
checkoverlap(Sym *a, Sym *b)
{
  Type *fa = a->ifort ? a->ifort : a->ipath;
  Type *fb = b->ifort ? b->ifort : b->ipath;
  int   ab, ba;

  if (!!a->ifort != !!b->ifort)
    return; /* a trait impl and an inherent one never share a slot */
  if (a->ifort && a->ipath->sym != b->ipath->sym)
    return;       /* different traits never conflict */
  if (a->ifort) { /* the row's own shape: the trait's arguments
                   * beside the type it is for */
    ab = rowspec(a, b);
    ba = rowspec(b, a);
  } else {
    ab = specializes(fa, fb);
    ba = specializes(fb, fa);
  }
  if (ab != ba)
    return; /* strictly ordered one way or the other */
  if (boundsexclude(a, b))
    return; /* provably disjoint through the exclusion table */
  if (boundsincl(a, b) != boundsincl(b, a))
    return; /* equal shape, ordered by bounds (04-generics.md) */
  if (rowdisjoint(a, b))
    return;
  if (a->ifort)
    cerrat(a->decl, "conflicting implementations of '%s' for %s", a->ipath->sym->name,
           tysprint1(fa));
  else
    cerrat(a->decl, "conflicting inherent impls for %s", tysprint1(fa));
}

/* -- pass 1 ------------------------------------------------------------- */

/* every item's Sym, parallel to the items; NULL for use and trait
 * items, which declare nothing pass 2 resolves. Per file now: a
 * project holds one each (12-projects.md) */
Sym **
declare(Ast **items, Ns *ns)
{
  usize i, n = vlen(items);
  Sym **syms = vnew(Sym *, n ? n : 1);

  for (i = 0; i < n; i++) {
    Ast        *it = items[i];
    const char *name = 0;
    int         kind = Snone;
    Ast       **gps = 0;
    usize       ngps = 0;
    Sym        *s = 0;

    switch (it->k) {
    case Nfn:
      name = it->v.fn.name;
      kind = Sfn;
      gps = it->v.fn.gparams;
      break;
    case Nstruct:
    case Nunion:
      name = it->v.ty.name;
      kind = Stype;
      gps = it->v.ty.gparams;
      break;
    case Nenum:
      name = it->v.en.name;
      kind = Stype;
      gps = it->v.en.gparams;
      break;
    case Ntypedef:
      name = it->v.td.name;
      kind = Stype;
      gps = it->v.td.gparams;
      break;
    case Ntrait:
      name = it->v.ty.name;
      kind = Strait;
      gps = it->v.ty.gparams;
      break;
    case Nconst:
      name = it->v.cst.name;
      kind = Sconst;
      break;
    case Nstatic:
      name = it->v.cst.name;
      kind = Sstatic;
      break;
    case Nimpl: /* nameless: only the pass-3 list holds it */
      s = arenaalloc(sizeof *s);
      memset(s, 0, sizeof *s);
      s->kind = Simpl;
      s->ownns = ns; /* the file's own: a method's mangle carries it */
      s->decl = it;
      s->gparams = it->v.impl.gparams; /* the head's own: the method
                                        * gate reads them (04) */
      s->ngparams = vlen(it->v.impl.gparams);
      break;
    default: /* Nuse: collected below, its bindings made once every
              * name is declared (11-namespaces.md) */
      break;
    }
    if (kind != Snone) {
      ngps = vlen(gps);
      if (prim(name))
        cerrat(it, "'%s' names a scalar type and cannot be declared", name);
      s = nsdecl(ns, name, kind, it, gps, ngps);
      if (!s)
        cerrat(it, "'%s' is declared twice", name);
      s->pub = it->pub; /* visible outside its namespace, a use's
                         * question (11-namespaces.md) */
      if (kind == Sfn)
        s->noreturn = attrfind(it->attrs, "noreturn") != 0; /* the
                                                             * diverge judgement's own mark
                                                             * (10-iteration.md) */
      if (kind == Stype)
        s->tykind = it->k == Nstruct  ? TYstruct
                    : it->k == Nunion ? TYunion
                    : it->k == Nenum  ? TYenum
                                      : TYalias;
      { /* a #[cfg]'s mode words on a fn, read where the name is made:
         * the modes that remove its calls make what follows them
         * reachable, a shape #[noreturn]'s own say contradicts
         * (01-types.md, Mode-gated functions). The words themselves
         * are the cull's own checked ones (12-projects.md), and on
         * every item but a fn they are that cull's alone -- a type or
         * an impl absent in the modes it does not name, its uses the
         * unknown names any absent thing's are */
        if (it->k == Nfn && s->noreturn && declmodes(it))
          cerrat(it, "#[cfg] and #[noreturn] share no fn: the modes that remove"
                     " its calls make what follows them reachable (01-types.md)");
      }
    }
    vappend(&syms, &s);
  }
  return syms;
}

/* -- the driver ---------------------------------------------------------- */

/* pass 3's impl table, read by pass 4 (sym.h) */
Sym **chk_impls;
usize chk_nimpls;
int   chk_rel; /* the build's own mode: -r's word, every mode door's say */

void
checkinit(void)
{
  syminit(); /* the tree, empty: the sugar's four and std's own files
              * arrive through the walks -- Option, Result, Copy, Drop
              * from the sysroot's source, taken back after pass 1
              * (12-projects.md) */
  chk_impls = 0;
  chk_nimpls = 0;
}

/* a declaration's own defaults, read once where they are declared
 * (04-generics.md): a default names the parameters before it -- and
 * Self, in a trait. A fn's parameters come from its arguments and
 * an impl's from the trait it implements: neither carries one. */
static void
checkdefaults(Sym *s)
{
  Ast **gps = s->gparams;
  usize n = s->ngparams, i, j;

  if (s->kind == Sfn || s->kind == Simpl) {
    for (i = 0; i < n; i++)
      if (gps[i]->v.gp.dflt)
        cerrat(gps[i]->v.gp.dflt, "%s '%s' carries no default: %s",
               s->kind == Sfn ? "a fn" : "an impl", s->name,
               s->kind == Sfn ? "its parameters come from its arguments"
                              : "it repeats the trait's shape");
    return;
  }
  for (i = 0; i < n; i++) {
    Ast *d = gps[i]->v.gp.dflt;

    if (d) { /* the default sees only the parameters before it */
      Env env = envnone();

      for (j = 0; j < i; j++)
        env = envpush(&env, gps[j]->v.gp.name, typaram(gps[j]));
      if (s->kind == Strait)
        env = envpush(&env, "Self", selfty());
      rty(d, &env);
    }
  }
}

/* pass 2: what each declaration is. Runs after the file's uses bind
 * in checkproject -- a const's own type may read one
 * (11-namespaces.md) */
void
resolveitems(Ast **items, Sym **syms)
{
  usize i, n = vlen(items);

  for (i = 0; i < n; i++) {
    Ast *it = items[i];
    Sym *s = syms[i];

    if (!s)
      continue;
    checkdefaults(s);
    switch (it->k) {
    case Nfn:
      resolvebounds(it, s); /* the signature may read its own bounds
                             * -- T::Item names the trait that carries
                             * it (04-generics.md) */
      resolvefn(s);
      break;
    case Nstruct:
    case Nunion:
      resolvebounds(it, s);
      resolvestruct(s);
      break;
    case Nenum:
      resolvebounds(it, s);
      resolveenum(s);
      break;
    case Ntypedef:
      resolvebounds(it, s);
      aliastarget(s);
      break;
    case Nconst:
    case Nstatic: {
      Env env = envnone();

      s->cty = rty(it->v.cst.t, &env);
      cevalsym(s); /* the initializer is compile-time known, or it is
                    * not a const (01-types.md); a static's first
                    * value is too, its storage is runtime */
      break;
    }
    case Ntrait: /* the members' signatures are pass 3's, possibly a
                  * lazy read under whoever asked -- their bounds
                  * read here, in the trait's own file (04) */
      resolvebounds(it, s);
      break;
    case Nimpl:
      resolveimpl(s); /* the head first: the members' bounds read
                       * its Self and the trait's arguments */
      resolvebounds(it, s);
      break;
    default: /* use items: namespaces are their own feature */
      break;
    }
  }
}

/* -- the uses ------------------------------------------------------------
 * A `use`'s own resolution (11-namespaces.md): the path walked from
 * the root, its last segment the item or the namespace itself, the
 * binding made in the use environment. Every name is declared by
 * now -- a collision reads the whole file, order-free. There is no
 * renaming: the way out of one is the full path. */

static Ns *
usewalk(Ast *it, Ast **segs, usize nsegs, char **last) /* the path's
                                                        * namespaces
                                                        * walked, the
                                                        * last segment
                                                        * left */
{
  Ns   *ns = nsroot();
  usize i;

  for (i = 0; i + 1 < nsegs; i++) {
    Ns *sub = nschild(ns, segs[i]->v.seg.name);

    if (!sub)
      cerrat(it, "no namespace '%s' in %s (11-namespaces.md)", segs[i]->v.seg.name,
             i ? nsname(ns) : "the root");
    ns = sub;
  }
  *last = segs[nsegs - 1]->v.seg.name;
  return ns;
}

static void
resolveuse1(Ast *it, Ast **head, usize nhead, int pub, Ns *home) /* one use
                                                                  * tree, its parent's path
                                                                  * carried in, pub the tree's
                                                                  * own flag, home the
                                                                  * namespace a pub use binds
                                                                  * -- its file's (11) */
{
  Ast **segs = it->v.use.path->v.path.segs;
  usize nsegs = vlen(segs), nfull = nhead + nsegs, i;

  { /* the whole path: the parent's segments, then this tree's own */
    Ast **full = vnew(Ast *, nfull ? nfull : 1);

    for (i = 0; i < nhead; i++)
      vappend(&full, &head[i]);
    for (i = 0; i < nsegs; i++)
      vappend(&full, &segs[i]);
    segs = full;
    nsegs = nfull;
  }
  if (it->v.use.star) { /* the glob: every pub item of the namespace,
                         * all or nothing -- a name colliding takes
                         * the whole use down (11-namespaces.md) */
    Ns   *ns = nsroot();
    Sym **all;
    usize nall, j;

    if (pub)
      cerrat(it, "a pub use names items, not a glob (11-namespaces.md)");
    if (nsegs < 1)
      cerrat(it, "a use's path is absolute (11-namespaces.md)");
    for (j = 0; j < nsegs; j++) { /* the whole path one namespace
                                   * walk: the star rides the ns
                                   * itself, no last segment left */
      Ns *sub = nschild(ns, segs[j]->v.seg.name);

      if (!sub)
        cerrat(it, "no namespace '%s' in %s (11-namespaces.md)", segs[j]->v.seg.name,
               j ? nsname(ns) : "the root");
      ns = sub;
    }
    all = nstable(ns, &nall);
    for (i = 0; i < nall; i++) {
      Sym *s = all[i];

      if (!s->pub)
        continue;
      if (nsitem(home, s->name) || nsreexpfind(home, s->name))
        cerrat(it, "'%s' is already declared; the glob cannot bring it in (11-namespaces.md)",
               s->name);
      if (usebind(s->name, s, 0, it))
        cerrat(it, "'%s' is brought in twice (11-namespaces.md)", s->name);
    }
    for (i = 0; i < vlen(ns->reexp); i++) { /* the re-exports ride
                                             * the glob, every one a
                                             * pub use made -- the
                                             * target bound under its
                                             * name (11-namespaces.md) */
      Rexp *r = ns->reexp[i];

      if (nsitem(home, r->name) || nsreexpfind(home, r->name))
        cerrat(it, "'%s' is already declared; the glob cannot bring it in (11-namespaces.md)",
               r->name);
      if (usebind(r->name, r->target, 0, it))
        cerrat(it, "'%s' is brought in twice (11-namespaces.md)", r->name);
    }
    return;
  }
  if (vlen(it->v.use.subs)) { /* the brace tree: each sub its own use,
                               * the parent's path carried in */
    usize nsub = vlen(it->v.use.subs);

    for (i = 0; i < nsub; i++)
      resolveuse1(it->v.use.subs[i], segs, nsegs, pub, home);
    return;
  }
  { /* the one item, or the namespace itself */
    char *nm;
    Ns   *ns = usewalk(it, segs, nsegs, &nm);
    Ns   *asns = nschild(ns, nm); /* the namespace itself: a path's
                                   * head names it after (below) */

    if (asns) {
      if (pub) /* the namespace is not an item: nothing to re-export */
        cerrat(it, "a pub use re-exports an item, not a namespace (11-namespaces.md)");
      if (nsitem(home, nm) || nsreexpfind(home, nm))
        cerrat(it, "'%s' is already declared; reach it by its path (11-namespaces.md)", nm);
      if (usebind(nm, 0, asns, it))
        cerrat(it, "'%s' is brought in twice (11-namespaces.md)", nm);
      return;
    }
    {
      Sym *s = nsitem(ns, nm);
      int  via = 0; /* the target reached through a re-export: the
                     * use reads the re-export's own target, a pub use
                     * may not point at one (11-namespaces.md) */

      if (!s) {
        s = nsreexpfind(ns, nm);
        if (s)
          via = 1;
        else
          cerrat(it, "no '%s' in %s (11-namespaces.md)", nm, nsname(ns));
      }
      if (pub && via)
        cerrat(it,
               "a re-export points at an item, not another re-export"
               " -- '%s' in %s is one (11-namespaces.md)",
               nm, nsname(ns));
      if (!via && !s->pub)
        cerrat(it, "'%s' is private to %s (11-namespaces.md)", nm, nsname(ns));
      if (pub) { /* the namespace binding: the name taken whole -- an
                  * fn does not chain onto a re-export (04) */
        if (nsitem(home, nm) || nsreexpfind(home, nm))
          cerrat(it, "'%s' is already in %s; a pub use cannot re-export it (11-namespaces.md)", nm,
                 nsname(home));
        nsreexp(home, nm, s, it);
      } else if (nsitem(home, nm) || nsreexpfind(home, nm))
        cerrat(it, "'%s' is already declared; reach it by its path (11-namespaces.md)", nm);
      if (usebind(nm, s, 0, it)) /* the file's own binding, pub or not */
        cerrat(it, "'%s' is brought in twice (11-namespaces.md)", nm);
    }
  }
}

/* the prelude: std's flat pub face, bound into every file after its
 * own uses -- the injected names yield to everything declared
 * (the tables the bare name reads first) and everything a use brought
 * in (a binding made first is kept, usebind's own rule). std's own
 * files read the face like anyone's: the declares-all pass made it
 * whole before any read, and a face file's own names meet it at its
 * table first anyway. The variants Some/None/Ok/Err ride their sugar,
 * not a use (01-types.md); std::meta is not in the face, a glob is
 * not recursive (11-namespaces.md) -- the reflection model is opted
 * into by its own use */
static void
injectstd(void)
{
  Ns   *ns = nschild(nsroot(), "std");
  Sym **all;
  usize nall, i;

  if (!ns) /* stdwalk made it; nothing to inject without it */
    return;
  all = nstable(ns, &nall);
  for (i = 0; i < nall; i++) {
    Sym *s = all[i];

    if (s->pub)
      usebind(s->name, s, 0, 0); /* a held name kept: the yield */
  }
  for (i = 0; i < vlen(ns->reexp); i++)
    usebind(ns->reexp[i]->name, ns->reexp[i]->target, 0, 0);
}

/* a file's own context, entered whole: the namespace, the uses, the
 * path a diagnostic names -- and, on the first entry, the file's
 * plain uses bound and the prelude injected after them. The pass-2
 * walk arrives here file by file; a lazy resolve may switch to a
 * file the walk has not reached yet -- the names read there are the
 * file's own, and they read whole (12-projects.md). The pub uses
 * bound before any of this, their own whole-project pass */
static void
filectx(Srcfile *sf)
{
  usize j, m = vlen(sf->items);

  nscur(sf->ns);
  usecur(sf->uses);
  lexsetpath(sf->path);
  if (sf->ctxdone)
    return;
  sf->ctxdone = 1;
  for (j = 0; j < m; j++)
    if (sf->items[j]->k == Nuse && !sf->items[j]->pub)
      resolveuse1(sf->items[j], 0, 0, 0, sf->ns);
  injectstd(); /* the prelude, after the file's own uses: the
                * injected names yield to them (12-projects.md) */
}

/* a #[test] fn's own shape, the main's rules over again
 * (13-testing.md): no arguments, no generic parameters, a body to
 * run, and the return one of the two endings a runner's entry
 * answers -- () or E?(). The attribute's own argument is a
 * description, one string. A method never reaches this walk: the
 * impl's member table holds it, not the namespace's, and the impls'
 * pass turned a #[test] method away at its own door. */
static void
checktests(Ns *ns)
{
  Sym **tbl;
  usize n, i;

  tbl = nstable(ns, &n); /* the table is hashed: the walk's own
                          * order, a glob's (11-namespaces.md) */
  for (i = 0; i < n; i++) {
    Sym  *s = tbl[i];
    Ast  *at;
    Type *rt;

    if (s->kind != Sfn || !s->decl) /* a prelude fn carries no
                                     * declaring node, no attribute
                                     * either */
      continue;
    at = attrfind(s->decl->attrs, "test");
    if (!at)
      continue;
    if (declmodes(s->decl)) /* the test build is the debug shape,
                             * the one mode a runner knows: a
                             * mode-gated test has no mode to run
                             * in (13-testing.md) */
      cerrat(s->decl, "a test is never mode-gated: the test build is the debug"
                      " shape, one mode only (13-testing.md)");
    if (s->ngparams)
      cerrat(s->decl, "a test takes no generic parameters -- no call site"
                      " picks them, the runner alone calls (13-testing.md)");
    if (vlen(s->decl->v.fn.params))
      cerrat(s->decl, "a test takes no arguments -- the runner calls with"
                      " none (13-testing.md)");
    if (!s->decl->v.fn.body)
      cerrat(s->decl, "a test needs a body to run (13-testing.md)");
    if (at->v.seg.args && (vlen(at->v.seg.args) > 1 || at->v.seg.args[0]->k != Nstr))
      cerrat(at, "#[test] takes one string, the report's description"
                 " (13-testing.md)");
    rt = fnsigof(s)->t; /* the lazy read, the main's own walk */
    if (!(rt->k == Tyunit || (rt->k == Tyenum && rt->sym == sym_result)))
      cerrat(s->decl, "a test returns () or E?() -- the runner answers both"
                      " endings, no other (13-testing.md)");
  }
  for (i = 0; i < vlen(ns->subs); i++)
    checktests(ns->subs[i]);
}

/* the test's own ends, past the impls pass 3 took -- the main's own
 * two checks, a test's shape over: an E?() hands its Err to the
 * runner, and the runner prints it through the error type's own
 * Fmt, no impl no print, said where the ending is; and #[extern(C)]
 * is for the fns that cross to C, while a test's call the runner
 * itself arranges (13-testing.md). */
static void
checktestslate(Ns *ns)
{
  Sym **tbl;
  usize n, i;

  tbl = nstable(ns, &n);
  for (i = 0; i < n; i++) {
    Sym  *s = tbl[i];
    Type *rt;

    if (s->kind != Sfn || !s->decl /* the prelude's own: no node, no
                                     * attribute */)
      continue;
    if (!attrfind(s->decl->attrs, "test"))
      continue;
    nscur(s->ownsf->ns); /* the file's own context: the bound's own
                          * walk reads its names (04-generics.md) */
    usecur(s->ownsf->uses);
    lexsetpath(s->ownsf->path);
    rt = fnsigof(s)->t;
    if (rt->k == Tyenum && rt->sym == sym_result && !implfor(sym_fmt, rt->args[1], 0))
      cerrat(s->decl, "the error type does not implement Fmt -- the Err half"
                      " prints through it (13-testing.md)");
    if (attrfind(s->decl->attrs, "extern"))
      cerrat(s->decl, "#[extern(C)] is for the fns that cross to C; a test's"
                      " call the runner arranges (13-testing.md)");
  }
  for (i = 0; i < vlen(ns->subs); i++)
    checktestslate(ns->subs[i]);
}

/* the project's four passes, a file at a time where a file's own
 * matters (12-projects.md): every name declared across the whole
 * project first -- cross-file reads are the point -- then each
 * file's uses bound and its declarations resolved in its own
 * context, the impl table built for all, and the bodies checked
 * back in their files. A single-file compilation is the degenerate
 * shape: one Srcfile, the root's. The sysroot's own files check
 * like any file, and the prelude is injected into them like
 * anyone's -- a library reads its own face, the declares-all pass
 * having made it whole before any read (12-projects.md). */
void
checkproject(Srcfile **files, usize nfiles)
{
  usize     i, f, nimpls;
  Sym     **impls;
  Srcfile **implsf; /* each impl's file, its coherence errors named
                     * there and its names read in its own context:
                     * the diagnostics follow the table, not
                     * whichever file the checker served last */

  cfgcull(files, nfiles); /* pass zero: the platform's own items kept,
                           * the rest never declared (12-projects.md) */

  /* pass 1: every file's every name, each into its own namespace --
   * a file may read a name another declared before any use binds or
   * any type resolves (12-projects.md). A file's uses are its own
   * from here on: usenew'd beside the Syms, switched to below. */
  for (f = 0; f < nfiles; f++) {
    Srcfile *sf = files[f];

    lexsetpath(sf->path);
    sf->uses = usenew();
    sf->syms = declare(sf->items, sf->ns);
    { /* each Sym its own file: a generic's body re-checks per
       * instantiation, and the emitter switches to this file's
       * context for the walk -- the names the body reads are the
       * ones this file bound (04-generics.md) */
      usize j, m = vlen(sf->items);

      for (j = 0; j < m; j++)
        if (sf->syms[j])
          sf->syms[j]->ownsf = sf;
    }
    { /* main is the project's own fn: the root's, nowhere else
       * (12-projects.md). Two in one namespace declare-errored
       * already; this is the one namespace it may live in. */
      usize j, m = vlen(sf->items);

      for (j = 0; j < m; j++)
        if (sf->items[j]->k == Nfn && strcmp(sf->items[j]->v.fn.name, "main") == 0 &&
            sf->ns != nsroot())
          cerrat(sf->items[j], "main lives in the project's root (12-projects.md)");
    }
  }

  { /* std's own face, taken back from the tree the walks filled: the
     * language's citizens -- Option and Result, every ?T and every
     * E?T reading them by pointer (01, 03, 05) -- std::meta's
     * TypeInfo, what every @typeinfo answers with
     * (08-reflection.md), and std's panic, the door every runtime
     * check fails into (01-types.md). A sysroot without one of them
     * is a broken one -- said here, not at the first sugar */
    Ns *std = nsopen("std");

    sym_option = nsitem(std, "Option");
    sym_result = nsitem(std, "Result");
    sym_typeinfo = nsitem(nsopen("std::meta"), "TypeInfo");
    sym_panic = nsitem(std, "panic");
    /* the entry fns, one per ending a main has, private to std: the
     * wrapper alone calls them, the face-taking here the one door in
     * (12-projects.md, 11-namespaces.md) */
    sym_entry_unit = nsitem(std, "run_unit");
    sym_entry_i32 = nsitem(std, "run_i32");
    sym_entry_err = nsitem(std, "run_err");
    sym_exit = nsitem(std, "exit"); /* the ending an E?() main has,
                                     * run_err's own arm, a program
                                     * free to call it itself
                                     * (entry.ce, 12-projects.md) */
    /* the Err half's own words: an E?() main's error type is
     * checked against it where the ending is declared
     * (12-projects.md) */
    sym_fmt = nsitem(nsopen("std::fmt"), "Fmt");
    { /* the operator traits, the sugar's own (07-operators.md), and
       * the two the compiler calls on its own -- Copy at a move,
       * Drop at a scope's end (03-move.md): the rewrite spells the
       * operators' paths, so those names never enter a scope, and
       * the two ride no prelude either -- a file that impls one
       * names it (12-projects.md) */
      static const char *const ops[] = {
          "Add", "Sub", "Mul",       "Div",       "Rem", "BitAnd", "BitOr",    "BitXor", "Shl",
          "Shr", "Neg", "ShlAssign", "ShrAssign", "Ord", "Eq",     "Ordering", "Copy",   "Drop"};
      Ns   *ons = nsopen("std::ops");
      usize oi;

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
        !sym_fnmut || !sym_fnonce) {
      fprintf(stderr, "cerium: the standard library is incomplete: Option, Result, Copy, Drop,"
                      " meta::TypeInfo, panic, fmt's Fmt, exit, entry's three runs, ops' Fn"
                      " family -- one is missing from the sysroot (12-projects.md)\n");
      exit(1);
    }
  }

  /* the pub uses first, every file's: a re-export is a namespace
   * declaration, order-free like any other -- one file's use reads
   * another's pub use whatever order the walk served them in
   * (11-namespaces.md) */
  for (f = 0; f < nfiles; f++) {
    Srcfile *sf = files[f];
    usize    j, m = vlen(sf->items);

    nscur(sf->ns);
    usecur(sf->uses);
    lexsetpath(sf->path);
    for (j = 0; j < m; j++)
      if (sf->items[j]->k == Nuse && sf->items[j]->pub)
        resolveuse1(sf->items[j], 0, 0, 1, sf->ns);
  }

  /* then the plain uses, file by file -- A's bindings are its own,
   * B reads none of them (11-namespaces.md) -- and pass 2 in the
   * same per-file context: a const's own type may read one */
  for (f = 0; f < nfiles; f++) {
    Srcfile *sf = files[f];

    filectx(sf); /* the uses and the prelude bound here on the
                  * file's first entry -- a lazy read that switched
                  * to it earlier found them whole already */
    resolveitems(sf->items, sf->syms);
  }
  { /* main's own return, resolved now: (), the i32 the exit code
     * is -- the platform's own word, the only one the parent reads
     * -- or E?() -- the program's end follows it
     * (12-projects.md). The Err half's reflection print is a later
     * milestone's; the shapes are taken now. */
    Sym *m = nsitem(nsroot(), "main");

    if (m && m->kind == Sfn) {
      Type *rt = fnsigof(m)->t; /* the lazy read: a forward reference
                                 * met it already, resolveitems just
                                 * did, either way the same answer */

      if (rt->k == Tyunit || (rt->k == Tyint && rt->num == IN_I32) ||
          (rt->k == Tyenum && rt->sym == sym_result))
        ;
      else
        cerrat(m->decl, "main returns (), i32, or E?() -- the exit code is an i32, the platform's"
                        " own word (12-projects.md)");
      if (declmodes(m->decl))
        cerrat(m->decl, "main is every mode's door: #[cfg] cannot hold it away"
                        " (12-projects.md)");
    }
  }
  checktests(nsroot()); /* every #[test] fn's own shape, the main's
                         * rules a shape over (13-testing.md) */

  /* pass 3: traits and impls, then coherence. The bounds check runs
   * first so a bound nobody overlaps against still gets diagnosed.
   * std's impls ride here with the project's own -- its files came
   * first, the reads pick through them, and the coherence below
   * orders both kinds. */
  impls = vnew(Sym *, 8);
  implsf = vnew(Srcfile *, 8); /* the file each impl came from: the
                                * walks below read names, and a name
                                * reads its file's context -- the
                                * namespace, its uses, the path a
                                * diagnostic prints (11) */
  for (f = 0; f < nfiles; f++) {
    Srcfile *sf = files[f];
    usize    j, m = vlen(sf->items);

    filectx(sf); /* members resolve in the impl's own context: a
                  * signature's types read its file's uses too */
    for (j = 0; j < m; j++) {
      Ast *it = sf->items[j];
      Sym *s = sf->syms[j];

      if (!s)
        continue;
      if (it->k == Ntrait)
        resolvetrait(s);
      if (it->k == Nimpl) {
        resolveimplmembers(s);
        { /* the members' fns the same file's own: their bodies
           * re-check per instance under this file's context, the
           * free fns' own rule above (04-generics.md) */
          usize k;

          for (k = 0; k < s->nmembers; k++)
            if (s->members[k].sym)
              s->members[k].sym->ownsf = sf;
        }
        { /* a method is no test's shape: the runner calls a fn by
           * address with the platform's own two, and a method's
           * self is not among what it hands (13-testing.md). Nor
           * does the cull walk an impl's rows -- a method's mode
           * words would sit unread, so the hand is stopped here;
           * an impl's own #[cfg] is its own cull, the whole table
           * gone in the modes it does not name (12-projects.md) */
          usize k;

          for (k = 0; k < s->nmembers; k++)
            if (s->members[k].kind == Mfn && s->members[k].decl) {
              if (attrfind(s->members[k].decl->attrs, "test"))
                cerrat(s->members[k].decl, "a test is a free fn -- a method's self"
                                           " the runner never hands (13-testing.md)");
              if (declmodes(s->members[k].decl))
                cerrat(s->members[k].decl, "a method's #[cfg] is its impl's own:"
                                           " gate the impl, whole, not one row"
                                           " (12-projects.md)");
            }
        }
        vappend(&impls, &s);
        vappend(&implsf, &sf);
      }
    }
  }
  chk_impls = impls; /* pass 4 reads this (body.c) */
  chk_nimpls = vlen(impls);
  nimpls = vlen(impls);
  for (i = 0; i < nimpls; i++) {
    Sym  *bs[16];
    usize nb;

    nscur(implsf[i]->ns);
    usecur(implsf[i]->uses);
    lexsetpath(implsf[i]->path);
    nb = collectbounds(impls[i], bs); /* the diagnostic is the point */
    {                                 /* the cache the specificity walks read: the emitter's picks
                                       * run in the caller's context, and a bound's name resolves
                                       * in the file that wrote it (04-generics.md) */
      impls[i]->ibounds = arenaalloc(nb * sizeof *impls[i]->ibounds);
      memcpy(impls[i]->ibounds, bs, nb * sizeof *bs);
      impls[i]->nibounds = nb;
    }
  }
  for (i = 0; i < nimpls; i++) {
    nscur(implsf[i]->ns);
    usecur(implsf[i]->uses);
    lexsetpath(implsf[i]->path);
    if (impls[i]->ifort)
      checkimplcomplete(impls[i]);
  }
  for (i = 0; i < nimpls; i++) {
    usize j;

    nscur(implsf[i]->ns);
    usecur(implsf[i]->uses);
    lexsetpath(implsf[i]->path);
    for (j = 0; j < i; j++)
      checkoverlap(impls[i], impls[j]);
  }

  { /* main's own ends, past the shapes pass 2 took: an E?() hands
     * its Err to the platform, and the platform prints it through
     * the error type's own Fmt -- no impl, no print, said where
     * the ending is declared (12-projects.md). And the door itself
     * is the compiler's to arrange: #[extern(C)] on a main would
     * take the wrapper's own C name, and one program cannot hold
     * two doors */
    Sym *m = nsitem(nsroot(), "main");

    if (m && m->kind == Sfn) {
      Type *rt = fnsigof(m)->t; /* pass 2's lazy read answered it
                                 * already; the same answer */

      nscur(m->ownsf->ns); /* the file's own context: a diagnostic
                            * says where the ending is, and the
                            * walk above left std's own behind
                            * (11-namespaces.md) */
      usecur(m->ownsf->uses);
      lexsetpath(m->ownsf->path);
      if (attrfind(m->decl->attrs, "extern"))
        cerrat(m->decl, "#[extern(C)] is for the fns that cross to C; main's door the "
                        "compiler arranges (12-projects.md)");
      if (rt->k == Tyenum && rt->sym == sym_result && !implfor(sym_fmt, rt->args[1], 0))
        cerrat(m->decl, "the error type does not implement Fmt -- the Err half prints through"
                        " it (12-projects.md)");
    }
  }
  checktestslate(nsroot()); /* every test's own ends, the impl table
                             * built -- the main's two checks, a
                             * test's shape over (13-testing.md) */

  /* pass 4: fn bodies, against the impl table pass 3 just built --
   * each file in its own context again, the same switch. std's panic
   * is a body like any, its file one of the walks (12-projects.md) */
  for (f = 0; f < nfiles; f++) {
    Srcfile *sf = files[f];
    usize    j, m = vlen(sf->items);

    nscur(sf->ns);
    usecur(sf->uses);
    lexsetpath(sf->path);
    for (j = 0; j < m; j++) {
      if (!sf->syms[j])
        continue;
      if (sf->items[j]->k == Nfn && sf->items[j]->v.fn.body) {
        if (modegated(sf->syms[j], chk_rel))
          continue; /* a gated fn this build holds away is not
                     * here: its body neither checked nor emitted --
                     * it exists only in the modes its words name
                     * (01-types.md, Mode-gated functions) */
        checkbodyfn(sf->syms[j], sf->items[j]);
      }
      if (sf->items[j]->k == Nimpl)
        checkbodyimpl(sf->syms[j], sf->items[j]);
    }
  }
}

/* -- the -T dump --------------------------------------------------------- */

static void
ind(int n)
{
  while (n-- > 0)
    putchar(' ');
}

/* one item a line, children indented two -- the -a shape */
static void
dumpmembers(Sym *s, int i)
{
  usize j;

  for (j = 0; j < s->nmembers; j++) {
    Member *m = &s->members[j];

    printf("\n");
    ind(i + 2);
    switch (m->kind) {
    case Mfn:
      printf("(fn %s ", m->name);
      tyfmt(m->ty);
      printf(")");
      break;
    case Mtype:
      if (m->val) { /* an impl supplies the type; a trait declares it */
        printf("(type %s ", m->name);
        tyfmt(m->val);
        printf(")");
      } else {
        printf("(type %s)", m->name);
      }
      break;
    default:
      printf("(const %s ", m->name);
      tyfmt(m->ty);
      printf(")");
      break;
    }
  }
}

static void
dumpitem(Ast *it, Sym *s, int i)
{
  ind(i);
  switch (it->k) {
  case Nfn:
    printf("(fn %s ", it->v.fn.name);
    tyfmt(s->fnty);
    printf(")");
    break;
  case Nstruct:
  case Nunion: {
    usize j;

    printf("(%s %s", it->k == Nstruct ? "struct" : "union", it->v.ty.name);
    for (j = 0; j < s->nfields; j++) {
      printf("\n");
      ind(i + 2);
      printf("(field%s %s ", s->fields[j].mut ? " mut" : "", s->fields[j].name);
      tyfmt(s->fields[j].ty);
      printf(")");
    }
    printf(")");
    break;
  }
  case Nenum: {
    usize j, k;

    printf("(enum %s", it->v.en.name);
    if (s->tagty) {
      printf(" : ");
      tyfmt(s->tagty);
    }
    for (j = 0; j < s->nvariants; j++) {
      Variant *v = &s->variants[j];

      printf("\n");
      ind(i + 2);
      printf("(variant %s = %lu", v->name, (unsigned long) v->disc);
      if (v->named) {
        for (k = 0; v->fields && k < v->nfields; k++) {
          printf(" (%s %s ", v->fields[k].mut ? "field mut" : "field", v->fields[k].name);
          tyfmt(v->fields[k].ty);
          printf(")");
        }
      } else {
        for (k = 0; v->payload && k < v->npayload; k++) {
          printf(" ");
          tyfmt(v->payload[k]);
        }
      }
      printf(")");
    }
    printf(")");
    break;
  }
  case Ntypedef: {
    usize j;

    printf("(type %s", it->v.td.name);
    for (j = 0; j < s->ngparams; j++)
      printf("%s%s", j ? ", " : "<", s->gparams[j]->v.gp.name);
    if (s->ngparams)
      printf(">");
    printf(" ");
    tyfmt(s->aliasty);
    printf(")");
    break;
  }
  case Ntrait: {
    usize j;

    printf("(trait %s", it->v.ty.name);
    for (j = 0; j < s->ngparams; j++)
      printf("%s%s", j ? ", " : "<", s->gparams[j]->v.gp.name);
    if (s->ngparams)
      printf(">");
    dumpmembers(s, i);
    printf(")");
    break;
  }
  case Nconst:
  case Nstatic:
    printf("(%s%s %s ", it->k == Nstatic ? "static" : "const",
           it->k == Nstatic && it->v.cst.mut ? " mut" : "", it->v.cst.name);
    tyfmt(s->cty);
    printf(")");
    break;
  case Nimpl:
    printf("(impl ");
    tyfmt(s->ipath);
    if (s->ifort) {
      printf(" for ");
      tyfmt(s->ifort);
    }
    dumpmembers(s, i);
    printf(")");
    break;
  case Nuse: {
    Ast **segs = it->v.use.path->v.path.segs;
    usize j;

    printf("(%suse ", it->pub ? "pub " : "");
    if (it->v.use.path->v.path.root)
      printf("::");
    for (j = 0; j < vlen(segs); j++)
      printf("%s%s", j ? "::" : "", segs[j]->v.seg.name);
    printf(")");
    break;
  }
  default:
    die("internal: checkdump: unhandled kind %s", nkname(it->k));
  }
}

/* one (file ...) block per file, the -T shape: a project's blocks
 * follow the walk's order -- the same order every pass reads them
 * in, so what a golden says is what the checker saw */
void
checkdump(Srcfile **files, usize nfiles)
{
  usize f, i, n;

  for (f = 0; f < nfiles; f++) {
    n = vlen(files[f]->items);
    printf("(file");
    for (i = 0; i < n; i++) {
      putchar('\n');
      dumpitem(files[f]->items[i], files[f]->syms[i], 2);
    }
    printf(")\n");
  }
}
