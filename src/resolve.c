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
#include "check.h"
#include "die.h"
#include "eval.h"
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
 * own generics; a struct's defaults have nothing outside them. */
static Type **
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
 * for the compile-time evaluator */
static Type **
rargs(Ast *seg, Env *env, usize *np)
{
  Ast  **as = seg->v.seg.args;
  usize  n = vlen(as);
  Type **ts;
  usize  i;

  if (!n) {
    *np = 0;
    return 0;
  }
  ts = tyargs(n);
  for (i = 0; i < n; i++)
    ts[i] = rty(as[i], env); /* a $$ among them: rty's own case
                              * splices it (08-reflection.md) */
  *np = n;
  return ts;
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
      if (!s)
        cerrat(p, "unknown type '%s' in %s", name, nsname(ns));
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
    Sym  *s = symfind(nm0);

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
  args = rargs(seg, env, &nargs);
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
    {                  /* the given associated types, aligned to the trait's Mtype
                        * declaration order -- what a handle exposes of them (06) */
      Ast  **as = t->v.tdyn.assocs;
      usize  na = vlen(as), i, j;
      Type **args = 0;
      usize  nty = 0, mi;

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
      args = nty ? tyargs(nty) : 0;
      mi = 0;
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
      return tydyn(s, args, nty, t->v.tdyn.mut);
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
 * lazy form is the same answer */
Type *
fnsigof(Sym *s)
{
  if (!s->fnty)
    s->fnty = resolvefnsig(s->decl, 0);
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
  args = rargs(seg, env, &nargs);
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
 * substitutes them away. */
static void
resolvetrait(Sym *s)
{
  Ast  *it = s->decl;
  Env   env = envgparams(0, it->v.ty.gparams, vlen(it->v.ty.gparams));
  Ast **ms = it->v.ty.members;
  usize n = vlen(ms);
  usize i, j;

  if (s->traitdone)
    return; /* built early by a signature read, or already built */
  s->traitdone = 1;
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
    Type **ts = t->nargs ? tyargs(t->nargs) : 0;

    for (i = 0; i < t->nargs; i++)
      ts[i] = tsubst(t->args[i], sub);
    return tyfn(ts, t->nargs, tsubst(t->t, sub));
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
 * bound is a trait's name (04-generics.md) */
static usize
collectbounds(Sym *s, Sym **out)
{
  Ast **gps = s->decl->v.impl.gparams;
  usize n = 0, i, j;

  for (i = 0; i < vlen(gps); i++) {
    Ast **bs = gps[i]->v.gp.bounds;

    for (j = 0; j < vlen(bs); j++) {
      Ast **segs = bs[j]->v.path.segs;
      Sym  *t;

      if (vlen(segs) != 1)
        cerrat(bs[j], "a bound is a trait's name");
      t = symfind(segs[0]->v.seg.name);
      if (!t)
        cerrat(bs[j], "unknown trait '%s'", segs[0]->v.seg.name);
      if (t->kind != Strait)
        cerrat(bs[j], "a bound names a trait, and '%s' is not one", segs[0]->v.seg.name);
      if (n < 16)
        out[n++] = t;
    }
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
 * bound written twice counts twice, which only ever overstates. */
int
boundsincl(Sym *a, Sym *b)
{
  Sym  *aa[16], *bb[16];
  usize na = collectbounds(a, aa), nb = collectbounds(b, bb), i;

  for (i = 0; i < nb; i++)
    if (!boundscontain(aa, na, bb[i]))
      return 0;
  return 1;
}

/* Copy against Drop, either way round -- the exclusion table
 * (04-generics.md), with its two entries and the one row */
static int
boundsexclude(Sym *a, Sym *b)
{
  Sym  *aa[16], *bb[16];
  usize na = collectbounds(a, aa), nb = collectbounds(b, bb);

  return (boundscontain(aa, na, sym_copy) && boundscontain(bb, nb, sym_drop)) ||
         (boundscontain(aa, na, sym_drop) && boundscontain(bb, nb, sym_copy));
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
    return; /* different traits never conflict */
  ab = specializes(fa, fb);
  ba = specializes(fb, fa);
  if (ab != ba)
    return; /* strictly ordered one way or the other */
  if (boundsexclude(a, b))
    return; /* provably disjoint through the exclusion table */
  if (boundsincl(a, b) != boundsincl(b, a))
    return; /* equal shape, ordered by bounds (04-generics.md) */
  if (disjoint(fa, fb))
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
      if (kind == Stype)
        s->tykind = it->k == Nstruct  ? TYstruct
                    : it->k == Nunion ? TYunion
                    : it->k == Nenum  ? TYenum
                                      : TYalias;
    }
    vappend(&syms, &s);
  }
  return syms;
}

/* -- the driver ---------------------------------------------------------- */

/* pass 3's impl table, read by pass 4 (sym.h) */
Sym **chk_impls;
usize chk_nimpls;

/* std's own impls, collected by preludefile's tail and held here for
 * every checkfile: the user's reads pick through them too */
static Sym **std_impls;
static usize nstd_impls;

void
checkinit(void)
{
  syminit();
  prelude();
  preludefile(); /* std's embedded source: declared and resolved under
                  * the clean table, before the user's items */
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
 * (11-namespaces.md); preludefile runs it back-to-back for the
 * embedded source, whose items use nothing */
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
      resolvefn(s);
      break;
    case Nstruct:
    case Nunion:
      resolvestruct(s);
      break;
    case Nenum:
      resolveenum(s);
      break;
    case Ntypedef:
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
    case Nimpl:
      resolveimpl(s);
      break;
    default: /* use items: namespaces are their own feature */
      break;
    }
  }
}

void
collectstdimpls(Ast **items, Sym **syms) /* preludefile's tail, with
                                          * syms the embedded source's parallel
                                          * table: the impls among std's items
                                          * leave here with their members
                                          * resolved, held for checkproject's
                                          * pass-3 table -- std's is_same rides
                                          * this (05-traits.md) */
{
  usize i, n = vlen(items);

  std_impls = vnew(Sym *, 4);
  for (i = 0; i < n; i++)
    if (items[i]->k == Nimpl) {
      resolveimplmembers(syms[i]);
      vappend(&std_impls, &syms[i]);
    }
  nstd_impls = vlen(std_impls);
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
resolveuse1(Ast *it, Ast **head, usize nhead) /* one use tree, its
                                               * parent's path
                                               * carried in */
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
      if (nsitem(nsroot(), s->name))
        cerrat(it, "'%s' is already declared; the glob cannot bring it in (11-namespaces.md)",
               s->name);
      if (usebind(s->name, s, 0, it))
        cerrat(it, "'%s' is brought in twice (11-namespaces.md)", s->name);
    }
    return;
  }
  if (vlen(it->v.use.subs)) { /* the brace tree: each sub its own use,
                               * the parent's path carried in */
    usize nsub = vlen(it->v.use.subs);

    for (i = 0; i < nsub; i++)
      resolveuse1(it->v.use.subs[i], segs, nsegs);
    return;
  }
  { /* the one item, or the namespace itself */
    char *nm;
    Ns   *ns = usewalk(it, segs, nsegs, &nm);
    Ns   *asns = nschild(ns, nm); /* the namespace itself: a path's
                                   * head names it after (below) */

    if (asns) {
      if (nsitem(nsroot(), nm))
        cerrat(it, "'%s' is already declared; reach it by its path (11-namespaces.md)", nm);
      if (usebind(nm, 0, asns, it))
        cerrat(it, "'%s' is brought in twice (11-namespaces.md)", nm);
      return;
    }
    {
      Sym *s = nsitem(ns, nm);

      if (!s)
        cerrat(it, "no '%s' in %s (11-namespaces.md)", nm, nsname(ns));
      if (!s->pub)
        cerrat(it, "'%s' is private to %s (11-namespaces.md)", nm, nsname(ns));
      if (nsitem(nsroot(), nm))
        cerrat(it, "'%s' is already declared; reach it by its path (11-namespaces.md)", nm);
      if (usebind(nm, s, 0, it))
        cerrat(it, "'%s' is brought in twice (11-namespaces.md)", nm);
    }
  }
}

/* the project's four passes, a file at a time where a file's own
 * matters (12-projects.md): every name declared across the whole
 * project first -- cross-file reads are the point -- then each
 * file's uses bound and its declarations resolved in its own
 * context, the impl table built for all, and the bodies checked
 * back in their files. A single-file compilation is the degenerate
 * shape: one Srcfile, the root's. */
void
checkproject(Srcfile **files, usize nfiles)
{
  usize        i, f, nimpls;
  Sym        **impls;
  const char **implp; /* each impl's file, its coherence errors named
                       * there: the diagnostics follow the table, not
                       * whichever file the lexer served last */

  /* pass 1: every file's every name, each into its own namespace --
   * a file may read a name another declared before any use binds or
   * any type resolves (12-projects.md). A file's uses are its own
   * from here on: usenew'd beside the Syms, switched to below. */
  for (f = 0; f < nfiles; f++) {
    Srcfile *sf = files[f];

    lexsetpath(sf->path);
    sf->uses = usenew();
    sf->syms = declare(sf->items, sf->ns);
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

  /* the uses bind file by file -- A's bindings are its own, B reads
   * none of them (11-namespaces.md) -- then pass 2 in the same
   * per-file context: a const's own type may read one */
  for (f = 0; f < nfiles; f++) {
    Srcfile *sf = files[f];
    usize    j, m = vlen(sf->items);

    nscur(sf->ns);
    usecur(sf->uses);
    lexsetpath(sf->path);
    for (j = 0; j < m; j++)
      if (sf->items[j]->k == Nuse)
        resolveuse1(sf->items[j], 0, 0);
    resolveitems(sf->items, sf->syms);
  }

  /* pass 3: traits and impls, then coherence. The bounds check runs
   * first so a bound nobody overlaps against still gets diagnosed.
   * std's own enter the table ahead of the project's: the reads pick
   * through them, and the coherence below orders both kinds. */
  impls = vnew(Sym *, 8);
  implp = vnew(const char *, 8);
  {
    const char *stdp = "<std>";

    for (i = 0; i < nstd_impls; i++) {
      vappend(&impls, &std_impls[i]);
      vappend(&implp, &stdp);
    }
  }
  for (f = 0; f < nfiles; f++) {
    Srcfile *sf = files[f];
    usize    j, m = vlen(sf->items);

    nscur(sf->ns); /* members resolve in the impl's own context: a
                    * signature's types read its file's uses too */
    usecur(sf->uses);
    lexsetpath(sf->path);
    for (j = 0; j < m; j++) {
      Ast *it = sf->items[j];
      Sym *s = sf->syms[j];

      if (!s)
        continue;
      if (it->k == Ntrait)
        resolvetrait(s);
      if (it->k == Nimpl) {
        resolveimplmembers(s);
        vappend(&impls, &s);
        vappend(&implp, &sf->path);
      }
    }
  }
  chk_impls = impls; /* pass 4 reads this (body.c) */
  chk_nimpls = vlen(impls);
  nimpls = vlen(impls);
  for (i = 0; i < nimpls; i++) {
    Sym *bs[16];

    lexsetpath(implp[i]);
    collectbounds(impls[i], bs); /* the diagnostic is the point */
  }
  for (i = 0; i < nimpls; i++) {
    lexsetpath(implp[i]);
    if (impls[i]->ifort)
      checkimplcomplete(impls[i]);
  }
  for (i = 0; i < nimpls; i++) {
    usize j;

    lexsetpath(implp[i]);
    for (j = 0; j < i; j++)
      checkoverlap(impls[i], impls[j]);
  }

  /* pass 4: fn bodies, against the impl table pass 3 just built --
   * each file in its own context again, the same switch */
  for (f = 0; f < nfiles; f++) {
    Srcfile *sf = files[f];
    usize    j, m = vlen(sf->items);

    nscur(sf->ns);
    usecur(sf->uses);
    lexsetpath(sf->path);
    for (j = 0; j < m; j++) {
      if (!sf->syms[j])
        continue;
      if (sf->items[j]->k == Nfn && sf->items[j]->v.fn.body)
        checkbodyfn(sf->syms[j], sf->items[j]);
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

    printf("(use ");
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
