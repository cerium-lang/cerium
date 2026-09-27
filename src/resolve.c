/* resolve.c -- pass 1 declares every name, pass 2 resolves every type.
 *
 * The file is one namespace (11-namespaces.md): pass 1 walks the
 * items and declares each name -- a function may be overloaded
 * (04-generics.md), so same-name fn Syms chain instead of colliding
 * -- and pass 2 resolves every type a declaration carries: fields,
 * params, returns, alias targets, enum payloads, impl heads. The
 * split is what lets a field name a type declared further down the
 * file.
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
#include "lex.h"
#include "sym.h"
#include "type.h"

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

/* a binding: a generic parameter bound to itself, or an alias
 * argument bound to its type */
struct Bind
{
  char *name;
  Type *t;
};

struct Env
{
  struct Bind *b;
  usize n;
};

static Type *
envfind(struct Env *env, char *name)
{
  usize i;

  for (i = env->n; i > 0; i--) /* innermost binding first */
    if (strcmp(env->b[i - 1].name, name) == 0)
      return env->b[i - 1].t;
  return 0;
}

/* bind a declaration's own generic parameters to themselves */
static struct Env
envgparams(Ast **gps, usize n)
{
  struct Env env;
  usize i;

  env.n = n;
  env.b = n ? arenaalloc(n * sizeof *env.b) : 0;
  for (i = 0; i < n; i++) {
    env.b[i].name = gps[i]->v.gp.name;
    env.b[i].t = typaram(gps[i]);
  }
  return env;
}

static struct Env
envnone(void)
{
  struct Env env;

  env.b = 0;
  env.n = 0;
  return env;
}

/* -- type resolution --------------------------------------------------- */

static Type *rty(Ast *t, struct Env *env);
static Type *rpath(Ast *p, struct Env *env);

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
    int num;
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
  struct Env env;

  if (s->aliasty)
    return s->aliasty;
  if (s->resolving)
    cerrat(s->decl, "type alias '%s' resolves through itself", s->name);
  s->resolving = 1;
  env = envgparams(s->gparams, s->ngparams);
  s->aliasty = rty(s->decl->v.td.t, &env);
  s->resolving = 0;
  return s->aliasty;
}

/* instantiate an alias with the given arguments. Defaults fill the
 * tail, and a default may name the parameters before it. */
static Type *
aliasinst(Sym *s, Type **args, usize nargs, Ast *at)
{
  struct Env env;
  usize i, n = s->ngparams;
  Type **all;
  Ast *target;

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

/* the generic arguments of a path segment, resolved; $$ and ^^ wait
 * for the compile-time evaluator */
static Type **
rargs(Ast *seg, struct Env *env, usize *np)
{
  Ast **as = seg->v.seg.args;
  usize n = vlen(as);
  Type **ts;
  usize i;

  if (!n) {
    *np = 0;
    return 0;
  }
  ts = tyargs(n);
  for (i = 0; i < n; i++) {
    Ast *a = as[i];

    if (a->k == Nun && (a->v.un.op == Tdollar2 || a->v.un.op == Tcaret2))
      cerrat(a, "type splicing needs the compile-time evaluator (not yet)");
    ts[i] = rty(a, env);
  }
  *np = n;
  return ts;
}

/* a path in type position */
static Type *
rpath(Ast *p, struct Env *env)
{
  Ast **segs = p->v.path.segs;
  usize nsegs = vlen(segs);
  Ast *seg;
  Sym *s;
  char *name;
  Type **args;
  usize nargs;

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
  /* ::name reaches the root, which is where the one namespace
   * already is (11-namespaces.md) */
  s = symfind(name);
  if (!s)
    cerrat(p, "unknown type '%s'", name);
  if (s->kind == Sfn || s->kind == Sconst || s->kind == Sstatic)
    cerrat(p, "'%s' is not a type", name);
  args = rargs(seg, env, &nargs);
  if (s->kind == Strait) {
    if (nargs)
      cerrat(p, "a trait's own arguments are bound by its impls (not yet)");
    return tysym(s, args, nargs);
  }
  if (s->kind == Stype && s->tykind == TYalias)
    return aliasinst(s, args, nargs, p);
  if (nargs > s->ngparams)
    cerrat(p, "'%s' takes %lu type argument%s, not %lu", name, (unsigned long) s->ngparams,
           s->ngparams == 1 ? "" : "s", (unsigned long) nargs);
  /* defaults fill the missing tail, the alias rule again */
  if (nargs < s->ngparams) {
    Type **full = tyargs(s->ngparams);
    usize i;

    for (i = 0; i < nargs; i++)
      full[i] = args[i];
    for (i = nargs; i < s->ngparams; i++) {
      Ast *d = s->gparams[i]->v.gp.dflt;

      if (!d)
        cerrat(p, "missing type argument '%s'", s->gparams[i]->v.gp.name);
      full[i] = rty(d, env);
    }
    args = full;
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

static Type *
rty(Ast *t, struct Env *env)
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
  case Ntarray: {
    Type *elem = rty(t->v.arrlit.t, env);
    Ast *len = t->v.arrlit.len;

    if (t->v.arrlit.mut)
      elem = tymut(elem);
    if (!len)
      return tyslice(elem);
    if (len->k == Nint)
      return tyarray(len->v.i.num, elem);
    { /* [N]T: the length names a const parameter */
      char *name = len->v.path.segs[0]->v.seg.name;
      Type *p = envfind(env, name);

      if (!p)
        cerrat(len, "unknown name '%s' as an array length", name);
      if (p->k != Typaram || !p->gp->v.gp.cnst)
        cerrat(len, "'%s' is not a const parameter", name);
      return tyarrayp(p->gp, elem);
    }
  }
  case Nttuple: {
    usize n = vlen(t->v.list.ts);
    Type **ts = n ? tyargs(n) : 0;
    usize i;

    for (i = 0; i < n; i++)
      ts[i] = rty(t->v.list.ts[i], env);
    return tytuple(ts, n);
  }
  case Ntfn: {
    usize n = vlen(t->v.fnty.args);
    Type **ts = n ? tyargs(n) : 0;
    usize i;

    for (i = 0; i < n; i++)
      ts[i] = rty(t->v.fnty.args[i], env);
    /* a missing return is () -- the two spellings intern to one */
    return tyfn(ts, n, t->v.fnty.ret ? rty(t->v.fnty.ret, env) : tyunit());
  }
  case Ntdyn: {
    Ast *path = t->v.un.e;
    Ast *seg;
    Sym *s;

    if (path->k != Npath || vlen(path->v.path.segs) != 1)
      cerrat(path, "expected a trait after dyn");
    seg = path->v.path.segs[0];
    if (seg->v.seg.args)
      cerrat(seg, "dyn with associated bindings is not checked yet");
    s = symfind(seg->v.seg.name);
    if (!s)
      cerrat(path, "unknown trait '%s'", seg->v.seg.name);
    if (s->kind != Strait)
      cerrat(path, "'%s' is not a trait", seg->v.seg.name);
    return tydyn(s, 0, 0, t->v.un.mut);
  }
  case Nttype:
    return tytype();
  case Npath:
    return rpath(t, env);
  default:
    cerrat(t, "expected a type");
  }
  return 0; /* unreachable */
}

/* -- pass 2, per declaration kind -------------------------------------- */

static void
resolvefn(Sym *s)
{
  Ast *it = s->decl;
  struct Env env = envgparams(it->v.fn.gparams, vlen(it->v.fn.gparams));
  usize n = vlen(it->v.fn.params);
  Type **ps = n ? tyargs(n) : 0;
  usize i, j;

  for (i = 0; i < n; i++) {
    Ast *p = it->v.fn.params[i];

    for (j = 0; j < i; j++)
      if (strcmp(p->v.param.name, it->v.fn.params[j]->v.param.name) == 0)
        cerrat(p, "duplicate parameter '%s'", p->v.param.name);
    ps[i] = rty(p->v.param.t, &env);
  }
  s->fnty = tyfn(ps, n, it->v.fn.ret ? rty(it->v.fn.ret, &env) : tyunit());
}

/* the fields of a struct or union, and of a named enum payload */
static struct Field *
resolvefields(Ast **fs, struct Env *env, usize n, int isunion)
{
  struct Field *fields = n ? arenaalloc(n * sizeof *fields) : 0;
  usize i, j;

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
    fields[i].ty = rty(f->v.variant.t, env);
  }
  return fields;
}

static void
resolvestruct(Sym *s)
{
  Ast *it = s->decl;
  struct Env env = envgparams(it->v.ty.gparams, vlen(it->v.ty.gparams));

  s->nfields = vlen(it->v.ty.fields);
  s->fields = resolvefields(it->v.ty.fields, &env, s->nfields, s->tykind == TYunion);
}

static void
resolveenum(Sym *s)
{
  Ast *it = s->decl;
  struct Env env = envgparams(it->v.en.gparams, vlen(it->v.en.gparams));
  usize n = vlen(it->v.en.variants);
  u64 next = 0;
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
    Ast *v = it->v.en.variants[i];
    struct Variant *dv = &s->variants[i];

    for (j = 0; j < i; j++)
      if (strcmp(v->v.variant.name, it->v.en.variants[j]->v.variant.name) == 0)
        cerrat(v, "duplicate variant '%s'", v->v.variant.name);
    dv->name = v->v.variant.name;
    dv->hasdisc = v->v.variant.hasdisc;
    dv->disc = v->v.variant.hasdisc ? v->v.variant.disc : next;
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

static void
resolveimpl(Sym *s)
{
  Ast *it = s->decl;
  struct Env env = envgparams(it->v.impl.gparams, vlen(it->v.impl.gparams));

  if (it->v.impl.fort) { /* a trait impl: the path names the trait */
    s->ipath = rpath(it->v.impl.path, &env);
    if (s->ipath->k != Tytrait)
      cerrat(it->v.impl.path, "an impl names a trait before 'for'");
    s->ifort = rty(it->v.impl.fort, &env);
  } else { /* inherent: the path is the type */
    s->ipath = rty(it->v.impl.path, &env);
  }
}

/* -- pass 1 ------------------------------------------------------------- */

/* the Syms, parallel to the items; NULL for use and trait items,
 * which declare nothing pass 2 resolves */
static Sym **syms;

static void
declare(Ast **items)
{
  usize i, n = vlen(items);

  syms = vnew(Sym *, n ? n : 1);
  for (i = 0; i < n; i++) {
    Ast *it = items[i];
    const char *name = 0;
    int kind = Snone;
    Ast **gps = 0;
    usize ngps = 0;
    Sym *s = 0;

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
      s->decl = it;
      break;
    default: /* Nuse: namespaces are their own feature */
      break;
    }
    if (kind != Snone) {
      ngps = vlen(gps);
      if (prim(name))
        cerrat(it, "'%s' names a scalar type and cannot be declared", name);
      s = symdecl(name, kind, it, gps, ngps);
      if (!s)
        cerrat(it, "'%s' is declared twice", name);
      if (kind == Stype)
        s->tykind = it->k == Nstruct  ? TYstruct
                    : it->k == Nunion ? TYunion
                    : it->k == Nenum  ? TYenum
                                      : TYalias;
    }
    vappend(&syms, &s);
  }
}

/* -- the driver ---------------------------------------------------------- */

void
checkinit(void)
{
  syminit();
  prelude();
}

void
checkfile(Ast **items)
{
  usize i, n = vlen(items);

  declare(items);
  for (i = 0; i < n; i++) {
    Ast *it = items[i];
    Sym *s = syms[i];

    if (!s)
      continue;
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
      struct Env env = envnone();

      s->cty = rty(it->v.cst.t, &env);
      break;
    }
    case Nimpl:
      resolveimpl(s);
      break;
    default: /* trait members wait for Self and the impl table */
      break;
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
      struct Variant *v = &s->variants[j];

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

void
checkdump(Ast **items)
{
  usize i, n = vlen(items);

  printf("(file");
  for (i = 0; i < n; i++) {
    putchar('\n');
    dumpitem(items[i], syms[i], 2);
  }
  printf(")\n");
}
