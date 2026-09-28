/* emit.c -- the layout tables of 02-layout.md, then the .ssa text.
 *
 * Stage 0 emits QBE's own SSA (README: codegen goes through QBE);
 * register allocation, instruction selection, and the ABI of a call
 * are qbe's to place. What is here is what only the source language
 * knows: how a struct is padded, how an enum carries its tag, and
 * the M3a pipeline itself -- a fn that returns a written or a
 * folded constant, end to end, through qbe and cc.
 *
 * The target is x86_64 SysV; one machine word is 8 bytes. The
 * emitter walks the tree the checker already accepted: it derives
 * no types, it re-reads the ones the AST's type arguments still
 * carry, and everything else arrives with the passes that grow it.
 */

#include <stdio.h>
#include <stdio.h>
#include <string.h>

#include "ast.h"
#include "check.h"
#include "emit.h"
#include "sym.h"
#include "type.h"
#include "vec.h"

/* -- layout (02-layout.md) ------------------------------------------------ */

#define WORD ((usize) 8)

static usize
alignto(usize off, usize a)
{
  return a ? (off + a - 1u) / a * a : off;
}

/* an unsigned int: picks cu* over cs*, extuw over extsw */
static int
isuintty(Type *t)
{
  return t && t->k == Tyint &&
         (t->num == IN_U8 || t->num == IN_U16 || t->num == IN_U32 || t->num == IN_U64 ||
          t->num == IN_USIZE || t->num == IN_U128);
}

/* an integer's width in bytes, its alignment with it (02-layout.md) */
static usize
intwidth(Type *t)
{
  switch (t->num) {
  case IN_I8:
  case IN_U8:
    return 1;
  case IN_I16:
  case IN_U16:
    return 2;
  case IN_I32:
  case IN_U32:
  case IN_F32:
    return 4;
  case IN_I128:
  case IN_U128:
    return 16;
  default:
    return 8; /* the 64-bit integers, f64, the isizes */
  }
}

/* a declaration's attribute, or NULL: #[name] or #[name(arg)] */
static Ast *
attrfind(Ast **attrs, const char *name)
{
  usize i;

  if (!attrs)
    return 0;
  for (i = 0; i < vlen(attrs); i++)
    if (strcmp(attrs[i]->v.seg.name, name) == 0)
      return attrs[i];
  return 0;
}

/* #[align(N)]'s N, at the declaration that carries it */
static usize
alignarg(Ast *at, Ast *decl)
{
  Ast **as = at->v.seg.args;

  if (!as || vlen(as) != 1 || as[0]->k != Nint)
    cerrat(decl, "#[align] takes one integer");
  return (usize) as[0]->v.i.num;
}

/* the layout attributes of a struct, union, or enum, checked
 * against each other: packed and align contradict (02-layout.md).
 * A prelude declaration has no node -- it carries no attributes. */
static void
layoutattrs(Ast *decl, int *packed, usize *alignk)
{
  Ast *pk = decl ? attrfind(decl->attrs, "packed") : 0;
  Ast *al = decl ? attrfind(decl->attrs, "align") : 0;

  if (pk && al)
    cerrat(decl, "#[packed] and #[align] contradict each other (02-layout.md)");
  *packed = pk != 0;
  *alignk = al ? alignarg(al, decl) : 0;
}

/* does a type hold a bit pattern it never uses? The pointer family
 * always does -- null. A ?T over one keeps the size (02-layout.md);
 * an E?T records its unit side in the other's (01-types.md). */
static int
hasniche(Type *t)
{
  return t && (t->k == Typtr || t->k == Tyvoidptr);
}

/* the tag an enum with none written gets: the smallest unsigned
 * integer that holds every discriminant (01-types.md) */
static Type *
picktag(Sym *s)
{
  u64   max = 0;
  usize i;

  for (i = 0; i < s->nvariants; i++)
    if (s->variants[i].disc > max)
      max = s->variants[i].disc;
  if (max <= 0xffu)
    return tyint(IN_U8);
  if (max <= 0xffffu)
    return tyint(IN_U16);
  if (max <= 0xffffffffu)
    return tyint(IN_U32);
  return tyint(IN_U64);
}

static usize sizeof_(Type *t);

static usize
alignof_(Type *t)
{
  usize i, al;

  if (!t)
    return 1;
  switch (t->k) {
  case Tyunit:
    return 1; /* a ZST (02-layout.md) */
  case Tybool:
    return 1;
  case Tyint:
    return intwidth(t);
  case Tyvoidptr:
  case Typtr:
  case Tyfn: /* a fn names its pointer */
    return WORD;
  case Tyslice:
  case Tydyn: /* fat: the pointer plus its length or vtable */
    return WORD;
  case Tymut: /* a slot property: no space of its own */
    return alignof_(t->t);
  case Tyarray:
    return alignof_(t->t);
  case Tytuple: /* the tuple rows of the struct table, in order */
    al = 1;
    for (i = 0; i < t->nargs; i++) {
      usize a = alignof_(t->args[i]);

      if (a > al)
        al = a;
    }
    return al;
  case Tystruct:
  case Tyunion: {
    int    packed;
    usize  alignk, fa, i;
    Field *fs = t->sym->fields;
    usize  nf = t->sym->nfields;

    layoutattrs(t->sym->decl, &packed, &alignk);
    if (packed)
      return 1;
    al = 1;
    for (i = 0; i < nf; i++) {
      Type *ft = gsubst(fs[i].ty, t->sym->gparams, t->args, t->nargs);

      fa = alignof_(ft);
      if (fa > al)
        al = fa;
    }
    if (alignk && alignk > al)
      al = alignk; /* #[align(N)] raises the whole type's */
    return al;
  }
  case Tyenum: {
    Type *tag;
    usize ta, i, j;

    if (t->sym == sym_option && t->nargs == 1 && hasniche(t->args[0]))
      return alignof_(t->args[0]);               /* the niche is the tag: no room added */
    if (t->sym == sym_result && t->nargs == 2) { /* the unit side rides
                                                  * the other's niche */
      if (t->args[0]->k == Tyunit && hasniche(t->args[1]))
        return alignof_(t->args[1]);
      if (t->args[1]->k == Tyunit && hasniche(t->args[0]))
        return alignof_(t->args[0]);
    }
    tag = t->sym->tagty ? t->sym->tagty : picktag(t->sym);
    ta = alignof_(tag);
    for (i = 0; i < t->sym->nvariants; i++) {
      Variant *v = &t->sym->variants[i];
      Type   **ps;
      usize    np, k;

      if (v->named) {
        ps = tyargs(v->nfields);
        for (k = 0; k < v->nfields; k++)
          ps[k] = v->fields[k].ty;
        np = v->nfields;
      } else {
        ps = v->payload;
        np = v->npayload;
      }
      for (j = 0; j < np; j++) {
        Type *pt = gsubst(ps[j], t->sym->gparams, t->args, t->nargs);
        usize pa = alignof_(pt);

        if (pa > ta)
          ta = pa;
      }
    }
    return ta;
  }
  case Typaram:
    cerrat(t->gp, "this type has no size until it is instantiated (04-generics.md)");
    return 1; /* unreachable */
  case Typroj:
  case Tytrait:
    cerrat(t->sym->decl, "this type has no size until it is instantiated (04-generics.md)");
    return 1; /* unreachable */
  default:
    return 1;
  }
}

static usize
sizeof_(Type *t)
{
  usize i, off, al;

  if (!t)
    return 0;
  switch (t->k) {
  case Tyunit:
    return 0; /* a ZST: no space, unit alignment */
  case Tybool:
    return 1;
  case Tyint:
    return intwidth(t);
  case Tyvoidptr:
  case Typtr:
  case Tyfn:
    return WORD;
  case Tyslice:
  case Tydyn:
    return 2 * WORD;
  case Tymut:
    return sizeof_(t->t);
  case Tyarray:
    if (t->gp)
      cerrat(t->gp, "this array's length is a const parameter: no size until it is instantiated "
                    "(04-generics.md)");
    return (usize) t->n * sizeof_(t->t);
  case Tytuple: /* declaration order, padding between, rounding at
                 * the end -- the struct rule */
    off = 0;
    al = 1;
    for (i = 0; i < t->nargs; i++) {
      usize a = alignof_(t->args[i]);

      off = alignto(off, a);
      off += sizeof_(t->args[i]);
      if (a > al)
        al = a;
    }
    return alignto(off, al);
  case Tystruct: {
    int    packed;
    usize  alignk, i;
    Field *fs = t->sym->fields;
    usize  nf = t->sym->nfields;

    layoutattrs(t->sym->decl, &packed, &alignk);
    off = 0;
    al = 1;
    for (i = 0; i < nf; i++) {
      Type *ft = gsubst(fs[i].ty, t->sym->gparams, t->args, t->nargs);

      if (!packed)
        off = alignto(off, alignof_(ft)); /* the gaps are padding */
      off += sizeof_(ft);
      if (!packed && alignof_(ft) > al)
        al = alignof_(ft);
    }
    if (alignk && alignk > al)
      al = alignk;
    return packed ? off : alignto(off, al);
  }
  case Tyunion: { /* every field at 0; the largest, rounded */
    int    packed;
    usize  alignk, i, sz;
    Field *fs = t->sym->fields;
    usize  nf = t->sym->nfields;

    layoutattrs(t->sym->decl, &packed, &alignk);
    sz = 0;
    al = 1;
    for (i = 0; i < nf; i++) {
      Type *ft = gsubst(fs[i].ty, t->sym->gparams, t->args, t->nargs);
      usize fsz = sizeof_(ft);

      if (fsz > sz)
        sz = fsz;
      if (!packed && alignof_(ft) > al)
        al = alignof_(ft);
    }
    if (alignk && alignk > al)
      al = alignk;
    return packed ? sz : alignto(sz, al);
  }
  case Tyenum: {
    int   packed;
    usize alignk, tag, pmax, pal, i, j;

    if (t->sym == sym_option && t->nargs == 1 && hasniche(t->args[0]))
      return sizeof_(t->args[0]); /* the niche is the tag */
    if (t->sym == sym_result && t->nargs == 2) {
      if (t->args[0]->k == Tyunit && hasniche(t->args[1]))
        return sizeof_(t->args[1]);
      if (t->args[1]->k == Tyunit && hasniche(t->args[0]))
        return sizeof_(t->args[0]);
    }
    layoutattrs(t->sym->decl, &packed, &alignk);
    tag = t->sym->tagty ? sizeof_(t->sym->tagty) : sizeof_(picktag(t->sym));
    pmax = 0; /* the payloads as a union: the largest, and its align */
    pal = 1;
    for (i = 0; i < t->sym->nvariants; i++) {
      Variant *v = &t->sym->variants[i];
      Type   **ps;
      usize    np, k, vsz;

      if (v->named) {
        ps = tyargs(v->nfields);
        for (k = 0; k < v->nfields; k++)
          ps[k] = v->fields[k].ty;
        np = v->nfields;
      } else {
        ps = v->payload;
        np = v->npayload;
      }
      for (j = 0, vsz = 0; j < np; j++) {
        Type *pt = gsubst(ps[j], t->sym->gparams, t->args, t->nargs);
        usize pa = alignof_(pt);

        vsz += sizeof_(pt);
        if (pa > pal)
          pal = pa;
      }
      if (vsz > pmax)
        pmax = vsz;
    }
    if (packed) /* the tag, then the payloads, no padding between */
      return tag + pmax;
    if (!pmax)
      return tag; /* no payload: exactly the tag (02-layout.md) */
    off = alignto(tag, pal);
    return alignto(off + pmax, pal);
  }
  default:
    return 0;
  }
}

/* -- the .ssa text (M3b: expressions, calls, aggregates, data) ------------- */

/* qbe's letter for a scalar. () returns a w anyway: cc's crt reads
 * one from main, and no caller of a unit fn reads anything -- a
 * zero written keeps the exit code honest. at is the node the
 * diagnostic points at. */
static int
qbety(Type *ret, Ast *at)
{
  if (!ret)
    return 0;
  if (ret->k == Tyunit)
    return 'w';
  if (ret->k == Tyint) {
    if (ret->num == IN_F32)
      return 's';
    if (ret->num == IN_F64)
      return 'd';
    return intwidth(ret) > 4 ? 'l' : 'w';
  }
  switch (ret->k) {
  case Tybool:
    return 'w';
  case Typtr:
  case Tyvoidptr:
  case Tyfn:
    return 'l';
  default:
    cerrat(at, "a value of this type arrives with a later milestone");
    return 0; /* unreachable */
  }
}

/* an aggregate lives in memory: its value, as the emitter says it,
 * is the address it sits at. Everything scalar is a qbe temporary.
 * Slices and dyns are aggregates here too -- two words in memory. */
static int
isagg(Type *t)
{
  return t && (t->k == Tystruct || t->k == Tyunion || t->k == Tyenum || t->k == Tyarray ||
               t->k == Tytuple || t->k == Tyslice || t->k == Tydyn);
}

/* a load instruction for a scalar slot: the width, and the sign
 * for the narrow ones -- the sign matters, for a w-domain compare
 * reads what the load left in the upper bits. */
static char *
ldins(Type *t)
{
  if (t->k == Tyint) {
    switch (t->num) {
    case IN_I8:
      return "loadsb";
    case IN_U8:
      return "loadub";
    case IN_I16:
      return "loadsh";
    case IN_U16:
      return "loaduh";
    case IN_I32:
      return "loadsw";
    case IN_U32:
      return "loaduw";
    case IN_F32:
      return "loads";
    case IN_F64:
      return "loadd";
    default:
      return "loadl";
    }
  }
  switch (t->k) {
  case Tybool:
    return "loadub";
  default: /* the pointers, a fn's own pointer, isize and friends */
    return "loadl";
  }
}

/* a store instruction for a scalar slot */
static char *
stins(Type *t)
{
  if (t->k == Tyint) {
    switch (t->num) {
    case IN_I8:
    case IN_U8:
      return "storeb";
    case IN_I16:
    case IN_U16:
      return "storeh";
    case IN_I32:
    case IN_U32:
      return "storew";
    case IN_F32:
      return "stores";
    case IN_F64:
      return "stored";
    default:
      return "storel";
    }
  }
  switch (t->k) {
  case Tybool:
    return "storeb";
  default:
    return "storel";
  }
}

/* a struct or union field's offset: declaration order, padding
 * between, #[packed] dropping it (02-layout.md). A union's fields
 * all sit at 0. */
static usize
foffset(Type *t, char *name, Ast *at)
{
  int    packed;
  usize  alignk, off, i;
  Field *fs;
  usize  nf;

  if (t->k == Tyslice) { /* the two named slots (01-types.md) */
    if (strcmp(name, "ptr") == 0)
      return 0;
    if (strcmp(name, "len") == 0)
      return WORD;
    cerrat(at, "a slice has 'ptr' and 'len'");
  }
  if (t->k != Tystruct && t->k != Tyunion)
    cerrat(at, "only a struct, a union, or a slice has fields");
  layoutattrs(t->sym->decl, &packed, &alignk);
  fs = t->sym->fields;
  nf = t->sym->nfields;
  off = 0;
  for (i = 0; i < nf; i++) {
    Type *ft = gsubst(fs[i].ty, t->sym->gparams, t->args, t->nargs);

    if (!packed)
      off = alignto(off, alignof_(ft));
    if (strcmp(fs[i].name, name) == 0)
      return off;
    off += sizeof_(ft);
  }
  cerrat(at, "'%s' has no field '%s'", t->sym->name, name);
  return 0; /* unreachable */
}

/* the emitter's per-fn state: temporaries, the locals a block
 * binds, and the data segments grown along the way */
typedef struct ELoc ELoc;
struct ELoc
{
  char *name; /* the binding's own name */
  char *slot; /* its storage: the alloc temporary's name */
  Type *ty;   /* what it holds */
};

typedef struct Em Em;
struct Em
{
  FILE  *o;
  usize  tmp;   /* one a temporary: %t.N */
  usize  strn;  /* one a string: $str.N */
  char **datas; /* the data lines, printed after the fns */
  ELoc  *locs;  /* the bindings in scope */
  usize  nlocs;
};

static char *
newtmp(Em *em)
{
  char *s = arenaalloc(32); /* %t. plus a u64: plenty */

  sprintf(s, "%%t.%lu", (unsigned long) ++em->tmp);
  return s;
}

/* a binding, by name; innermost last, so the search runs backwards */
static ELoc *
locfind(Em *em, char *name)
{
  usize i;

  for (i = em->nlocs; i > 0; i--)
    if (strcmp(em->locs[i - 1].name, name) == 0)
      return &em->locs[i - 1];
  return 0;
}

static void
locbind(Em *em, char *name, char *slot, Type *ty)
{
  ELoc l;

  l.name = name;
  l.slot = slot;
  l.ty = ty;
  vappend(&em->locs, &l);
  em->nlocs = vlen(em->locs);
}

/* the fn's symbol: #[extern(C)] and main keep their own name, every
 * other fn is mangled -- overloading and namespaces both force it
 * (01-types.md, External functions) */
static char *
fsymname(Sym *s, Ast *it)
{
  char *n;

  if (attrfind(it->attrs, "extern"))
    return s->name;
  if (strcmp(s->name, "main") == 0)
    return s->name;
  n = arenaalloc(strlen(s->name) + 8);
  sprintf(n, "xyz_%s", s->name);
  return n;
}

static char *emaexpr(Em *em, Ast *e);
static char *emaplace(Em *em, Ast *e);

/* a scalar's storage touched by its load or store: the address is
 * emaplace's, the temporary qbety names the domain */
static char *
emaload(Em *em, Ast *e)
{
  char *p = emaplace(em, e);
  char *t = newtmp(em);

  fprintf(em->o, "\t%s =%c %s %s\n", t, qbety(e->ty, e), ldins(e->ty), p);
  return t;
}

/* the address a place names. A local's slot is an address by
 * construction; a field rides its base's; a deref is the pointer. */
static char *
emaplace(Em *em, Ast *e)
{
  switch (e->k) {
  case Npath: {
    ELoc *l = locfind(em, e->v.path.segs[0]->v.seg.name);

    if (!l)
      cerrat(e, "'%s' is not a local here", e->v.path.segs[0]->v.seg.name);
    return l->slot;
  }
  case Naccess: {
    char *b = emaexpr(em, e->v.fld.e);
    char *t = newtmp(em);
    usize off = foffset(derefthrough(e->v.fld.e->ty), e->v.fld.name, e);

    fprintf(em->o, "\t%s =l add %s, %lu\n", t, b, (unsigned long) off);
    return t;
  }
  case Nun:
    if (e->v.un.op == Tstar)
      return emaexpr(em, e->v.un.e);
    break;
  default:
    break;
  }
  cerrat(e, "this is not a place the emitter can assign to yet");
  return 0; /* unreachable */
}

/* an expression's value: a qbe temporary, or -- for an aggregate --
 * the address it lives at */
static char *
emaexpr(Em *em, Ast *e)
{
  switch (e->k) {
  case Nint: {
    char *t = newtmp(em);

    fprintf(em->o, "\t%s =%c copy %lu\n", t, qbety(e->ty, e), (unsigned long) e->v.i.num);
    return t;
  }
  case Nflt: { /* qbe takes float immediates only in call and phi
                * arguments, so the bits go to the data segment and
                * come back with a load */
    char *base = arenaalloc(16), *t = newtmp(em);
    char  c = qbety(e->ty, e);
    char *d = arenaalloc(48);

    sprintf(base, "$flt.%lu", (unsigned long) ++em->strn);
    fprintf(em->o, "\t%s =%c load%s %s\n", t, c, c == 's' ? "s" : "d", base);
    /* 9 and 17 significant digits: the least that round-trips */
    sprintf(d, "data %s = { %c %c_%.*g }", base, c, c, c == 's' ? 9 : 17, e->v.f.flt);
    vappend(&em->datas, &d);
    return t;
  }
  case Npath: {
    char *nm = e->v.path.segs[0]->v.seg.name;
    ELoc *l = locfind(em, nm);
    Sym  *s;

    if (l)
      return isagg(l->ty) ? l->slot : emaload(em, e);
    if (vlen(e->v.path.segs) != 1)
      cerrat(e, "this name arrives with a later milestone");
    s = symfind(nm);
    if (!s)
      cerrat(e, "unknown name '%s'", nm);
    if (s->kind == Sfn) { /* a fn as a value: its address */
      char *t = newtmp(em);

      if (s->next)
        cerrat(e, "overloads as values arrive with monomorphization (M3d)");
      fprintf(em->o, "\t%s =l copy $%s\n", t, fsymname(s, s->decl));
      return t;
    }
    cerrat(e, "a value of this kind arrives with a later milestone");
    return 0; /* unreachable */
  }
  case Naccess: { /* a read: the address, then a load unless
                   * aggregate */
    char *p = emaplace(em, e);

    if (!e->ty)
      cerrat(e, "this field was never checked");
    return isagg(e->ty) ? p : emaload(em, e);
  }
  case Nun: {
    Tok   op = e->v.un.op;
    char *v, *t;

    if (op == Tamp)
      return emaplace(em, e->v.un.e); /* &place: the address itself */
    v = emaexpr(em, e->v.un.e);
    t = newtmp(em);
    if (op == Tminus)
      fprintf(em->o, "\t%s =%c neg %s\n", t, qbety(e->ty, e), v);
    else if (op == Ttilde)
      fprintf(em->o, "\t%s =%c xor %s, -1\n", t, qbety(e->ty, e), v);
    else if (op == Tbang)
      fprintf(em->o, "\t%s =w ceqw %s, 0\n", t, v);
    else if (op == Tstar) { /* *p: the pointer names the place; a
                             * scalar loads, an aggregate is the
                             * address it already is */
      if (isagg(e->ty))
        return v;
      return emaload(em, e);
    } else
      cerrat(e, "this unary operator arrives with a later milestone");
    return t;
  }
  case Nbin: {
    static struct
    {
      Tok   t;
      char *i; /* signed */
      char *u; /* unsigned, when it differs */
    } ops[] = {
        {Tplus, "add", 0},         {Tminus, "sub", 0},   {Tstar, "mul", 0}, {Tslash, "div", "udiv"},
        {Tpercent, "rem", "urem"}, {Tamp, "and", 0},     {Tbar, "or", 0},   {Tcaret, "xor", 0},
        {Tshl, "shl", 0},          {Tshr, "sar", "shr"},
    };
    Tok   op = e->v.bin.op;
    char *a = emaexpr(em, e->v.bin.l);
    char *b = emaexpr(em, e->v.bin.r);
    char *t = newtmp(em);
    Type *lt = e->v.bin.l->ty;
    usize i;

    if (op == Tampamp || op == Tbarbar)
      cerrat(e, "the short circuits arrive with control flow (M3c)");
    for (i = 0; i < sizeof ops / sizeof ops[0]; i++)
      if (ops[i].t == op) {
        char *ins = ops[i].i;

        if (ops[i].u && isuintty(lt))
          ins = ops[i].u;
        fprintf(em->o, "\t%s =%c %s %s, %s\n", t, qbety(e->ty, e), ins, a, b);
        return t;
      }
    { /* a comparison: the domain is the operands', the result a w */
      char *ins = 0;
      int   dom = qbety(lt, e);

      if (lt && lt->k == Tyint && (lt->num == IN_F32 || lt->num == IN_F64)) {
        switch (op) {
        case Teqeq:
          ins = dom == 's' ? "ceqs" : "ceqd";
          break;
        case Tne:
          ins = dom == 's' ? "cnes" : "cned";
          break;
        case Tlt:
          ins = dom == 's' ? "clts" : "cltd";
          break;
        case Tle:
          ins = dom == 's' ? "cles" : "cled";
          break;
        case Tgt:
          ins = dom == 's' ? "cgts" : "cgtd";
          break;
        case Tge:
          ins = dom == 's' ? "cges" : "cged";
          break;
        default:
          break;
        }
      } else {
        int uns = isuintty(lt);
        switch (op) {
        case Teqeq:
          ins = "ceq";
          break;
        case Tne:
          ins = "cne";
          break;
        case Tlt:
          ins = uns ? "cult" : "cslt";
          break;
        case Tle:
          ins = uns ? "cule" : "csle";
          break;
        case Tgt:
          ins = uns ? "cugt" : "csgt";
          break;
        case Tge:
          ins = uns ? "cuge" : "csge";
          break;
        default:
          break;
        }
      }
      if (!ins)
        cerrat(e, "this operator arrives with a later milestone");
      if (dom == 's' || dom == 'd')
        fprintf(em->o, "\t%s =w %s %s, %s\n", t, ins, a, b);
      else
        fprintf(em->o, "\t%s =w %s%c %s, %s\n", t, ins, dom, a, b);
      return t;
    }
  }
  case Ncall: {
    Ast   *f = e->v.call.f;
    Ast  **args = e->v.call.args;
    usize  n = vlen(args), i;
    char **as = n ? arenaalloc(n * sizeof *as) : 0;
    char  *t = newtmp(em);
    Sym   *s;

    for (i = 0; i < n; i++)
      as[i] = emaexpr(em, args[i]);
    if (f->k == Npath && vlen(f->v.path.segs) == 1 && !locfind(em, f->v.path.segs[0]->v.seg.name)) {
      s = symfind(f->v.path.segs[0]->v.seg.name);
      if (!s || s->kind != Sfn)
        cerrat(f, "'%s' is not a fn", f->v.path.segs[0]->v.seg.name);
      if (s->next)
        cerrat(f, "overload resolution at emit time arrives with M3d");
      fprintf(em->o, "\t%s =%c call $%s(", t, qbety(e->ty, e), fsymname(s, s->decl));
    } else { /* a fn held in a value, called through it */
      char *fp = emaexpr(em, f);

      fprintf(em->o, "\t%s =%c call %s(", t, qbety(e->ty, e), fp);
    }
    for (i = 0; i < n; i++)
      fprintf(em->o, "%s%c %s", i ? ", " : "", qbety(args[i]->ty, args[i]), as[i]);
    fputs(")\n", em->o);
    return t;
  }
  case Nbuiltin: {
    char *nm = e->v.blt.name;
    Ast **targs = e->v.blt.targs;

    if (strcmp(nm, "sizeof") == 0 || strcmp(nm, "alignof") == 0) {
      char *t = newtmp(em);
      Type *ty = targs[0]->ty;
      usize v = strcmp(nm, "sizeof") == 0 ? sizeof_(ty) : alignof_(ty);

      fprintf(em->o, "\t%s =l copy %lu\n", t, (unsigned long) v);
      return t;
    }
    if (strcmp(nm, "cast") == 0) {
      Ast **args = e->v.blt.args;
      Type *from = args[0]->ty, *to = targs[0]->ty;
      char *a = emaexpr(em, args[0]);
      char *t;
      /* the four registers the value can sit in: an int in a word,
       * an int in a long, f32, f64 (the 128-bit ints arrive later) */
      int ff = from->k == Tyint && (from->num == IN_F32 || from->num == IN_F64);
      int tf = to->k == Tyint && (to->num == IN_F32 || to->num == IN_F64);
      int fw = from->k == Tyint && !ff && intwidth(from) < 8;
      int fl = from->k == Tyint && !ff && intwidth(from) == 8;
      int tw = to->k == Tyint && !tf && intwidth(to) < 8;
      int tl = to->k == Tyint && !tf && intwidth(to) == 8;
      int fb = from->k == Tybool, tb = to->k == Tybool;

      if (fb && tb)
        return a; /* both are the 0/1 in the word */
      if (fb && tw)
        return a; /* already a 0/1 in the word */
      if (fb && tl) {
        t = newtmp(em);
        fprintf(em->o, "\t%s =l extuw %s\n", t, a);
        return t;
      }
      if ((fw || fl) && tb) { /* anything nonzero is true */
        t = newtmp(em);
        fprintf(em->o, "\t%s =w cne%c %s, 0\n", t, fw ? 'w' : 'l', a);
        return t;
      }
      if ((!fw && !fl && !ff) || (!tw && !tl && !tf))
        cerrat(e, "casts between these types arrive with a later milestone");
      t = newtmp(em);
      /* w->w keeps the one temp: the loadsx already extended it on
       * load, and every w consumer re-reads only its low half */
      if (fw && tw)
        return a;
      if (ff && tf) {
        if (from->num == IN_F32 && to->num == IN_F64)
          fprintf(em->o, "\t%s =d exts %s\n", t, a);
        else if (from->num == IN_F64 && to->num == IN_F32)
          fprintf(em->o, "\t%s =s truncd %s\n", t, a);
        else
          return a;
        return t;
      }
      if (ff && (tw || tl)) { /* f -> i: the instruction per the float
                               * kind, the signedness per the target */
        int su = isuintty(to);

        fprintf(em->o, "\t%s =%c %s%s %s\n", t, qbety(to, e), from->num == IN_F32 ? "sto" : "dto",
                su ? "ui" : "si", a);
        return t;
      }
      if ((fw || fl) && tf) { /* i -> f: the instruction per the int
                               * kind, the signedness per the source */
        int su = isuintty(from);

        fprintf(em->o, "\t%s =%c %s %s\n", t, qbety(to, e),
                fw ? (su ? "uwtof" : "swtof") : (su ? "ultof" : "sltof"), a);
        return t;
      }
      if (fw && tl) { /* widen: the extension follows the source */
        fprintf(em->o, "\t%s =l %s %s\n", t, isuintty(from) ? "extuw" : "extsw", a);
        return t;
      }
      if (fl && tl)
        return a; /* l<->l: one register */
      /* fl && tw: copy truncates to the word */
      fprintf(em->o, "\t%s =w copy %s\n", t, a);
      return t;
    }
    cerrat(e, "this builtin arrives with a later milestone");
    return 0; /* unreachable */
  }
  case Nstr: { /* the bytes go to the data segment; the value is the
                * slice itself, two words of memory */
    char *base = arenaalloc(16), *t = newtmp(em);
    usize i;

    sprintf(base, "$str.%lu", (unsigned long) ++em->strn);
    fprintf(em->o, "\t%s =l alloc8 16\n", t);
    fprintf(em->o, "\tstorel %s, %s\n", base, t);
    { /* the len goes in the second word */
      char *p = newtmp(em);

      fprintf(em->o, "\t%s =l add %s, 8\n", p, t);
      fprintf(em->o, "\tstorel %lu, %s\n", (unsigned long) e->v.s.len, p);
    }
    {                                                  /* the data line, kept for after the fns */
      char *d = arenaalloc(64 + 8 * (e->v.s.len + 2)); /* b 255, is 7 */
      char *p;

      p = d + sprintf(d, "data %s = { ", base);
      for (i = 0; i < e->v.s.len; i++)
        p += sprintf(p, "b %u, ", (unsigned) (unsigned char) e->v.s.s[i]);
      sprintf(p, "b 0 }"); /* the NUL: C interop wants it */
      vappend(&em->datas, &d);
    }
    return t;
  }
  case Nstructlit: { /* storage first, then a field at a time */
    Type *st = e->ty;
    usize sz = sizeof_(st) ? sizeof_(st) : 1;
    char *t = newtmp(em);
    Ast **inits = e->v.slit.inits;
    usize i;

    fprintf(em->o, "\t%s =l alloc8 %lu\n", t, (unsigned long) sz);
    for (i = 0; i < vlen(inits); i++) {
      Ast  *ini = inits[i];
      char *v = emaexpr(em, ini->v.init.e);
      Type *ft = 0;
      usize off = foffset(st, ini->v.init.name, e);
      usize j;

      for (j = 0; j < st->sym->nfields; j++)
        if (strcmp(st->sym->fields[j].name, ini->v.init.name) == 0)
          ft = gsubst(st->sym->fields[j].ty, st->sym->gparams, st->args, st->nargs);
      if (isagg(ft)) { /* the nested literal has its own storage; a
                        * blit copies it into the field */
        if (off) {
          char *p = newtmp(em);

          fprintf(em->o, "\t%s =l add %s, %lu\n", p, t, (unsigned long) off);
          fprintf(em->o, "\tblit %s, %s, %lu\n", v, p, (unsigned long) sizeof_(ft));
        } else
          fprintf(em->o, "\tblit %s, %s, %lu\n", v, t, (unsigned long) sizeof_(ft));
        continue;
      }
      if (off) { /* the store wants the address, offset and all */
        char *p = newtmp(em);

        fprintf(em->o, "\t%s =l add %s, %lu\n", p, t, (unsigned long) off);
        fprintf(em->o, "\t%s %s, %s\n", stins(ft), v, p);
      } else
        fprintf(em->o, "\t%s %s, %s\n", stins(ft), v, t);
    }
    return t;
  }
  default:
    cerrat(e, "this expression arrives with a later milestone");
    return 0; /* unreachable */
  }
}

/* one statement */
static void
emastmt(Em *em, Ast *st)
{
  switch (st->k) {
  case Nlet: {
    Ast  *pat = st->v.let.pat;
    char *nm;
    char *v;
    Type *t = st->ty;

    if (pat->k == Npath)
      nm = pat->v.path.segs[0]->v.seg.name;
    else if (pat->k == Nppath && vlen(pat->v.ppath.path->v.path.segs) == 1 &&
             !pat->v.ppath.payload && !pat->v.ppath.named)
      nm = pat->v.ppath.path->v.path.segs[0]->v.seg.name;
    else
      cerrat(pat, "destructuring lets arrive with M3c");
    v = st->v.let.e ? emaexpr(em, st->v.let.e) : 0;
    if (isagg(t)) /* the initializer's storage is the binding's: a
                   * move, not a copy (03-move.md) */
      locbind(em, nm, v, t);
    else {
      char *slot = newtmp(em);

      fprintf(em->o, "\t%s =l alloc8 %lu\n", slot, (unsigned long) (sizeof_(t) ? sizeof_(t) : 1));
      if (v)
        fprintf(em->o, "\t%s %s, %s\n", stins(t), v, slot);
      locbind(em, nm, slot, t);
    }
    return;
  }
  case Nassign: {
    Ast  *lhs = st->v.bin.l;
    char *p, *v;

    if (lhs->k == Naccess && isagg(lhs->ty)) /* the field itself is
                                              * an aggregate: its
                                              * copy is M3c's blit */
      cerrat(st, "assigning an aggregate field arrives with M3c");
    if (lhs->k == Npath && locfind(em, lhs->v.path.segs[0]->v.seg.name) &&
        isagg(locfind(em, lhs->v.path.segs[0]->v.seg.name)->ty))
      cerrat(st, "assigning an aggregate arrives with M3c");
    p = emaplace(em, lhs);
    v = emaexpr(em, st->v.bin.r);
    if (!lhs->ty)
      cerrat(lhs, "this place was never checked");
    fprintf(em->o, "\t%s %s, %s\n", stins(lhs->ty), v, p);
    return;
  }
  case Nreturn: {
    if (st->v.n1.e) {
      char *v = emaexpr(em, st->v.n1.e);

      if (isagg(st->v.n1.e->ty))
        cerrat(st, "returning an aggregate arrives with M3c");
      fprintf(em->o, "\tret %s\n", v);
    } else
      fputs("\tret 0\n", em->o); /* (): the w carries nothing */
    return;
  }
  case Nexprstmt:
    emaexpr(em, st->v.n1.e);
    return;
  default:
    cerrat(st, "this statement arrives with M3c");
  }
}

/* a block: its statements, then its tail as the fn's value. A
 * return ends it -- what follows is dead. */
static void
emablock(Em *em, Ast *body)
{
  Ast **stmts = body->v.blk.stmts;
  usize i, nbase = em->nlocs;

  for (i = 0; i < vlen(stmts); i++) {
    emastmt(em, stmts[i]);
    if (stmts[i]->k == Nreturn)
      return; /* the rest is dead */
  }
  if (body->v.blk.tail) {
    char *v = emaexpr(em, body->v.blk.tail);

    if (isagg(body->v.blk.tail->ty))
      cerrat(body->v.blk.tail, "returning an aggregate arrives with M3c");
    fprintf(em->o, "\tret %s\n", v);
    return;
  }
  fputs("\tret 0\n", em->o); /* "{}": the unit */
  em->nlocs = nbase;
}

static void
emitfn(FILE *o, Sym *s, Ast *it)
{
  Type *fnty = s->fnty;
  Type *ret = fnty->t;
  Em    em;
  usize i;

  if (s->next)
    cerrat(it, "an overloaded fn's own body arrives with M3d");
  memset(&em, 0, sizeof em);
  em.o = o;
  em.datas = vnew(char *, 8);
  em.locs = vnew(ELoc, 16);
  fputs("export function", o);
  if (qbety(ret, it))
    fprintf(o, " %c", qbety(ret, it));
  fprintf(o, " $%s(", fsymname(s, it));
  for (i = 0; i < fnty->nargs; i++) {
    if (i)
      fputs(", ", o);
    fprintf(o, "%c %%%s", qbety(fnty->args[i], it), it->v.fn.params[i]->v.param.name);
  }
  fputs(") {\n@start\n", o);
  for (i = 0; i < fnty->nargs; i++) { /* every parameter a slot: one
                                       * path reads them all */
    char *slot = newtmp(&em);

    fprintf(o, "\t%s =l alloc8 %lu\n", slot,
            (unsigned long) (sizeof_(fnty->args[i]) ? sizeof_(fnty->args[i]) : 1));
    fprintf(o, "\t%s %%%s, %s\n", stins(fnty->args[i]), it->v.fn.params[i]->v.param.name, slot);
    locbind(&em, it->v.fn.params[i]->v.param.name, slot, fnty->args[i]);
  }
  emablock(&em, it->v.fn.body);
  fputs("}\n\n", o);
  for (i = 0; i < vlen(em.datas); i++) /* the strings this fn grew */
    fprintf(o, "%s\n", em.datas[i]);
}

void
emitfile(FILE *out, Ast **items)
{
  usize i;

  for (i = 0; i < vlen(items); i++) {
    Ast *it = items[i];
    Sym *s;

    if (it->k != Nfn || it->v.fn.body == 0)
      continue; /* a declaration: an import, or a fn type's use */
    s = symfind(it->v.fn.name);
    if (s->ngparams)
      continue; /* a generic fn emits per instance, from its call
                 * sites (04-generics.md) -- M3d */
    emitfn(out, s, it);
  }
}
