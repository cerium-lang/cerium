/* layout.c -- the layout tables of 02-layout.md: how a struct is
 * padded, how an enum carries its tag, what a niche saves.
 *
 * Pure table queries -- no emitter state, no .ssa. The checker
 * writes every type back into the tree (README); these tables
 * answer for a type: its size, its alignment, its niche shape. */

#include <string.h>

#include "ast.h"
#include "check.h"
#include "layout.h"
#include "sym.h"
#include "type.h"
#include "vec.h"

usize
alignto(usize off, usize a)
{
  return a ? (off + a - 1u) / a * a : off;
}

/* an integer's width in bytes, its alignment with it (02-layout.md) */
usize
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
Ast *
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
void
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

/* an enum's niche encoding, if it has one: the null of a pointer
 * payload stands for the payloadless variant. Option<T> and
 * Result<T, E> are the two shapes (01-types.md, 02-layout.md); any
 * other enum, or a payload no pointer family lives in, tags. */

int
nicheness(Type *t)
{
  if (!t || t->k != Tyenum || t->nargs != (usize) t->sym->ngparams)
    return NICHE_NONE;
  if (t->sym == sym_option && t->nargs == 1 && hasniche(t->args[0]))
    return NICHE_OPT;
  if (t->sym == sym_result && t->nargs == 2) {
    if (t->args[0]->k == Tyunit && hasniche(t->args[1]))
      return NICHE_OKUNIT;
    if (t->args[1]->k == Tyunit && hasniche(t->args[0]))
      return NICHE_ERRUNIT;
  }
  return NICHE_NONE;
}

usize
alignof_(Type *t)
{
  usize i, al;

  if (!t)
    return 1;
  switch (t->k) {
  case Tyunit:
  case Tytype: /* a ZST beside (): a type's value carries no bits,
                * its uses compile-time's own (08-reflection.md) */
    return 1;  /* a ZST (02-layout.md) */
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

usize
sizeof_(Type *t)
{
  usize i, off, al;

  if (!t)
    return 0;
  switch (t->k) {
  case Tyunit:
  case Tytype: /* no space: a type's value rides nothing */
    return 0;  /* a ZST: no space, unit alignment */
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

/* where a variant's payloads begin inside a tagged enum: right
 * after the tag, aligned to the payloads. The payload fields
 * themselves are packed (02-layout.md: the payloads are a union,
 * each side laid end to end). */
usize
payloadoff(Type *t)
{
  int   packed;
  usize alignk, tag, pal = 1, pmax = 0, i, j;

  layoutattrs(t->sym->decl, &packed, &alignk);
  tag = sizeof_(t->sym->tagty ? t->sym->tagty : picktag(t->sym));
  for (i = 0; i < t->sym->nvariants; i++) {
    Variant *v = &t->sym->variants[i];
    Type   **ps;
    usize    np, k, vsz = 0;

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

      vsz += sizeof_(pt);
      if (pa > pal)
        pal = pa;
    }
    if (vsz > pmax)
      pmax = vsz;
  }
  if (packed || !pmax)
    return tag;
  return alignto(tag, pal);
}

/* a variant's tag width, as a type: stins/ldins take it */
Type *
tagtyof(Type *t)
{
  return t->sym->tagty ? t->sym->tagty : picktag(t->sym);
}
