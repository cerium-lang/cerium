/* abi.c -- the aggregate calling convention (M3d): xyz's own
 * convention is the platform's C convention (01-types.md).
 *
 * Where a value crosses a call, an aggregate wears a :type, the
 * value is the address of its storage either way, and qbe lowers
 * the rest -- register eightbytes, memory, an sret. The registry
 * names every aggregate that crosses a call; abidecls prints the
 * declarations ahead of the functions, the order qbe reads. */

#include <stdio.h>
#include <string.h>

#include "abi.h"
#include "ast.h"
#include "check.h"
#include "die.h"
#include "layout.h"
#include "sym.h"
#include "type.h"
#include "vec.h"

/* qbe's letter for a scalar. () returns a w anyway: cc's crt reads
 * one from main, and no caller of a unit fn reads anything -- a
 * zero written keeps the exit code honest. at is the node the
 * diagnostic points at. */
int
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
int
isagg(Type *t)
{
  return t && (t->k == Tystruct || t->k == Tyunion || t->k == Tyenum || t->k == Tyarray ||
               t->k == Tytuple || t->k == Tyslice || t->k == Tydyn);
}

/* -- the aggregate calling convention (M3d) --------------------------------
 * xyz's own convention is the platform's C convention (01-types.md):
 * where a value crosses a call, an aggregate wears a :type, the
 * value is the address of its storage either way, and qbe lowers
 * the rest -- register eightbytes, memory, an sret. The registry
 * names every aggregate that crosses a call; the declarations
 * print ahead of the functions, the order qbe reads. */
typedef struct
{
  Type *ty;   /* 0: a helper union, only its declaration matters */
  char *name; /* ":t.N"; a niche shape shares its payload's */
  char *decl; /* the "type :" line, 0 when riding another's */
} TyDef;

static TyDef *tydefs; /* vnew'd lazily: vappend refuses NULL */

char *typereg(Type *t);

/* a niche shape is its payload's own size (01-types.md) -- and the
 * payload is always a pointer, hasniche only seeing those, so at a
 * call a niche ?T or E?T is the scalar 'l'. The aggregate half of
 * the convention starts past them. */
int
isabb(Type *t)
{
  return isagg(t) && !nicheness(t);
}

/* qbe's letter for one scalar field */
static char
qbefty(Type *t)
{
  if (t->k == Tyint) {
    switch (t->num) {
    case IN_I8:
    case IN_U8:
      return 'b';
    case IN_I16:
    case IN_U16:
      return 'h';
    case IN_I32:
    case IN_U32:
      return 'w';
    case IN_F32:
      return 's';
    case IN_F64:
      return 'd';
    default:
      return 'l';
    }
  }
  return t->k == Tybool ? 'b' : 'l'; /* the pointer family */
}

/* one field of a type expression: its letter, or the nested
 * aggregate's registered name -- typereg has already run on it. A
 * niche field is its one pointer: 'l', never a type of its own */
static char *
fieldty(Type *ft)
{
  static char b[2][16];
  static int  r = 0;

  r = (r + 1) % 2;
  if (!isabb(ft)) {
    if (ft->k == Tyenum) { /* the niche shape: one pointer */
      b[r][0] = 'l';
      b[r][1] = 0;
      return b[r];
    }
    b[r][0] = qbefty(ft);
    b[r][1] = 0;
    return b[r];
  }
  return typereg(ft);
}

/* the natural size of a field sequence -- the struct rule -- to
 * compare against a payload's packed one (02-layout.md: the
 * payloads are a union, each side laid end to end) */
static usize
naturalsize(Type **ps, usize np)
{
  usize off = 0, al = 1, i;

  for (i = 0; i < np; i++) {
    usize a = alignof_(ps[i]);

    off = alignto(off, a);
    off += sizeof_(ps[i]);
    if (a > al)
      al = a;
  }
  return alignto(off, al);
}

/* the registry name of an aggregate, making it -- and everything it
 * holds -- first. Scalars never arrive: sigty keeps them out. */
char *
typereg(Type *t)
{
  char  buf[2048];
  char  nm[24];
  char *cp;
  TyDef d;
  usize i, o, myidx;

  while (t && t->k == Tymut)
    t = t->t; /* the mut layer takes no space */
  if (tydefs)
    for (i = 0; i < vlen(tydefs); i++)
      if (tydefs[i].ty == t)
        return tydefs[i].name;
  if (!tydefs)
    tydefs = vnew(TyDef, 0);
  memset(&d, 0, sizeof d);
  d.ty = t;
  sprintf(nm, ":t.%lu", (unsigned long) vlen(tydefs));
  { /* take the number before anything this holds registers: a
     * nested type would otherwise land on the same one */
    char *name = arenaalloc(strlen(nm) + 1);

    strcpy(name, nm);
    d.name = name;
    vappend(&tydefs, &d); /* the decl lands below, once made */
    myidx = vlen(tydefs) - 1;
  }
  switch (t->k) {
  case Tystruct: {
    Field *fs = t->sym->fields;
    usize  nf = t->sym->nfields;
    int    packed;
    usize  alignk;

    layoutattrs(t->sym->decl, &packed, &alignk);
    if (packed) { /* qbe's types lay out like C; a packed struct
                   * does not: opaque, and memory carries it right */
      sprintf(buf, "type %s = align 1 { %lu }", nm, (unsigned long) sizeof_(t));
      break;
    }
    o = sprintf(buf, "type %s = align %lu { ", nm, (unsigned long) alignof_(t));
    for (i = 0; i < nf; i++) {
      Type *ft = gsubst(fs[i].ty, t->sym->gparams, t->args, t->nargs);

      if (isagg(ft))
        typereg(ft); /* the nested one names itself first */
      o += sprintf(buf + o, "%s%s", i ? ", " : "", fieldty(ft));
      if (o + 32 >= sizeof buf)
        die("a struct too wide for the emitter's line");
    }
    sprintf(buf + o, " }");
    break;
  }
  case Tytuple:
    o = sprintf(buf, "type %s = align %lu { ", nm, (unsigned long) alignof_(t));
    for (i = 0; i < t->nargs; i++) {
      if (isagg(t->args[i]))
        typereg(t->args[i]);
      o += sprintf(buf + o, "%s%s", i ? ", " : "", fieldty(t->args[i]));
      if (o + 32 >= sizeof buf)
        die("a tuple too wide for the emitter's line");
    }
    sprintf(buf + o, " }");
    break;
  case Tyunion: {
    Field *fs = t->sym->fields;
    usize  nf = t->sym->nfields;
    int    packed;
    usize  alignk;

    layoutattrs(t->sym->decl, &packed, &alignk);
    if (packed) { /* ditto: opaque */
      sprintf(buf, "type %s = align 1 { %lu }", nm, (unsigned long) sizeof_(t));
      break;
    }
    o = sprintf(buf, "type %s = align %lu { ", nm, (unsigned long) alignof_(t));
    for (i = 0; i < nf; i++) { /* every member its own group */
      Type *ft = gsubst(fs[i].ty, t->sym->gparams, t->args, t->nargs);

      if (isagg(ft))
        typereg(ft);
      o += sprintf(buf + o, "%s{ %s }", i ? " " : "", fieldty(ft));
      if (o + 32 >= sizeof buf)
        die("a union too wide for the emitter's line");
    }
    sprintf(buf + o, " }");
    break;
  }
  case Tyslice:
    sprintf(buf, "type %s = align %lu { l, l }", nm, (unsigned long) WORD);
    break;
  case Tyarray: {
    Type *et = t->t;

    if (!t->n)
      die("an empty array crossing a call arrives with its iterators");
    if (isagg(et))
      typereg(et);
    sprintf(buf, "type %s = align %lu { %s %lu }", nm, (unsigned long) alignof_(t), fieldty(et),
            (unsigned long) t->n);
    break;
  }
  case Tydyn:
    die("dyn values arrive with dispatch (06-dispatch.md)");
    return 0;    /* unreachable */
  case Tyenum: { /* tagged: the tag, then the payloads as a union --
                  * the niche shapes never arrive, isabb holding them
                  * out as the scalars they are at a call */
    {
      Variant *vs = t->sym->variants;
      usize    nv = t->sym->nvariants, k, j;
      Type  ***inst = arenaalloc(nv * sizeof *inst); /* each side's
                                                      * payload, instantiated */
      usize *nps = arenaalloc(nv * sizeof *nps);
      usize  pmax = 0, pal = 1;
      int    natural = 1, packed;
      usize  alignk;
      char   un[24];
      TyDef  du;

      layoutattrs(t->sym->decl, &packed, &alignk);
      memset(&du, 0, sizeof du);
      for (i = 0; i < nv; i++) {
        Variant *v = &vs[i];
        Type   **ps;
        usize    np, vsz = 0;

        if (v->named) {
          ps = tyargs(v->nfields);
          for (k = 0; k < v->nfields; k++)
            ps[k] = v->fields[k].ty;
          np = v->nfields;
        } else {
          ps = v->payload;
          np = v->npayload;
        }
        inst[i] = tyargs(np);
        nps[i] = np;
        for (j = 0; j < np; j++) {
          inst[i][j] = gsubst(ps[j], t->sym->gparams, t->args, t->nargs);
          vsz += sizeof_(inst[i][j]); /* end to end (02-layout.md) */
          if (alignof_(inst[i][j]) > pal)
            pal = alignof_(inst[i][j]);
        }
        if (vsz > pmax)
          pmax = vsz;
        if (np && naturalsize(inst[i], np) != vsz)
          natural = 0; /* a side C would pad: the union cannot be
                        * spelled as a type */
      }
      if (!pmax) { /* no payload anywhere: the tag alone (02) */
        sprintf(buf, "type %s = align %lu { %c }", nm, (unsigned long) alignof_(t),
                qbefty(tagtyof(t)));
        break;
      }
      sprintf(un, ":t.%lu", (unsigned long) vlen(tydefs));
      if (packed || !natural) { /* opaque: memory always carries a
                                 * shape the types cannot spell */
        char ub[128];

        sprintf(ub, "type %s = align %lu { %lu }", un, packed ? 1ul : (unsigned long) pal,
                (unsigned long) pmax);
        cp = arenaalloc(strlen(ub) + 1);
        strcpy(cp, ub);
        du.ty = 0;
        du.name = arenaalloc(strlen(un) + 1);
        strcpy(du.name, un);
        du.decl = cp;
        vappend(&tydefs, &du);
      } else {
        char ub[2048];
        int  firstgrp = 1;

        o = sprintf(ub, "type %s = align %lu { ", un, (unsigned long) pal);
        for (i = 0; i < nv; i++) {
          if (!nps[i])
            continue; /* a payloadless side joins nothing */
          o += sprintf(ub + o, "%s{ ",
                       firstgrp ? "" : " "); /* qbe
                                              * juxtaposes union members -- no commas */
          firstgrp = 0;
          for (j = 0; j < nps[i]; j++) {
            if (isagg(inst[i][j]))
              typereg(inst[i][j]);
            o += sprintf(ub + o, "%s%s", j ? ", " : "", fieldty(inst[i][j]));
          }
          o += sprintf(ub + o, " }");
        }
        sprintf(ub + o, " }");
        du.ty = 0;
        du.name = arenaalloc(strlen(un) + 1);
        strcpy(du.name, un);
        du.decl = arenaalloc(strlen(ub) + 1);
        strcpy(du.decl, ub);
        vappend(&tydefs, &du);
      }
      sprintf(buf, "type %s = align %lu { %c, %s }", nm, (unsigned long) alignof_(t),
              qbefty(tagtyof(t)), un);
      break;
    }
  }
  default:
    die("this aggregate arrives with a later milestone");
    return 0; /* unreachable */
  }
  cp = arenaalloc(strlen(buf) + 1);
  strcpy(cp, buf);
  tydefs[myidx].decl = cp; /* the entry took its number above */
  return d.name;
}

/* the annotation a value wears where it crosses a call: a scalar
 * names its class, an aggregate names its type. at is the node a
 * diagnostic would point at. */
char *
sigty(Type *t, Ast *at)
{
  static char b[2];

  while (t && t->k == Tymut)
    t = t->t;
  if (!isabb(t)) {
    if (t && t->k == Tyenum) { /* the niche shape: one pointer in a
                                * register, not a type of its own */
      b[0] = 'l';
      b[1] = 0;
      return b;
    }
    b[0] = qbety(t, at);
    b[1] = 0;
    return b;
  }
  return typereg(t);
}

/* the registry's declarations, printed ahead of the functions:
 * innermost first -- a type's number was taken before anything it
 * held registered, so counting down is the topological order qbe
 * reads (a use never precedes its definition). Emitfile calls this
 * between its naming pass and the real one. */
void
abidecls(FILE *out)
{
  usize i = vlen(tydefs);

  while (i--)
    if (tydefs[i].decl)
      fprintf(out, "%s\n", tydefs[i].decl);
  if (vlen(tydefs))
    fputs("\n", out);
}
