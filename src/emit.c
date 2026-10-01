/* emit.c -- the .ssa text: the expression language, the control
 * flow, and the fns that wrap them.
 *
 * Stage 0 emits QBE's own SSA (README: codegen goes through QBE);
 * register allocation, instruction selection, and the ABI of a call
 * are qbe's to place (abi.c carries the aggregate half). What is
 * here is what only the source language knows: the M3 pipeline
 * itself -- a fn that returns a written or a folded constant, end
 * to end, through qbe and cc.
 *
 * The target is x86_64 SysV; one machine word is 8 bytes. The
 * emitter walks the tree the checker already accepted: it derives
 * no types, it re-reads the ones the AST's type arguments still
 * carry, and everything else arrives with the passes that grow it.
 */

#include <stdio.h>
#include <string.h>

#include "abi.h"
#include "ast.h"
#include "check.h"
#include "die.h"
#include "emit.h"
#include "eval.h" /* Val: a const's memoized aggregate, folded into
                   * the segment it rides (08-reflection.md) */
#include "layout.h"
#include "sym.h"
#include "type.h"
#include "vec.h"

/* an unsigned int: picks cu* over cs*, extuw over extsw */
static int
isuintty(Type *t)
{
  return t && t->k == Tyint &&
         (t->num == IN_U8 || t->num == IN_U16 || t->num == IN_U32 || t->num == IN_U64 ||
          t->num == IN_USIZE || t->num == IN_U128);
}

static usize dsn; /* one a data symbol: $flt.N, $str.N. File-wide,
                   * not per fn -- the names are global, so a per-fn
                   * counter would hand two fns the same $flt.1 */

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
  usize  tmp;    /* one a temporary: %t.N */
  char **allocs; /* the entry's stack asks, held for the front: a
                  * slot asked for inside a loop is bytes taken every
                  * round, and a long loop walks the frame off the
                  * guard page -- so every ask goes out at @start,
                  * once, and the rounds share the slot (Clang's
                  * alloca discipline) */
  char **datas;  /* the data lines, printed after the fns */
  usize *zerosz; /* the zero blocks already grown, by size */
  char **zeros;  /* their symbols, parallel */
  usize  nzeros;
  ELoc  *locs; /* the bindings in scope */
  usize  nlocs;
  usize  ormark; /* (usize)-1 when no or-pattern is being emitted:
                  * the reuse scan is off then -- a same-named slot
                  * an earlier pattern made is a different binding,
                  * not a shared one. Under an or-pattern it is the
                  * first pre-bound slot: the alternatives bind the
                  * same names, and a later one reuses the slot the
                  * pre-binding made (09-match.md) -- else the join
                  * would read a slot only one path defined */
  usize lbl;     /* one a block label: @L.N */
  struct
  {
    char *brk;  /* where break lands */
    char *cont; /* where continue lands */
  } loops[32];  /* the fors in effect, innermost last */
  int nloops;
};

static char *
newtmp(Em *em)
{
  char *s = arenaalloc(32); /* %t. plus a u64: plenty */

  sprintf(s, "%%t.%lu", (unsigned long) ++em->tmp);
  return s;
}

/* a stack slot: the instruction is held for the entry block, where
 * qbe lowers it once, and the rounds of a loop share the memory it
 * asked for -- each ask names a fresh temporary, so no two wants
 * collide, and @start dominates every use */
static char *
stackslot(Em *em, usize sz)
{
  char *t = newtmp(em);
  char *l = arenaalloc(48);

  sprintf(l, "\t%s =l alloc8 %lu\n", t, (unsigned long) sz);
  vappend(&em->allocs, &l);
  return t;
}

/* a niche value sits in storage as one pointer; where it crosses a
 * call the signature says 'l' (isabb) -- load it out to hand over */
static char *
nicheout(Em *em, Type *t, char *v)
{
  char *tv;

  if (!t || !nicheness(t))
    return v;
  tv = newtmp(em);
  fprintf(em->o, "\t%s =l loadl %s\n", tv, v);
  return tv;
}

/* ...and store what came back: the value model stays an address */
static char *
nichein(Em *em, Type *t, char *v)
{
  char *slot;

  if (!t || !nicheness(t))
    return v;
  slot = stackslot(em, 8);
  fprintf(em->o, "\tstorel %s, %s\n", v, slot);
  return slot;
}

/* a block label, and the jump to one: qbe's blocks end in an
 * explicit jump, so control flow reads as written */
static char *
newlbl(Em *em)
{
  char *s = arenaalloc(16);

  sprintf(s, "@L.%lu", (unsigned long) ++em->lbl);
  return s;
}

static void
jump(Em *em, char *lbl)
{
  fprintf(em->o, "\tjmp %s\n", lbl);
}

/* a store/load pair by type, the value-merge primitives: a branch
 * leaves its value in a slot, the join reads it back. qbe's memory
 * promotion turns the pair back into a phi, so this costs nothing */
static char *
mkslot(Em *em, Type *t)
{
  usize sz = sizeof_(t) ? sizeof_(t) : 1;

  return stackslot(em, sz);
}

static void
slotstore(Em *em, Type *t, char *v, char *slot)
{
  fprintf(em->o, "\t%s %s, %s\n", stins(t), v, slot);
}

static char *
slotload(Em *em, Type *t, Ast *at, char *slot)
{
  char *v = newtmp(em);

  if (isagg(t))
    return slot; /* an aggregate's value is the address it sits at */
  fprintf(em->o, "\t%s =%c %s %s\n", v, qbety(t, at), ldins(t), slot);
  return v;
}

/* a value into a slot, by type: the store/load pair a scalar takes,
 * the blit an aggregate takes */
static void
slotput(Em *em, Type *t, char *v, char *slot)
{
  if (isagg(t))
    fprintf(em->o, "\tblit %s, %s, %lu\n", v, slot, (unsigned long) sizeof_(t));
  else
    fprintf(em->o, "\t%s %s, %s\n", stins(t), v, slot);
}

/* a base plus a constant offset: the address a field or a payload
 * lives at. Offset zero is the base unchanged -- the add buys nothing */
static char *
addrplus(Em *em, char *b, usize off)
{
  char *t;

  if (!off)
    return b;
  t = newtmp(em);
  fprintf(em->o, "\t%s =l add %s, %lu\n", t, b, (unsigned long) off);
  return t;
}

/* the value at an offset inside storage: the address itself when
 * the type is an aggregate, the word loaded from it when not */
static void
subval(Em *em, Type *t, char *addr, usize off, Ast *at, char **paddr, char **pval)
{
  char *ap = addrplus(em, addr, off);

  if (isagg(t)) {
    *paddr = ap;
    *pval = 0;
  } else {
    char *v = newtmp(em);

    fprintf(em->o, "\t%s =%c %s %s\n", v, qbety(t, at), ldins(t), ap);
    *paddr = 0;
    *pval = v;
  }
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
  if (em->nlocs < vlen(em->locs)) /* a popped binding's slot, reused:
                                   * the count is the stack, the vec
                                   * only its high-water mark -- a
                                   * bind here must not wake what a
                                   * scope popped */
    em->locs[em->nlocs] = l;
  else
    vappend(&em->locs, &l);
  em->nlocs++;
}

/* the fn's symbol: #[extern(C)] and main keep their own name, every
 * other fn is mangled -- overloading, namespaces, and generic
 * instantiation all force it (01-types.md, External functions). A
 * lone fn keeps the short name; an overload carries its whole
 * signature; an instantiation carries its binding (instname, below).
 * qbe's symbols are letters, digits, and underscore -- what tysprint
 * says, folded onto that alphabet, says the name. */
static char *tymangle(Type *t, char *buf, usize n);
static void  ovlspell(Sym *s, char *buf, usize n);
static int   fsymsame(Sym *p, char *buf);

/* a method's symbol: the impl's type spelled, then the member's own
 * name. Overloading is no method's trouble -- one type, one name --
 * but two impls of one generic type at different bindings are two
 * methods, and a spelling the folding cannot tell apart still gets
 * its index, the same scan the fn mangler runs on its chains. */
static char *
memname(Sym *s)
{
  char  mt[256];
  char  buf[1024];
  char  b2[1024];
  usize same, i, j;

  tymangle(s->impl->ifort ? s->impl->ifort : s->impl->ipath, mt, sizeof mt);
  sprintf(buf, "xyz_%s_%s", mt, s->name);
  same = 0;
  for (i = 0; i < chk_nimpls; i++) { /* every impl's methods, in
                                      * declaration order -- a trait
                                      * impl's target names it, an
                                      * inherent's target is it */
    Sym    *iv = chk_impls[i];
    Member *ms;

    ms = iv->members;
    for (j = 0; j < iv->nmembers; j++) {
      if (ms[j].kind != Mfn || !ms[j].sym)
        continue;
      tymangle(iv->ifort ? iv->ifort : iv->ipath, mt, sizeof mt);
      sprintf(b2, "xyz_%s_%s", mt, ms[j].name);
      if (strcmp(b2, buf) != 0)
        continue;
      if (ms[j].sym == s)
        goto found; /* this one: the count so far is its index */
      same++;
    }
  }
found:
  if (!same) { /* stable: the arena keeps the spelling one name */
    char *n = arenaalloc(strlen(buf) + 1);

    strcpy(n, buf);
    return n;
  }
  { /* a twin: spell them apart */
    char *n = arenaalloc(strlen(buf) + 12);

    sprintf(n, "%s_%lu", buf, (unsigned long) same);
    return n;
  }
}

static char *
fsymname(Sym *s, Ast *it)
{
  char  buf[1024];
  Sym  *p;
  usize same;
  int   multi;

  if (attrfind(it->attrs, "extern"))
    return s->name;
  if (s->impl)
    return memname(s); /* a method: its impl's type, its own name */
  if (strcmp(s->name, "main") == 0)
    return s->name;
  multi = s->next != 0 || symfind(s->name) != s; /* a name's chain
                                                  * holds more than this */
  if (!multi) {                                  /* the common case: one fn, one name */
    char *n;

    n = arenaalloc(strlen(s->name) + 8);
    sprintf(n, "xyz_%s", s->name);
    return n;
  }
  /* an overload: this signature, and an index among the chain's
   * same-spelled siblings -- two overloads may differ only where the
   * mangling cannot see */
  ovlspell(s, buf, sizeof buf);
  same = 0;
  for (p = symfind(s->name); p && p != s; p = p->next) /* the earlier
                                                        * same-spelled ones */
    if (fsymsame(p, buf))
      same++;
  if (same) { /* a twin: spell them apart */
    char *n;

    n = arenaalloc(strlen(buf) + 12);
    sprintf(n, "%s_%lu", buf, (unsigned long) same);
    return n;
  }
  { /* stable: the arena keeps the spelling one name */
    char *n;

    n = arenaalloc(strlen(buf) + 1);
    strcpy(n, buf);
    return n;
  }
}

/* -- monomorphization (04-generics.md) ---------------------------------- */

/* One generic fn, one binding of its parameters. Types are interned,
 * so the key is the Sym with the Type pointers themselves; the same
 * instantiation is emitted once, and its name is what every call
 * site says. */
typedef struct Inst Inst;
struct Inst
{
  Sym   *s;
  Type **tys; /* s->ngparams of them, the call sites' binding */
  char  *name;
  int    mark; /* the drain this entry is queued for */
};

static Inst **insts;  /* every one made, in first-seen order */
static Inst **iqueue; /* the current drain's worklist */
static int    ipass;  /* scratch is 1, the text is 2 */

/* qbe's alphabet only: what tysprint says, everything else folded
 * onto '_' -- i32 stays i32, *mut i32 becomes _mut_i32 */
static char *
tymangle(Type *t, char *buf, usize n)
{
  char  tmp[256];
  usize i;

  tysprint(tmp, sizeof tmp, t);
  if (strlen(tmp) >= n)
    die("a type too wide for the emitter's names");
  for (i = 0; tmp[i]; i++) {
    char c = tmp[i];

    buf[i] = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') ? c : '_';
  }
  buf[i] = 0;
  return buf;
}

/* an overload's spelling: the name, then the argument types, then
 * the return, folded onto qbe's alphabet */
static void
ovlspell(Sym *s, char *buf, usize n)
{
  char  tb[256];
  usize o, i;

  o = sprintf(buf, "xyz_%s__", s->name);
  for (i = 0; i < s->fnty->nargs; i++) {
    tymangle(s->fnty->args[i], tb, sizeof tb);
    o += sprintf(buf + o, "%s%s", i ? "_" : "", tb);
    if (o + 256 >= n)
      die("an overload too wide for the emitter's line");
  }
  tymangle(s->fnty->t, tb, sizeof tb);
  sprintf(buf + o, "_%s", tb);
}

/* does p's signature fold to the same spelling? The chain's twins --
 * overloads a mangling cannot tell apart -- number off */
static int
fsymsame(Sym *p, char *buf)
{
  char pb[1024];

  ovlspell(p, pb, sizeof pb);
  return strcmp(pb, buf) == 0;
}

/* the instance's name: the fn's, its binding's -- g marks it apart
 * from an overload's arg spelling -- numbered only if an earlier
 * entry already took the spelling (a pair of twins the key sees
 * apart and the alphabet cannot) */
static char *
instname(Sym *s, Type **tys)
{
  char  buf[1024];
  char  tb[256];
  usize o, i, same;

  o = sprintf(buf, "xyz_%s__g", s->name);
  for (i = 0; i < s->ngparams; i++) {
    tymangle(tys[i], tb, sizeof tb);
    o += sprintf(buf + o, "_%s", tb);
    if (o + 256 >= sizeof buf)
      die("an instantiation too wide for the emitter's line");
  }
  same = 0;
  for (i = 0; i < vlen(insts); i++)
    if (strcmp(insts[i]->name, buf) == 0)
      same++;
  if (same) { /* a twin: spell them apart */
    char *n;

    n = arenaalloc(strlen(buf) + 12);
    sprintf(n, "%s_%lu", buf, (unsigned long) same);
    return n;
  }
  { /* stable: the arena keeps the spelling one name */
    char *n;

    n = arenaalloc(strlen(buf) + 1);
    strcpy(n, buf);
    return n;
  }
}

/* the instance for this call, made if it is new. Every drain queues
 * each entry once: the mark is the pass, so a body's nested call can
 * re-ensure what a plain fn already found without doubling it */
static Inst *
instensure(Sym *s, Type **tys)
{
  Inst *in;
  usize i, j;

  for (i = 0; i < vlen(insts); i++) {
    in = insts[i];
    if (in->s != s)
      continue;
    for (j = 0; j < s->ngparams; j++)
      if (in->tys[j] != tys[j])
        break;
    if (j == s->ngparams)
      goto found;
  }
  if (!insts) {
    insts = vnew(Inst *, 16);
    iqueue = vnew(Inst *, 16);
  }
  in = arenaalloc(sizeof *in);
  in->s = s;
  in->tys = tys;
  in->name = instname(s, tys);
  vappend(&insts, &in);
found:
  if (in->mark != ipass) {
    in->mark = ipass;
    vappend(&iqueue, &in);
  }
  return in;
}

static char *emaexpr(Em *em, Ast *e);
static char *emablockval(Em *em, Ast *body, int *reached);
static char *emaplace(Em *em, Ast *e);
static void  emafor(Em *em, Ast *st);
static void  emapat(Em *em, Ast *p, Type *t, char *addr, char *val, char *fail);
static char *emamatch(Em *em, Ast *e, int *reached);
static char *emavariant(Em *em, Type *t, Variant *v, Ast **args, usize n, Ast *at);

/* a zero block in the data segment, one per size: a literal's
 * left-out fields and elements read as zero (01-types.md), so the
 * storage starts zeroed and what is written lands on top. Per fn:
 * the strings grow the same way, one symbol each */
static char *
zeroblk(Em *em, usize sz)
{
  char *d, *base;
  usize i;

  for (i = 0; i < em->nzeros; i++)
    if (em->zerosz[i] == sz)
      return em->zeros[i];
  if (!em->zeros) {
    em->zeros = vnew(char *, 8);
    em->zerosz = vnew(usize, 8);
  }
  base = arenaalloc(16);
  sprintf(base, "$z.%lu", (unsigned long) ++dsn);
  d = arenaalloc(48);
  sprintf(d, "data %s = { z %lu }", base, (unsigned long) sz);
  vappend(&em->datas, &d);
  vappend(&em->zeros, &base);
  vappend(&em->zerosz, &sz);
  em->nzeros = vlen(em->zeros);
  return base;
}

/* -- the segment a const's aggregate rides (08-reflection.md) -----------
 *
 * A const has no address to lend (01-types.md), but its value has
 * bytes, and a body's read copies them: the evaluator's memoized
 * Val spelled as qbe data items, the layout tables' offsets walked
 * and the padding zero. A static's first value rides the same way,
 * its symbol its own slot -- whole-program, writable where mut
 * marked it (01-types.md). */

/* the variant a discriminant names: the value's own half */
static Variant *
dsvariant(Type *t, u64 disc)
{
  usize i;

  for (i = 0; i < t->sym->nvariants; i++)
    if (t->sym->variants[i].disc == disc)
      return &t->sym->variants[i];
  return 0;
}

/* the bytes a value takes, as qbe data items: the offsets the
 * layout tables spell, the evaluator's value folded in, the
 * padding zero. *p is the write cursor; the bytes written return */
static char **dssubs; /* the child lines the value being spelled
                       * holds, collected as dswrite meets the
                       * slices; constsym owns the set */
static usize dsleaves(Type *t);
static usize
dswrite(char **p, Type *t, Val *v)
{
  switch (t->k) {
  case Tybool:
    *p += sprintf(*p, "b %lu, ", (unsigned long) (v ? v->i : 0));
    return 1;
  case Tyint:
    if (t->num >= IN_F32) { /* 9 and 17 significant digits: the
                             * least that round-trips */
      char c = t->num == IN_F32 ? 's' : 'd';

      *p += sprintf(*p, "%c %c_%.*g, ", c, c, c == 's' ? 9 : 17, v ? v->f : 0.0);
      return intwidth(t);
    }
    {
      char c = intwidth(t) == 1 ? 'b' : intwidth(t) == 2 ? 'h' : intwidth(t) == 4 ? 'w' : 'l';

      *p += sprintf(*p, "%c %lu, ", c, (unsigned long) (v ? v->i : 0));
      return intwidth(t);
    }
  case Typtr:
  case Tyvoidptr: /* a compile-time pointer is the null one, but the
                   * walk stays general */
    *p += sprintf(*p, "l %lu, ", (unsigned long) (v ? v->i : 0));
    return WORD;
  case Tystruct: {
    Sym  *s = t->sym;
    int   packed;
    usize alignk, off = 0, cur = 0, i;

    layoutattrs(s->decl, &packed, &alignk);
    for (i = 0; i < s->nfields; i++) { /* the walk foffset takes */
      Type *ft = gsubst(s->fields[i].ty, s->gparams, t->args, t->nargs);
      usize w;

      if (!packed)
        off = alignto(off, alignof_(ft));
      if (off > cur) {
        *p += sprintf(*p, "z %lu, ", (unsigned long) (off - cur));
        cur = off;
      }
      w = dswrite(p, ft, v ? &v->elems[i] : 0);
      cur += w;
      off += sizeof_(ft);
    }
    if (sizeof_(t) > cur) { /* the tail: the symbol's size is the
                             * type's, the reads walk it all */
      *p += sprintf(*p, "z %lu, ", (unsigned long) (sizeof_(t) - cur));
      cur = sizeof_(t);
    }
    return cur;
  }
  case Tyunion: {
    usize sz = sizeof_(t), w = 0;

    if (v && v->tag != VNONROW) { /* the one row a literal wrote; the
                                   * zeroed whole reads zero every
                                   * row (01-types.md) */
      Type *ft = gsubst(t->sym->fields[v->tag].ty, t->sym->gparams, t->args, t->nargs);

      w = dswrite(p, ft, &v->elems[0]);
    }
    if (sz > w) /* the rows overlap: what the active one left is zero */
      *p += sprintf(*p, "z %lu, ", (unsigned long) (sz - w));
    return sz;
  }
  case Tytuple: {
    usize i, off = 0, cur = 0;

    for (i = 0; i < t->nargs; i++) { /* the walk Ntupidx takes */
      usize w;

      off = alignto(off, alignof_(t->args[i]));
      if (off > cur) {
        *p += sprintf(*p, "z %lu, ", (unsigned long) (off - cur));
        cur = off;
      }
      w = dswrite(p, t->args[i], v ? &v->elems[i] : 0);
      cur += w;
      off += sizeof_(t->args[i]);
    }
    if (sizeof_(t) > cur) {
      *p += sprintf(*p, "z %lu, ", (unsigned long) (sizeof_(t) - cur));
      cur = sizeof_(t);
    }
    return cur;
  }
  case Tyarray: {
    usize i, cur = 0;

    for (i = 0; i < t->n; i++) /* the elements packed, no padding
                                * between them (02-layout.md) */
      cur += dswrite(p, t->t, v ? &v->elems[i] : 0);
    if (sizeof_(t) > cur) {
      *p += sprintf(*p, "z %lu, ", (unsigned long) (sizeof_(t) - cur));
      cur = sizeof_(t);
    }
    return cur;
  }
  case Tyslice: { /* the two words a slice is: the child symbol its
                   * rows ride, the count after it. @typeinfo's are
                   * the only slices a value holds (08-reflection.md)
                   * -- the user's own literals make arrays, []T
                   * borrows at runtime -- so the rows here are owned
                   * data, spelled beside the parent as the child's
                   * own data line */
    char *base, *line, *q;
    usize n = v ? v->len : 0, cur = 0, i;

    if (!n) { /* empty: no rows anywhere, the reads walk none */
      *p += sprintf(*p, "z %lu, ", (unsigned long) (2 * WORD));
      return 2 * WORD;
    }
    base = arenaalloc(16);
    sprintf(base, "$s.%lu", (unsigned long) ++dsn);
    line = arenaalloc(dsleaves(t->t) * n * 32 + 2 * 32 + 64);
    q = line + sprintf(line, "data %s = { ", base);
    for (i = 0; i < n; i++) /* the rows packed, the array rule */
      cur += dswrite(&q, t->t, &v->elems[i]);
    if (n * sizeof_(t->t) > cur) {
      q += sprintf(q, "z %lu, ", (unsigned long) (n * sizeof_(t->t) - cur));
      cur = n * sizeof_(t->t);
    }
    q -= 2; /* the last item's ", ": the line's own close */
    sprintf(q, "}");
    vappend(&dssubs, &line); /* the parent's set: replayed with it */
    *p += sprintf(*p, "l %s, l %lu, ", base, (unsigned long) n);
    return 2 * WORD;
  }
  case Tyenum: {
    usize sz = sizeof_(t);

    if (nicheness(t) != NICHE_NONE) { /* the one word it is: the unit
                                       * side the null, the other
                                       * side the value (02) */
      Variant *vr = v ? dsvariant(t, v->tag) : 0;
      usize    np = 0;

      if (vr)
        np = vr->named ? vr->nfields : vr->npayload;
      if (np && v) /* the value's own bits, the pointer itself */
        *p += sprintf(*p, "l %lu, ", (unsigned long) v->elems[0].i);
      else
        *p += sprintf(*p, "z %lu, ", (unsigned long) sz);
      return sz;
    }
    { /* the discriminant, then the payloads packed after it, the
       * walk emavariant takes */
      Type    *tt = tagtyof(t);
      Variant *vr = v ? dsvariant(t, v->tag) : 0;
      char   c = intwidth(tt) == 1 ? 'b' : intwidth(tt) == 2 ? 'h' : intwidth(tt) == 4 ? 'w' : 'l';
      usize  off, i, np = 0, cur;
      Type **ps = 0;

      *p += sprintf(*p, "%c %lu, ", c, (unsigned long) (v ? v->tag : 0));
      cur = intwidth(tt);
      off = payloadoff(t);
      if (off > cur) {
        *p += sprintf(*p, "z %lu, ", (unsigned long) (off - cur));
        cur = off;
      }
      if (vr) { /* the active variant's own payload types */
        if (vr->named) {
          ps = tyargs(vr->nfields);
          for (i = 0; i < vr->nfields; i++)
            ps[i] = gsubst(vr->fields[i].ty, t->sym->gparams, t->args, t->nargs);
          np = vr->nfields;
        } else if (vr->payload && vr->npayload) {
          ps = tyargs(vr->npayload);
          for (i = 0; i < vr->npayload; i++)
            ps[i] = gsubst(vr->payload[i], t->sym->gparams, t->args, t->nargs);
          np = vr->npayload;
        }
        for (i = 0; i < np; i++)
          cur += dswrite(p, ps[i], v ? &v->elems[i] : 0);
      }
      if (sz > cur) {
        *p += sprintf(*p, "z %lu, ", (unsigned long) (sz - cur));
        cur = sz;
      }
      return cur;
    }
  }
  default: /* unit and the rest: nothing rides, no bytes taken */
    return 0;
  }
}

/* an upper bound on the items the value spells, for the line's
 * allocation: each takes its literal's worst case, a float's 17
 * digits among the letters */
static usize
dsleaves(Type *t)
{
  usize i, n = 0;

  switch (t->k) {
  case Tystruct:
    for (i = 0; i < t->sym->nfields; i++)
      n += dsleaves(gsubst(t->sym->fields[i].ty, t->sym->gparams, t->args, t->nargs));
    return n;
  case Tyunion:
    for (i = 0; i < t->sym->nfields; i++) /* the widest row would do;
                                           * the sum is simpler */
      n += dsleaves(gsubst(t->sym->fields[i].ty, t->sym->gparams, t->args, t->nargs));
    return n;
  case Tytuple:
    for (i = 0; i < t->nargs; i++)
      n += dsleaves(t->args[i]);
    return n;
  case Tyarray:
    return t->n ? dsleaves(t->t) * t->n : 1;
  case Tyslice: /* the reference's two words: the child symbol's
                 * spelling, the count */
    return 2;
  case Tyenum: {
    usize m = 0;

    if (nicheness(t) != NICHE_NONE)
      return 1;
    for (i = 0; i < t->sym->nvariants; i++) { /* the widest payload */
      Variant *vr = &t->sym->variants[i];
      usize    k = 0, j;

      if (vr->named)
        for (j = 0; j < vr->nfields; j++)
          k += dsleaves(gsubst(vr->fields[j].ty, t->sym->gparams, t->args, t->nargs));
      else
        for (j = 0; j < vr->npayload; j++)
          k += dsleaves(gsubst(vr->payload[j], t->sym->gparams, t->args, t->nargs));
      if (k > m)
        m = k;
    }
    return 2 + m;
  }
  default:
    return 1;
  }
}

/* the data symbol a const's or a static's value rides: the line
 * printed after the fns with the rest, the symbol handed to whoever
 * reads the value -- a blit's source, a place's base. The symbols
 * are the compilation's own -- a name's line goes out once a pass,
 * with whichever fn first read it, and the naming pass's text goes
 * nowhere, so the real pass takes it again */
static Sym   **conss;     /* the ones already named, per compilation */
static char  **conssyms;  /* their symbols, parallel */
static char  **conslines; /* their data lines, for the passes after */
static char ***conssubs;  /* their slices' child lines, parallel: the
                           * rows a slice's words point at, spelled as
                           * data lines of their own -- replayed beside
                           * the parent's, or a later pass reads a
                           * symbol nothing spelled */
static int  *conspass;    /* the pass each line last went out in */
static usize nconss;

static char *
constsym(Em *em, Sym *s)
{
  char *base, *line, *p;
  usize i, sz;

  for (i = 0; i < nconss; i++)
    if (conss[i] == s) {
      if (conspass[i] == ipass)
        return conssyms[i]; /* this pass already carries the line */
      vappend(&em->datas, &conslines[i]);
      {
        usize j;

        for (j = 0; j < vlen(conssubs[i]); j++)
          vappend(&em->datas, &conssubs[i][j]);
      }
      conspass[i] = ipass;
      return conssyms[i];
    }
  base = arenaalloc(strlen(s->name) + 16);
  sprintf(base, "$%s.%s", s->kind == Sconst ? "const" : "static", s->name);
  sz = sizeof_(s->cty);
  line = arenaalloc(dsleaves(s->cty) * 32 + 2 * 32 + 64);
  p = line + sprintf(line, "data %s = { ", base);
  dssubs = vnew(char *, 8);
  if (sz) {
    Val top; /* the Sym's halves, as the walk's one value: cval the
              * tag half -- an enum's discriminant, a union's active
              * row -- celems the elements beside it, clen the length
              * a slice's own (@typeinfo's are the only slices a
              * const holds -- 08-reflection.md); tyval the one half
              * no bytes ride: a type's value carries none */

    top.t = s->cty;
    top.i = s->cval;
    top.f = s->cflt;
    top.tag = s->ctag;
    top.tyval = 0;
    top.elems = s->celems;
    top.len = s->clen;
    dswrite(&p, s->cty, &top);
    p -= 2; /* the last item's ", ": the line's own close */
    sprintf(p, "}");
  } else /* a sizeless aggregate: a byte the symbol wants, nothing
          * reads it */
    sprintf(p, "z 1 }");
  vappend(&em->datas, &line);
  {
    usize j;

    for (j = 0; j < vlen(dssubs); j++)
      vappend(&em->datas, &dssubs[j]);
  }
  if (!conss) {
    conss = vnew(Sym *, 8);
    conssyms = vnew(char *, 8);
    conslines = vnew(char *, 8);
    conssubs = vnew(char **, 8);
    conspass = vnew(int, 8);
  }
  vappend(&conss, &s);
  vappend(&conssyms, &base);
  vappend(&conslines, &line);
  vappend(&conssubs, &dssubs);
  {
    int pi = ipass;

    vappend(&conspass, &pi);
  }
  nconss = vlen(conss);
  return base;
}

/* -- vtables (06-dispatch.md) -------------------------------------------- */

/* One table per trait and concrete type, an entry a method in the
 * trait's declaration order. The construction site names its pair;
 * the tables print after every fn, the instances they name already
 * emitted -- a generic impl's method lands here as an instance the
 * no static call ever found, so the print is what ensures it. */
typedef struct Vt Vt;
struct Vt
{
  Sym  *tr;   /* the trait */
  Type *ty;   /* the concrete type behind the handle */
  char *name; /* the data symbol, $-less */
};

static Vt   *vts;       /* every pair named, in first-seen order */
static usize vtprinted; /* how many of them the text already holds */

static char *
vtname(Sym *tr, Type *ty)
{
  char  tb[256], buf[512];
  usize i, same;

  for (i = 0; i < vlen(vts); i++)
    if (vts[i].tr == tr && vts[i].ty == ty)
      return vts[i].name;
  tymangle(ty, tb, sizeof tb);
  sprintf(buf, "vt_%s_%s", tr->name, tb);
  same = 0;
  for (i = 0; i < vlen(vts); i++) /* two pairs the folding cannot
                                   * tell apart: number them off */
    if (strcmp(vts[i].name, buf) == 0)
      same++;
  { /* stable: the arena keeps the spelling one name */
    Vt    vt;
    char *n = arenaalloc(strlen(buf) + 12);

    sprintf(n, "%s%s%lu", buf, same ? "_" : "", (unsigned long) same);
    vt.tr = tr;
    vt.ty = ty;
    vt.name = n;
    if (!vts)
      vts = vnew(Vt, 8);
    vappend(&vts, &vt);
    return n;
  }
}

/* the tables not yet printed: one line each, its entries the impl
 * the pair picked -- a pattern impl's methods as instances, ensured
 * here so the drain that follows emits them */
static void
printvts(FILE *o)
{
  usize i, j;

  for (i = vtprinted; i < vlen(vts); i++) {
    Vt    *vt = &vts[i];
    Sym   *im;
    Type **tys = 0;
    char  *line;
    char  *p;
    usize  len = 32;

    im = implfor(vt->tr, vt->ty, &tys);
    if (!im)
      cerrat(vt->tr->decl, "unreachable: the construction site checked");
    for (j = 0; j < vt->tr->nmembers; j++)
      if (vt->tr->members[j].kind == Mfn)
        len += strlen(vt->tr->members[j].name) + 24;
    line = arenaalloc(len);
    p = line + sprintf(line, "data $%s = { ", vt->name);
    for (j = 0; j < vt->tr->nmembers; j++) {
      Member *tm = &vt->tr->members[j];
      Member *fm = 0;
      usize   k;
      char   *nm;

      if (tm->kind != Mfn)
        continue; /* a handle exposes the methods (06-dispatch.md) */
      for (k = 0; k < im->nmembers; k++)
        if (strcmp(im->members[k].name, tm->name) == 0) {
          fm = &im->members[k];
          break;
        }
      if (!fm || fm->kind != Mfn || !fm->sym)
        cerrat(vt->tr->decl, "unreachable: the impl supplies it");
      if (tys)
        nm = instensure(fm->sym, tys)->name; /* the instance this
                                              * table names */
      else
        nm = fsymname(fm->sym, fm->sym->decl);
      p += sprintf(p, "l $%s, ", nm);
    }
    sprintf(p, "}");
    if (ipass == 2)
      fprintf(o, "%s\n", line);
  }
  vtprinted = vlen(vts);
}

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

/* an index in the l domain, ready to scale: the narrow integers
 * widen by their signedness, the 64-bit ones are there already */
static char *
idxl(Em *em, Ast *ix)
{
  char *v = emaexpr(em, ix);
  char *t;

  if (intwidth(ix->ty) > 4)
    return v;
  t = newtmp(em);
  fprintf(em->o, "\t%s =l %s %s\n", t, isuintty(ix->ty) ? "extuw" : "extsw", v);
  return t;
}

/* the storage an aggregate expression's value sits at, without
 * copying it: a const's rides the data segment read-only, a
 * static's is its own slot -- writable where mut marked it. What
 * walks a value this way reads it -- a place's base, a view's, a
 * loop's iterable, a match's scrutinee -- and what writes goes
 * through the checker's mut first (01-types.md) */
static char *
aggbase(Em *em, Ast *e)
{
  if (e->k == Npath && e->ty && isagg(e->ty) && vlen(e->v.path.segs) == 1 && !e->v.path.root) {
    Sym *s = symfind(e->v.path.segs[0]->v.seg.name);

    if (s && s->cvaldone && (s->kind == Sconst || s->kind == Sstatic))
      return constsym(em, s);
  }
  return emaexpr(em, e);
}

/* the address of one element. The base's value is its storage -- a
 * slice's first word is the data pointer -- and the index scales by
 * the element's size, qbe having no scaled addressing. A constant
 * index folds; the checker range-checked it (01-types.md). */
static char *
idxaddr(Em *em, Ast *e)
{
  Type *bt = e->v.n2.a->ty;
  Type *et = bt->t;
  usize sz;
  char *b = aggbase(em, e->v.n2.a);

  while (et && et->k == Tymut) /* []mut T: the element's own type */
    et = et->t;
  sz = sizeof_(et);
  if (bt->k == Tyslice) {
    char *p = newtmp(em);

    fprintf(em->o, "\t%s =l loadl %s\n", p, b);
    b = p;
  }
  if (!sz)
    return b; /* a ZST element: every index is the array itself */
  if (e->v.n2.b->k == Nint)
    return addrplus(em, b, (usize) e->v.n2.b->v.i.num * sz);
  {
    char *ix = idxl(em, e->v.n2.b);
    char *sc = newtmp(em);
    char *a = newtmp(em);

    fprintf(em->o, "\t%s =l mul %s, %lu\n", sc, ix, (unsigned long) sz);
    fprintf(em->o, "\t%s =l add %s, %s\n", a, b, sc);
    return a;
  }
}

/* the address a place names. A local's slot is an address by
 * construction; a field rides its base's; a deref is the pointer. */
static char *
emaplace(Em *em, Ast *e)
{
  switch (e->k) {
  case Npath: {
    ELoc *l = locfind(em, e->v.path.segs[0]->v.seg.name);

    if (!l) { /* a static's own slot: the one global a place names
               * (01-types.md) */
      Sym *s = symfind(e->v.path.segs[0]->v.seg.name);

      if (s && s->kind == Sstatic && s->cvaldone)
        return constsym(em, s);
      cerrat(e, "'%s' is not a local here", e->v.path.segs[0]->v.seg.name);
    }
    return l->slot;
  }
  case Naccess: {
    char *b = aggbase(em, e->v.fld.e);
    char *t = newtmp(em);
    usize off = foffset(derefthrough(e->v.fld.e->ty), e->v.fld.name, e);

    fprintf(em->o, "\t%s =l add %s, %lu\n", t, b, (unsigned long) off);
    return t;
  }
  case Nindex:
    return idxaddr(em, e); /* the element's own slot */
  case Ntupidx: {          /* the row's address: the offset walk emaexpr takes,
                            * stopping before the load (01-types.md) */
    Type *tt = e->v.tup.e->ty;
    char *b = aggbase(em, e->v.tup.e); /* an aggregate base is its address */
    usize i, off = 0;

    for (i = 0; i < e->v.tup.idx; i++) {
      off = alignto(off, alignof_(tt->args[i]));
      off += sizeof_(tt->args[i]);
    }
    off = alignto(off, alignof_(e->ty));
    {
      char *t = newtmp(em);

      fprintf(em->o, "\t%s =l add %s, %lu\n", t, b, (unsigned long) off);
      return t;
    }
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

/* an if: the branches each leave their value in a slot, the join
 * reads it back -- qbe's memory promotion turns the pair into a
 * phi, so nothing is lost. Reached says whether control comes out
 * the end at all; a branch that returns or breaks takes its own
 * way out, and the join only owes the paths that arrive. */
static char *
emaif(Em *em, Ast *e, int *reached)
{
  Type *t = e->ty;
  char *c = emaexpr(em, e->v.ifx.cond);
  char *lt = newlbl(em);
  char *lf = e->v.ifx.els ? newlbl(em) : 0;
  char *lend = newlbl(em);
  char *slot = 0;
  int   rt, re = 1;
  char *vt = 0;

  *reached = 1;
  if (lf && t && t->k != Tyunit && t->k != Tytype) /* a type's value
                                                    * joins as the word it is, storage
                                                    * would ask more of it than zero */
    slot = mkslot(em, t);
  fprintf(em->o, "\tjnz %s, %s, %s\n", c, lt, lf ? lf : lend);
  fprintf(em->o, "%s\n", lt);
  vt = emablockval(em, e->v.ifx.then, &rt);
  if (slot && rt)
    slotput(em, t, vt, slot);
  if (rt)
    jump(em, lend);
  if (lf) {
    fprintf(em->o, "%s\n", lf);
    if (e->v.ifx.els->k == Nif) { /* else if: the chain folds in */
      int r2;

      vt = emaif(em, e->v.ifx.els, &r2);
      if (slot && r2)
        slotput(em, t, vt, slot);
      if (r2)
        jump(em, lend);
      re = r2;
    } else {
      vt = emablockval(em, e->v.ifx.els, &re);
      if (slot && re)
        slotput(em, t, vt, slot);
      if (re)
        jump(em, lend);
    }
    *reached = rt || re;
  }
  if (!*reached)
    return 0; /* every way out left already: no join to read */
  fprintf(em->o, "%s\n", lend);
  if (!slot) { /* the statement form, an implicit unit -- or a type,
                * whose word is the constant zero it always was */
    if (t && t->k == Tytype) {
      char *z = newtmp(em);

      fprintf(em->o, "\t%s =w copy 0\n", z);
      return z;
    }
    return 0;
  }
  return slotload(em, t, e, slot);
}

/* an arm's body: a block carries statements, anything else is the
 * value itself. Reached says whether control comes out the end. */
static char *
armbody(Em *em, Ast *b, int *reached)
{
  if (b->k == Nblock)
    return emablockval(em, b, reached);
  *reached = 1;
  return emaexpr(em, b);
}

/* construct a variant: the value of e->ty, the enum the checker
 * wrote back. A niche enum is one pointer -- the payloadless side
 * is its null; a tagged enum is the discriminant, then the payloads
 * packed after it (02-layout.md). */
static char *
emavariant(Em *em, Type *t, Variant *v, Ast **args, usize n, Ast *at)
{
  Sym  *es = t->sym;
  int   nc = nicheness(t);
  usize sz = sizeof_(t) ? sizeof_(t) : 1;
  char *s = stackslot(em, sz);
  if (nc != NICHE_NONE) {
    char *nulln = nc == NICHE_OPT ? "None" : nc == NICHE_OKUNIT ? "Ok" : "Err";

    if (symvarfind(es, nulln) == v)
      fprintf(em->o, "\tstorel 0, %s\n", s); /* the payloadless side */
    else {                                   /* the value side: one pointer payload */
      Type *pt = gsubst(v->payload[0], es->gparams, t->args, t->nargs);
      char *av = emaexpr(em, args[0]);

      fprintf(em->o, "\t%s %s, %s\n", stins(pt), av, s);
    }
    return s;
  }
  { /* the discriminant, then the payloads packed after it */
    Type  *tt = tagtyof(t);
    Type **ps;
    usize  np, i, off = payloadoff(t);

    fprintf(em->o, "\t%s %lu, %s\n", stins(tt), (unsigned long) v->disc, s);
    if (v->named) {
      ps = tyargs(v->nfields);
      for (i = 0; i < v->nfields; i++)
        ps[i] = gsubst(v->fields[i].ty, es->gparams, t->args, t->nargs);
      np = v->nfields;
    } else {
      ps = v->payload && v->npayload ? tyargs(v->npayload) : 0;
      if (ps)
        for (i = 0; i < v->npayload; i++)
          ps[i] = gsubst(v->payload[i], es->gparams, t->args, t->nargs);
      np = ps ? v->npayload : 0;
    }
    if (n != np) /* the checker saw this: not reachable, only honest */
      cerrat(at, "'%s' carries %lu payloads, %lu given", v->name, (unsigned long) np,
             (unsigned long) n);
    for (i = 0; i < np; i++) {
      char *av, *p;

      if (!sizeof_(ps[i])) /* a zero-sized payload (a 'type' field,
                            * 08-reflection.md) holds no value: nothing
                            * to store, and nothing to evaluate for it
                            * either -- the only producers of a type
                            * value are side-effect-free */
        continue;
      av = emaexpr(em, args[i]);
      p = addrplus(em, s, off);
      if (isagg(ps[i]))
        fprintf(em->o, "\tblit %s, %s, %lu\n", av, p, (unsigned long) sizeof_(ps[i]));
      else
        fprintf(em->o, "\t%s %s, %s\n", stins(ps[i]), av, p);
      off += sizeof_(ps[i]);
    }
  }
  return s;
}

/* bind a name to a value: an aggregate binds the storage address it
 * already has, a scalar gets a slot of its own -- the shape a let
 * gives it. Val may be absent (a unit): nothing to store then. */
static void
patbindv(Em *em, char *name, Type *t, char *addr, char *val)
{
  char *slot;
  usize i;

  if (addr) { /* an aggregate binds the storage it sits at */
    locbind(em, name, addr, t);
    return;
  }
  if (em->ormark != (usize) -1) /* an or-pattern is being emitted:
                                 * a sibling alternative already bound this name -- the paths
                                 * join, so the value waits in the pre-bound slot */
    for (i = em->ormark; i < em->nlocs; i++)
      if (strcmp(em->locs[i].name, name) == 0) {
        if (val && sizeof_(t))
          fprintf(em->o, "\t%s %s, %s\n", stins(t), val, em->locs[i].slot);
        return;
      }
  slot = stackslot(em, sizeof_(t) ? sizeof_(t) : 1);
  if (val && sizeof_(t))
    fprintf(em->o, "\t%s %s, %s\n", stins(t), val, slot);
  locbind(em, name, slot, t);
}

/* every name a pattern binds, with the type the checker wrote back
 * on the node: an or-pattern pre-binds them once, before its first
 * test, so every alternative stores into the same slot -- a slot an
 * alternative made itself would be undefined on its sibling's path */
typedef struct
{
  char *name;
  Type *ty;
} PBind;

static void
patboundnames(Ast *p, PBind **out)
{
  switch (p->k) {
  case Npath:
    if (vlen(p->v.path.segs) == 1) {
      PBind b;

      b.name = p->v.path.segs[0]->v.seg.name;
      b.ty = p->ty;
      vappend(out, &b);
    }
    return;
  case Nppath: {
    Ast **segs = p->v.ppath.path->v.path.segs;

    if (vlen(segs) == 1 && !p->v.ppath.payload && !p->v.ppath.named) {
      PBind b;

      /* a short variant name binds nothing, but telling it from a
       * binding wants the type this walk does not carry; the slot
       * it still gets is never stored to nor read */
      b.name = segs[0]->v.seg.name;
      b.ty = p->ty;
      vappend(out, &b);
      return;
    }
    if (p->v.ppath.payload) { /* the sub-patterns, in order */
      Ast **ps = p->v.ppath.payload;
      usize n = vlen(ps), i;

      for (i = 0; i < n; i++)
        if (p->v.ppath.named && !ps[i]->v.init.e) {
          PBind b;

          b.name = ps[i]->v.init.name;
          b.ty = ps[i]->ty;
          vappend(out, &b);
        } else
          patboundnames(p->v.ppath.named ? ps[i]->v.init.e : ps[i], out);
    }
    return;
  }
  case Nptuple: {
    Ast **ps = p->v.list.ts;
    usize n = vlen(ps), i;

    for (i = 0; i < n; i++)
      patboundnames(ps[i], out);
    return;
  }
  case Npstruct: {
    Ast **fs = p->v.pstruct.fields;
    usize n = vlen(fs), i;

    for (i = 0; i < n; i++)
      if (!fs[i]->v.init.e) {
        PBind b;

        b.name = fs[i]->v.init.name;
        b.ty = fs[i]->ty;
        vappend(out, &b);
      } else
        patboundnames(fs[i]->v.init.e, out);
    return;
  }
  case Npor: /* the alternatives bind the same names; the first says */
    patboundnames(p->v.list.ts[0], out);
    return;
  default: /* wild, unit, literals: nothing to bind */
    return;
  }
}

/* match a pattern: on success the bindings are in scope and control
 * falls through, on failure it leaves for fail. The value is the
 * storage address when the type is an aggregate, the loaded
 * temporary otherwise -- exactly one of the two. A NULL fail is
 * let's irrefutable context: a variant pattern there is a compile
 * error, not a runtime one (09-match.md). */
static void
emapat(Em *em, Ast *p, Type *t, char *addr, char *val, char *fail)
{
  while (t && t->k == Tymut) /* the permission layer stays behind */
    t = t->t;
  switch (p->k) {
  case Npwild:
  case Nunit: /* () fits (): the checker saw to it */
    return;
  case Npath:
    patbindv(em, p->v.path.segs[0]->v.seg.name, t, addr, val);
    return;
  case Nppath: {
    Ast    **segs = p->v.ppath.path->v.path.segs;
    Sym     *es;
    Variant *v;

    if (vlen(segs) == 1 && !p->v.ppath.payload && !p->v.ppath.named &&
        (!t || t->k != Tyenum || !symvarfind(t->sym, segs[0]->v.seg.name))) {
      patbindv(em, segs[0]->v.seg.name, t, addr, val); /* the bare binding */
      return;
    }
    if (vlen(segs) == 2) { /* Enum::Variant */
      es = symfind(segs[0]->v.seg.name);
      if (!es || es->kind != Stype || es->tykind != TYenum)
        cerrat(p, "'%s' is not an enum", segs[0]->v.seg.name);
    } else { /* the short name: the matched type picks it out */
      if (!t || t->k != Tyenum)
        cerrat(p, "this pattern needs an enum's value");
      es = t->sym;
    }
    v = symvarfind(es, segs[vlen(segs) - 1]->v.seg.name);
    if (!v)
      cerrat(p, "'%s' has no variant '%s'", es->name, segs[vlen(segs) - 1]->v.seg.name);
    if (!fail)
      cerrat(p, "a let pattern is irrefutable; use match or for let (09-match.md)");
    {
      int nc = nicheness(t);

      if (nc != NICHE_NONE) { /* one pointer: the payloadless side
                               * rides the null */
        char *nulln = nc == NICHE_OPT ? "None" : nc == NICHE_OKUNIT ? "Ok" : "Err";
        int   isnull = symvarfind(es, nulln) == v;
        char *pv = newtmp(em);
        char *c = newtmp(em);
        char *ok = newlbl(em);

        fprintf(em->o, "\t%s =l loadl %s\n", pv, addr);
        if (isnull)
          fprintf(em->o, "\t%s =w ceql %s, 0\n", c, pv);
        else
          fprintf(em->o, "\t%s =w cnel %s, 0\n", c, pv);
        fprintf(em->o, "\tjnz %s, %s, %s\n", c, ok, fail);
        fprintf(em->o, "%s\n", ok);
        if (p->v.ppath.payload) { /* the payload, against its pattern */
          if (isnull)             /* the unit side: the sub-pattern fits a () */
            emapat(em, p->v.ppath.payload[0], tyunit(), 0, 0, fail);
          else {
            Type *pt = gsubst(v->payload[0], es->gparams, t->args, t->nargs);

            emapat(em, p->v.ppath.payload[0], pt, 0, pv, fail);
          }
        }
        return;
      }
      { /* the tag, then the payloads packed after it */
        Type *tt = tagtyof(t);
        char *tv = newtmp(em);
        char *c = newtmp(em);
        char *ok = newlbl(em);
        int   dom = intwidth(tt) > 4 ? 'l' : 'w';

        fprintf(em->o, "\t%s =%c %s %s\n", tv, dom, ldins(tt), addr);
        fprintf(em->o, "\t%s =w ceq%c %s, %lu\n", c, dom, tv, (unsigned long) v->disc);
        fprintf(em->o, "\tjnz %s, %s, %s\n", c, ok, fail);
        fprintf(em->o, "%s\n", ok);
        if (p->v.ppath.named) { /* by field name, mirroring the decl */
          Ast **pfs = p->v.ppath.payload;
          usize nf = vlen(pfs), k;

          for (k = 0; k < nf; k++) {
            Ast  *pf = pfs[k];
            Type *pt = 0;
            usize fo = payloadoff(t), m;

            for (m = 0; m < v->nfields; m++) {
              if (m)
                fo += sizeof_(gsubst(v->fields[m - 1].ty, es->gparams, t->args, t->nargs));
              if (strcmp(v->fields[m].name, pf->v.init.name) == 0) {
                pt = gsubst(v->fields[m].ty, es->gparams, t->args, t->nargs);
                break;
              }
            }
            if (!pt)
              cerrat(pf, "'%s' has no field '%s'", v->name, pf->v.init.name);
            { /* the field name binds, or its sub-pattern matches */
              char *sa, *sv;

              subval(em, pt, addr, fo, pf, &sa, &sv);
              if (pf->v.init.e)
                emapat(em, pf->v.init.e, pt, sa, sv, fail);
              else
                patbindv(em, pf->v.init.name, pt, sa, sv);
            }
          }
          return;
        }
        { /* positional payloads, in declaration order */
          usize np = v->payload ? v->npayload : 0;
          usize off = payloadoff(t), i;

          if (vlen(p->v.ppath.payload) != np)
            cerrat(p, "'%s' carries %lu payloads, %lu given", v->name, (unsigned long) np,
                   (unsigned long) vlen(p->v.ppath.payload));
          for (i = 0; i < np; i++) {
            Type *pt = gsubst(v->payload[i], es->gparams, t->args, t->nargs);
            Ast  *sp = p->v.ppath.payload[i];
            char *sa, *sv;

            subval(em, pt, addr, off, sp, &sa, &sv);
            emapat(em, sp, pt, sa, sv, fail);
            off += sizeof_(pt);
          }
        }
        return;
      }
    }
  }
  case Nptuple: { /* the tuple rows, in order */
    Ast **ps = p->v.list.ts;
    usize n = vlen(ps), i, off = 0;

    for (i = 0; i < n; i++) {
      Type *et = t->args[i];
      char *sa, *sv;

      off = alignto(off, alignof_(et));
      subval(em, et, addr, off, ps[i], &sa, &sv);
      emapat(em, ps[i], et, sa, sv, fail);
      off += sizeof_(et);
    }
    return;
  }
  case Npstruct: { /* the fields it names; ".." ignores the rest */
    Ast **fs = p->v.pstruct.fields;
    usize n = vlen(fs), i;

    for (i = 0; i < n; i++) {
      Ast  *pf = fs[i];
      Type *ft = 0;
      usize k;

      for (k = 0; k < t->sym->nfields; k++)
        if (strcmp(t->sym->fields[k].name, pf->v.init.name) == 0)
          break;
      if (k == t->sym->nfields)
        cerrat(pf, "'%s' has no field '%s'", t->sym->name, pf->v.init.name);
      ft = gsubst(t->sym->fields[k].ty, t->sym->gparams, t->args, t->nargs);
      { /* the field name binds, or its sub-pattern matches */
        char *sa, *sv;
        Ast  *sp = pf->v.init.e ? pf->v.init.e : pf;

        subval(em, ft, addr, foffset(t, pf->v.init.name, pf), sp, &sa, &sv);
        if (pf->v.init.e)
          emapat(em, pf->v.init.e, ft, sa, sv, fail);
        else
          patbindv(em, pf->v.init.name, ft, sa, sv);
      }
    }
    return;
  }
  case Npor: { /* whichever alternative fits: each tests, binds, and
                * joins; the last failure leaves for the outer fail */
    Ast  **ps = p->v.list.ts;
    usize  n = vlen(ps), i;
    char **ls = arenaalloc(n * sizeof *ls);
    char  *join = newlbl(em);
    usize  save = em->ormark;
    PBind *bs = vnew(PBind, 4);

    em->ormark = em->nlocs; /* what the alternatives share */
    patboundnames(ps[0], &bs);
    for (i = 0; i < vlen(bs); i++) { /* one slot per name, made where
                                      * every alternative and the join can see it */
      char *slot = stackslot(em, sizeof_(bs[i].ty) ? sizeof_(bs[i].ty) : 1);

      locbind(em, bs[i].name, slot, bs[i].ty);
    }
    for (i = 1; i < n; i++)
      ls[i] = newlbl(em);
    for (i = 0; i < n; i++) {
      if (i)
        fprintf(em->o, "%s\n", ls[i]);
      emapat(em, ps[i], t, addr, val, i + 1 < n ? ls[i + 1] : fail);
      jump(em, join);
    }
    em->ormark = save;
    fprintf(em->o, "%s\n", join);
    return;
  }
  default:
    cerrat(p, "this pattern arrives with a later milestone");
  }
}

/* a match: the scrutinee once, then an arm at a time -- the
 * pattern's failure goes to the next arm, the body's value waits in
 * a slot like an if's. Exhaustiveness was checked (09-match.md), so
 * the last failure is statically unreachable; abort is the net. */
static char *
emamatch(Em *em, Ast *e, int *reached)
{
  Ast **arms = e->v.call.args;
  usize n = vlen(arms), i;
  Type *st = e->v.call.f->ty;
  Type *t = e->ty;
  int   agg;
  char *sv, *slot = 0, *lend = newlbl(em);
  int   any = 0;

  while (st && st->k == Tymut)
    st = st->t;
  agg = isagg(st);
  sv = aggbase(em, e->v.call.f);             /* the scrutinee, read where it sits */
  if (t && t->k != Tyunit && t->k != Tytype) /* as an if's: a type
                                              * joins in its word */
    slot = mkslot(em, t);
  for (i = 0; i < n; i++) {
    Ast  *arm = arms[i];
    char *next = newlbl(em);
    int   rt;
    usize nbase = em->nlocs;
    char *v;

    emapat(em, arm->v.n2.a, st, agg ? sv : 0, agg ? 0 : sv, next);
    v = armbody(em, arm->v.n2.b, &rt);
    if (slot && v)
      slotput(em, t, v, slot);
    if (rt) {
      jump(em, lend);
      any = 1;
    }
    em->nlocs = nbase;
    fprintf(em->o, "%s\n", next);
  }
  fprintf(em->o, "\tcall $abort()\n"); /* the net: not reachable */
  fprintf(em->o, "\tret 0\n");
  fprintf(em->o, "%s\n", lend);
  *reached = any;
  if (!slot) { /* as an if's: the unit is nothing, a type the word */
    if (t && t->k == Tytype) {
      char *z = newtmp(em);

      fprintf(em->o, "\t%s =w copy 0\n", z);
      return z;
    }
    return 0;
  }
  return slotload(em, t, e, slot);
}

/* the one loop, in its three shapes (10-iteration.md): a condition
 * re-read every round, a pattern re-matched against a re-read
 * value, an iteration. Break and continue land on the shape's own
 * labels -- a continue skips the body's rest, never the step. */
static void
emafor(Em *em, Ast *st)
{
  Ast *body = st->v.forx.body;

  if (em->nloops >= (int) (sizeof em->loops / sizeof em->loops[0]))
    cerrat(st, "loops nest deeper than the emitter carries");
  switch (st->v.forx.shape) {
  case FCOND: { /* while the condition holds */
    char *lc = newlbl(em), *lb = newlbl(em), *lx = newlbl(em);
    char *c;
    int   reached;

    em->loops[em->nloops].brk = lx;
    em->loops[em->nloops].cont = lc;
    em->nloops++;
    fprintf(em->o, "%s\n", lc);
    c = emaexpr(em, st->v.forx.a);
    fprintf(em->o, "\tjnz %s, %s, %s\n", c, lb, lx);
    fprintf(em->o, "%s\n", lb);
    emablockval(em, body, &reached);
    if (reached)
      jump(em, lc);
    em->nloops--;
    fprintf(em->o, "%s\n", lx);
    return;
  }
  case FLET: { /* while the pattern fits the re-read value */
    Type *et = st->v.forx.b->ty;
    char *lc = newlbl(em), *lx = newlbl(em);
    int   agg, reached;
    char *v;
    usize nbase;

    while (et && et->k == Tymut)
      et = et->t;
    agg = isagg(et);
    em->loops[em->nloops].brk = lx;
    em->loops[em->nloops].cont = lc;
    em->nloops++;
    fprintf(em->o, "%s\n", lc);
    v = aggbase(em, st->v.forx.b); /* re-read every round: a static's
                                    * own slot, a const's segment */
    nbase = em->nlocs;
    emapat(em, st->v.forx.a, et, agg ? v : 0, agg ? 0 : v, lx);
    emablockval(em, body, &reached);
    em->nlocs = nbase;
    if (reached)
      jump(em, lc);
    em->nloops--;
    fprintf(em->o, "%s\n", lx);
    return;
  }
  case FIN: {
    Type *et = st->v.forx.b->ty;

    while (et && et->k == Tymut)
      et = et->t;
    if (et->k == Tyenum && et->sym == sym_option) {
      /* ?T yields its one payload, or nothing: one round at most,
       * so the loop is an if -- a continue ends it like a break
       * would (10-iteration.md) */
      Type *pt = et->args[0];
      int   nc = nicheness(et);
      char *sv = aggbase(em, st->v.forx.b);
      char *lsome = newlbl(em), *lx = newlbl(em);
      char *c, *pv = 0;
      int   reached;
      usize nbase;

      em->loops[em->nloops].brk = lx;
      em->loops[em->nloops].cont = lx;
      em->nloops++;
      if (nc != NICHE_NONE) { /* the null is the None */
        pv = newtmp(em);
        c = newtmp(em);
        fprintf(em->o, "\t%s =l loadl %s\n", pv, sv);
        fprintf(em->o, "\t%s =w cnel %s, 0\n", c, pv);
      } else {
        Type    *tt = tagtyof(et);
        Variant *some = symvarfind(et->sym, "Some");
        char    *tv = newtmp(em);
        int      dom = intwidth(tt) > 4 ? 'l' : 'w';

        fprintf(em->o, "\t%s =%c %s %s\n", tv, dom, ldins(tt), sv);
        c = newtmp(em);
        fprintf(em->o, "\t%s =w ceq%c %s, %lu\n", c, dom, tv, (unsigned long) some->disc);
      }
      fprintf(em->o, "\tjnz %s, %s, %s\n", c, lsome, lx);
      fprintf(em->o, "%s\n", lsome);
      nbase = em->nlocs;
      if (nc != NICHE_NONE) /* the payload is the pointer itself */
        emapat(em, st->v.forx.a, pt, 0, pv, lx);
      else {
        char *sa, *svv;

        subval(em, pt, sv, payloadoff(et), st->v.forx.a, &sa, &svv);
        emapat(em, st->v.forx.a, pt, sa, svv, lx);
      }
      emablockval(em, body, &reached);
      em->nlocs = nbase;
      em->nloops--;
      fprintf(em->o, "%s\n", lx);
      return;
    }
    { /* a slice or an array: ptr/len stepped by the element size */
      Type *it = et->t;
      usize sz = sizeof_(it);
      char *sv = aggbase(em, st->v.forx.b); /* the storage it sits at */
      char *ptr, *len, *islot, *i, *c;
      char *lc = newlbl(em), *lb = newlbl(em), *lcont = newlbl(em), *lx = newlbl(em);
      int   reached;
      usize nbase;

      if (et->k == Tyslice) { /* its two named slots (01-types.md) */
        ptr = newtmp(em);
        len = newtmp(em);
        fprintf(em->o, "\t%s =l loadl %s\n", ptr, sv);
        fprintf(em->o, "\t%s =l loadl %s\n", len, addrplus(em, sv, WORD));
      } else { /* the length comes from the type */
        ptr = sv;
        len = newtmp(em);
        fprintf(em->o, "\t%s =l copy %lu\n", len, (unsigned long) et->n);
      }
      islot = stackslot(em, 8);
      fprintf(em->o, "\tstorel 0, %s\n", islot);
      em->loops[em->nloops].brk = lx;
      em->loops[em->nloops].cont = lcont;
      em->nloops++;
      fprintf(em->o, "%s\n", lc);
      i = newtmp(em);
      fprintf(em->o, "\t%s =l loadl %s\n", i, islot);
      c = newtmp(em);
      fprintf(em->o, "\t%s =w cultl %s, %s\n", c, i, len);
      fprintf(em->o, "\tjnz %s, %s, %s\n", c, lb, lx);
      fprintf(em->o, "%s\n", lb);
      nbase = em->nlocs;
      { /* the element's address, ptr + i*size: a slice lends it out
         * as a pointer (10-iteration.md), an owned array yields the
         * element itself -- the loop consumed it (10) */
        char *m = newtmp(em);
        char *ea = newtmp(em);

        fprintf(em->o, "\t%s =l mul %s, %lu\n", m, i, (unsigned long) sz);
        fprintf(em->o, "\t%s =l add %s, %s\n", ea, ptr, m);
        if (et->k == Tyarray) {
          char *sa, *svv;

          subval(em, it, ea, 0, st->v.forx.a, &sa, &svv);
          emapat(em, st->v.forx.a, it, sa, svv, lx);
        } else
          emapat(em, st->v.forx.a, typtr(it), 0, ea, lx);
      }
      emablockval(em, body, &reached);
      em->nlocs = nbase;
      fprintf(em->o, "%s\n", lcont); /* the step: continue lands here */
      {
        char *ni = newtmp(em);

        fprintf(em->o, "\t%s =l add %s, 1\n", ni, i);
        fprintf(em->o, "\tstorel %s, %s\n", ni, islot);
      }
      jump(em, lc);
      em->nloops--;
      fprintf(em->o, "%s\n", lx);
      return;
    }
  }
  default:
    cerrat(st, "this for shape is not one of the three");
  }
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
  case Nbool: { /* a w: 0 or 1, as a comparison would leave */
    char *t = newtmp(em);

    fprintf(em->o, "\t%s =w copy %d\n", t, e->v.i.num ? 1 : 0);
    return t;
  }
  case Nflt: { /* qbe takes float immediates only in call and phi
                * arguments, so the bits go to the data segment and
                * come back with a load */
    char *base = arenaalloc(16), *t = newtmp(em);
    char  c = qbety(e->ty, e);
    char *d = arenaalloc(48);

    sprintf(base, "$flt.%lu", (unsigned long) ++dsn);
    fprintf(em->o, "\t%s =%c load%s %s\n", t, c, c == 's' ? "s" : "d", base);
    /* 9 and 17 significant digits: the least that round-trips */
    sprintf(d, "data %s = { %c %c_%.*g }", base, c, c, c == 's' ? 9 : 17, e->v.f.flt);
    vappend(&em->datas, &d);
    return t;
  }
  case Nunit:
    return 0; /* (): no value, no code */
  case Npath: {
    char *nm = e->v.path.segs[0]->v.seg.name;
    ELoc *l = locfind(em, nm);
    Sym  *s;

    if (l) { /* a local's value: an aggregate is its storage, a ZST
              * -- () or a type's reference -- a word of zero (the
              * registers stay honest), the rest a load */
      if (isagg(l->ty))
        return l->slot;
      if (!sizeof_(l->ty)) {
        char *z = newtmp(em);

        fprintf(em->o, "\t%s =w copy 0\n", z);
        return z;
      }
      return emaload(em, e);
    }
    if (vlen(e->v.path.segs) == 2) { /* Enum::Variant, the
                                      * payloadless read (01-types.md) */
      char *tn = e->v.path.segs[0]->v.seg.name;
      char *vn = e->v.path.segs[1]->v.seg.name;
      Sym  *s0 = symfind(tn);

      if (s0 && s0->kind == Stype && s0->tykind == TYenum) {
        Variant *v = symvarfind(s0, vn);

        if (!v || v->named || v->payload)
          cerrat(e, "'%s' carries a payload; construct it", vn);
        return emavariant(em, e->ty, v, 0, 0, e);
      }
      if (e->v.path.sym) { /* Type::method, a fn as a value: its
                            * address (05-traits.md) */
        char *t = newtmp(em);

        fprintf(em->o, "\t%s =l copy $%s\n", t, fsymname(e->v.path.sym, e->v.path.sym->decl));
        return t;
      }
      cerrat(e, "this name arrives with a later milestone");
    }
    if (vlen(e->v.path.segs) != 1)
      cerrat(e, "this name arrives with a later milestone");
    s = symfind(nm);
    if (!s) { /* None, Ok, Err: bare, the payloadless side */
      Sym *owner = symvariantowner(nm);

      if (owner && e->ty && e->ty->k == Tyenum && e->ty->sym == owner) {
        Variant *v = symvarfind(owner, nm);

        if (v->named || v->payload)
          cerrat(e, "'%s' carries a payload; construct it", nm);
        return emavariant(em, e->ty, v, 0, 0, e);
      }
      cerrat(e, "unknown name '%s'", nm);
    }
    if (s->kind == Sfn) { /* a fn as a value: its address -- an
                           * instantiation's own, when the want made
                           * one, and the chain member the want
                           * picked (04-generics.md) */
      char *t = newtmp(em);
      char *nm;

      if (e->v.path.tys)
        nm = instensure(e->v.path.sym, e->v.path.tys)->name;
      else if (e->v.path.sym)
        nm = fsymname(e->v.path.sym, e->v.path.sym->decl);
      else
        nm = fsymname(s, s->decl);
      fprintf(em->o, "\t%s =l copy $%s\n", t, nm);
      return t;
    }
    if (s->kind == Sconst && s->cvaldone) { /* the value, as an
                                             * immediate: compile-time
                                             * known, so nothing to
                                             * load (08-reflection.md) */
      Type *t = e->ty ? e->ty : s->cty;

      if (isagg(t)) { /* the segment the value rides, copied into
                       * storage of the read's own: a const has no
                       * address to lend, so the copy is the read
                       * (01-types.md, 08-reflection.md) */
        char *slot = stackslot(em, sizeof_(t) ? sizeof_(t) : 1);

        if (sizeof_(t))
          fprintf(em->o, "\tblit %s, %s, %lu\n", constsym(em, s), slot, (unsigned long) sizeof_(t));
        return slot;
      }
      if (t->k == Tyint && t->num >= IN_F32) { /* a float rides the
                                                * data segment, the
                                                * literal's ride */
        char *base = arenaalloc(16), *v = newtmp(em);
        char  c = qbety(t, e);
        char *d = arenaalloc(48);

        sprintf(base, "$flt.%lu", (unsigned long) ++dsn);
        fprintf(em->o, "\t%s =%c load%s %s\n", v, c, c == 's' ? "s" : "d", base);
        /* 9 and 17 significant digits: the least that round-trips */
        sprintf(d, "data %s = { %c %c_%.*g }", base, c, c, c == 's' ? 9 : 17, s->cflt);
        vappend(&em->datas, &d);
        return v;
      }
      {
        char *v = newtmp(em);

        fprintf(em->o, "\t%s =%c copy %lu\n", v, qbety(t, e), (unsigned long) s->cval);
        return v;
      }
    }
    if (s->kind == Sstatic && s->cvaldone) { /* a slot that lives the
                                              * whole program: the
                                              * read loads it, an
                                              * aggregate's value
                                              * copies out of it
                                              * (01-types.md) */
      Type *t = e->ty ? e->ty : s->cty;

      if (isagg(t)) {
        char *slot = stackslot(em, sizeof_(t) ? sizeof_(t) : 1);

        if (sizeof_(t))
          fprintf(em->o, "\tblit %s, %s, %lu\n", constsym(em, s), slot, (unsigned long) sizeof_(t));
        return slot;
      }
      {
        char *v = newtmp(em);

        fprintf(em->o, "\t%s =%c %s %s\n", v, qbety(t, e), ldins(t), constsym(em, s));
        return v;
      }
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

    if (op == Tdyn) { /* &dyn b / &mut dyn b: the fat -- the place's
                       * address, the impl's table (06-dispatch.md) */
      char *addr = emaplace(em, e->v.un.e);
      char *p8 = newtmp(em);
      char *vt = vtname(e->ty->sym, e->v.un.e->ty);

      t = stackslot(em, 16);
      fprintf(em->o, "\tstorel %s, %s\n", addr, t);
      fprintf(em->o, "\t%s =l add %s, 8\n", p8, t);
      fprintf(em->o, "\tstorel $%s, %s\n", vt, p8);
      return t;
    }
    if (op == Tamp)
      return emaplace(em, e->v.un.e); /* &place: the address itself */
    if (op == Tcaret2) {              /* a type's value: sizeless -- a word of
                                       * zero keeps the registers honest, for the
                                       * uses are compile-time's own, the $$
                                       * splices (08-reflection.md) */
      char *z = newtmp(em);

      fprintf(em->o, "\t%s =w copy 0\n", z);
      return z;
    }
    if (op == Tminus && e->v.un.e->k == Nint) {
      /* the folded least (01-types.md): a signed type's least is
       * negated at the check, and here in one step -- a copy of the
       * negative, not a neg of a magnitude the width cannot hold */
      long m = (long) (0 - e->v.un.e->v.i.num);

      t = newtmp(em);
      fprintf(em->o, "\t%s =%c copy %ld\n", t, qbety(e->ty, e), m);
      return t;
    }
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
    char *a, *b, *t = newtmp(em);
    Type *lt = e->v.bin.l->ty;
    usize i;

    if (op == Tampamp || op == Tbarbar) { /* the value rides a slot
                                           * like an if's does */
      char *lv = newlbl(em), *lend = newlbl(em);
      char *slot = mkslot(em, e->ty);
      char *vw;

      a = emaexpr(em, e->v.bin.l);
      slotstore(em, e->ty, a, slot); /* the left alone decides:
                                      * false for &&, true for || */
      fprintf(em->o, "\tjnz %s, %s, %s\n", a, op == Tampamp ? lv : lend, op == Tampamp ? lend : lv);
      fprintf(em->o, "%s\n", lv);
      vw = emaexpr(em, e->v.bin.r);
      slotstore(em, e->ty, vw, slot);
      fprintf(em->o, "%s\n", lend);
      return slotload(em, e->ty, e, slot);
    }
    a = emaexpr(em, e->v.bin.l);
    b = emaexpr(em, e->v.bin.r);
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
    char  *nm = 0;     /* a direct callee's symbol */
    char  *ra = 0;     /* the sugar's receiver, walking first */
    char  *dynfp = 0;  /* a handle's call: the vtable slot (06) */
    Type  *selfty = 0; /* the receiver's parameter type, for the call's spelling */
    Type  *rty = 0;    /* the receiver as written */

    if (f->k == Npath) { /* a variant's construction reads as a
                          * call: Some(v), Enum::V(v) (01-types.md) */
      Ast **segs = f->v.path.segs;

      if (vlen(segs) == 1) {
        char *nm = segs[0]->v.seg.name;

        if (!locfind(em, nm) && !symfind(nm)) {
          Sym *owner = symvariantowner(nm);

          if (!owner || !e->ty || e->ty->k != Tyenum || e->ty->sym != owner)
            cerrat(f, "unknown name '%s'", nm);
          return emavariant(em, e->ty, symvarfind(owner, nm), args, n, e);
        }
      } else if (vlen(segs) == 2) {
        s = symfind(segs[0]->v.seg.name);
        if (s && s->kind == Stype && s->tykind == TYenum) {
          Variant *v = symvarfind(s, segs[1]->v.seg.name);

          if (!v)
            cerrat(f, "'%s' has no variant '%s'", s->name, segs[1]->v.seg.name);
          return emavariant(em, e->ty, v, args, n, e);
        }
      }
    }
    { /* the method calls: the sugar's receiver walks first, adapted
       * the way the sugar defines it (05-traits.md); a Type::member
       * call spells its arguments out in full, self among them */
      Sym *ms = e->v.call.sym;

      if (f->k == Naccess) {
        Type *ft; /* the instance's signature, when the receiver
                   * bound a pattern impl's parameters */

        rty = f->v.fld.e->ty;
        if (rty && rty->k == Tydyn) { /* the fat call: the pointer
                                       * the fat holds, the method
                                       * its table names -- the
                                       * choice was made where the
                                       * handle was (06-dispatch.md) */
          Sym  *tr = rty->sym;
          char *fat = emaexpr(em, f->v.fld.e);
          char *p8 = newtmp(em);
          char *vt = newtmp(em);
          char *mf = newtmp(em);
          usize idx = 0, mi;

          for (mi = 0; mi < tr->nmembers; mi++) {
            if (tr->members[mi].kind != Mfn)
              continue;
            if (strcmp(tr->members[mi].name, f->v.fld.name) == 0)
              break;
            idx++;
          }
          if (mi == tr->nmembers)
            cerrat(f, "unreachable: the checker found the member");
          ra = newtmp(em);
          fprintf(em->o, "\t%s =l loadl %s\n", ra, fat);
          fprintf(em->o, "\t%s =l add %s, 8\n", p8, fat);
          fprintf(em->o, "\t%s =l loadl %s\n", vt, p8);
          if (idx) { /* the slot, in declaration order */
            char *o = newtmp(em);

            fprintf(em->o, "\t%s =l add %s, %lu\n", o, vt, (unsigned long) idx * 8);
            vt = o;
          }
          fprintf(em->o, "\t%s =l loadl %s\n", mf, vt);
          selfty = typtr(tyvoidptr()); /* Self erased (06): what
                                        * survives is pointers, one
                                        * width */
          dynfp = mf;
        } else {
          if (!ms || ms->kind != Sfn)
            cerrat(f, "this method call was never checked");
          ft = ms->fnty;
          if (e->v.call.tys)
            ft = gsubst(ft, ms->gparams, e->v.call.tys, ms->ngparams);
          selfty = ft->nargs ? ft->args[0] : 0;
          if (selfty && selfty->k == Typtr && !(rty && rty->k == Typtr && tysame(rty, selfty)))
            ra = emaplace(em, f->v.fld.e); /* a pointer self: &place */
          else
            ra = emaexpr(em, f->v.fld.e); /* as written: the pointer, or the move */
          ra = nicheout(em, selfty, ra);
          nm = e->v.call.tys ? instensure(ms, e->v.call.tys)->name : fsymname(ms, ms->decl);
        }
      } else if (f->k == Npath && vlen(f->v.path.segs) == 2 && ms) {
        nm = e->v.call.tys ? instensure(ms, e->v.call.tys)->name : fsymname(ms, ms->decl);
      }
    }
    for (i = 0; i < n; i++) {
      as[i] = nicheout(em, args[i]->ty, emaexpr(em, args[i]));
      if (!as[i]) /* a ZST argument -- () or a type's reference --
                   * crosses as the word zero it arrived in
                   * (08-reflection.md) */
        as[i] = "0";
    }
    if (!nm && f->k == Npath && vlen(f->v.path.segs) == 1 &&
        !locfind(em, f->v.path.segs[0]->v.seg.name)) {
      s = symfind(f->v.path.segs[0]->v.seg.name);
      if (!s || s->kind != Sfn)
        cerrat(f, "'%s' is not a fn", f->v.path.segs[0]->v.seg.name);
      if (e->v.call.sym) { /* the checker's pick: which overload, and
                            * which instantiation -- the latter names
                            * its own copy (04-generics.md) */
        s = e->v.call.sym;
        if (e->v.call.tys)
          nm = instensure(s, e->v.call.tys)->name;
        else
          nm = fsymname(s, s->decl);
      } else {
        if (s->next)
          cerrat(f, "overload resolution at emit time arrives with M3d");
        nm = fsymname(s, s->decl);
      }
    }
    if (nm)
      fprintf(em->o, "\t%s =%s call $%s(", t, sigty(e->ty, e), nm);
    else if (dynfp) /* the vtable slot: called through it (06) */
      fprintf(em->o, "\t%s =%s call %s(", t, sigty(e->ty, e), dynfp);
    else { /* a fn held in a value, called through it */
      char *fp = emaexpr(em, f);

      fprintf(em->o, "\t%s =%s call %s(", t, sigty(e->ty, e), fp);
    }
    if (ra)
      fprintf(em->o, "%s %s", sigty(selfty, f->v.fld.e), ra);
    for (i = 0; i < n; i++)
      fprintf(em->o, "%s%s %s", (i || ra) ? ", " : "", sigty(args[i]->ty, args[i]), as[i]);
    fputs(")\n", em->o);
    return nichein(em, e->ty, t);
  }
  case Nbuiltin: {
    char *nm = e->v.blt.name;
    Ast **targs = e->v.blt.targs;

    if (strcmp(nm, "compileError") == 0) {
      char *t = newtmp(em); /* the evaluator ran the fn and did not
                             * reach this branch: emit gives it no
                             * runtime behavior -- panic is the
                             * running program's report (08) */
      fprintf(em->o, "\t%s =w copy 0\n", t);
      return t;
    }
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

      if (from->k == Tyenum) { /* the discriminant out: the tag at
                                * the head of the value, loaded at
                                * its own width and widened by the
                                * target -- a tag is unsigned, the
                                * numbers count up (01-types.md) */
        Type *tt = tagtyof(from);
        char *tv = newtmp(em);
        char *r;

        if ((to->k == Tyint && to->num >= IN_F32) || tb)
          cerrat(e, "an enum casts to an integer, the tag's own kind (01-types.md)");
        fprintf(em->o, "\t%s =%c %s %s\n", tv, qbety(tt, e), ldins(tt), a);
        if (tl) {
          r = newtmp(em);
          fprintf(em->o, "\t%s =l extuw %s\n", r, tv);
        } else /* the word: the load zero-extended it already */
          r = tv;
        return r;
      }
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
    if (strcmp(nm, "typeof") == 0) { /* a type's reference: sizeless
                                      * -- a word of zero keeps the
                                      * registers honest; the $$
                                      * splices it at compile time
                                      * (08-reflection.md) */
      char *z = newtmp(em);

      fprintf(em->o, "\t%s =w copy 0\n", z);
      return z;
    }
    if (strcmp(nm, "typeinfo") == 0) { /* the deferred splice, the
                                        * one form that reaches here:
                                        * the fn around it ran at
                                        * compile time already, and
                                        * this text answers a runtime
                                        * call nothing can usefully
                                        * make -- a zeroed TypeInfo,
                                        * the Bool it reads, is the
                                        * same no-op @compileError's
                                        * branch is (08) */
      char *slot = newtmp(em);
      char *t = newtmp(em);

      fprintf(em->o, "\t%s =l alloc8 %lu\n", slot,
              (unsigned long) (sizeof_(typeinfoty()) ? sizeof_(typeinfoty()) : 1));
      fprintf(em->o, "\tstorew 0, %s\n", slot); /* the tag: Bool's own 0 */
      fprintf(em->o, "\t%s =l copy %s\n", t, slot);
      return t;
    }
    cerrat(e, "this builtin arrives with a later milestone");
    return 0; /* unreachable */
  }
  case Nstr: { /* the bytes go to the data segment; the value is the
                * slice itself, two words of memory */
    char *base = arenaalloc(16);
    char *t;
    usize i;

    sprintf(base, "$str.%lu", (unsigned long) ++dsn);
    t = stackslot(em, 16);
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
    char *t = stackslot(em, sz);
    Ast **inits = e->v.slit.inits;
    usize i;

    fprintf(em->o, "\tblit %s, %s, %lu\n", zeroblk(em, sz), t, (unsigned long) sz);
    for (i = 0; i < vlen(inits); i++) {
      Ast  *ini = inits[i];
      char *v;
      Type *ft = 0;
      usize off = foffset(st, ini->v.init.name, e);
      usize j;

      for (j = 0; j < st->sym->nfields; j++)
        if (strcmp(st->sym->fields[j].name, ini->v.init.name) == 0)
          ft = gsubst(st->sym->fields[j].ty, st->sym->gparams, st->args, st->nargs);
      if (!sizeof_(ft)) /* a zero-sized field holds no value -- the
                         * same skip a variant's payload makes */
        continue;
      v = emaexpr(em, ini->v.init.e);
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
  case Ntuple: { /* storage first, then a row at a time -- the
                  * struct rule, rows in order (02-layout.md) */
    Type *tt = e->ty;
    Ast **es = e->v.list.ts;
    usize n = vlen(es), i, off = 0;
    char *t = mkslot(em, tt);

    for (i = 0; i < n; i++) {
      Type *rt = es[i]->ty;
      char *v = emaexpr(em, es[i]);
      char *p;

      off = alignto(off, alignof_(rt));
      p = addrplus(em, t, off);
      if (isagg(rt))
        fprintf(em->o, "\tblit %s, %s, %lu\n", v, p, (unsigned long) sizeof_(rt));
      else
        fprintf(em->o, "\t%s %s, %s\n", stins(rt), v, p);
      off += sizeof_(rt);
    }
    return t;
  }
  case Ntupidx: { /* the row's offset, then the row */
    Type *tt = e->v.tup.e->ty;
    char *b = emaexpr(em, e->v.tup.e);
    usize i, off = 0;

    for (i = 0; i < e->v.tup.idx; i++) {
      off = alignto(off, alignof_(tt->args[i]));
      off += sizeof_(tt->args[i]);
    }
    off = alignto(off, alignof_(e->ty));
    { /* the row itself: an aggregate is the address, a scalar loads */
      char *p = addrplus(em, b, off);

      return slotload(em, e->ty, e, p);
    }
  }
  case Nindex: { /* the element's address, then the element */
    char *p = idxaddr(em, e);

    return slotload(em, e->ty, e, p);
  }
  case Nrangeindex: { /* a view: the data plus lo, the length hi-lo */
    Type *bt = e->v.ridx.e->ty;
    Type *et = bt->t;
    usize sz;
    char *b = aggbase(em, e->v.ridx.e);
    char *data = b, *len = 0, *t = mkslot(em, e->ty);
    char *lo = 0, *hi = 0;

    while (et && et->k == Tymut) /* []mut T: the element's own type */
      et = et->t;
    sz = sizeof_(et);
    if (bt->k == Tyslice) { /* both words of the borrowed view */
      char *p = newtmp(em);

      data = newtmp(em);
      len = newtmp(em);
      fprintf(em->o, "\t%s =l loadl %s\n", data, b);
      p = addrplus(em, b, WORD);
      fprintf(em->o, "\t%s =l loadl %s\n", len, p);
    }
    if (e->v.ridx.lo)
      lo = idxl(em, e->v.ridx.lo);
    if (e->v.ridx.hi)
      hi = idxl(em, e->v.ridx.hi);
    else if (bt->k == Tyarray) { /* a[..]: the whole length, a constant */
      hi = newtmp(em);
      fprintf(em->o, "\t%s =l copy %lu\n", hi, (unsigned long) bt->n);
    } /* else a slice: hi is the len that was loaded */
    if (lo && sz) { /* the data pointer moves; the length counts */
      char *sc = newtmp(em);
      char *nd = newtmp(em);

      fprintf(em->o, "\t%s =l mul %s, %lu\n", sc, lo, (unsigned long) sz);
      fprintf(em->o, "\t%s =l add %s, %s\n", nd, data, sc);
      data = nd;
    }
    if (!hi)
      hi = len;
    if (lo) {
      char *nl = newtmp(em);

      fprintf(em->o, "\t%s =l sub %s, %s\n", nl, hi, lo);
      hi = nl;
    }
    fprintf(em->o, "\tstorel %s, %s\n", data, t);
    { /* the length rides in the second word */
      char *p = addrplus(em, t, WORD);

      fprintf(em->o, "\tstorel %s, %s\n", hi, p);
    }
    return t;
  }
  case Narraylit: {   /* the elements in a row; a slice literal then
                       * names them with a view of its own */
    Type *at = e->ty; /* [N]T or []T (01-types.md) */
    Type *et = at->t;
    Ast **es = e->v.arrlit.es;
    usize n = vlen(es), i, sz;
    usize whole; /* the storage: [N]T holds N, a slice only what it lists */
    char *t;

    while (et && et->k == Tymut) /* []mut T: the element's own type */
      et = et->t;
    sz = sizeof_(et);
    whole = at->k == Tyarray ? at->n * sz : n * sz;
    t = stackslot(em, whole ? whole : 1);
    if (at->k == Tyarray && n < at->n) /* the elements left out read
                                        * as zero (01-types.md) */
      fprintf(em->o, "\tblit %s, %s, %lu\n", zeroblk(em, whole), t, (unsigned long) whole);
    for (i = 0; i < n; i++) { /* element i sits at i * size: no
                               * padding between, the elements one
                               * type (02-layout.md) */
      Type *rt = es[i]->ty;
      char *v = emaexpr(em, es[i]);
      char *p = addrplus(em, t, i * sz);

      if (isagg(rt))
        fprintf(em->o, "\tblit %s, %s, %lu\n", v, p, (unsigned long) sz);
      else
        fprintf(em->o, "\t%s %s, %s\n", stins(rt), v, p);
    }
    if (at->k != Tyslice)
      return t; /* the array is its own storage */
    {           /* the slice: a fresh view, the elements borrowed (01) */
      char *v = mkslot(em, at);

      fprintf(em->o, "\tstorel %s, %s\n", t, v);
      { /* the length rides in the second word */
        char *p = addrplus(em, v, WORD);
        char *l = newtmp(em);

        fprintf(em->o, "\t%s =l copy %lu\n", l, (unsigned long) n);
        fprintf(em->o, "\tstorel %s, %s\n", l, p);
      }
      return v;
    }
  }
  case Nif: {
    int reached;

    return emaif(em, e, &reached);
  }
  case Nmatch: {
    int reached;

    return emamatch(em, e, &reached);
  }
  case Nblock: { /* a block in expression position: its bindings end
                  * with it, its tail is its value */
    int reached;

    return emablockval(em, e, &reached);
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
    Type *t = st->ty;
    char *nm = 0;
    char *v;

    if (pat->k == Npath)
      nm = pat->v.path.segs[0]->v.seg.name;
    else if (pat->k == Nppath && vlen(pat->v.ppath.path->v.path.segs) == 1 &&
             !pat->v.ppath.payload && !pat->v.ppath.named)
      nm = pat->v.ppath.path->v.path.segs[0]->v.seg.name;
    if (nm) { /* the one-name form: the binding is the value's */
      v = st->v.let.e ? emaexpr(em, st->v.let.e) : 0;
      if (isagg(t)) /* the initializer's storage is the binding's: a
                     * move, not a copy (03-move.md) */
        locbind(em, nm, v, t);
      else {
        char *slot = stackslot(em, sizeof_(t) ? sizeof_(t) : 1);

        if (v && sizeof_(t))
          fprintf(em->o, "\t%s %s, %s\n", stins(t), v, slot);
        locbind(em, nm, slot, t);
      }
      return;
    }
    /* a destructuring pattern: the value once, the bindings in it --
     * read where it sits, for the bindings copy out of it */
    v = st->v.let.e ? aggbase(em, st->v.let.e) : 0;
    emapat(em, pat, t, isagg(t) ? v : 0, isagg(t) ? 0 : v, 0);
    return;
  }
  case Nassign: {
    Ast  *lhs = st->v.bin.l;
    Tok   op = st->v.bin.op;
    char *p;

    if (!lhs->ty)
      cerrat(lhs, "this place was never checked");
    p = emaplace(em, lhs);
    if (op != Teq) { /* the compound forms: read, apply, write back */
      static struct
      {
        Tok   t;
        char *i; /* signed */
        char *u; /* unsigned, when it differs */
      } ops[] = {
          {Tpluseq, "add", 0},       {Tminuseq, "sub", 0}, {Tstareq, "mul", 0},
          {Tslasheq, "div", "udiv"}, {Tshleq, "shl", 0},   {Tshreq, "sar", "shr"},
      };
      char *old = newtmp(em), *nv = newtmp(em), *v;
      usize i;

      v = emaexpr(em, st->v.bin.r);
      fprintf(em->o, "\t%s =%c %s %s\n", old, qbety(lhs->ty, lhs), ldins(lhs->ty), p);
      for (i = 0; i < sizeof ops / sizeof ops[0]; i++)
        if (ops[i].t == op) {
          fprintf(em->o, "\t%s =%c %s %s, %s\n", nv, qbety(lhs->ty, lhs),
                  ops[i].u && isuintty(lhs->ty) ? ops[i].u : ops[i].i, old, v);
          break;
        }
      if (i == sizeof ops / sizeof ops[0])
        cerrat(st, "this compound assignment arrives with a later milestone");
      fprintf(em->o, "\t%s %s, %s\n", stins(lhs->ty), nv, p);
      return;
    }
    if (isagg(lhs->ty)) { /* an aggregate's copy is a blit */
      char *v = emaexpr(em, st->v.bin.r);

      fprintf(em->o, "\tblit %s, %s, %lu\n", v, p, (unsigned long) sizeof_(lhs->ty));
      return;
    }
    {
      char *v = emaexpr(em, st->v.bin.r);

      if (sizeof_(lhs->ty)) /* a ZST's assignment moves no bits */
        fprintf(em->o, "\t%s %s, %s\n", stins(lhs->ty), v, p);
    }
    return;
  }
  case Nreturn: {
    if (st->v.n1.e) { /* an aggregate's value is its address, and
                       * the :type convention carries it; a niche is
                       * one pointer, loaded out to its 'l' (M3d) */
      char *v = nicheout(em, st->v.n1.e->ty, emaexpr(em, st->v.n1.e));

      fprintf(em->o, "\tret %s\n", v);
    } else
      fputs("\tret 0\n", em->o); /* (): the w carries nothing */
    return;
  }
  case Nexprstmt:
    emaexpr(em, st->v.n1.e);
    return;
  case Nbreak: {
    if (em->nloops <= 0)
      cerrat(st, "this break is not in a for");
    jump(em, em->loops[em->nloops - 1].brk);
    return;
  }
  case Ncontinue: {
    if (em->nloops <= 0)
      cerrat(st, "this continue is not in a for");
    jump(em, em->loops[em->nloops - 1].cont);
    return;
  }
  case Nfor:
    emafor(em, st);
    return;
  case Ncfor: { /* the unroll the evaluator spelled: each round's
                 * let, the body it owns -- and no loop surviving
                 * into this text, which is the marker's whole
                 * promise (10-iteration.md) */
    Ast **un = st->v.forx.unroll;
    usize i;

    for (i = 0; i < vlen(un); i++)
      emastmt(em, un[i]);
    return;
  }
  default: /* if, match, blocks: expressions in statement position */
    emaexpr(em, st);
  }
}

/* a block as a value: its statements, then its tail as its value.
 * A return mid-way ends the walk -- what follows is dead -- and
 * says so by not reaching. The bindings a block makes end with it,
 * so the location count goes back to where it was. */
static char *
emablockval(Em *em, Ast *body, int *reached)
{
  Ast **stmts = body->v.blk.stmts;
  usize i, nbase = em->nlocs;
  char *v = 0;

  *reached = 1;
  for (i = 0; i < vlen(stmts); i++) {
    emastmt(em, stmts[i]);
    if (stmts[i]->k == Nreturn || stmts[i]->k == Nbreak || stmts[i]->k == Ncontinue) {
      *reached = 0; /* the rest is dead */
      goto out;
    }
  }
  if (body->v.blk.tail)
    v = emaexpr(em, body->v.blk.tail);
  else
    v = 0; /* "{}": the unit */
out:
  em->nlocs = nbase;
  return v;
}

/* one fn's text. name is the symbol it goes by -- an instance's own
 * (emitinst), or the fn's (emitall); ats and ret are the signature,
 * the fn's own when the caller passes NULL -- an instance carries
 * its substituted one (04-generics.md). */
static void
emitfn(FILE *o, Sym *s, Ast *it, char *name, Type **ats, Type *ret)
{
  Type *fnty = s->fnty;
  Em    em;
  FILE *body;
  usize i;

  if (!ret)
    ret = fnty->t; /* the fn's own: not an instantiation */
  memset(&em, 0, sizeof em);
  em.ormark = (usize) -1; /* no or-pattern yet: reuse is off */
  em.allocs = vnew(char *, 16);
  em.datas = vnew(char *, 8);
  em.locs = vnew(ELoc, 16);
  body = tmpfile(); /* the fn's text, held back: the stack its body
                     * asks for goes ahead of it, at the entry */
  if (!body)
    die("a fn's body could not be buffered");
  em.o = body;
  fputs("export function", o);
  if (ret)
    fprintf(o, " %s", sigty(ret, it));
  fprintf(o, " $%s(", name);
  for (i = 0; i < fnty->nargs; i++) {
    if (i)
      fputs(", ", o);
    fprintf(o, "%s %%%s", sigty(ats ? ats[i] : fnty->args[i], it),
            it->v.fn.params[i]->v.param.name);
  }
  fputs(") {\n@start\n", o);
  for (i = 0; i < fnty->nargs; i++) { /* every parameter a slot: one
                                       * path reads them all */
    char *nm = it->v.fn.params[i]->v.param.name;
    Type *pt = ats ? ats[i] : fnty->args[i];

    while (pt && pt->k == Tymut)
      pt = pt->t;
    if (isabb(pt)) { /* the incoming temp is the copy's own address:
                      * nothing to store (01-types.md: the C
                      * convention, which qbe lowers) */
      char *tmp = arenaalloc(strlen(nm) + 2);

      sprintf(tmp, "%%%s", nm);
      locbind(&em, nm, tmp, pt);
      continue;
    }
    if (nicheness(pt)) { /* one pointer arrived in a register: the
                          * slot the value model wants */
      char *slot = newtmp(&em);

      fprintf(o, "\t%s =l alloc8 8\n", slot);
      fprintf(o, "\tstorel %%%s, %s\n", nm, slot);
      locbind(&em, nm, slot, pt);
      continue;
    }
    {
      char *slot = newtmp(&em);

      fprintf(o, "\t%s =l alloc8 %lu\n", slot, (unsigned long) (sizeof_(pt) ? sizeof_(pt) : 1));
      if (sizeof_(pt)) /* a ZST -- () or `type` -- crosses in the
                        * word it arrived in, and the slot stays
                        * unused (08-reflection.md) */
        fprintf(o, "\t%s %%%s, %s\n", stins(pt), nm, slot);
      locbind(&em, nm, slot, pt);
    }
  }
  {
    int   reached;
    char *v = emablockval(&em, it->v.fn.body, &reached);

    if (reached) { /* a niche returns as its one pointer, an
                    * aggregate as the address the :type names */
      v = nicheout(&em, ret, v);
      fprintf(em.o, "\tret %s\n", v ? v : "0");
    }
  }
  for (i = 0; i < vlen(em.allocs); i++) /* the entry's asks, ahead of
                                         * the text that asked for
                                         * them */
    fprintf(o, "%s", em.allocs[i]);
  {
    char   buf[4096];
    size_t n;

    rewind(body);
    while ((n = fread(buf, 1, sizeof buf, body)) > 0)
      fwrite(buf, 1, n, o);
  }
  fclose(body);
  fputs("}\n\n", o);
  for (i = 0; i < vlen(em.datas); i++) /* the strings this fn grew */
    fprintf(o, "%s\n", em.datas[i]);
}

/* one instantiation: re-check the body under this binding -- the
 * writeback the emitter reads -- then emit it under its own name.
 * The body is the instance's own copy: the walks write what they
 * walk -- @typeinfo into the description it builds, @field into the
 * borrow it spells -- and what one instance wrote the next must not
 * read (04-generics.md, 10-iteration.md) */
static void
emitinst(FILE *o, Inst *in)
{
  Sym   *s;
  Ast   *it;
  usize  ng, i;
  Type **ats;

  s = in->s;
  it = astclone(s->decl); /* the shared declaration, copied whole:
                           * every instance rewrites its own */
  ng = s->ngparams;
  ats = vlen(it->v.fn.params) ? tyargs(vlen(it->v.fn.params)) : 0;
  for (i = 0; i < vlen(it->v.fn.params); i++)
    ats[i] = gsubst(s->fnty->args[i], s->gparams, in->tys, ng);
  recheckfn(s, it, in->tys);
  emitfn(o, s, it, in->name, ats, gsubst(s->fnty->t, s->gparams, in->tys, ng));
}

/* the worklist until it empties: emitting a body finds more calls,
 * and the cursor grows with them. Each pass queues every instance
 * once; the queue itself is fresh, the table is not */
static void
draininsts(FILE *o)
{
  usize i;
  Inst *in;

  i = 0;
  while (i < vlen(iqueue)) {
    in = iqueue[i++];
    emitinst(o, in);
  }
  iqueue = vnew(Inst *, 16);
}

static void
emitall(FILE *out, Ast **items)
{
  usize i;

  for (i = 0; i < vlen(items); i++) {
    Ast *it = items[i];
    Sym *s;

    if (it->k == Nimpl) {
      /* an impl's methods emit as the fns they are (05-traits.md) --
       * inherent or trait alike, when the impl is not a pattern. A
       * generic impl's wait for the calls that instantiate them
       * (04-generics.md): the drain below emits those. */
      usize j;

      for (j = 0; j < chk_nimpls; j++)
        if (chk_impls[j]->decl == it)
          break;
      if (j == chk_nimpls)
        continue; /* unreachable: the resolver collected it */
      s = chk_impls[j];
      if (s->ngparams)
        continue; /* a generic impl's methods arrive with
                   * specialization (04-generics.md) */
      for (j = 0; j < s->nmembers; j++) {
        Member *dm = &s->members[j];

        if (dm->kind != Mfn || !dm->decl || dm->decl->v.fn.body == 0)
          continue; /* a declaration: an import */
        if (vlen(dm->decl->v.fn.gparams))
          continue; /* a generic method arrives with
                     * specialization (04-generics.md) */
        emitfn(out, dm->sym, dm->decl, fsymname(dm->sym, dm->decl), 0, 0);
      }
      continue;
    }
    if (it->k != Nfn || it->v.fn.body == 0)
      continue; /* a declaration: an import, or a fn type's use */
    s = symfind(it->v.fn.name);
    while (s && s->decl != it) /* its own Sym: a name's chain holds
                                * every overload of it */
      s = s->next;
    if (!s)
      continue; /* unreachable: the resolver made one */
    if (s->ngparams)
      continue; /* a generic fn emits per instance, from its call
                 * sites (04-generics.md) */
    emitfn(out, s, it, fsymname(s, it), 0, 0);
  }
}

void
emitfile(FILE *out, Ast **items)
{
  FILE *scratch = tmpfile(); /* pass one names the aggregates and
                              * finds every instantiation; its text
                              * goes nowhere (qbe wants the type
                              * declarations before their first use) */

  if (!scratch) {
    fprintf(stderr, "xyz: no scratch file for the type pass\n");
    exit(1);
  }
  ipass = 1;
  emitall(scratch, items);
  draininsts(scratch);
  for (;;) { /* the tables the handles named: their entries name
              * instances no static call found, so the print is
              * what queues them */
    draininsts(scratch);
    if (vtprinted == vlen(vts))
      break;
    printvts(scratch);
  }
  fclose(scratch);
  dsn = 0;       /* the naming pass burned numbers; the real one restarts */
  vtprinted = 0; /* the text restarts with it */
  abidecls(out); /* the :type declarations, the order qbe reads */
  ipass = 2;
  emitall(out, items); /* pass two: the text */
  draininsts(out);
  for (;;) {
    draininsts(out);
    if (vtprinted == vlen(vts))
      break;
    printvts(out);
  }
}
