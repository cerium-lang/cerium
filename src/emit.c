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

/* an int, the floats out: isuintty's kin, flow.c's isintty the
 * same read -- the checks want the ints alone */
static int
isintty(Type *t)
{
  return t && t->k == Tyint && t->num != IN_F32 && t->num != IN_F64;
}

__extension__ typedef long long i64; /* the signed kin of lex.h's u64,
                                      * eval.c's own move */

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
  int   openend; /* the fn body's last text ends in the call the abort
                  * never returns from: qbe still wants its terminator
                  * -- emitfn's dead ret (10-iteration.md) */
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
 * instantiation all force it (01-types.md, External functions). The
 * spelling is segments -- a length, then the bytes -- with the
 * project's own name first and the type codes a closed alphabet, so
 * two symbols fold the same only by naming the same thing
 * (12-projects.md, Symbols). */
static usize segput(char *buf, usize o, usize n, const char *s);
static usize segnum(char *buf, usize o, usize n, u64 v);
static usize tymang2(Type *t, char *buf, usize o, usize n);
static usize projhead(Sym *s, char *buf, usize n);

/* a method's symbol: the head, the target type's own code, the
 * trait's beside it for a trait impl -- an inherent's slot says none
 * -- then the member's name. The target's code is its whole path and
 * binding, so two impls of one generic type at different bindings,
 * or one builtin twice over, are different symbols by construction:
 * the index the old spelling's twins needed is gone
 * (12-projects.md, Symbols) */
static char *
memname(Sym *s)
{
  char  buf[1024];
  usize o;

  o = projhead(s, buf, sizeof buf);
  o = tymang2(s->impl->ifort ? s->impl->ifort : s->impl->ipath, buf, o, sizeof buf);
  if (s->impl->ifort) /* a trait impl: the trait names the method's
                       * other home, an inherent has none */
    o = tymang2(s->impl->ipath, buf, o, sizeof buf);
  else
    buf[o++] = 'z';
  o = segput(buf, o, sizeof buf, s->name);
  buf[o] = 0;
  {
    char *n = arenaalloc(o + 1);

    memcpy(n, buf, o + 1);
    return n;
  }
}

static char *
fsymname(Sym *s, Ast *it)
{
  char  buf[1024];
  usize o, i;

  if (attrfind(it->attrs, "extern"))
    return s->name;
  if (s->impl)
    return memname(s); /* a method: its target, its own name */
  o = projhead(s, buf, sizeof buf);
  o = segput(buf, o, sizeof buf, s->name);
  if (s->next || nsitem(s->ownns, s->name) != s) { /* an overload:
                                                    * this signature,
                                                    * the argument
                                                    * count then each
                                                    * type then the
                                                    * return -- the
                                                    * codes say two
                                                    * signatures apart
                                                    * wherever they
                                                    * differ, and no
                                                    * declaration
                                                    * order is in the
                                                    * spelling */
    o = segnum(buf, o, sizeof buf, s->fnty->nargs);
    for (i = 0; i < s->fnty->nargs; i++)
      o = tymang2(s->fnty->args[i], buf, o, sizeof buf);
    o = tymang2(s->fnty->t, buf, o, sizeof buf);
  }
  buf[o] = 0;
  {
    char *n = arenaalloc(o + 1);

    memcpy(n, buf, o + 1);
    return n;
  }
}

/* -- the runtime checks (01-types.md, Build modes) ---------------------- */

/* debug inserts them, release leaves them out: rel, which -r set
 * through emitfile, says which. The failure is always the one
 * door -- a panic that names the check, std's own fn, the slice
 * built where the failure lands, the same shape a call site
 * spells (12-projects.md). */
static int rel;

enum
{
  CHK_INDEX,
  CHK_OVERFLOW,
  CHK_SHIFT,
  CHK_CAST,
  NCHK
};

/* the words the spec itself names, the cast's among them. A
 * check's message ends in a newline, the terminal's own kindness;
 * a user's panic says what it says (01-types.md) */
static const char *const chkmsgs[NCHK] = {
    "index out of range\n",
    "arithmetic overflow\n",
    "shift amount out of range\n",
    "cast out of range\n",
};
static char *chkbase[NCHK]; /* the data symbol each message landed
                             * in, one per message the whole pass's:
                             * reset beside dsn, whose numbering
                             * restarts with it */

/* an integer type's own ends, the checks' bounds: the least and
 * the greatest the type holds (01-types.md) */
static void
intends(Type *t, i64 *mn, u64 *mx)
{
  int bits = intwidth(t) * 8;

  if (isuintty(t)) {
    *mn = 0;
    *mx = bits == 64 ? ~(u64) 0 : (((u64) 1 << bits) - 1);
  } else {
    *mn = -(i64) ((u64) 1 << (bits - 1));
    *mx = ((u64) 1 << (bits - 1)) - 1;
  }
}

/* the failure door: the message a slice in a slot, the call std's
 * panic -- the bytes once per message, in the data segment. The
 * call never returns, and the ret behind it only ends the block,
 * qbe's own ask (the match's net the same, 09-match.md) */
static void
empanic(Em *em, int chk)
{
  const char *msg = chkmsgs[chk];
  usize       len = strlen(msg);
  char       *slot = stackslot(em, 16);
  char       *p;

  if (!chkbase[chk]) {
    char *base = arenaalloc(32);
    char *d = arenaalloc(64 + 8 * (len + 2));
    char *q;
    usize i;

    sprintf(base, "$str.%lu", (unsigned long) ++dsn);
    q = d + sprintf(d, "data %s = { ", base);
    for (i = 0; i < len; i++)
      q += sprintf(q, "b %u, ", (unsigned) (unsigned char) msg[i]);
    sprintf(q, "b 0 }"); /* the NUL: C interop wants it */
    vappend(&em->datas, &d);
    chkbase[chk] = base;
  }
  fprintf(em->o, "\tstorel %s, %s\n", chkbase[chk], slot);
  p = newtmp(em);
  fprintf(em->o, "\t%s =l add %s, 8\n", p, slot);
  fprintf(em->o, "\tstorel %lu, %s\n", (unsigned long) len, p);
  fprintf(em->o, "\tcall $%s(%s %s)\n", fsymname(sym_panic, sym_panic->decl),
          sigty(sym_panic->fnty->args[0], sym_panic->decl), slot);
  fprintf(em->o, "\tret 0\n");
}

/* one check's arm: bad, a w, nonzero fails into the panic; the
 * label after it is the check passed */
static void
emguard(Em *em, char *bad, int chk)
{
  char *ok = newlbl(em), *fail = newlbl(em);

  fprintf(em->o, "\tjnz %s, %s, %s\n", bad, fail, ok);
  fprintf(em->o, "%s\n", fail);
  empanic(em, chk);
  fprintf(em->o, "%s\n", ok);
}

/* a value already emitted, widened to the l domain by its own
 * signedness: the narrow loads extended on load, so the register
 * holds the extension already -- idxl's move, for a temp that
 * exists */
static char *
tol(Em *em, char *v, Type *t)
{
  char *r;

  if (intwidth(t) > 4)
    return v;
  r = newtmp(em);
  fprintf(em->o, "\t%s =l %s %s\n", r, isuintty(t) ? "extuw" : "extsw", v);
  return r;
}

/* an arithmetic's overflow -- add, sub or mul -- read against the
 * type's own ends (01-types.md). A type narrower than its register
 * never wraps the register: the widened operands recompute the
 * result exactly, and the ends answer -- which is every w-domain
 * mul too, its product always fitting an l. A type that fills its
 * register wraps it: the add and the sub read the sign flip off
 * the wrapped result, the classic xor pair; the l-domain mul alone
 * cannot widen further -- its product is divided back and
 * compared, the zero and the minus one guarded ahead of the traps
 * they would raise. */
static void
emarithchk(Em *em, Tok op, char *a, char *b, char *t, Type *ty, Ast *at)
{
  char  c = qbety(ty, at);
  int   w = intwidth(ty);
  int   su = isuintty(ty);
  char *bad;

  if (w > 8)
    return; /* the 128-bit ints arrive with a later milestone */

  if (op == Tstar && w == 8) { /* the round trip: the two traps
                                * guarded, the quotient compared
                                * back */
    char *ok = newlbl(em), *lnz = newlbl(em), *ldiv = newlbl(em), *lneg = newlbl(em);
    char *fail = newlbl(em);
    char *z = newtmp(em), *m = newtmp(em), *q = newtmp(em);

    fprintf(em->o, "\t%s =w ceql %s, 0\n", z, b);
    fprintf(em->o, "\tjnz %s, %s, %s\n", z, ok, lnz); /* zero: the product one */
    fprintf(em->o, "%s\n", lnz);
    if (!su) {
      fprintf(em->o, "\t%s =w ceql %s, -1\n", z, b);
      fprintf(em->o, "\tjnz %s, %s, %s\n", z, lneg, ldiv);
      fprintf(em->o, "%s\n", ldiv);
    }
    fprintf(em->o, "\t%s =l %sdiv %s, %s\n", m, su ? "u" : "", t, b);
    fprintf(em->o, "\t%s =w cnel %s, %s\n", q, m, a);
    fprintf(em->o, "\tjnz %s, %s, %s\n", q, fail, ok);
    if (!su) { /* the product is minus a: it overflows only of the
                * least, the one value negative on both sides of
                * its own negation */
      char *n = newtmp(em), *p1 = newtmp(em), *p2 = newtmp(em), *q2 = newtmp(em);

      fprintf(em->o, "%s\n", lneg);
      fprintf(em->o, "\t%s =l neg %s\n", n, a);
      fprintf(em->o, "\t%s =w csltl %s, 0\n", p1, n);
      fprintf(em->o, "\t%s =w csltl %s, 0\n", p2, a);
      fprintf(em->o, "\t%s =w and %s, %s\n", q2, p1, p2);
      fprintf(em->o, "\tjnz %s, %s, %s\n", q2, fail, ok);
    }
    fprintf(em->o, "%s\n", fail);
    empanic(em, CHK_OVERFLOW);
    fprintf(em->o, "%s\n", ok);
    return;
  }
  if (op == Tstar || (c == 'w' && w < 4)) {
    /* exact in the l domain: the operands widened, the result
     * redone there, the ends the judge */
    char *wa = tol(em, a, ty);
    char *wb = tol(em, b, ty);
    char *r = newtmp(em);
    char *hi = newtmp(em);
    char *o2 = newtmp(em);
    i64   mn;
    u64   mx;

    fprintf(em->o, "\t%s =l %s %s, %s\n", r,
            op == Tplus    ? "add"
            : op == Tminus ? "sub"
                           : "mul",
            wa, wb);
    intends(ty, &mn, &mx);
    bad = newtmp(em);
    if (su)
      fprintf(em->o, "\t%s =w cugel %s, %lu\n", bad, r, (unsigned long) mx);
    else {
      fprintf(em->o, "\t%s =w csltl %s, %ld\n", bad, r, (long) mn);
      fprintf(em->o, "\t%s =w csgtl %s, %ld\n", hi, r, (long) mx);
      fprintf(em->o, "\t%s =w or %s, %s\n", o2, bad, hi);
      bad = o2;
    }
    emguard(em, bad, CHK_OVERFLOW);
    return;
  }
  { /* the add or the sub that fills its register: the sign flip
     * off the wrapped result -- both operands flipped it, an add;
     * the left alone, a sub; unsigned, the end it fell off */
    char *s1 = newtmp(em), *s2 = newtmp(em), *ov = newtmp(em);

    bad = newtmp(em);
    if (su) {
      if (op == Tplus)
        fprintf(em->o, "\t%s =w cult%c %s, %s\n", bad, c, t, a);
      else
        fprintf(em->o, "\t%s =w cugt%c %s, %s\n", bad, c, t, a);
    } else {
      if (op == Tplus) {
        fprintf(em->o, "\t%s =%c xor %s, %s\n", s1, c, a, t);
        fprintf(em->o, "\t%s =%c xor %s, %s\n", s2, c, b, t);
      } else {
        fprintf(em->o, "\t%s =%c xor %s, %s\n", s1, c, a, b);
        fprintf(em->o, "\t%s =%c xor %s, %s\n", s2, c, a, t);
      }
      fprintf(em->o, "\t%s =%c and %s, %s\n", ov, c, s1, s2);
      fprintf(em->o, "\t%s =w cslt%c %s, 0\n", bad, c, ov);
    }
    emguard(em, bad, CHK_OVERFLOW);
  }
}

/* the shift's amount against the left operand's own width: at or
 * above it fails (07-operators.md). The one unsigned compare
 * reads a negative amount huge -- the widening its load already
 * did -- and a past-the-end one past */
static void
emshiftchk(Em *em, char *amt, Type *amtty, Type *lt)
{
  char *a;
  char *bad = newtmp(em);

  if (intwidth(lt) > 8)
    return; /* the 128-bit ints arrive with a later milestone */
  a = tol(em, amt, amtty);
  fprintf(em->o, "\t%s =w cugel %s, %lu\n", bad, a, (unsigned long) (intwidth(lt) * 8));
  emguard(em, bad, CHK_SHIFT);
}

/* the negation's own one end: the least, the only value below the
 * negated greatest -- 0's negation is 0, and passes */
static void
emnegchk(Em *em, char *v, Type *ty, Ast *at)
{
  char  c = qbety(ty, at);
  char *bad = newtmp(em);
  i64   mn;
  u64   mx;

  intends(ty, &mn, &mx);
  fprintf(em->o, "\t%s =w cslt%c %s, %ld\n", bad, c, v, -(long) mx);
  emguard(em, bad, CHK_OVERFLOW);
}

/* a float constant a compare reads: qbe takes float immediates
 * only in call and phi arguments, so the bits ride the data
 * segment and come back with a load -- Nflt's own move */
static char *
emflt(Em *em, double v, char c)
{
  char *base = arenaalloc(16);
  char *t = newtmp(em);
  char *d = arenaalloc(48);

  sprintf(base, "$flt.%lu", (unsigned long) ++dsn);
  fprintf(em->o, "\t%s =%c load%s %s\n", t, c, c == 's' ? "s" : "d", base);
  /* 9 and 17 significant digits: the least that round-trips */
  sprintf(d, "data %s = { %c %c_%.*g }", base, c, c, c == 's' ? 9 : 17, v);
  vappend(&em->datas, &d);
  return t;
}

/* the @cast that narrows, or lands from a float: the value held
 * against the target's own ends -- the failed conversion the
 * Panic section names (01-types.md). An int source's register
 * holds it widened by its own signedness, so the l domain
 * compares for every int; a float's compares in its own, the ends
 * read as the least exactly and one past the greatest -- a
 * truncation never reaches past it -- and a NaN, failing both, is
 * caught with the rest */
static void
emcastchk(Em *em, char *v, Type *from, Type *to, Ast *at)
{
  char *bad = newtmp(em);

  if (from->k == Tyint && (from->num == IN_F32 || from->num == IN_F64)) {
    char  c = qbety(from, at);
    char *ge = newtmp(em), *lt = newtmp(em), *ok = newtmp(em);
    char *lo, *hi;
    i64   mn;
    u64   mx;

    intends(to, &mn, &mx);
    lo = emflt(em, (double) mn, c);
    hi = emflt(em, (double) mx + 1.0, c);
    fprintf(em->o, "\t%s =w cge%c %s, %s\n", ge, c, v, lo);
    fprintf(em->o, "\t%s =w clt%c %s, %s\n", lt, c, v, hi);
    fprintf(em->o, "\t%s =w and %s, %s\n", ok, ge, lt);
    fprintf(em->o, "\t%s =w ceqw %s, 0\n", bad, ok);
  } else {
    char *l = tol(em, v, from);
    char *lo = newtmp(em), *hi = newtmp(em);
    i64   mn;
    u64   mx;

    intends(to, &mn, &mx);
    if (isuintty(from))
      fprintf(em->o, "\t%s =w copy 0\n", lo); /* an unsigned never reads below zero */
    else if (isuintty(to))
      fprintf(em->o, "\t%s =w csltl %s, 0\n", lo, l); /* a negative never fits one */
    else
      fprintf(em->o, "\t%s =w csltl %s, %ld\n", lo, l, (long) mn);
    if (isuintty(to))
      fprintf(em->o, "\t%s =w cugel %s, %lu\n", hi, l, (unsigned long) mx);
    else
      fprintf(em->o, "\t%s =w csgtl %s, %ld\n", hi, l, (long) mx);
    fprintf(em->o, "\t%s =w or %s, %s\n", bad, lo, hi);
  }
  emguard(em, bad, CHK_CAST);
}

/* -- monomorphization (04-generics.md) ---------------------------------- */

/* One generic fn, one binding of its parameters -- the types, and
 * the const parameters' baked values with them. Types are interned,
 * so the key is the Sym with the Type pointers themselves; the
 * values compare by what they hold. The same instantiation is
 * emitted once, and its name is what every call site says
 * (08-reflection.md: the const arguments are baked into each). */
typedef struct Inst Inst;
struct Inst
{
  Sym   *s;
  Type **tys;   /* s->ngparams of them, the call sites' binding */
  Val  **cvals; /* the const parameters' values, the parameters' own
                 * order -- NULL when the fn marks none (08) */
  Val **gcvals; /* the const generic parameters' numbers, the angle
                 * brackets' own order -- what a [N]T binding tells
                 * apart from another the types alone cannot (08) */
  char *name;
  int   mark; /* the drain this entry is queued for */
  Inst *from; /* the instance whose re-check found this call: the
               * chain a pack's recursion unfolds down, the depth
               * the cap reads (04-generics.md) */
};

static Inst **insts;  /* every one made, in first-seen order */
static Inst **iqueue; /* the current drain's worklist */
static int    ipass;  /* scratch is 1, the text is 2 */
static Inst  *icur;   /* the instance being re-checked, its calls'
                       * children (04-generics.md) */

/* -- the symbols' alphabet (12-projects.md, Symbols) --------------------
 *
 * Every name the mangler writes is a segment: the byte count in
 * decimal, then the bytes. A segment's bytes never begin with a
 * digit -- the identifier's own law -- so the count's digits end
 * exactly where the name begins and the split is the string's own:
 * a namespace named my_app and a my holding an app never fold the
 * same. Counts ride the same way (an arity, an array's length), each
 * ahead of things a letter begins. Values -- the numbers a const
 * parameter bakes -- are fixed-width hex instead, a length never
 * naming a value: decimal lengths next to decimal digits would read
 * two ways ("1" beside "0" the same string as "10"), and the width
 * says what the alphabet cannot. */

/* a segment: the byte count, then the bytes. Returns the new offset */
static usize
segput(char *buf, usize o, usize n, const char *s)
{
  usize l = strlen(s), i;

  if (o + l + 24 >= n) /* 20 digits of count, the bytes, the NUL */
    die("a name too wide for the emitter's names");
  o += (usize) sprintf(buf + o, "%lu", (unsigned long) l);
  for (i = 0; i < l; i++)
    buf[o++] = s[i];
  return o;
}

/* a count's own segment: its digits, the same law */
static usize
segnum(char *buf, usize o, usize n, u64 v)
{
  char tmp[21]; /* 2^64-1 is 20 digits */

  sprintf(tmp, "%lu", (unsigned long) v);
  return segput(buf, o, n, tmp);
}

/* a value's bits, w hex digits wide, leading zeros kept -- the
 * fixed width is the law above: no length names these */
static usize
hexput(char *buf, usize o, usize n, u64 v, int w)
{
  if (o + (usize) w + 1 >= n)
    die("a name too wide for the emitter's names");
  o += (usize) sprintf(buf + o, "%0*lx", w, (unsigned long) v);
  return o;
}

/* the namespace chain above ns, one segment each, the root's own
 * name ("") never said: the chain stops under it */
static usize
tymns(Ns *ns, char *buf, usize o, usize n)
{
  if (ns->parent && ns->parent->parent) /* the parent carries segments of its own */
    o = tymns(ns->parent, buf, o, n);
  return segput(buf, o, n, ns->name);
}

/* a type's own code -- the closed alphabet the mangler spells types
 * in. Primitives keep their words, composites take a tag letter and
 * nest; a named type says its whole path from the root and its
 * bindings. The words are prefix-free, every tag says what follows,
 * and the segments carry their own counts, so the code two types
 * fold onto is the types themselves -- the twins the old folding
 * numbered off are gone. A generic parameter keeps its name: an
 * impl's declared target spells one where the instance spells the
 * binding beside it (instname) */
static usize
tymang2(Type *t, char *buf, usize o, usize n)
{
  if (!t)
    die("a type the emitter cannot name");
  if (o + 16 >= n)
    die("a name too wide for the emitter's names");
  switch (t->k) {
  case Tyunit:
    buf[o++] = 'z';
    return o;
  case Tybool:
    buf[o++] = 'b';
    return o;
  case Tyvoidptr:
    buf[o++] = 'v';
    return o;
  case Tyint: { /* the word itself, bare: i32, u64 -- every one
                 * starts a letter and no one starts another, so the
                 * words read apart with nothing before them */
    const char *w = inname(t->num);
    usize       l, i;

    if (w[0] == '?')
      die("an integer width the mangler's alphabet does not carry");
    l = strlen(w);
    if (o + l + 1 >= n)
      die("a name too wide for the emitter's names");
    for (i = 0; i < l; i++)
      buf[o++] = w[i];
    return o;
  }
  case Typtr:
    if (t->t && t->t->k == Tymut) { /* *mut T: the writable slot's own tag */
      buf[o++] = 'P';
      return tymang2(t->t->t, buf, o, n);
    }
    buf[o++] = 'p';
    return tymang2(t->t, buf, o, n);
  case Tyslice:
    if (t->t && t->t->k == Tymut) {
      buf[o++] = 'S';
      return tymang2(t->t->t, buf, o, n);
    }
    buf[o++] = 's';
    return tymang2(t->t, buf, o, n);
  case Tyarray:
    if (t->gp)
      die("a const generic length has no code: the binding spells the number (08-reflection.md)");
    buf[o++] = t->t && t->t->k == Tymut ? 'A' : 'a';
    o = segnum(buf, o, n, t->n);
    return tymang2(t->t && t->t->k == Tymut ? t->t->t : t->t, buf, o, n);
  case Tytuple: {
    usize i;

    buf[o++] = 't';
    o = segnum(buf, o, n, t->nargs);
    for (i = 0; i < t->nargs; i++)
      o = tymang2(t->args[i], buf, o, n);
    return o;
  }
  case Tystruct:
  case Tyunion:
  case Tyenum:
  case Tytrait:
  case Tydyn: { /* the path from the root, the bindings -- std's own
                 * types say std in it, a user's cannot: the namespace
                 * is the library's (11-namespaces.md) */
    Ns   *ns = t->sym ? t->sym->ownns : 0;
    Ns   *w;
    usize d = 0, i;

    for (w = ns; w && w->parent; w = w->parent)
      d++;
    buf[o++] = t->k == Tydyn ? (t->mut ? 'D' : 'd') : 'n';
    o = segnum(buf, o, n, d + 1); /* the namespaces, then the name */
    if (ns && ns->parent)
      o = tymns(ns, buf, o, n);
    o = segput(buf, o, n, t->sym ? t->sym->name : "?");
    o = segnum(buf, o, n, t->nargs);
    for (i = 0; i < t->nargs; i++)
      o = tymang2(t->args[i], buf, o, n);
    return o;
  }
  case Tyfn: {
    usize i;

    buf[o++] = 'f';
    o = segnum(buf, o, n, t->nargs);
    for (i = 0; i < t->nargs; i++)
      o = tymang2(t->args[i], buf, o, n);
    return tymang2(t->t, buf, o, n);
  }
  case Tytype:
    buf[o++] = 'q';
    return o;
  case Typaram: /* the declared shape: the instance's suffix names the binding */
    buf[o++] = 'u';
    return segput(buf, o, n, t->gp && t->gp->v.gp.name ? t->gp->v.gp.name : "?");
  default:
    die("a type the mangler's alphabet does not carry");
  }
  return o; /* unreachable */
}

/* a symbol's head: ceri, the project's own name, then the namespace
 * path below the project's root. A user project's files stand in
 * the anonymous root, so the project's name says what the root
 * cannot; std's stand in the std namespace, which is its project's
 * root, and the path walks from there (12-projects.md, Symbols).
 * Returns the new offset */
static const char *emproj; /* the user project's name, emitfile's say */

static usize
projhead(Sym *s, char *buf, usize n)
{
  Ns   *chain[64]; /* the named chain, innermost first */
  Ns   *ns = s->ownns;
  Ns   *std;
  usize d = 0, i, o;

  while (ns && ns->parent) {
    if (d == 64)
      die("a namespace nesting too wide for the emitter's names");
    chain[d++] = ns;
    ns = ns->parent;
  }
  if (5 >= n)
    die("a name too wide for the emitter's names");
  memcpy(buf, "ceri", 5);
  o = 4;
  std = nschild(nsroot(), "std");
  if (d && std && chain[d - 1] == std) { /* std's own: the std
                                          * namespace is the
                                          * project's root, its
                                          * name the project's */
    o = segput(buf, o, n, "std");
    for (i = d - 1; i-- > 0;) /* the path below it, outermost first */
      o = segput(buf, o, n, chain[i]->name);
  } else {
    o = segput(buf, o, n, emproj);
    for (i = d; i-- > 0;) /* the whole chain, outermost first */
      o = segput(buf, o, n, chain[i]->name);
  }
  return o;
}

/* a baked value's own code -- the numbers a const parameter bakes
 * into its instance. Fixed-width hex by the alphabet's law above:
 * the bits an integer holds, a float's through the same cast, a
 * slice's elements one by one, the enum's tag beside -- so two
 * values the instance key (cvalsame) sees apart never share a name */
static usize
cvalput(char *buf, usize o, usize n, Val *v)
{
  if (!v) { /* the slot says: nothing baked here */
    if (o + 1 >= n)
      die("a name too wide for the emitter's names");
    buf[o++] = 'z';
    return o;
  }
  if (o + 24 >= n)
    die("a name too wide for the emitter's names");
  if (v->t->k == Tyslice) { /* the bytes, element by element */
    usize j;

    buf[o++] = 'h';
    o = hexput(buf, o, n, (u64) v->len, 16);
    for (j = 0; j < v->len; j++)
      o = hexput(buf, o, n, v->elems[j].i, 16);
    return o;
  }
  buf[o++] = 'x';
  if (v->t->k == Tyint &&
      (v->t->num == IN_F32 || v->t->num == IN_F64)) { /* a float's bits are not its value */
    double d = v->f;
    u64    b;

    memcpy(&b, &d, 8);
    o = hexput(buf, o, n, b, 16);
  } else
    o = hexput(buf, o, n, v->i, 16);
  return hexput(buf, o, n, v->tag, 4);
}

/* the instance's name: the fn's own mangle -- a method's the
 * declared target's code with the trait's path beside it, the
 * generic parameters keeping their names -- then every generic
 * slot's shape, the const generic slots' numbers, the const
 * parameters' baked values: everything the instance key
 * (instensure) sees, the name says too, in codes the folding
 * cannot blur and no declaration order touches
 * (04-generics.md, 08-reflection.md) */
static char *
instname(Sym *s, Type **tys, Val **cvals, Val **gcvals)
{
  char  buf[2048];
  usize o, i;

  o = projhead(s, buf, sizeof buf);
  if (s->impl) { /* a method's instance: the impl's own declared
                  * target, the trait where there is one */
    o = tymang2(s->impl->ifort ? s->impl->ifort : s->impl->ipath, buf, o, sizeof buf);
    if (s->impl->ifort)
      o = tymang2(s->impl->ipath, buf, o, sizeof buf);
    else
      buf[o++] = 'z';
  }
  o = segput(buf, o, sizeof buf, s->name);
  o = segnum(buf, o, sizeof buf, s->ngparams);
  for (i = 0; i < s->ngparams; i++) /* every slot's shape -- a const
                                     * generic's usize spells every
                                     * binding the same, the number
                                     * below tells them apart (08) */
    o = tymang2(tys[i], buf, o, sizeof buf);
  if (gcvals) { /* the const generic slots' numbers, the shapes' own */
    buf[o++] = 'g';
    for (i = 0; i < s->ngparams; i++) /* a slot without a number says z */
      o = gcvals[i] ? hexput(buf, o, sizeof buf, gcvals[i]->i, 16) : (buf[o++] = 'z', o);
  }
  if (cvals) { /* the const parameters' baked values (08-reflection.md) */
    Ast **ps = s->decl->v.fn.params;
    usize np = vlen(ps);

    buf[o++] = 'c';
    for (i = 0; i < np; i++)
      o = cvalput(buf, o, sizeof buf, ps[i]->v.param.cnst ? cvals[i] : 0);
  }
  buf[o] = 0;
  {
    char *nm = arenaalloc(o + 1);

    memcpy(nm, buf, o + 1);
    return nm;
  }
}

/* one baked value against another: the same value, or not. The
 * scalars by their bits, a slice by its elements -- the key the
 * name's spelling folds onto '_' cannot see through, this does
 * (08-reflection.md) */
static int
cvalsame(Val *a, Val *b)
{
  usize i;

  if (!a || !b)
    return a == b;
  if (a->t != b->t)
    return 0;
  if (a->t->k == Tyslice) { /* the rows, element by element */
    if (a->len != b->len)
      return 0;
    for (i = 0; i < a->len; i++)
      if (a->elems[i].i != b->elems[i].i)
        return 0;
    return 1;
  }
  if (a->t->k == Tyint && (a->t->num == IN_F32 || a->t->num == IN_F64))
    return a->f == b->f; /* a float's bits are not its value */
  return a->i == b->i && a->tag == b->tag;
}

/* the instance for this call, made if it is new. Every drain queues
 * each entry once: the mark is the pass, so a body's nested call can
 * re-ensure what a plain fn already found without doubling it */
static Inst *
instensure(Sym *s, Type **tys, Val **cvals, Val **gcvals)
{
  Ast **ps = s->decl->v.fn.params;
  usize np = vlen(ps), i, j;
  Inst *in;

  for (i = 0; i < vlen(insts); i++) {
    in = insts[i];
    if (in->s != s)
      continue;
    for (j = 0; j < s->ngparams; j++)
      if (in->tys[j] != tys[j])
        break;
    if (j != s->ngparams)
      continue;
    for (j = 0; j < np; j++) /* the baked values beside the types:
                              * what one instance tells apart from
                              * another (08-reflection.md) */
      if (!cvalsame(in->cvals ? in->cvals[j] : 0, cvals ? cvals[j] : 0))
        break;
    if (j != np)
      continue;
    for (j = 0; j < s->ngparams; j++) /* the const generic parameters'
                                       * numbers beside the slots'
                                       * shapes -- usize spells every
                                       * binding the same (08) */
      if (!cvalsame(in->gcvals ? in->gcvals[j] : 0, gcvals ? gcvals[j] : 0))
        break;
    if (j == s->ngparams)
      goto found;
  }
  if (!insts) {
    insts = vnew(Inst *, 16);
    iqueue = vnew(Inst *, 16);
  }
  { /* the chain a pack's recursion unfolds down: each instance one
     * unfolding, the empty pack's own the last -- 256 allowed, the
     * 257th the error (04-generics.md) */
    Inst *f = icur;
    usize d = 0;

    while (f) {
      d++;
      f = f->from;
    }
    if (d > 256)
      die("'%s' unfolds past 256 levels (04-generics.md)", s->name);
  }
  in = arenaalloc(sizeof *in);
  in->s = s;
  in->tys = tys;
  in->cvals = cvals;
  in->gcvals = gcvals;
  in->from = icur;
  in->name = instname(s, tys, cvals, gcvals);
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
static void  emdrops(Em *em, Ast **drops); /* the pre-made
                                            * destructors, spelled in
                                            * the checker's own order
                                            * (03-move.md) */
int mustexit(Ast *st);                     /* flow.c's syntactic judgement, body.h's own:
                                            * the emit's dead-path marking rides it. The
                                            * header itself stays unwelcome here -- its
                                            * locfind is the checker's, this file's its
                                            * own (10-iteration.md) */

/* the end of the text a diverging statement writes, read for what
 * closed it: the ret or jump the walk itself wrote -- a return, a
 * break, the match's no-fit abort -- or the call the abort never
 * returns from, the one ending that leaves the block open. What
 * emits the terminator either way wants to know (10-iteration.md) */
static int
endsopen(Ast *st)
{
  switch (st->k) {
  case Ncall: /* the call itself: no terminator after it */
    return 1;
  case Nlet: /* the store behind the init closes nothing */
    return st->v.let.e && endsopen(st->v.let.e);
  case Nexprstmt:
    return endsopen(st->v.n1.e);
  case Nblock: { /* the statements, then the tail: the last thing
                  * written is the last thing that counts */
    Ast **ss = st->v.blk.stmts;
    usize n = vlen(ss);

    if (n)
      return endsopen(ss[n - 1]);
    return st->v.blk.tail && endsopen(st->v.blk.tail);
  }
  case Nif: /* mustexit walked here only with the else in hand, and
             * its text is the last the if writes -- an else-if chain
             * ends in its own else */
  case Ncif:
    return endsopen(st->v.ifx.els);
  case Nmatch: /* every arm left, and the no-fit abort behind them
                * carries its own ret: closed */
  default:     /* the return, the break, the continue: their own ret or
                * jump closed the block */
    return 0;
  }
}
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

/* the table's own name: the trait's whole path, the concrete type's
 * code beside it -- the pair the construction site names, spelled
 * so two pairs fold the same only by being the same pair (06-dispatch.md) */
static char *
vtname(Sym *tr, Type *ty)
{
  char  buf[1024];
  usize i, o;

  for (i = 0; i < vlen(vts); i++)
    if (vts[i].tr == tr && vts[i].ty == ty)
      return vts[i].name;
  if (2 >= sizeof buf)
    die("a name too wide for the emitter's names");
  memcpy(buf, "vt", 3);
  o = 2;
  if (tr->ownns && tr->ownns->parent)
    o = tymns(tr->ownns, buf, o, sizeof buf);
  o = segput(buf, o, sizeof buf, tr->name);
  o = tymang2(ty, buf, o, sizeof buf);
  buf[o] = 0;
  {
    Vt    vt;
    char *n = arenaalloc(o + 1);

    memcpy(n, buf, o + 1);
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
    char **nms = vnew(char *, 8); /* the entries' names, spelled
                                   * ahead of the line: its length
                                   * is what they say, not a guess
                                   * a longer mangle outgrows */
    usize len, nn = 0;

    im = implfor(vt->tr, vt->ty, &tys);
    if (!im)
      cerrat(vt->tr->decl, "unreachable: the construction site checked");
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
        nm = instensure(fm->sym, tys, 0, 0)->name; /* the instance this
                                                    * table names */
      else
        nm = fsymname(fm->sym, fm->sym->decl);
      vappend(&nms, &nm);
      nn++;
    }
    len = strlen(vt->name) + 16; /* data $ = { } and the NUL */
    for (j = 0; j < nn; j++)
      len += strlen(nms[j]) + 8; /* the entry's own: l $ NAME,  */
    line = arenaalloc(len);
    p = line + sprintf(line, "data $%s = { ", vt->name);
    for (j = 0; j < nn; j++)
      p += sprintf(p, "l $%s, ", nms[j]);
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
 * index on an array folds; the checker range-checked it
 * (01-types.md). Everything else runs the check: the bound the
 * slice's own length or the array's, and the one unsigned compare
 * catching a negative index with the past-the-end one -- the
 * widening idxl kept reads a negative huge. */
static char *
idxaddr(Em *em, Ast *e)
{
  Type *bt = e->v.n2.a->ty;
  Type *et = bt->t;
  usize sz;
  char *b = aggbase(em, e->v.n2.a);
  char *len = 0; /* a slice's own length, the check's bound */

  while (et && et->k == Tymut) /* []mut T: the element's own type */
    et = et->t;
  sz = sizeof_(et);
  if (bt->k == Tyslice) {
    char *p = newtmp(em);
    char *l8 = newtmp(em);

    fprintf(em->o, "\t%s =l loadl %s\n", p, b);
    fprintf(em->o, "\t%s =l add %s, 8\n", l8, b);
    len = newtmp(em);
    fprintf(em->o, "\t%s =l loadl %s\n", len, l8);
    b = p;
  }
  if (bt->k == Tyarray && e->v.n2.b->k == Nint)
    return addrplus(em, b, (usize) e->v.n2.b->v.i.num * sz);
  {
    char *ix = idxl(em, e->v.n2.b);
    char *sc = newtmp(em);
    char *a = newtmp(em);

    if (!rel) {
      char *bad = newtmp(em);

      if (len) /* a slice: the length the view carries; a constant
                * index lands here too -- a slice's length is a
                * runtime thing, the checker's contract an array's
                * own */
        fprintf(em->o, "\t%s =w cugel %s, %s\n", bad, ix, len);
      else
        fprintf(em->o, "\t%s =w cugel %s, %lu\n", bad, ix, (unsigned long) bt->n);
      emguard(em, bad, CHK_INDEX);
    }
    if (!sz)
      return b; /* a ZST element: the check held, and every index's
                 * address the one the data is */
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
    } else if (e->v.ifx.els->k == Ncif) { /* the walk routes a const
                                           * if, the taken block rewritten in its place -- one
                                           * still standing here skipped its re-check */
      cerrat(e->v.ifx.els, "the const branch did not land (08-reflection.md)");
      return 0; /* unreachable */
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
  *reached = !mustexit(b); /* a call that never comes back takes its
                            * own way out, the arm's value nothing
                            * (10-iteration.md) */
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
  *reached = any;
  if (!any) /* every arm left already: no join to read, its label and
             * its load dead text -- an if's own rule, the match's
             * the same (10-iteration.md) */
    return 0;
  fprintf(em->o, "%s\n", lend);
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
    if (reached) { /* the round reached its end: the pattern's own
                    * bindings die with it -- this shape has none, a
                    * condition's loop, so the array is empty */
      emdrops(em, st->v.forx.drops);
      jump(em, lc);
    }
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
    if (reached) /* the round reached its end: the pattern's
                  * bindings die with it (03-move.md, 10) */
      emdrops(em, st->v.forx.drops);
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
      if (reached) /* the one round's bindings, its end reached */
        emdrops(em, st->v.forx.drops);
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
      if (reached) /* the round reached its end: the element's own
                    * bindings die with it -- a slice lends a
                    * pointer, which owns nothing, an owned array
                    * yields the element, which the round drops
                    * (03-move.md, 10-iteration.md) */
        emdrops(em, st->v.forx.drops);
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
    Ast **psegs = e->v.path.segs;
    usize pnsegs = vlen(psegs);
    Ns   *pns;
    usize pk = nshead(psegs, pnsegs, &pns, e->v.path.root);
    char *nm;
    ELoc *l;
    Sym  *s;

    if (pk == pnsegs) /* the whole path a namespace walk (11) */
      cerrat(e, "a namespace names no value; name what is in it (11-namespaces.md)");
    psegs += pk; /* the namespaces walked fall away (11-namespaces.md) */
    pnsegs -= pk;
    nm = psegs[0]->v.seg.name;
    l = locfind(em, nm);
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
    if (pnsegs == 2) { /* Enum::Variant, the
                        * payloadless read (01-types.md) */
      char *tn = psegs[0]->v.seg.name;
      char *vn = psegs[1]->v.seg.name;
      Sym  *s0 = pk ? nsitem(pns, tn) : symfind(tn);

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
    if (pnsegs != 1)
      cerrat(e, "this name arrives with a later milestone");
    s = pk ? nsitem(pns, nm) : symfind(nm);
    if (!s && !pk) { /* None, Ok, Err: bare, the payloadless side */
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

      if (e->v.path.tys) {
        { /* the const generic parameters' own did-it-land: a
           * black-box length defers to the re-check, and this tree
           * is not the clone it filled (08-reflection.md) */
          Sym  *ps = e->v.path.sym;
          Ast **gps = ps->decl->v.fn.gparams;
          usize ng = vlen(gps), ci;

          for (ci = 0; ci < ng; ci++)
            if (gps[ci]->v.gp.cnst && (!e->v.path.gcvals || !e->v.path.gcvals[ci]))
              cerrat(e, "the const generic argument did not land: the re-check under the "
                        "binding fills it, and this tree is not the clone it filled");
        }
        nm = instensure(e->v.path.sym, e->v.path.tys, 0, e->v.path.gcvals)->name;
      } else if (e->v.path.sym)
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
    if (op == Tminus) {
      fprintf(em->o, "\t%s =%c neg %s\n", t, qbety(e->ty, e), v);
      if (!rel && isintty(e->ty) && !isuintty(e->ty)) /* the least:
                                                       * the one value
                                                       * its own
                                                       * negation
                                                       * does not
                                                       * hold; an
                                                       * unsigned
                                                       * negation
                                                       * wraps, the
                                                       * defined way
                                                       * (01) */
        emnegchk(em, v, e->ty, e);
    } else if (op == Ttilde)
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
    if ((op == Tplus || op == Tminus) && lt && lt->k == Typtr) {
      /* pointer arithmetic (01-types.md): the amount widened
       * whole, scaled by the element's own size, the pointer
       * stepped by the product -- the built-in table's own
       * answer, the std's Add<usize> row spelling it a call
       * (07-operators.md). A voidptr is no Typtr and never
       * arrives: there is no element size to scale by */
      Type *el = lt->t;
      char *s = newtmp(em);

      while (el && el->k == Tymut) /* *mut T: the element's own type */
        el = el->t;
      b = tol(em, b, e->v.bin.r->ty);
      fprintf(em->o, "\t%s =l mul %s, %lu\n", s, b, (unsigned long) sizeof_(el));
      fprintf(em->o, "\t%s =l %s %s, %s\n", t, op == Tplus ? "add" : "sub", a, s);
      return t;
    }
    for (i = 0; i < sizeof ops / sizeof ops[0]; i++)
      if (ops[i].t == op) {
        char *ins = ops[i].i;

        if (ops[i].u && isuintty(lt))
          ins = ops[i].u;
        fprintf(em->o, "\t%s =%c %s %s, %s\n", t, qbety(e->ty, e), ins, a, b);
        if (!rel && (op == Tshl || op == Tshr)) /* the amount
                                                 * against the left
                                                 * operand's width
                                                 * (07-operators.md) */
          emshiftchk(em, b, e->v.bin.r->ty, lt);
        else if (!rel && isintty(lt) && (op == Tplus || op == Tminus || op == Tstar))
          emarithchk(em, op, a, b, t, lt, e); /* the wrap the machine
                                               * already made, read
                                               * back (01-types.md) */
        return t;
      }
    { /* a comparison: the domain is the operands', the result a w */
      char *ins = 0;
      int   dom;

      if (lt && lt->k == Tyenum) { /* an enum without payloads, where
                                    * the tag is the whole value: the
                                    * tags at each head, loaded at
                                    * their own width and compared
                                    * (01-types.md). One with payloads
                                    * orders through Ord, and equality
                                    * is its own to define
                                    * (07-operators.md) */
        Type *tt;
        char *ta, *tb;
        usize vi;

        if (op != Teqeq && op != Tne)
          cerrat(e, "an enum compares through Ord, not the operators (07-operators.md)");
        for (vi = 0; vi < lt->sym->nvariants; vi++)
          if (lt->sym->variants[vi].npayload || lt->sym->variants[vi].nfields)
            cerrat(e, "an enum with payloads arrives with a later milestone");
        tt = tagtyof(lt);
        ta = newtmp(em);
        tb = newtmp(em);
        fprintf(em->o, "\t%s =%c %s %s\n", ta, qbety(tt, e), ldins(tt), a);
        fprintf(em->o, "\t%s =%c %s %s\n", tb, qbety(tt, e), ldins(tt), b);
        fprintf(em->o, "\t%s =w %s%c %s, %s\n", t, op == Tne ? "cne" : "ceq", qbety(tt, e), ta, tb);
        return t;
      }
      dom = qbety(lt, e);

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
      usize nsegs = vlen(segs);
      Ns   *ns;
      usize k = nshead(segs, nsegs, &ns, f->v.path.root);

      if (k == nsegs) /* a namespace names no call (11-namespaces.md) */
        cerrat(f, "a namespace names no call; name what is in it (11-namespaces.md)");
      segs += k; /* the namespaces walked fall away (11-namespaces.md) */
      nsegs -= k;
      if (nsegs == 1) {
        char *nm = segs[0]->v.seg.name;

        if (!k && !locfind(em, nm) && !symfind(nm)) {
          Sym *owner = symvariantowner(nm);

          if (!owner || !e->ty || e->ty->k != Tyenum || e->ty->sym != owner)
            cerrat(f, "unknown name '%s'", nm);
          return emavariant(em, e->ty, symvarfind(owner, nm), args, n, e);
        }
      } else if (nsegs == 2) {
        s = k ? nsitem(ns, segs[0]->v.seg.name) : symfind(segs[0]->v.seg.name);
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
          nm = e->v.call.tys ? instensure(ms, e->v.call.tys, 0, 0)->name : fsymname(ms, ms->decl);
        }
      } else if (f->k == Npath && ms) {
        Ast **fsegs = f->v.path.segs;
        Ns   *fns;
        usize fn = vlen(fsegs);

        if (nshead(fsegs, fn, &fns, f->v.path.root) + 2 == fn) /* Type::member
                                                                * under any
                                                                * namespaces
                                                                * walked
                                                                * (11) */
          nm = e->v.call.tys ? instensure(ms, e->v.call.tys, 0, 0)->name : fsymname(ms, ms->decl);
      }
    }
    for (i = 0; i < n; i++) {
      as[i] = nicheout(em, args[i]->ty, emaexpr(em, args[i]));
      if (!as[i]) /* a ZST argument -- () or a type's reference --
                   * crosses as the word zero it arrived in
                   * (08-reflection.md) */
        as[i] = "0";
    }
    if (!nm && f->k == Npath) { /* a plain call, the name however many
                                 * namespaces it walked (11): the walk
                                 * falls away, the one name under it
                                 * the fn. A prefixed name no local
                                 * shadows -- the walk chose it -- a
                                 * bare one a local may, and then this
                                 * is a call through the local's value */
      Ast **segs = f->v.path.segs;
      usize nsegs = vlen(segs);
      Ns   *ns;
      usize k = nshead(segs, nsegs, &ns, f->v.path.root);

      segs += k;
      nsegs -= k;
      if (nsegs == 1 && (k || !locfind(em, segs[0]->v.seg.name))) {
        s = k ? nsitem(ns, segs[0]->v.seg.name) : symfind(segs[0]->v.seg.name);
        if (!s || s->kind != Sfn)
          cerrat(f, "'%s' is not a fn", segs[0]->v.seg.name);
        if (e->v.call.sym) { /* the checker's pick: which overload, and
                              * which instantiation -- the latter names
                              * its own copy (04-generics.md). A const
                              * parameter's baked value rides the pick
                              * with the types: one instantiation per
                              * value, the call site's own words
                              * (08-reflection.md) */
          Ast **ps;
          usize np, ci;

          s = e->v.call.sym;
          ps = s->decl->v.fn.params;
          np = vlen(ps);
          for (ci = 0; ci < np; ci++)
            if (ps[ci]->v.param.cnst && (!e->v.call.cvals || !e->v.call.cvals[ci]))
              cerrat(e, "the const argument did not land: the re-check under the binding "
                        "fills it, and this tree is not the clone it filled");
          { /* the const generic parameters ride the same pick: a
             * black-box length means the same thing -- the re-check
             * under the outer binding binds it (08-reflection.md) */
            Ast **gps = s->decl->v.fn.gparams;
            usize ng = vlen(gps);

            for (ci = 0; ci < ng; ci++)
              if (gps[ci]->v.gp.cnst && (!e->v.call.gcvals || !e->v.call.gcvals[ci]))
                cerrat(e, "the const generic argument did not land: the re-check under the binding "
                          "fills it, and this tree is not the clone it filled");
          }
          if (e->v.call.tys || e->v.call.cvals || e->v.call.gcvals)
            nm = instensure(s, e->v.call.tys, e->v.call.cvals, e->v.call.gcvals)->name;
          else
            nm = fsymname(s, s->decl);
        } else {
          if (s->next)
            cerrat(f, "overload resolution at emit time arrives with M3d");
          nm = fsymname(s, s->decl);
        }
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
        if (!rel && intwidth(tt) > intwidth(to)) /* the tag's width
                                                  * the judge: a
                                                  * narrower target
                                                  * reads the ends
                                                  * (01-types.md) */
          emcastchk(em, r, tt, to, e);
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
      if (fw && tw) {
        if (!rel && intwidth(to) < intwidth(from)) /* the narrowing:
                                                    * the target's
                                                    * own ends the
                                                    * judge, a same-
                                                    * width change of
                                                    * sign no change
                                                    * at all
                                                    * (01-types.md) */
          emcastchk(em, a, from, to, e);
        return a;
      }
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
                               * kind, the signedness per the target;
                               * the ends checked ahead of it -- the
                               * failed conversion (01-types.md) */
        int su = isuintty(to);

        if (!rel)
          emcastchk(em, a, from, to, e);
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
      if (!rel) /* the narrowing: the target's own ends the judge
                 * (01-types.md) */
        emcastchk(em, a, from, to, e);
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
    if (strcmp(nm, "take") == 0) { /* the value out, the zero value
                                    * back (03-move.md): the place keeps
                                    * something whoever owns it can
                                    * still destruct, and the taken
                                    * value is the expression's own */
      Ast **args = e->v.blt.args;
      Type *t = e->ty;
      usize sz = sizeof_(t);
      char *p = emaexpr(em, args[0]);
      char *v = stackslot(em, sz ? sz : 1);

      if (sz) { /* a ZST moves no bits: the zero is itself */
        char *z = zeroblk(em, sz);

        fprintf(em->o, "\tblit %s, %s, %lu\n", p, v, (unsigned long) sz);
        fprintf(em->o, "\tblit %s, %s, %lu\n", z, p, (unsigned long) sz);
        return slotload(em, t, e, v);
      }
      if (isagg(t))
        return v; /* the empty slot: a ZST aggregate's address */
      {
        char *z = newtmp(em);

        fprintf(em->o, "\t%s =w copy 0\n", z);
        return z;
      }
    }
    if (strcmp(nm, "slice") == 0) { /* the two words written together,
                                     * the view whole from its first
                                     * instruction: no half-built
                                     * slice ever runs (01-types.md) */
      Ast **args = e->v.blt.args;
      char *p = emaexpr(em, args[0]); /* the reach: a *T, its own
                                       * word */
      char *n = emaexpr(em, args[1]); /* the length: a usize word */
      char *t = stackslot(em, 16);
      char *w = newtmp(em);

      fprintf(em->o, "\tstorel %s, %s\n", p, t);
      fprintf(em->o, "\t%s =l add %s, 8\n", w, t);
      fprintf(em->o, "\tstorel %s, %s\n", n, w);
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
    if (!rel) { /* the range the view's own: lo past hi, hi past the
                 * length -- either leaves it (01-types.md); the
                 * unsigned compares read a negative end huge, the
                 * widening idxl kept */
      char *b1 = 0, *b2 = newtmp(em);

      if (lo) {
        b1 = newtmp(em);
        fprintf(em->o, "\t%s =w cugtl %s, %s\n", b1, lo, hi);
      }
      if (bt->k == Tyarray)
        fprintf(em->o, "\t%s =w cugtl %s, %lu\n", b2, hi, (unsigned long) bt->n);
      else /* a slice: hi is the len when the right end was left
            * out -- the compare reads zero, dead but uniform */
        fprintf(em->o, "\t%s =w cugtl %s, %s\n", b2, hi, len);
      if (b1) {
        char *bad = newtmp(em);

        fprintf(em->o, "\t%s =w or %s, %s\n", bad, b1, b2);
        b2 = bad;
      }
      emguard(em, b2, CHK_INDEX);
    }
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
  case Ncif: /* the walk routes it -- the taken block rewritten in
              * place, the untaken discarded -- so the emitter never
              * sees one: a tree that skipped its re-check */
    cerrat(e, "the const branch did not land (08-reflection.md)");
    return 0; /* unreachable */
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
      if ((op == Tpluseq || op == Tminuseq) && lhs->ty && lhs->ty->k == Typtr) {
        /* a pointer stepped in place (01-types.md): the amount
         * scaled, the same arithmetic the expression's own walks */
        Type *el = lhs->ty->t;
        char *s = newtmp(em);

        while (el && el->k == Tymut) /* *mut T: the element's own type */
          el = el->t;
        v = tol(em, v, st->v.bin.r->ty);
        fprintf(em->o, "\t%s =l mul %s, %lu\n", s, v, (unsigned long) sizeof_(el));
        fprintf(em->o, "\t%s =l %s %s, %s\n", nv, op == Tpluseq ? "add" : "sub", old, s);
        fprintf(em->o, "\t%s %s, %s\n", stins(lhs->ty), nv, p);
        return;
      }
      for (i = 0; i < sizeof ops / sizeof ops[0]; i++)
        if (ops[i].t == op) {
          fprintf(em->o, "\t%s =%c %s %s, %s\n", nv, qbety(lhs->ty, lhs),
                  ops[i].u && isuintty(lhs->ty) ? ops[i].u : ops[i].i, old, v);
          if (!rel && isintty(lhs->ty)) { /* the same checks the
                                           * expression's own run
                                           * (01-types.md) */
            if (op == Tpluseq || op == Tminuseq || op == Tstareq)
              emarithchk(em,
                         op == Tpluseq    ? Tplus
                         : op == Tminuseq ? Tminus
                                          : Tstar,
                         old, v, nv, lhs->ty, st);
            else if (op == Tshleq || op == Tshreq)
              emshiftchk(em, v, st->v.bin.r->ty, lhs->ty);
          }
          break;
        }
      if (i == sizeof ops / sizeof ops[0])
        cerrat(st, "this compound assignment arrives with a later milestone");
      fprintf(em->o, "\t%s %s, %s\n", stins(lhs->ty), nv, p);
      return;
    }
    if (st->v.bin.drop) { /* the old value's destructors (03): the
                           * new value in hand first, fixed to its own
                           * storage -- `x = x` would hand the
                           * destructor the very place it reads -- then
                           * the drops, then the store over what died */
      char *v = emaexpr(em, st->v.bin.r);
      char *tmp = stackslot(em, sizeof_(lhs->ty));

      fprintf(em->o, "\tblit %s, %s, %lu\n", v, tmp, (unsigned long) sizeof_(lhs->ty));
      emdrops(em, st->v.bin.drop);
      fprintf(em->o, "\tblit %s, %s, %lu\n", tmp, p, (unsigned long) sizeof_(lhs->ty));
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

      emdrops(em, st->v.n1.drops); /* the frame's own bindings, the
                                    * value home before them (03) */
      fprintf(em->o, "\tret %s\n", v);
    } else {
      emdrops(em, st->v.n1.drops);
      fputs("\tret 0\n", em->o); /* (): the w carries nothing */
    }
    return;
  }
  case Nexprstmt:
    emaexpr(em, st->v.n1.e);
    emdrops(em, st->v.n1.drops); /* a temporary no one owns: the
                                  * statement's end is its scope
                                  * (03-move.md) */
    return;
  case Nbreak: {
    if (em->nloops <= 0)
      cerrat(st, "this break is not in a for");
    emdrops(em, st->v.n1.drops); /* the round's bindings, the loop's
                                  * own (03-move.md) */
    jump(em, em->loops[em->nloops - 1].brk);
    return;
  }
  case Ncontinue: {
    if (em->nloops <= 0)
      cerrat(st, "this continue is not in a for");
    emdrops(em, st->v.n1.drops); /* the round ends here as surely as
                                  * at its body's closing brace (03) */
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

/* the destructors the checker pre-made -- the calls and the matches
 * it spelled, sent in the order it spelled them (03-move.md) */
static void
emdrops(Em *em, Ast **drops)
{
  usize i;

  for (i = 0; i < vlen(drops); i++)
    emaexpr(em, drops[i]);
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
    if (mustexit(stmts[i])) { /* the statement leaves -- a return, a
                               * break, a continue, a call that never
                               * comes back: the rest is dead
                               * (10-iteration.md) */
      *reached = 0;
      em->openend = endsopen(stmts[i]); /* what the fn's own last text
                                         * is owes the terminator */
      goto out;
    }
  }
  if (body->v.blk.tail) {
    v = emaexpr(em, body->v.blk.tail);
    if (mustexit(body->v.blk.tail)) {
      *reached = 0; /* the tail never lands: no join reads a value
                     * out of it, no trailing ret owes one
                     * (10-iteration.md) */
      em->openend = endsopen(body->v.blk.tail);
    }
  } else
    v = 0; /* "{}": the unit */
out:
  if (*reached) /* the closing brace the block reached on its own:
                 * the bindings die here. A way out that left carries
                 * its own destructors; an abort runs none (03) */
    emdrops(em, body->v.blk.drops);
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
      emdrops(&em, it->v.fn.drops); /* the parameters' own slots, the
                                     * body's value home before them
                                     * (03-move.md) */
      fprintf(em.o, "\tret %s\n", v ? v : "0");
    } else if (em.openend) /* the body ended in the call the abort
                            * never returns from: qbe wants its
                            * terminator whatever the flow -- the ret
                            * is dead the moment the call runs
                            * (10-iteration.md) */
      fputs("\tret 0\n", em.o);
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
  if (s->ownsf) { /* the re-check reads the body's names -- a
                   * generic's bare self-reference the first among
                   * them (07-operators.md) -- in the file that
                   * wrote it: the caller's uses bind nothing for
                   * the body, and its own context ends at its own
                   * file (04-generics.md) */
    nscur(s->ownsf->ns);
    usecur(s->ownsf->uses);
    lexsetpath(s->ownsf->path);
  }
  it = astclone(s->decl); /* the shared declaration, copied whole:
                           * every instance rewrites its own */
  ng = s->ngparams;
  ats = vlen(it->v.fn.params) ? tyargs(vlen(it->v.fn.params)) : 0;
  for (i = 0; i < vlen(it->v.fn.params); i++)
    ats[i] = projopen(gsubstv(s->fnty->args[i], s->gparams, in->tys, in->gcvals, ng), it);
  icur = in; /* the calls the re-check and the emit that follows find
              * are the chain's next level -- the instances the
              * emitter names arrive here, after the body's own
              * walk, both under this instance (04-generics.md) */
  recheckfn(s, it, in->tys, in->cvals, in->gcvals);
  emitfn(o, s, it, in->name, ats,
         projopen(gsubstv(s->fnty->t, s->gparams, in->tys, in->gcvals, ng), it));
  icur = 0;
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
emitall(FILE *out, Srcfile **files, usize nfiles)
{
  usize f, i;

  for (f = 0; f < nfiles; f++) { /* a file's fns read from its own
                                  * namespace, its own uses beside:
                                  * symfind walks the current ones
                                  * first, so each file's walk
                                  * switches to them
                                  * (12-projects.md) */
    Srcfile *sf = files[f];
    usize    n = vlen(sf->items);

    nscur(sf->ns);
    usecur(sf->uses);
    lexsetpath(sf->path);
    for (i = 0; i < n; i++) {
      Ast *it = sf->items[i];
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
      if (fnconstparams(s))
        continue; /* a const fn's body is the instance's own copy: the
                   * baked values differ per call, and the shared
                   * declaration emits none of them -- the calls queue
                   * every instance (08-reflection.md) */
      emitfn(out, s, it, fsymname(s, it), 0, 0);
    }
  }
}

/* the platform's door: the wrapper the compiler arranges -- an
 * unmangled C main that hands the project's own main to one of
 * std::entry's runs, by address with the platform's own two, the
 * count and the table of words (12-projects.md). The project's
 * main mangles like any other name; this one alone keeps the
 * platform's own, and the linker hands it the program. The ending
 * is the entry's own work -- a unit a clean zero, an i32 the code
 * itself, an E?() printed through its error's words and answered
 * -- and the wrapper says none of it: one call the whole door. A
 * library has none to arrange. */
static void
emitmain(FILE *o)
{
  Sym  *m = nsitem(nsroot(), "main");
  Type *rt;
  char *en;

  if (!m || m->kind != Sfn)
    return; /* a library: no door to arrange */
  rt = m->fnty->t;
  if (rt->k == Tyenum) { /* E?(): the entry is a generic fn over the E
                          * the Err half carries (01-types.md: E?T is
                          * Result<T, E>) -- an instance per error
                          * type, the args arena-held the instance
                          * keeps, this frame's own would dangle under
                          * the drain it queues */
    Type **tys = tyargs(1);

    tys[0] = rt->args[1];
    en = instensure(sym_entry_err, tys, 0, 0)->name;
  } else { /* () or i32: the plain fn, no instance to make */
    Sym *e = rt->k == Tyunit ? sym_entry_unit : sym_entry_i32;

    en = fsymname(e, e->decl);
  }
  fprintf(o,
          "export function w $main(w %%argc, l %%argv) {\n@start\n"
          "\t%%m =l copy $%s\n"
          "\t%%r =w call $%s(l %%m, w %%argc, l %%argv)\n"
          "\tret %%r\n"
          "}\n\n",
          fsymname(m, m->decl), en);
}

void
emitfile(FILE *out, Srcfile **files, usize nfiles, int release, const char *proj)
{
  FILE *scratch = tmpfile(); /* pass one names the aggregates and
                              * finds every instantiation; its text
                              * goes nowhere (qbe wants the type
                              * declarations before their first use) */

  if (!scratch) {
    fprintf(stderr, "cerium: no scratch file for the type pass\n");
    exit(1);
  }
  emproj = proj; /* the project's name, the symbols' first segment
                  * (12-projects.md, Symbols) */
  rel = release; /* the runtime checks' own mode: debug inserts
                  * them, release leaves them out (01-types.md) */
  ipass = 1;
  emitall(scratch, files, nfiles);
  emitmain(scratch);
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
  dsn = 0;                            /* the naming pass burned numbers; the real one restarts */
  vtprinted = 0;                      /* the text restarts with it */
  memset(chkbase, 0, sizeof chkbase); /* the messages' data symbols
                                       * with them: the numbering
                                       * they ride restarted */
  abidecls(out);                      /* the :type declarations, the order qbe reads */
  ipass = 2;
  emitall(out, files, nfiles); /* pass two: the text */
  emitmain(out);
  draininsts(out);
  for (;;) {
    draininsts(out);
    if (vtprinted == vlen(vts))
      break;
    printvts(out);
  }
}
