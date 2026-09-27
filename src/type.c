/* type.c -- interning and the printable form.
 *
 * Every type is built exactly once: intern() hashes the content and
 * returns the existing Type when there is one, so two types are the
 * same iff their pointers are (tysame). The table is the classic
 * open-addressing one -- linear probing, power-of-two capacity,
 * resized at 3/4 full. C89 keeps it honest: no designated
 * initializers, so the singletons intern lazily on first use.
 *
 * tysprint is the source of every type a human reads: the -T dump
 * and the checker's diagnostics both go through it. Aliases are
 * resolved away before a Type exists, so what prints is expanded;
 * the sugar (?T, E?T) prints as itself, recognized by the prelude
 * syms it was built from.
 */

#include <stdio.h>
#include <string.h>

#include "ast.h"
#include "die.h"
#include "sym.h"
#include "type.h"

/* -- interning --------------------------------------------------------- */

static Type **tbl; /* open addressing; a NULL slot is empty */
static usize tblcap, tbln;

static unsigned
mix(unsigned h, unsigned x)
{
  h ^= x;
  h *= 2654435761u; /* Knuth's multiplicative hash */
  return h;
}

static unsigned
phash(void *p)
{
  return (unsigned) ((unsigned long) p >> 3);
}

static unsigned
thash(Type *t)
{
  unsigned h = (unsigned) t->k;
  usize i;

  h = mix(h, (unsigned) t->num);
  h = mix(h, (unsigned) t->mut);
  h = mix(h, (unsigned) (t->n & 0xffffffffu));
  h = mix(h, (unsigned) (t->n >> 32));
  h = mix(h, (unsigned) t->nargs);
  if (t->sym)
    h = mix(h, phash(t->sym));
  if (t->gp)
    h = mix(h, phash(t->gp));
  if (t->t)
    h = mix(h, thash(t->t));
  for (i = 0; i < t->nargs; i++)
    h = mix(h, thash(t->args[i]));
  return h;
}

static int
teq(Type *a, Type *b)
{
  usize i;

  if (a == b)
    return 1;
  if (a->k != b->k || a->num != b->num || a->mut != b->mut || a->n != b->n ||
      a->nargs != b->nargs || a->sym != b->sym || a->gp != b->gp)
    return 0;
  if (!a->t != !b->t) /* one has a child, the other does not */
    return 0;
  if (a->t && !teq(a->t, b->t))
    return 0;
  for (i = 0; i < a->nargs; i++)
    if (!teq(a->args[i], b->args[i]))
      return 0;
  return 1;
}

static void
grow(void)
{
  usize newcap = tblcap ? tblcap * 2u : 1024u;
  Type **nt = arenaalloc(newcap * sizeof *nt);
  usize i;

  memset(nt, 0, newcap * sizeof *nt);
  for (i = 0; i < tblcap; i++) {
    if (tbl[i]) {
      usize j = thash(tbl[i]) & (newcap - 1u);

      while (nt[j])
        j = (j + 1u) & (newcap - 1u);
      nt[j] = tbl[i];
    }
  }
  tbl = nt;
  tblcap = newcap;
}

/* the one constructor: interns a copy of *t, hands back the canonical
 * one. grow() first -- the caller's stack Type must not be rehashed
 * into a stale table. */
static Type *
intern(Type *t)
{
  unsigned h;
  usize i;

  if (tbln * 4u >= tblcap * 3u)
    grow();
  h = thash(t);
  i = h & (tblcap - 1u);
  for (;;) {
    if (!tbl[i]) {
      Type *c = arenaalloc(sizeof *c);

      *c = *t;
      tbl[i] = c;
      tbln++;
      return c;
    }
    if (teq(tbl[i], t))
      return tbl[i];
    i = (i + 1u) & (tblcap - 1u);
  }
}

/* -- the constructors --------------------------------------------------- */

/* fill a stack Type from (kind, child), zeroed first -- unset slots
 * must read as 0, the same bargain mk's memset makes */
static Type *
mk1(u8 k, Type *t)
{
  Type x;

  memset(&x, 0, sizeof x);
  x.k = k;
  x.t = t;
  return intern(&x);
}

Type **
tyargs(usize n)
{
  Type **a = arenaalloc(n * sizeof *a);

  memset(a, 0, n * sizeof *a);
  return a;
}

Type *
tyunit(void)
{
  return mk1(Tyunit, 0);
}

Type *
tybool(void)
{
  return mk1(Tybool, 0);
}

Type *
tyvoidptr(void)
{
  return mk1(Tyvoidptr, 0);
}

Type *
tytype(void)
{
  return mk1(Tytype, 0);
}

Type *
tyint(int num)
{
  Type x;

  memset(&x, 0, sizeof x);
  x.k = Tyint;
  x.num = (u8) num;
  return intern(&x);
}

Type *
typaram(Ast *gp)
{
  Type x;

  memset(&x, 0, sizeof x);
  x.k = Typaram;
  x.gp = gp;
  return intern(&x);
}

Type *
tymut(Type *t)
{
  return mk1(Tymut, t);
}

Type *
typtr(Type *t)
{
  return mk1(Typtr, t);
}

Type *
tyslice(Type *t)
{
  return mk1(Tyslice, t);
}

Type *
tyarray(u64 n, Type *t)
{
  Type x;

  memset(&x, 0, sizeof x);
  x.k = Tyarray;
  x.n = n;
  x.t = t;
  return intern(&x);
}

Type *
tyarrayp(Ast *gp, Type *t) /* [N]T, N a const parameter */
{
  Type x;

  memset(&x, 0, sizeof x);
  x.k = Tyarray;
  x.gp = gp;
  x.t = t;
  return intern(&x);
}

Type *
tytuple(Type **ts, usize n)
{
  Type x;

  memset(&x, 0, sizeof x);
  x.k = Tytuple;
  x.args = n ? ts : 0;
  x.nargs = n;
  return intern(&x);
}

Type *
tyfn(Type **args, usize n, Type *ret)
{
  Type x;

  memset(&x, 0, sizeof x);
  x.k = Tyfn;
  x.args = n ? args : 0;
  x.nargs = n;
  x.t = ret ? ret : tyunit(); /* no return type is -> () */
  return intern(&x);
}

Type *
tysym(Sym *s, Type **args, usize n)
{
  Type x;

  memset(&x, 0, sizeof x);
  x.k = s->kind == Strait       ? Tytrait
        : s->tykind == TYstruct ? Tystruct
        : s->tykind == TYunion  ? Tyunion
                                : Tyenum;
  x.sym = s;
  x.args = n ? args : 0;
  x.nargs = n;
  return intern(&x);
}

Type *
tydyn(Sym *s, Type **args, usize n, int mut)
{
  Type x;

  memset(&x, 0, sizeof x);
  x.k = Tydyn;
  x.mut = (u8) (mut != 0);
  x.sym = s;
  x.args = n ? args : 0;
  x.nargs = n;
  return intern(&x);
}

Type *
tyopt(Type *t)
{
  Type **a = tyargs(1);

  a[0] = t;
  return tysym(sym_option, a, 1);
}

Type *
tyres(Type *t, Type *e) /* E?T is Result<T, E> */
{
  Type **a = tyargs(2);

  a[0] = t;
  a[1] = e;
  return tysym(sym_result, a, 2);
}

int
tysame(Type *a, Type *b)
{
  return a == b;
}

/* -- the printable form ------------------------------------------------- */

struct SBuf
{
  char *p;
  char *end; /* one byte short of the buffer's end, for the NUL */
};

static void
sbputs(struct SBuf *b, const char *s)
{
  while (*s && b->p < b->end)
    *b->p++ = *s++;
}

static void
sbputu(struct SBuf *b, u64 v)
{
  char d[20]; /* 2^64-1 is 20 digits */
  int n = 0;

  do {
    d[n++] = (char) ('0' + (int) (v % 10));
    v /= 10;
  } while (v != 0);
  while (n > 0 && b->p < b->end)
    *b->p++ = d[--n];
}

static const char *
inname(int num)
{
  static const char *names[IN_N] = {"i8",  "i16", "i32",  "i64",   "i128",  "u8",  "u16",
                                    "u32", "u64", "u128", "isize", "usize", "f32", "f64"};

  return (unsigned) num < IN_N ? names[num] : "?";
}

/* is this the prelude Option/Result? then it prints as sugar */
static int
isopt(Type *t)
{
  return t->k == Tyenum && t->sym == sym_option && t->nargs == 1;
}

static int
isres(Type *t)
{
  return t->k == Tyenum && t->sym == sym_result && t->nargs == 2;
}

static void sbfmt(struct SBuf *b, Type *t);

/* the sym kinds and dyn: a name, then <args> when there are any */
static void
sbname(struct SBuf *b, Type *t)
{
  usize i;

  sbputs(b, t->sym ? t->sym->name : "?");
  if (t->nargs == 0)
    return;
  sbputs(b, "<");
  for (i = 0; i < t->nargs; i++) {
    if (i > 0)
      sbputs(b, ", ");
    sbfmt(b, t->args[i]);
  }
  sbputs(b, ">");
}

static void
sbfmt(struct SBuf *b, Type *t)
{
  usize i;

  if (!t) {
    sbputs(b, "?");
    return;
  }
  switch (t->k) {
  case Tyunit:
    sbputs(b, "()");
    break;
  case Tybool:
    sbputs(b, "bool");
    break;
  case Tyint:
    sbputs(b, inname(t->num));
    break;
  case Tyvoidptr:
    sbputs(b, "voidptr");
    break;
  case Typaram:
    sbputs(b, t->gp ? t->gp->v.gp.name : "?");
    break;
  case Tymut: /* only under a slot; printed by its parent */
    sbputs(b, "mut ");
    sbfmt(b, t->t);
    break;
  case Typtr:
    sbputs(b, t->t && t->t->k == Tymut ? "*mut " : "*");
    sbfmt(b, t->t && t->t->k == Tymut ? t->t->t : t->t);
    break;
  case Tyslice:
    sbputs(b, t->t && t->t->k == Tymut ? "[]mut " : "[]");
    sbfmt(b, t->t && t->t->k == Tymut ? t->t->t : t->t);
    break;
  case Tyarray:
    sbputs(b, "[");
    if (t->gp)
      sbputs(b, t->gp->v.gp.name);
    else
      sbputu(b, t->n);
    sbputs(b, t->t && t->t->k == Tymut ? "]mut " : "]");
    sbfmt(b, t->t && t->t->k == Tymut ? t->t->t : t->t);
    break;
  case Tytuple:
    sbputs(b, "(");
    for (i = 0; i < t->nargs; i++) {
      if (i > 0)
        sbputs(b, ", ");
      sbfmt(b, t->args[i]);
    }
    if (t->nargs == 1)
      sbputs(b, ","); /* (T,) -- a one-element tuple, not a group */
    sbputs(b, ")");
    break;
  case Tystruct:
  case Tyunion:
  case Tyenum:
  case Tytrait:
    if (isopt(t)) {
      sbputs(b, "?");
      sbfmt(b, t->args[0]);
      break;
    }
    if (isres(t)) {
      sbfmt(b, t->args[1]); /* E?T: the error side leads */
      sbputs(b, "?");
      sbfmt(b, t->args[0]);
      break;
    }
    sbname(b, t);
    break;
  case Tydyn:
    sbputs(b, t->mut ? "dyn mut " : "dyn ");
    sbname(b, t);
    break;
  case Tyfn:
    sbputs(b, "fn(");
    for (i = 0; i < t->nargs; i++) {
      if (i > 0)
        sbputs(b, ", ");
      sbfmt(b, t->args[i]);
    }
    sbputs(b, ")");
    if (t->t && t->t->k != Tyunit) { /* -> () is left unsaid */
      sbputs(b, " -> ");
      sbfmt(b, t->t);
    }
    break;
  case Tytype:
    sbputs(b, "type");
    break;
  default:
    sbputs(b, "?");
    break;
  }
}

char *
tysprint(char *buf, usize n, Type *t)
{
  struct SBuf b;

  if (n == 0)
    return buf;
  b.p = buf;
  b.end = buf + n - 1u;
  sbfmt(&b, t);
  *b.p = 0;
  return buf;
}

void
tyfmt(Type *t)
{
  char buf[256];

  fputs(tysprint(buf, sizeof buf, t), stdout);
}
