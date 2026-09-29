/* eval.c -- the compile-time evaluator, leaf layer (08-reflection.md).
 *
 * Compile-time execution is the ordinary language evaluated by the
 * compiler. This file's half is the leaves: literals, arithmetic and
 * the operators over them, the layout queries, @cast, and the const
 * references that chain them -- the expressions a type's own parts
 * need before any body is checked. The fn-call half waits for the
 * checker's passes (M5b); an aggregate waits with it.
 *
 * The four promises (08): determinism -- the evaluator observes
 * nothing; no Drop -- a move here is a value, and no destructor is
 * looked for; overflow is an error -- every step is checked against
 * the type it lands in, not only the literals; an end -- a step
 * limit, and past it a compile error, never a hang.
 *
 * Integers live in their 64-bit two's complement; the domain check
 * reads them signed, so a step one past an i32's max reads as the
 * 2147483648 it is, not the -2147483648 its bits would spell. */

#include "eval.h"

#include "check.h" /* cerrat, rty */
#include "layout.h"
#include "type.h"

__extension__ typedef long long i64; /* the signed kin of lex.h's u64 */

/* the budget: enough for a table a program would build, short of a
 * search a program should not run at compile time */
#define STEPS (1u << 20)
#define MAXDEPTH                                                                                   \
  64 /* consts in flight: a cycle is an error, not a                                               \
      * deep one */

typedef struct
{
  Type  *t; /* what the checker would say; the derivation's answer */
  u64    i; /* an integer's or a bool's bits, two's complement */
  double f; /* a float's value */
} Val;

static Sym *inflight[MAXDEPTH]; /* the consts being resolved, for the
                                 * cycle check */
static usize ninflight;
static usize steps;

/* -- the value domain ---------------------------------------------------- */

/* a type's name, for a message: two a line, at the most */
static char *
tnm(Type *t)
{
  static char b[2][64];
  static int  k;

  k = (k + 1) % 2;
  return tysprint(b[k], sizeof b[0], t);
}

/* do the bits fit the type? The domain check is the overflow promise,
 * on every step (08) */
static int
inrange(u64 v, Type *t)
{
  i64 iv = (i64) v;

  if (t->k == Tybool)
    return v <= 1;
  if (t->k != Tyint)
    return 0;
  switch (t->num) {
  case IN_I8:
    return iv >= -128 && iv <= 127;
  case IN_I16:
    return iv >= -32768 && iv <= 32767;
  case IN_I32:
    return iv >= -2147483647 - 1 && iv <= 2147483647;
  case IN_I64:
  case IN_ISIZE:
    return 1; /* every u64 is some i64 */
  case IN_U8:
    return v <= 0xffu;
  case IN_U16:
    return v <= 0xffffu;
  case IN_U32:
    return v <= 0xffffffffu;
  case IN_U64:
  case IN_USIZE: /* the sizes are the machine's: 64 (layout.c) */
    return 1;
  default: /* the 128s: this evaluator does not carry them */
    return 0;
  }
}

static int
isfloatty(Type *t)
{
  return t->k == Tyint && t->num >= IN_F32;
}

static int
issignedty(Type *t)
{
  return t->k == Tyint && (t->num <= IN_I64 || t->num == IN_ISIZE);
}

static void
domerr(Ast *at, Type *t)
{
  cerrat(at, "overflow: the step does not fit %s (08-reflection.md)", tnm(t));
}

static void
domcheck(u64 v, Type *t, Ast *at)
{
  if (!inrange(v, t))
    domerr(at, t);
}

/* -- the operators, overflow checked ------------------------------------- */

static u64
ovadd(u64 a, u64 b, Type *t, Ast *at)
{
  u64 r = a + b;

  if (r < a) /* the u64 wrapped: no type but u64 could hold it */
    domerr(at, t);
  domcheck(r, t, at);
  return r;
}

static u64
ovsub(u64 a, u64 b, Type *t, Ast *at)
{
  u64 r = a - b;

  if (r > a) /* wrapped the other way */
    domerr(at, t);
  domcheck(r, t, at);
  return r;
}

static u64
ovmul(u64 a, u64 b, Type *t, Ast *at)
{
  u64 r = a * b;

  if (a && b > ~(u64) 0 / a)
    domerr(at, t);
  domcheck(r, t, at);
  return r;
}

/* C89 leaves the direction of a signed division to the host; the
 * language takes C99's -- toward zero -- so the evaluator does too,
 * without asking the host what it would do (07-operators.md) */
static u64
sdiv(u64 a, u64 b, Type *t, Ast *at)
{
  i64 x = (i64) a;
  i64 y = (i64) b;
  i64 q;

  if (b == 0)
    cerrat(at, "division by zero at compile time (08-reflection.md)");
  if (a == ((u64) 1 << 63) && b == ~(u64) 0) /* min / -1: the one
                                              * quotient no signed
                                              * type holds */
    domerr(at, t);
  q = x / y;
  if (x % y != 0 && ((x < 0) != (y < 0)))
    q--; /* the host's floor, to trunc */
  domcheck((u64) q, t, at);
  return (u64) q;
}

static u64
smod(u64 a, u64 b, Ast *at)
{
  i64 x = (i64) a;
  i64 y = (i64) b;

  if (b == 0)
    cerrat(at, "division by zero at compile time (08-reflection.md)");
  return (u64) (x % y); /* trunc's remainder: the sign follows the
                         * dividend */
}

static int
intbits(Type *t)
{
  switch (t->num) {
  case IN_I8:
  case IN_U8:
    return 8;
  case IN_I16:
  case IN_U16:
    return 16;
  case IN_I32:
  case IN_U32:
    return 32;
  default:
    return 64; /* the 64s and the sizes; the 128s never arrived */
  }
}

static u64
ovshl(u64 a, u64 b, Type *t, Ast *at)
{
  u64 r;

  if (b >= (u64) intbits(t))
    cerrat(at, "a shift past the type's width (08-reflection.md)");
  r = a << b;
  domcheck(r, t, at);
  return r;
}

static u64
ovshr(u64 a, u64 b, Type *t, Ast *at)
{
  if (b >= (u64) intbits(t))
    cerrat(at, "a shift past the type's width (08-reflection.md)");
  if (issignedty(t)) /* the sign rides along: the hosts the compiler
                      * runs on shift arithmetically */
    return (u64) (((i64) a) >> b);
  return a >> b;
}

static Val
valint(u64 v, Type *t)
{
  Val r;

  r.t = t;
  r.i = v;
  r.f = 0;
  return r;
}

/* -- the walk ------------------------------------------------------------ */

static Val ceval(Ast *e, Env env, Type *want);
static Val symval(Sym *s, Ast *at);

/* a bare literal bends to the type the other side brought; nothing
 * else does */
static int
barelit(Ast *e)
{
  return e->k == Nint || e->k == Nflt;
}

/* the type a literal takes: the one expected of it, when the
 * expectation is of its kind; an integer's default is i32, a float's
 * f32 -- so 3.14 is an f32 (01-types.md) */
static Type *
litty(Ast *e, Type *want)
{
  if (e->k == Nint) {
    if (want && want->k == Tyint && !isfloatty(want))
      return want;
    return tyint(IN_I32);
  }
  if (want && isfloatty(want))
    return want;
  return tyint(IN_F32);
}

static void
tick(Ast *e)
{
  if (++steps > STEPS)
    cerrat(e, "evaluation did not end: the step limit (08-reflection.md)");
}

static Val
ceval(Ast *e, Env env, Type *want)
{
  tick(e);
  switch (e->k) {
  case Nint:
    return valint(e->v.i.num, litty(e, want));
  case Nflt: {
    Val r;

    r.t = litty(e, want);
    r.i = 0;
    r.f = e->v.f.flt;
    return r;
  }
  case Nbool:
    return valint(e->v.i.num ? 1 : 0, tybool());
  case Nunit:
    return valint(0, tyunit());
  case Npath: { /* a const reference: the chain (08) */
    char *nm = e->v.path.segs[0]->v.seg.name;
    Sym  *s;
    Type *p;

    if (vlen(e->v.path.segs) != 1 || e->v.path.root)
      cerrat(e, "this path is not compile-time known (08-reflection.md)");
    p = envfind(&env, nm);
    if (p) /* a generic's const parameter: it has a value only at a
            * call, and the evaluator runs before any */
      cerrat(e, "'%s' is a const parameter: it has no value until the call (08-reflection.md)", nm);
    s = symfind(nm);
    if (!s || s->kind != Sconst)
      cerrat(e, "'%s' is not a const here (08-reflection.md)", nm);
    return symval(s, e);
  }
  case Nun: {
    Val v = ceval(e->v.un.e, env, want);

    switch (e->v.un.op) {
    case Tminus:
      if (e->v.un.e->k == Nint) { /* the sign rides the literal: i32's
                                   * least, -2147483648, is spelled
                                   * with it, and no other way is */
        Type *lt = litty(e->v.un.e, want);
        u64   m = 0 - e->v.un.e->v.i.num;

        domcheck(m, lt, e);
        return valint(m, lt);
      }
      if (isfloatty(v.t)) {
        v.f = -v.f;
        return v;
      }
      return valint(ovsub(0, v.i, v.t, e), v.t);
    case Ttilde:
      return valint(~v.i, v.t);
    case Tbang:
      return valint(v.i == 0, tybool());
    default:
      cerrat(e, "this operator is not compile-time known (08-reflection.md)");
      return valint(0, tyint(IN_I32)); /* unreachable */
    }
  }
    /* unreachable: every way out returned */
    return valint(0, tyint(IN_I32));
  case Nbin: {
    Ast *l = e->v.bin.l;
    Ast *r = e->v.bin.r;
    Tok  op = e->v.bin.op;
    Val  a, b;

    if (op == Tampamp || op == Tbarbar) { /* the short circuit, kept */
      a = ceval(l, env, tybool());
      if (a.t->k != Tybool)
        cerrat(l, "the side is not a bool (07-operators.md)");
      if ((op == Tampamp && a.i == 0) || (op == Tbarbar && a.i != 0))
        return a;
      b = ceval(r, env, tybool());
      if (b.t->k != Tybool)
        cerrat(r, "the side is not a bool (07-operators.md)");
      return b;
    }
    /* the sides must agree; a bare literal takes the other's type */
    if (barelit(l) && !barelit(r)) {
      b = ceval(r, env, 0);
      a = ceval(l, env, b.t);
    } else {
      a = ceval(l, env, 0);
      b = ceval(r, env, a.t);
    }
    if (!tysame(a.t, b.t))
      cerrat(e, "the sides differ: %s and %s (07-operators.md)", tnm(a.t), tnm(b.t));
    if (isfloatty(a.t)) {
      switch (op) {
      case Tplus:
        a.f = a.f + b.f;
        return a;
      case Tminus:
        a.f = a.f - b.f;
        return a;
      case Tstar:
        a.f = a.f * b.f;
        return a;
      case Tslash:
        if (b.f == 0)
          cerrat(e, "division by zero at compile time (08-reflection.md)");
        a.f = a.f / b.f;
        return a;
      case Tlt:
        return valint(a.f < b.f, tybool());
      case Tgt:
        return valint(a.f > b.f, tybool());
      case Tle:
        return valint(a.f <= b.f, tybool());
      case Tge:
        return valint(a.f >= b.f, tybool());
      case Teqeq:
        return valint(a.f == b.f, tybool());
      case Tne:
        return valint(a.f != b.f, tybool());
      default:
        cerrat(e, "this operator is not defined for floats (07-operators.md)");
        return valint(0, tybool()); /* unreachable */
      }
    }
    if (a.t->k == Tybool) {
      switch (op) {
      case Teqeq:
        return valint(a.i == b.i, tybool());
      case Tne:
        return valint(a.i != b.i, tybool());
      default:
        cerrat(e, "this operator is not defined for bools (07-operators.md)");
        return valint(0, tybool()); /* unreachable */
      }
    }
    if (a.t->k != Tyint)
      cerrat(e, "this expression is not compile-time known (08-reflection.md)");
    switch (op) {
    case Tplus:
      return valint(ovadd(a.i, b.i, a.t, e), a.t);
    case Tminus:
      return valint(ovsub(a.i, b.i, a.t, e), a.t);
    case Tstar:
      return valint(ovmul(a.i, b.i, a.t, e), a.t);
    case Tslash:
      return valint(sdiv(a.i, b.i, a.t, e), a.t);
    case Tpercent:
      return valint(smod(a.i, b.i, e), a.t);
    case Tamp:
      return valint(a.i & b.i, a.t);
    case Tbar:
      return valint(a.i | b.i, a.t);
    case Tcaret:
      return valint(a.i ^ b.i, a.t);
    case Tshl:
      return valint(ovshl(a.i, b.i, a.t, e), a.t);
    case Tshr:
      return valint(ovshr(a.i, b.i, a.t, e), a.t);
    case Tlt:
    case Tgt:
    case Tle:
    case Tge: {
      int sg = issignedty(a.t);

      switch (op) {
      case Tlt:
        return valint(sg ? (i64) a.i < (i64) b.i : a.i < b.i, tybool());
      case Tgt:
        return valint(sg ? (i64) a.i > (i64) b.i : a.i > b.i, tybool());
      case Tle:
        return valint(sg ? (i64) a.i <= (i64) b.i : a.i <= b.i, tybool());
      default:
        return valint(sg ? (i64) a.i >= (i64) b.i : a.i >= b.i, tybool());
      }
    }
    case Teqeq:
      return valint(a.i == b.i, tybool());
    case Tne:
      return valint(a.i != b.i, tybool());
    default:
      cerrat(e, "this operator is not compile-time known (08-reflection.md)");
      return valint(0, tyint(IN_I32)); /* unreachable */
    }
  }
  case Nbuiltin: {
    char *nm = e->v.blt.name;

    if (strcmp(nm, "sizeof") == 0 || strcmp(nm, "alignof") == 0) {
      Env   e2 = env; /* rty may bind the generic names it reads */
      Type *t;
      usize v;

      if (vlen(e->v.blt.targs) != 1)
        cerrat(e, "@%s takes one type argument (08-reflection.md)", nm);
      t = rty(e->v.blt.targs[0], &e2);
      v = strcmp(nm, "sizeof") == 0 ? sizeof_(t) : alignof_(t);
      return valint(v, tyint(IN_USIZE));
    }
    if (strcmp(nm, "cast") == 0) { /* the well-defined conversions
                                    * (01-types.md) */
      Env   e2 = env;
      Val   v;
      Type *t;

      if (vlen(e->v.blt.targs) != 1 || vlen(e->v.blt.args) != 1)
        cerrat(e, "@cast takes one type argument and one value");
      t = rty(e->v.blt.targs[0], &e2);
      v = ceval(e->v.blt.args[0], env, t);
      if (isfloatty(v.t)) { /* from a float: toward zero, the spec's
                             * truncation (01-types.md: @cast<u32>
                             * (3.14) is 3); the target's domain is
                             * the check */
        double d = v.f;

        if (isfloatty(t)) {
          v.t = t;
          v.f = t->num == IN_F32 ? (double) (float) d : d;
          return v;
        }
        {
          i64 q;

          if (d != d || d < -9223372036854775808.0 || d >= 9223372036854775808.0)
            cerrat(e, "overflow: %g does not fit %s (08-reflection.md)", d, tnm(t));
          q = (i64) d; /* the host's cast truncates, as the spec's
                        * does */
          domcheck((u64) q, t, e);
          return valint((u64) q, t);
        }
      }
      if (isfloatty(t)) { /* to a float: the widening */
        v.f = (double) (i64) v.i;
        v.i = 0;
        v.t = t;
        return v;
      }
      if (t->k == Tybool && v.t->k != Tybool)
        cerrat(e, "@cast to bool is not a conversion (01-types.md)");
      if (v.t->k == Tyint || v.t->k == Tybool)
        domcheck(v.i, t, e); /* the widening and the narrowing both:
                              * the target's domain is the check */
      else
        cerrat(e, "this cast is not compile-time known (08-reflection.md)");
      return valint(v.i, t);
    }
    cerrat(e, "@%s is not compile-time known here (08-reflection.md)", nm);
    return valint(0, tyint(IN_I32)); /* unreachable */
  }
  default:
    cerrat(e, "this expression is not compile-time known (08-reflection.md)");
  }
  return valint(0, tyint(IN_I32)); /* unreachable */
}

/* a const's own value: the initializer, against the declared type.
 * Lazy -- the chain resolves on demand -- and the in-flight stack is
 * the cycle check: a const that depends on itself is an error, not a
 * hang (08: the budget) */
static Val
symval(Sym *s, Ast *at)
{
  Val v;

  if (s->cvaldone) { /* the chain landed here before */
    v.t = s->cty;
    v.i = s->cval;
    v.f = s->cflt;
    return v;
  }
  if (s->kind != Sconst && s->kind != Sstatic) /* an entry from pass 2
                                                * takes either; a
                                                * reference was
                                                * narrowed at the
                                                * path, before the
                                                * chain began */
    cerrat(at, "'%s' is not a const here (08-reflection.md)", s->name);
  if (ninflight >= MAXDEPTH)
    cerrat(at, "the const chain nests too deep (08-reflection.md)");
  {
    usize d;

    for (d = 0; d < ninflight; d++)
      if (inflight[d] == s)
        cerrat(at, "'%s' depends on itself (08-reflection.md)", s->name);
  }
  inflight[ninflight++] = s;
  if (!s->cty) { /* a forward reference: the type resolves on demand
                  * -- a scalar's type wants nothing later than
                  * itself */
    Env env = envnone();

    s->cty = rty(s->decl->v.cst.t, &env);
  }
  if (!s->decl->v.cst.e)
    cerrat(s->decl, "a const carries an initializer (01-types.md)");
  v = ceval(s->decl->v.cst.e, envnone(), s->cty);
  if (!tysame(v.t, s->cty)) { /* the derivation's answer against the
                               * declaration: an integer may land wide
                               * and narrow by fit; a float rounds, it
                               * does not overflow; the rest is a
                               * mismatch */
    if (v.t->k == Tyint && s->cty->k == Tyint && !isfloatty(v.t) && !isfloatty(s->cty))
      domcheck(v.i, s->cty, s->decl->v.cst.e);
    else if (isfloatty(v.t) && isfloatty(s->cty))
      v.f = s->cty->num == IN_F32 ? (double) (float) v.f : v.f;
    else
      cerrat(s->decl->v.cst.e, "'%s' is %s, the initializer is %s", s->name, tnm(s->cty), tnm(v.t));
  }
  s->cval = v.i;
  s->cflt = v.f;
  s->cvaldone = 1;
  ninflight--;
  return v;
}

void
cevalsym(Sym *s)
{
  symval(s, s->decl);
}

/* an integer's value, at a place a type's own parts need one: an
 * array's length, a variant's discriminant. The expression runs in
 * the env the enclosing declaration reads (a const parameter there
 * has no value yet, and the walk says so); the want is the caller's
 * -- a length is a usize, a discriminant the tag's shape -- and the
 * answer must be an integer, and not negative */
u64
cevallong(Ast *e, Env env, Type *want)
{
  Val v = ceval(e, env, want);

  if (v.t->k != Tyint || isfloatty(v.t))
    cerrat(e, "the value is not an integer: a length and a discriminant are (08-reflection.md)");
  if ((i64) v.i < 0)
    cerrat(e, "the value is negative: a length and a discriminant are not (08-reflection.md)");
  return v.i;
}
