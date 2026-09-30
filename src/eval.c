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
#define CALLDEPTH                                                                                  \
  128                   /* frames in flight: recursion has an end, as                              \
                         * the 256 unfoldings of a generic do (04-generics.md) */
#define MAXLOCALS 16384 /* bindings across every frame on the stack */

/* a value the walk carries: a scalar in its two words, an array as
 * its elements -- one Val each, the nesting recursive. The aggregate
 * half is the evaluator's own: nothing outside this file reads it,
 * for emit has no aggregate immediate to fold (08-reflection.md) */
typedef struct Val Val;
struct Val
{
  Type  *t;     /* what the checker would say; the derivation's answer */
  u64    i;     /* an integer's or a bool's bits, two's complement */
  double f;     /* a float's value */
  Val   *elems; /* an array's elements, or NULL: the scalars' mark */
};

/* a binding a frame made: a parameter, a let. The frames stack
 * upward in one array -- locbase marks the current frame's floor,
 * so an inner frame never writes a slot a live outer one owns */
typedef struct
{
  char *name;
  Val   v;
  int   mut;
} Loc;

static Sym *inflight[MAXDEPTH]; /* the consts being resolved, for the
                                 * cycle check */
static usize ninflight;
static usize steps;
static Loc   locs[MAXLOCALS];
static usize nlocs;   /* the stack's top */
static usize locbase; /* the current frame's floor */
static usize calldepth;
static int   returning; /* a return is unwinding this frame */
static int   breaking;  /* a break is unwinding to its for, one level
                         * deep (10-iteration.md) */
static int   continuing;
static Val   retv;
static Type *fnret; /* the frame's fn, its return type: a return's
                     * want */

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

/* an aggregate's value is its parts, not its words: the elements a
 * Val carries, an enum's tag, a union's one -- the operators ask the
 * trait table for these, and that table is a later milestone */
static int
isaggty(Type *t)
{
  return t->k == Tyarray || t->k == Tystruct || t->k == Tyunion || t->k == Tytuple ||
         t->k == Tyenum;
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

/* the signs decide a signed step: the u64 bits are two's complement,
 * and a wrap there is one only the u64's own ends make. The operands
 * agreeing, an answer that flipped is past the i64; differing, one
 * that took the first's sign is (add goes one way, sub the other) */
static u64
ovadd(u64 a, u64 b, Type *t, Ast *at)
{
  u64 r = a + b;

  if (issignedty(t)) {
    if (((i64) a >= 0) == ((i64) b >= 0) && ((i64) r >= 0) != ((i64) a >= 0))
      domerr(at, t);
  } else if (r < a) /* unsigned: the u64 wrapped */
    domerr(at, t);
  domcheck(r, t, at);
  return r;
}

static u64
ovsub(u64 a, u64 b, Type *t, Ast *at)
{
  u64 r = a - b;

  if (issignedty(t)) {
    if (((i64) a >= 0) != ((i64) b >= 0) && ((i64) r >= 0) != ((i64) a >= 0))
      domerr(at, t);
  } else if (r > a) /* unsigned: wrapped the other way */
    domerr(at, t);
  domcheck(r, t, at);
  return r;
}

static u64
ovmul(u64 a, u64 b, Type *t, Ast *at)
{
  u64 r = a * b;

  if (issignedty(t)) { /* the u64 domain asks the wrong question of a
                        * negative pair; the i64's own wrap is what
                        * cannot be -- the signs the product must
                        * carry, the quotient that does not divide
                        * back, and the one the division cannot ask */
    i64 ia = (i64) a, ib = (i64) b, ir = (i64) r;

    if (ia == 0 || ib == 0)
      return 0;
    if (ia == ((i64) 1 << 63) && ib == -1)
      domerr(at, t);
    if (((ia >= 0) == (ib >= 0)) != (ir >= 0))
      domerr(at, t); /* operands agreeing carry a nonnegative
                      * product, differing a nonpositive one */
    if (ir / ib != ia)
      domerr(at, t);                /* the product does not divide back */
  } else if (a && b > ~(u64) 0 / a) /* unsigned: the u64 wrapped */
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
  u64 ax, ay, q;

  if (b == 0)
    cerrat(at, "division by zero at compile time (08-reflection.md)");
  if (a == ((u64) 1 << 63) && b == ~(u64) 0) /* min / -1: the one
                                              * quotient no signed
                                              * type holds */
    domerr(at, t);
  /* absolute values in the u64: C89 leaves a negative division's
   * rounding to the host, and the running program's is qbe's --
   * trunc, toward zero. The sign goes back after; the least's own
   * absolute value wraps to itself, and the quotient survives it
   * (min / 1 is min) */
  ax = x < 0 ? (u64) 0 - a : a;
  ay = y < 0 ? (u64) 0 - b : b;
  q = ax / ay;
  q = (x < 0) != (y < 0) ? (u64) 0 - q : q;
  domcheck(q, t, at);
  return q;
}

static u64
smod(u64 a, u64 b, Type *t, Ast *at)
{
  i64 x = (i64) a;
  i64 y = (i64) b;

  if (b == 0)
    cerrat(at, "division by zero at compile time (08-reflection.md)");
  if (!issignedty(t)) /* the u64's own remainder: the bits are the
                       * answer, no reading of them as signed */
    return a % b;
  { /* trunc's remainder: the magnitude the operands', the sign the
     * dividend's -- C89 leaves the host to choose, the running
     * program's choice is qbe's (07-operators.md) */
    u64 ax = x < 0 ? (u64) 0 - a : a;
    u64 ay = y < 0 ? (u64) 0 - b : b;
    u64 r = ax % ay;

    return x < 0 ? (u64) 0 - r : r;
  }
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
  r.elems = 0;
  return r;
}

/* a value landing where a type is spelled: an integer may land wide
 * and narrow by fit, a float rounds, the rest must be the same type
 * (01-types.md) -- the const declaration's own rule, at a let, a
 * parameter, a return, an assignment */
static Val
valcoerce(Val v, Type *t, Ast *at)
{
  if (tysame(v.t, t))
    return v;
  if (v.t->k == Tyint && t->k == Tyint && !isfloatty(v.t) && !isfloatty(t)) {
    domcheck(v.i, t, at);
    return valint(v.i, t);
  }
  if (isfloatty(v.t) && isfloatty(t)) {
    v.f = t->num == IN_F32 ? (double) (float) v.f : v.f;
    v.t = t;
    return v;
  }
  cerrat(at, "the value is %s, %s is expected (01-types.md)", tnm(v.t), tnm(t));
  return valint(0, t); /* unreachable */
}

/* the current frame's binding, innermost last: a later let shadows
 * an earlier one, a local shadows a const of the same name */
static Loc *
locfind(char *name)
{
  usize i;

  for (i = nlocs; i > locbase;)
    if (strcmp(locs[--i].name, name) == 0)
      return &locs[i];
  return 0;
}

/* two agreeing sides and an operator: the arithmetic, the
 * comparisons, the bitwise -- every step checked against the type it
 * lands in (08-reflection.md). Extracted so a compound assignment
 * runs the same rules a binary expression does */
static Val
valbin(Val a, Val b, Tok op, Ast *e)
{
  if (a.elems || b.elems || isaggty(a.t) || isaggty(b.t)) /* the words
                                                           * are an address or a tag here, and
                                                           * comparing those compares nothing; the
                                                           * operators over aggregates are the trait
                                                           * table's, a later milestone (07) */
    cerrat(e, "an operator over aggregates arrives with a later milestone (08-reflection.md)");
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
    return valint(smod(a.i, b.i, a.t, e), a.t);
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

/* a compound assignment's operator: += is +, the rest as they come;
 * the plain = is not one of them */
static int
assignop(Tok op, Tok *base)
{
  switch (op) {
  case Tpluseq:
    *base = Tplus;
    return 1;
  case Tminuseq:
    *base = Tminus;
    return 1;
  case Tstareq:
    *base = Tstar;
    return 1;
  case Tslasheq:
    *base = Tslash;
    return 1;
  case Tshleq:
    *base = Tshl;
    return 1;
  case Tshreq:
    *base = Tshr;
    return 1;
  default:
    return 0;
  }
}

/* -- the walk ------------------------------------------------------------ */

static Val ceval(Ast *e, Env env, Type *want);
static Val symval(Sym *s, Ast *at);
static Val callval(Ast *e, Env env);
static Val execblk(Ast *b, Env env, Type *want);

/* a variant's value: the discriminant in the tag's own bits, the
 * payload in the variant's own order -- under the enum's instance a
 * call spelled or a want named (01-types.md) */
static Val
variantval(Sym *s, Variant *v, Type **targs, usize ntargs, Ast **args, usize na, Env env, Ast *at)
{
  Val    r;
  Type  *et;
  Type **ps;
  usize  np, i;

  et = tysym(s, targs, ntargs);
  if (v->named) {
    np = v->nfields;
    ps = tyargs(np);
    for (i = 0; i < np; i++)
      ps[i] =
          s->ngparams ? gsubst(v->fields[i].ty, s->gparams, targs, s->ngparams) : v->fields[i].ty;
  } else {
    np = v->payload && v->npayload ? v->npayload : 0;
    ps = np ? tyargs(np) : 0;
    if (ps)
      for (i = 0; i < np; i++)
        ps[i] = s->ngparams ? gsubst(v->payload[i], s->gparams, targs, s->ngparams) : v->payload[i];
  }
  if (na != np) /* the checker's own count, kept here (01-types.md) */
    cerrat(at, "'%s' carries %lu payload%s, %lu given (01-types.md)", v->name, (unsigned long) np,
           np == 1 ? "" : "s", (unsigned long) na);
  r.t = et;
  r.i = v->disc; /* the discriminant: @cast reads it out (01) */
  r.f = 0;
  r.elems = np ? arenaalloc(np * sizeof *r.elems) : 0;
  for (i = 0; i < np; i++) {
    r.elems[i] = valcoerce(ceval(args[i], env, ps[i]), ps[i], args[i]);
  }
  return r;
}

/* a struct or union's literal, against its own declared shape: the
 * rows by name, the ones left out zero. A union keeps one row active
 * -- the last name a literal wrote, or the zeroed whole -- and a read
 * of any other is the unspecified thing the spec says not to rely
 * on, which the promise of determinism turns into a report (01, 08) */
static Val
structlitval(Type *t, Ast **inits, Env env, Ast *at)
{
  Sym  *s = t->sym;
  usize n = vlen(inits), nf = s->nfields, i, k;
  int   seen = -1;
  Val  *els;
  Val   r;

  (void) at;                /* the errors point at the rows themselves */
  for (i = 0; i < n; i++) { /* the checker's own rule, kept here:
                             * a name the shape does not hold, a name
                             * given twice */
    int f = -1;

    for (k = 0; k < nf; k++)
      if (strcmp(s->fields[k].name, inits[i]->v.init.name) == 0) {
        f = (int) k;
        break;
      }
    if (f < 0)
      cerrat(inits[i], "'%s' has no field '%s' (01-types.md)", s->name, inits[i]->v.init.name);
    for (k = 0; k < i; k++)
      if (strcmp(inits[k]->v.init.name, inits[i]->v.init.name) == 0)
        cerrat(inits[i], "field '%s' given twice (01-types.md)", inits[i]->v.init.name);
    if (s->tykind == TYunion) /* the last write wins, as C's own
                               * designated rule (01-types.md) */
      seen = f;
  }
  if (t->k == Tyunion) {
    r.t = t;
    r.i = ~(u64) 0; /* the zeroed whole: every row reads zero */
    r.f = 0;
    r.elems = arenaalloc(sizeof *r.elems);
    if (n) {
      Ast *in = inits[n - 1];

      r.i = (u64) seen;
      r.elems[0] =
          valcoerce(ceval(in->v.init.e, env, s->fields[seen].ty), s->fields[seen].ty, in->v.init.e);
    }
    return r;
  }
  els = arenaalloc((nf ? nf : 1) * sizeof *els);
  for (i = 0; i < nf; i++) { /* the zero every left-out row is */
    els[i] = valint(0, s->fields[i].ty);
    if (isfloatty(s->fields[i].ty))
      els[i].f = 0;
  }
  for (i = 0; i < n; i++) {
    Ast *in = inits[i];

    for (k = 0; k < nf; k++)
      if (strcmp(s->fields[k].name, in->v.init.name) == 0)
        break;
    els[k] = valcoerce(ceval(in->v.init.e, env, s->fields[k].ty), s->fields[k].ty, in->v.init.e);
  }
  r.t = t;
  r.i = 0;
  r.f = 0;
  r.elems = els;
  return r;
}

/* a binding into the frame, above the rounds and the arms that made
 * it -- a later binding of the name shadows this one, for the find
 * walks from the top */
static void
locpush(char *name, Val v, int mut, Ast *at)
{
  if (nlocs >= MAXLOCALS)
    cerrat(at, "too many bindings in flight (08-reflection.md)");
  locs[nlocs].name = name;
  locs[nlocs].v = v;
  locs[nlocs].mut = mut;
  nlocs++;
}

/* -- patterns (09-match.md) ------------------------------------------------
 *
 * A pattern destructures; it does not test. The one thing it can
 * miss is a variant pattern naming another variant, so fitting is a
 * discriminant's compare and binding is the rows the checker typed
 * each name with -- the checker's rpat, run. */

/* the variant an Nppath names, when it names one: Enum::V by its two
 * segments, or the short name the scrutinee's own enum picks out.
 * NULL: the pattern is the binding form (09-match.md) */
static Variant *
patvariant(Ast *p, Type *vt)
{
  Ast **segs = p->v.ppath.path->v.path.segs;
  Sym  *s;

  if (vlen(segs) == 2) {
    s = symfind(segs[0]->v.seg.name);
    if (!s || s->kind != Stype || s->tykind != TYenum)
      return 0; /* unreachable: the checker fitted it */
    return symvarfind(s, segs[1]->v.seg.name);
  }
  if (vt->k != Tyenum)
    return 0; /* the bare name that binds, over anything else */
  return symvarfind(vt->sym, segs[0]->v.seg.name);
}

/* does the pattern fit the value? The arms take their turn by it,
 * and a for let runs its rounds while it holds (09, 10) */
static int
patfits(Ast *p, Val v)
{
  switch (p->k) {
  case Npwild:
    return 1;
  case Nppath: {
    Variant *var = patvariant(p, v.t);

    if (!var)
      return 1; /* the binding form: it fits */
    if (v.t->k != Tyenum || v.i != var->disc)
      return 0; /* another variant's round */
    if (!p->v.ppath.payload)
      return 1;             /* the variant names itself; the niche's unit payload
                             * binds nothing (09) */
    if (p->v.ppath.named) { /* by field name, mirroring the decl */
      Ast **fs = p->v.ppath.payload;
      usize n = vlen(fs), i;

      for (i = 0; i < n; i++) {
        Ast  *pf = fs[i];
        usize k;

        if (!pf->v.init.e)
          continue; /* the field name is the binding name */
        for (k = 0; k < var->nfields; k++)
          if (strcmp(var->fields[k].name, pf->v.init.name) == 0)
            break;
        if (k == var->nfields)
          continue; /* unreachable: the checker fitted it */
        if (!patfits(pf->v.init.e, v.elems[k]))
          return 0;
      }
      return 1;
    }
    { /* by position */
      Ast **ps = p->v.ppath.payload;
      usize n = vlen(ps), i;

      for (i = 0; i < n; i++)
        if (!patfits(ps[i], v.elems[i]))
          return 0;
      return 1;
    }
  }
  case Nptuple: {
    Ast **ps = p->v.list.ts;
    usize n = vlen(ps), i;

    for (i = 0; i < n; i++)
      if (!patfits(ps[i], v.elems[i]))
        return 0;
    return 1;
  }
  case Npstruct: {
    Ast **fs = p->v.pstruct.fields;
    usize n = vlen(fs), i;

    for (i = 0; i < n; i++) {
      Ast  *pf = fs[i];
      usize k;

      if (!pf->v.init.e)
        continue;
      for (k = 0; k < v.t->sym->nfields; k++)
        if (strcmp(v.t->sym->fields[k].name, pf->v.init.name) == 0)
          break;
      if (k == v.t->sym->nfields)
        continue; /* unreachable: the checker fitted it */
      if (!patfits(pf->v.init.e, v.elems[k]))
        return 0;
    }
    return 1;
  }
  case Npor: { /* whichever of them fits (09-match.md) */
    Ast **ps = p->v.list.ts;
    usize n = vlen(ps), i;

    for (i = 0; i < n; i++)
      if (patfits(ps[i], v))
        return 1;
    return 0;
  }
  default:
    return 1; /* the shapes the parser does not make; nothing tests */
  }
}

/* bind the pattern's names against the value: every name the pattern
 * spells gets the row the checker typed it with. An or-pattern binds
 * each alternative that fits, as the emitter binds them all to the
 * one place -- the body has one spelling to refer to them by (09) */
static void
bindpat(Ast *p, Val v, int mut)
{
  switch (p->k) {
  case Npwild:
    return;
  case Nppath: {
    Ast    **segs = p->v.ppath.path->v.path.segs;
    Variant *var = patvariant(p, v.t);
    usize    i;

    if (!var) { /* the binding form */
      locpush(segs[0]->v.seg.name, v, mut, p);
      return;
    }
    if (!p->v.ppath.payload)
      return;
    if (p->v.ppath.named) { /* by field name, mirroring the decl */
      Ast **fs = p->v.ppath.payload;
      usize n = vlen(fs);

      for (i = 0; i < n; i++) {
        Ast  *pf = fs[i];
        usize k;

        for (k = 0; k < var->nfields; k++)
          if (strcmp(var->fields[k].name, pf->v.init.name) == 0)
            break;
        if (k == var->nfields)
          continue; /* unreachable: the checker fitted it */
        if (pf->v.init.e)
          bindpat(pf->v.init.e, v.elems[k], mut || var->fields[k].mut);
        else /* the field name is the binding name (09) */
          locpush(pf->v.init.name, v.elems[k], mut || var->fields[k].mut, pf);
      }
      return;
    }
    { /* by position */
      Ast **ps = p->v.ppath.payload;
      usize n = vlen(ps);

      for (i = 0; i < n; i++)
        bindpat(ps[i], v.elems[i], mut);
    }
    return;
  }
  case Nptuple: {
    Ast **ps = p->v.list.ts;
    usize n = vlen(ps), i;

    for (i = 0; i < n; i++)
      bindpat(ps[i], v.elems[i], mut);
    return;
  }
  case Npstruct: {
    Ast **fs = p->v.pstruct.fields;
    usize n = vlen(fs), i;

    for (i = 0; i < n; i++) {
      Ast  *pf = fs[i];
      usize k;

      for (k = 0; k < v.t->sym->nfields; k++)
        if (strcmp(v.t->sym->fields[k].name, pf->v.init.name) == 0)
          break;
      if (k == v.t->sym->nfields)
        continue; /* unreachable: the checker fitted it */
      if (pf->v.init.e)
        bindpat(pf->v.init.e, v.elems[k], mut || v.t->sym->fields[k].mut);
      else /* the field name is the binding name (09) */
        locpush(pf->v.init.name, v.elems[k], mut || v.t->sym->fields[k].mut, pf);
    }
    return;
  }
  case Npor: { /* each alternative that fits, its own names (09) */
    Ast **ps = p->v.list.ts;
    usize n = vlen(ps), i;

    for (i = 0; i < n; i++)
      if (patfits(ps[i], v))
        bindpat(ps[i], v, mut);
    return;
  }
  default:
    return;
  }
}

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
    r.elems = 0;
    return r;
  }
  case Nbool:
    return valint(e->v.i.num ? 1 : 0, tybool());
  case Nunit:
    return valint(0, tyunit());
  case Narraylit: { /* the elements, the zero fill behind them
                     * (01-types.md); the length is the const
                     * expression it always was (08) */
    Env   e2 = env;
    Type *et = rty(e->v.arrlit.t, &e2);
    Type *vt;
    Ast **es = e->v.arrlit.es;
    usize n = vlen(es), ln, i;
    Val  *els;
    Val   r;

    if (!e->v.arrlit.len) /* []T borrows storage someone else owns:
                           * an allocation is an effect, and the
                           * evaluation has none (08-reflection.md) */
      cerrat(e, "a slice literal is a borrow: evaluation allocates nothing (08-reflection.md)");
    if (e->v.arrlit.mut) /* the writability is the type's own row,
                          * as a type-position [N]mut T takes */
      et = tymut(et);
    vt = et;
    while (vt->k == Tymut) /* the value the elements land in: the mut
                            * is the slot's permission, not the
                            * element's own type */
      vt = vt->t;
    ln = cevallong(e->v.arrlit.len, env, tyint(IN_USIZE));
    if (ln < n) /* the checker says it too; this walk says it wherever
                 * it runs alone (01-types.md) */
      cerrat(e, "[%lu] holds %lu elements, %lu given (01-types.md)", (unsigned long) ln,
             (unsigned long) ln, (unsigned long) n);
    els = arenaalloc((ln ? ln : 1) * sizeof *els);
    for (i = 0; i < n; i++)
      els[i] = valcoerce(ceval(es[i], env, vt), vt, es[i]);
    for (; i < ln; i++) { /* fewer than the length: the zero fill */
      els[i] = valint(0, vt);
      if (isfloatty(vt))
        els[i].f = 0;
    }
    r.t = tyarray(ln, et);
    r.i = 0;
    r.f = 0;
    r.elems = els;
    return r;
  }
  case Nindex: { /* an element of a known array: the index checked
                  * against the length, the read against the element's
                  * own type (01-types.md) */
    Val b = ceval(e->v.n2.a, env, 0);
    Val ix = ceval(e->v.n2.b, env, 0);

    if (b.t->k != Tyarray)
      cerrat(e->v.n2.a,
             "only an array's elements are known at compile time: %s is not one (08-reflection.md)",
             tnm(b.t));
    if (ix.t->k != Tyint || isfloatty(ix.t))
      cerrat(e->v.n2.b, "an index is an integer, this is %s (01-types.md)", tnm(ix.t));
    if ((i64) ix.i < 0 || ix.i >= b.t->n)
      cerrat(e->v.n2.b, "index %ld out of range for %s (01-types.md)", (long) (i64) ix.i, tnm(b.t));
    return b.elems[ix.i];
  }
  case Nstructlit: { /* the rows by name, the ones left out zero
                      * (01-types.md); a generic's rows arrive bound
                      * or not at all here */
    Ast **segs = e->v.slit.path->v.path.segs;
    Sym  *s;
    Type *t;

    if (vlen(segs) == 2 && !e->v.slit.path->v.path.root) { /* Enum::V{x:
                                                            * y}: a
                                                            * named
                                                            * payload,
                                                            * by name */
      Sym     *es = symfind(segs[0]->v.seg.name);
      Variant *v;

      if (!es || es->kind != Stype || es->tykind != TYenum)
        cerrat(e, "this literal is not compile-time known (08-reflection.md)");
      v = symvarfind(es, segs[1]->v.seg.name);
      if (!v)
        cerrat(e, "'%s' has no variant '%s' (01-types.md)", es->name, segs[1]->v.seg.name);
      if (!v->named)
        cerrat(e, "'%s' is positional: construct it with a call (01-types.md)", v->name);
      if (es->ngparams)
        cerrat(e, "a generic enum's construction arrives with a later milestone (04-generics.md)");
      { /* the rows by name, in the payload's own order */
        Ast **inits = e->v.slit.inits;
        usize n = vlen(inits), nf = v->nfields, i, k;
        Ast **args;

        if (n != nf)
          cerrat(e, "'%s' carries %lu payloads, %lu given (01-types.md)", v->name,
                 (unsigned long) nf, (unsigned long) n);
        args = arenaalloc((nf ? nf : 1) * sizeof *args);
        for (k = 0; k < nf; k++) {
          for (i = 0; i < n; i++)
            if (strcmp(inits[i]->v.init.name, v->fields[k].name) == 0)
              break;
          if (i == n)
            cerrat(e, "'%s' has no field '%s' (01-types.md)", v->name, v->fields[k].name);
          args[k] = inits[i]->v.init.e;
        }
        return variantval(es, v, 0, 0, args, nf, env, e);
      }
    }
    if (vlen(segs) != 1 || e->v.slit.path->v.path.root)
      cerrat(e, "this literal is not compile-time known (08-reflection.md)");
    s = symfind(segs[0]->v.seg.name);
    if (!s || s->kind != Stype || (s->tykind != TYstruct && s->tykind != TYunion))
      cerrat(e, "'%s' is not a struct or union here (01-types.md)", segs[0]->v.seg.name);
    if (s->ngparams)
      cerrat(e, "a generic struct's literal arrives with a later milestone (04-generics.md)");
    t = tysym(s, 0, 0);
    return structlitval(t, e->v.slit.inits, env, e);
  }
  case Nbarestructlit: { /* the want names the struct (01-types.md) */
    Type *t = want;

    while (t && t->k == Tymut)
      t = t->t;
    if (!t || (t->k != Tystruct && t->k != Tyunion))
      cerrat(e, "a bare literal needs the struct from its context (01-types.md)");
    if (t->sym->ngparams)
      cerrat(e, "a generic struct's literal arrives with a later milestone (04-generics.md)");
    return structlitval(t, e->v.list.ts, env, e);
  }
  case Ntuple: { /* the rows, by position (01-types.md) */
    Ast **ts = e->v.list.ts;
    usize n = vlen(ts), i;
    Val  *els;
    Val   r;

    els = arenaalloc((n ? n : 1) * sizeof *els);
    for (i = 0; i < n; i++)
      els[i] = ceval(ts[i], env, 0);
    { /* the rows' own types, the derivation's answer; the want's
       * coerce happens after */
      Type **ats = n ? tyargs(n) : 0;
      Val    w;

      for (i = 0; i < n; i++)
        ats[i] = els[i].t;
      r.t = tytuple(ats, n);
      r.i = 0;
      r.f = 0;
      r.elems = els;
      if (want && want->k == Tytuple &&
          want->nargs == n) { /* the
                               * want names the rows: each lands in its own, the
                               * elements' coerce the declaration's rule */
        for (i = 0; i < n; i++)
          els[i] = valcoerce(els[i], want->args[i], ts[i]);
        w = r;
        w.t = want;
        return w;
      }
    }
    return r;
  }
  case Ntupidx: { /* a row by position: the tuple's own count bounds
                   * it, the checker's rule the walk keeps (01) */
    Val b = ceval(e->v.tup.e, env, 0);

    if (b.t->k != Tytuple)
      cerrat(e->v.tup.e, "only a tuple's rows are read by number: %s is not one (01-types.md)",
             tnm(b.t));
    if (e->v.tup.idx >= b.t->nargs)
      cerrat(e, "row %lu out of range for %s (01-types.md)", (unsigned long) e->v.tup.idx,
             tnm(b.t));
    return b.elems[e->v.tup.idx];
  }
  case Naccess: { /* a field by name: the struct's rows in their
                   * declaration order, the union's one, the slice's
                   * two a borrow the evaluation does not take (01) */
    Val   b = ceval(e->v.fld.e, env, 0);
    char *nm = e->v.fld.name;
    usize i;

    if (b.t->k == Tyslice)
      cerrat(e,
             "'%s' of a slice is a borrow of its storage: evaluation takes none (08-reflection.md)",
             nm);
    if (b.t->k != Tystruct && b.t->k != Tyunion)
      cerrat(e, "%s has no fields (01-types.md)", tnm(b.t));
    if (b.t->nargs != (usize) b.t->sym->ngparams) /* a generic's rows
                                                   * arrive bound; the
                                                   * eval reads them
                                                   * bound or not at
                                                   * all */
      cerrat(e, "a generic struct's fields arrive with a later milestone (04-generics.md)");
    for (i = 0; i < b.t->sym->nfields; i++)
      if (strcmp(b.t->sym->fields[i].name, nm) == 0)
        break;
    if (i == b.t->sym->nfields)
      cerrat(e, "'%s' has no field '%s' (01-types.md)", b.t->sym->name, nm);
    if (b.t->k == Tyunion) { /* the one row the write made active;
                              * the zeroed whole reads zero every
                              * row, the write of one makes the
                              * others unspecified, and the promise
                              * is determinism (08) */
      if (b.i == ~(u64) 0)
        return valint(0, b.t->sym->fields[i].ty);
      if (b.i != i)
        cerrat(e,
               "'%s' was not the field this union's value set: that read is unspecified, and the "
               "promise is determinism (01-types.md, 08-reflection.md)",
               nm);
      return b.elems[0];
    }
    return b.elems[i];
  }
  case Npath: { /* a local's read, or a const reference: the chain (08) */
    char *nm = e->v.path.segs[0]->v.seg.name;
    Sym  *s;
    Type *p;

    if (vlen(e->v.path.segs) == 2 && !e->v.path.root) { /* Enum::V:
                                                         * the payloadless
                                                         * constructor */
      Sym     *es = symfind(e->v.path.segs[0]->v.seg.name);
      Variant *v;

      if (!es || es->kind != Stype || es->tykind != TYenum)
        cerrat(e, "this path is not compile-time known (08-reflection.md)");
      v = symvarfind(es, e->v.path.segs[1]->v.seg.name);
      if (!v)
        cerrat(e, "'%s' has no variant '%s' (01-types.md)", es->name,
               e->v.path.segs[1]->v.seg.name);
      if (v->named || v->payload)
        cerrat(e, "'%s' carries a payload; construct it (01-types.md)", v->name);
      return variantval(es, v, 0, 0, 0, 0, env, e);
    }
    if (vlen(e->v.path.segs) != 1 || e->v.path.root)
      cerrat(e, "this path is not compile-time known (08-reflection.md)");
    if (nlocs > locbase) { /* a frame is running: its bindings are
                            * the innermost names, shadowing the
                            * consts below them */
      Loc *l = locfind(nm);

      if (l)
        return l->v;
    }
    p = envfind(&env, nm);
    if (p) /* a generic's const parameter: it has a value only at a
            * call, and the evaluator runs before any */
      cerrat(e, "'%s' is a const parameter: it has no value until the call (08-reflection.md)", nm);
    s = symfind(nm);
    if (!s) { /* Some, None, Ok: a bare constructor, the want naming
               * the enum (01-types.md) */
      Sym     *owner = symvariantowner(nm);
      Variant *v;

      if (owner && want && want->k == Tyenum && want->sym == owner) {
        v = symvarfind(owner, nm);
        if (v->named || v->payload)
          cerrat(e, "'%s' carries a payload; construct it (01-types.md)", nm);
        return variantval(owner, v, want->args, want->nargs, 0, 0, env, e);
      }
    }
    if (!s || s->kind != Sconst)
      cerrat(e, "'%s' is not a const here (08-reflection.md)", nm);
    return symval(s, e);
  }
  case Nif: { /* the condition is known, so the branch is (08) */
    Val c = ceval(e->v.ifx.cond, env, tybool());

    if (c.t->k != Tybool)
      cerrat(e->v.ifx.cond, "the condition is not a bool (10-iteration.md)");
    if (c.i)
      return execblk(e->v.ifx.then, env, want);
    if (e->v.ifx.els)
      return e->v.ifx.els->k == Nif ? ceval(e->v.ifx.els, env, want)
                                    : execblk(e->v.ifx.els, env, want);
    return valint(0, tyunit()); /* no else: the statement form; a
                                 * value wanted of it is the mismatch
                                 * it is */
  }
  case Nblock:
    return execblk(e, env, want);
  case Nmatch: { /* the arms take their turn: a pattern destructures,
                  * so the one that can miss names another variant
                  * (09-match.md). Exhaustiveness was checked -- the
                  * report past the last arm is the net the emitter's
                  * abort is */
    Val   v = ceval(e->v.call.f, env, 0);
    Ast **arms = e->v.call.args;
    usize n = vlen(arms), i;

    for (i = 0; i < n; i++) {
      Ast  *arm = arms[i];
      usize save = nlocs;
      Val   r;

      if (!patfits(arm->v.n2.a, v))
        continue; /* the next arm's turn */
      bindpat(arm->v.n2.a, v, 0);
      r = arm->v.n2.b->k == Nblock ? execblk(arm->v.n2.b, env, want)
                                   : ceval(arm->v.n2.b, env, want);
      nlocs = save; /* the arm's bindings end with the arm */
      return r;
    }
    cerrat(e, "the match fell past its arms: the checker says it cannot (09-match.md)");
    return valint(0, tyint(IN_I32)); /* unreachable */
  }
  case Ncall: { /* a constructor first -- Enum::V(...), Some(v) --
                 * then a fn, its arguments known (08-reflection.md) */
    Ast **segs;
    Ast  *f = e->v.call.f;

    if (f->k == Npath && !f->v.path.root && !f->v.path.segs[0]->v.seg.args) {
      segs = f->v.path.segs;
      if (vlen(segs) == 1) { /* Some(3), Ok(v): the prelude's bare
                              * constructors, the want naming the
                              * enum (01-types.md) */
        char *nm = segs[0]->v.seg.name;
        Sym  *owner = symfind(nm) ? 0 : symvariantowner(nm);

        if (owner && want && want->k == Tyenum && want->sym == owner)
          return variantval(owner, symvarfind(owner, nm), want->args, want->nargs, e->v.call.args,
                            vlen(e->v.call.args), env, e);
      } else if (vlen(segs) == 2) { /* Enum::V(...): the enum's own */
        Sym *es = symfind(segs[0]->v.seg.name);

        if (es && es->kind == Stype && es->tykind == TYenum) {
          Variant *v = symvarfind(es, segs[1]->v.seg.name);

          if (!v)
            cerrat(f, "'%s' has no variant '%s' (01-types.md)", es->name, segs[1]->v.seg.name);
          if (es->ngparams) /* the instance a call would spell rides
                             * in the path's own arguments; a want
                             * may name one, and both arrive later */
            cerrat(f,
                   "a generic enum's construction arrives with a later milestone (04-generics.md)");
          return variantval(es, v, 0, 0, e->v.call.args, vlen(e->v.call.args), env, e);
        }
      }
    }
    return callval(e, env);
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
      a = ceval(l, env, want); /* the want reaches the left too: a
                                * wide literal's default would else be
                                * i32, narrower than the answer (08) */
      b = ceval(r, env, a.t);
    }
    if (!tysame(a.t, b.t))
      cerrat(e, "the sides differ: %s and %s (07-operators.md)", tnm(a.t), tnm(b.t));
    return valbin(a, b, op, e);
  }
  case Nbuiltin: {
    char *nm = e->v.blt.name;

    if (strcmp(nm, "compileError") == 0) { /* how compile-time code
                                            * reports (08-reflection.md):
                                            * reached is raised, an
                                            * unreached branch never
                                            * runs to raise */
      if (vlen(e->v.blt.args) != 1 || e->v.blt.args[0]->k != Nstr)
        cerrat(e, "@compileError takes one string");
      cerrat(e, "%.*s", (int) e->v.blt.args[0]->v.s.len, e->v.blt.args[0]->v.s.s);
    }
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
      if (v.t->k == Tyenum) { /* the discriminant out, the spec's own
                               * read of a variant's number
                               * (01-types.md: @cast<u32>(X::A)) */
        if (isfloatty(t) || t->k == Tybool)
          cerrat(e, "an enum casts to an integer, the tag's own kind (01-types.md)");
        domcheck(v.i, t, e);
        return valint(v.i, t);
      }
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
    v.elems = s->celems; /* the aggregate half rides the same memo */
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
  s->celems = v.elems;
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

/* -- the calls (08-reflection.md) ----------------------------------------
 *
 * A fn runs where its arguments are compile-time known, no
 * annotation asked of it. The statement set is the language's own
 * now -- a let of any pattern, an assignment to a mut one, an if, a
 * match, a for, a return -- and what it does not cover says so,
 * with the milestone it arrives with: a place that is not a
 * binding, a borrow, a method, an extern. */

/* does the value land in the type? -- valcoerce's question, asked
 * without the answer it would give */
static int
valfits(Val v, Type *t)
{
  if (tysame(v.t, t))
    return 1;
  if (v.t->k == Tyint && t->k == Tyint && !isfloatty(v.t) && !isfloatty(t))
    return inrange(v.i, t);
  return isfloatty(v.t) && isfloatty(t);
}

static void execstmt(Ast *st, Env env);

/* a block: its statements, then its tail, the block's own value.
 * A return unwinds past the rest, the frame's value in hand; a break
 * or a continue unwinds to the for they belong to, one level deep,
 * and the tail waits for it (10-iteration.md) */
static Val
execblk(Ast *b, Env env, Type *want)
{
  Ast **sts = b->v.blk.stmts;
  usize n = vlen(sts), i;

  for (i = 0; i < n; i++) {
    execstmt(sts[i], env);
    if (returning)
      return retv;
    if (breaking || continuing)
      return valint(0, tyunit());
  }
  return b->v.blk.tail ? ceval(b->v.blk.tail, env, want) : valint(0, tyunit());
}

/* one round of a loop: the pattern's names first, the body's own
 * lets above them, all popped at the round's end -- a binding is
 * fresh every round (10-iteration.md). Returns 1 when the loop ends:
 * a return unwinds past it, a break ends its run; a continue only
 * its round. */
static int
execround(Ast *body, Ast *pat, Val pv, Env env)
{
  usize save = nlocs;

  if (pat)
    bindpat(pat, pv, 0);
  execblk(body, env, 0);
  nlocs = save;
  if (returning)
    return 1;
  if (breaking) {
    breaking = 0;
    return 1;
  }
  continuing = 0;
  return 0;
}

/* the for, unwound: the three heads the grammar spells, the round
 * the budget bounds -- a step a round, for a round's body may be
 * empty and evaluate nothing (08: an end) */
static void
execfor(Ast *st, Env env)
{
  Ast *body = st->v.forx.body;

  switch (st->v.forx.shape) {
  case FCOND: { /* while the condition holds (10-iteration.md) */
    for (;;) {
      Val c;

      tick(st);
      c = ceval(st->v.forx.a, env, tybool());
      if (c.t->k != Tybool)
        cerrat(st->v.forx.a, "the condition is not a bool (10-iteration.md)");
      if (!c.i)
        return;
      if (execround(body, 0, valint(0, tyunit()), env))
        return;
    }
  }
  case FLET: { /* while the pattern fits the re-read value (10) */
    for (;;) {
      Val v;

      tick(st);
      v = ceval(st->v.forx.b, env, 0);
      if (!patfits(st->v.forx.a, v))
        return; /* a value the pattern does not fit ends the loop */
      if (execround(body, st->v.forx.a, v, env))
        return;
    }
  }
  case FIN: { /* what the iterable yields, one binding a round (10) */
    Val   src = ceval(st->v.forx.b, env, 0);
    Type *et = src.t;

    while (et && et->k == Tymut)
      et = et->t;
    if (et->k == Tyenum && et->sym == sym_option) {
      /* ?T yields its one payload or nothing: one round at most, so
       * the loop is an if -- a continue ends it like a break would
       * (10-iteration.md) */
      Variant *some = symvarfind(et->sym, "Some");

      if (some && src.i == some->disc) {
        tick(st);
        if (execround(body, st->v.forx.a, src.elems[0], env))
          return;
        breaking = continuing = 0; /* either jump ends the one round */
      }
      return;
    }
    if (et->k == Tyslice) /* a slice lends each element out: the
                           * binding would be a pointer, and
                           * evaluation takes none (10) */
      cerrat(st->v.forx.b,
             "iterating a slice lends each element out: the binding is a pointer, and evaluation "
             "takes none (10-iteration.md)");
    if (et->k != Tyarray)
      cerrat(st->v.forx.b,
             "iterating %s is not compile-time known: its iterators arrive with a "
             "later milestone (10-iteration.md)",
             tnm(et));
    { /* an owned array: each element itself, the array consumed */
      usize n = (usize) et->n, i;

      for (i = 0; i < n; i++) {
        tick(st);
        if (!patfits(st->v.forx.a, src.elems[i]))
          return; /* a pattern the element does not fit ends the loop, as a for let's would (10) */
        if (execround(body, st->v.forx.a, src.elems[i], env))
          return;
      }
    }
    return;
  }
  default:
    cerrat(st, "this for shape is not one of the three (10-iteration.md)");
  }
}

static void
execstmt(Ast *st, Env env)
{
  switch (st->k) {
  case Nlet: { /* a pattern, not only a name: the tuple by position,
                * the struct by field, the variant's payload under it
                * (09-match.md) */
    Ast  *pat = st->v.let.pat;
    Type *want = 0;
    Val   v;

    if (st->v.let.t) {
      Env e2 = env;
      want = rty(st->v.let.t, &e2);
    }
    v = ceval(st->v.let.e, env, want);
    if (want)
      v = valcoerce(v, want, st->v.let.e);
    if (!patfits(pat, v)) /* the binding is irrefutable, the value
                           * says otherwise: the abort it would be
                           * where it runs, reported here (09) */
      cerrat(st, "the pattern does not fit this value: a let is irrefutable, and where it runs "
                 "this would abort (09-match.md)");
    bindpat(pat, v, st->v.let.mut);
    return;
  }
  case Nexprstmt:
    ceval(st->v.n1.e, env, 0);
    return;
  case Nfor:
    execfor(st, env);
    return;
  case Nbreak:
    breaking = 1;
    return;
  case Ncontinue:
    continuing = 1;
    return;
  case Nassign: {
    Ast *l = st->v.bin.l;
    Tok  op = st->v.bin.op;
    Loc *loc;
    Tok  base;

    if (l->k != Npath || vlen(l->v.path.segs) != 1 || l->v.path.root)
      cerrat(l, "this place is not a binding: a field or an element arrives with a later milestone "
                "(08-reflection.md)");
    loc = locfind(l->v.path.segs[0]->v.seg.name);
    if (!loc)
      cerrat(l, "'%s' is not a binding of this frame (08-reflection.md)",
             l->v.path.segs[0]->v.seg.name);
    if (!loc->mut)
      cerrat(l, "'%s' is not mut (01-types.md)", loc->name);
    if (assignop(op, &base)) { /* a = a op b: the operator's rules */
      Val a = loc->v;          /* read first: the slot keeps its type */
      Val b = ceval(st->v.bin.r, env, a.t);

      if (!tysame(a.t, b.t))
        cerrat(st, "the sides differ: %s and %s (07-operators.md)", tnm(a.t), tnm(b.t));
      loc->v = valbin(a, b, base, st);
      return;
    }
    if (op != Teq)
      cerrat(st, "this assignment arrives with a later milestone (08-reflection.md)");
    {
      Val nv = ceval(st->v.bin.r, env, loc->v.t);

      loc->v = valcoerce(nv, loc->v.t, st->v.bin.r);
    }
    return;
  }
  case Nreturn: {
    retv = st->v.n1.e ? ceval(st->v.n1.e, env, fnret) : valint(0, tyunit());
    if (fnret)
      retv = valcoerce(retv, fnret, st);
    returning = 1;
    return;
  }
  default:
    cerrat(st, "this statement is not compile-time known (08-reflection.md)");
  }
}

/* a call at compile time: the chain's member whose parameters take
 * these values, its frame, its body -- the value its tail or its
 * return leaves, against the signature it declared */
static Val
callval(Ast *e, Env env)
{
  Ast        *f = e->v.call.f;
  Ast       **args = e->v.call.args;
  usize       na = vlen(args), i;
  Val        *avs;
  Sym        *c, *pick = 0;
  int         arity = 0, usable = 0, fits = 0;
  const char *why = 0;
  Type       *sig;
  usize       savenlocs, savelocbase, saveret;
  int         savebrk, savecont;
  Val         saveretv, r;
  Type       *savefnret;

  /* the callee: a plain fn by name. A method, a trait member, a
   * spelling with generic arguments -- anything the impl table or a
   * substitution answers -- arrives with the passes that know them */
  if (f->k != Npath || vlen(f->v.path.segs) != 1 || f->v.path.root || f->v.path.segs[0]->v.seg.args)
    cerrat(f, "this call is not compile-time known here (08-reflection.md)");
  c = symfind(f->v.path.segs[0]->v.seg.name);
  if (!c || c->kind != Sfn)
    cerrat(f, "'%s' is not a fn here (08-reflection.md)", f->v.path.segs[0]->v.seg.name);

  { /* the arguments' wants, when the chain holds exactly one plain
     * fn that takes this many: its parameters are the types the
     * checker gave them, and a bare constructor -- Some(3) in an
     * argument's place -- needs its enum named by one (01-types.md).
     * Two candidates or none: no want to give, as before. */
    Sym  *c2, *one = 0;
    Type *wsig = 0;
    int   nplain = 0;

    for (c2 = c; c2; c2 = c2->next) {
      if (vlen(c2->decl->v.fn.params) != na || c2->ngparams || c2->impl)
        continue;
      if (attrfind(c2->decl->attrs, "extern") || !c2->decl->v.fn.body)
        continue;
      one = c2;
      nplain++;
    }
    if (nplain == 1)
      wsig = fnsigof(one);
    avs = na ? arenaalloc(na * sizeof *avs) : 0;
    for (i = 0; i < na; i++)
      avs[i] = ceval(args[i], env, wsig ? wsig->args[i] : 0);
  }

  for (; c; c = c->next) { /* the overload chain: the one whose
                            * parameters take these values
                            * (04-generics.md) */
    Type *cs;

    if (vlen(c->decl->v.fn.params) != na)
      continue;
    arity = 1;
    if (c->ngparams) { /* a stencil: the call's own words bind it,
                        * and those arrive with M5c */
      if (!why)
        why = "is generic: the call's own words bind it, and those arrive with a later milestone "
              "(08-reflection.md)";
      continue;
    }
    if (c->impl) { /* a method: its impl's table decides */
      if (!why)
        why = "is a method: the impl's table decides, and that arrives with a later milestone "
              "(08-reflection.md)";
      continue;
    }
    if (attrfind(c->decl->attrs, "extern")) { /* the linker is not
                                               * part of evaluation
                                               * (08-reflection.md) */
      if (!why)
        why = "is #[extern(C)]: the linker is not part of evaluation (08-reflection.md)";
      continue;
    }
    if (!c->decl->v.fn.body) {
      if (!why)
        why = "has no body to run";
      continue;
    }
    cs = fnsigof(c);
    usable = 1;
    for (i = 0; i < na; i++)
      if (!valfits(avs[i], cs->args[i])) {
        usable = 0;
        break;
      }
    if (!usable) {
      fits = 0;
      continue;
    }
    if (pick)
      cerrat(e, "the call is ambiguous at compile time (04-generics.md)");
    pick = c;
  }
  if (!pick) {
    if (!arity)
      cerrat(e, "no '%s' takes %lu arguments (04-generics.md)", f->v.path.segs[0]->v.seg.name,
             (unsigned long) na);
    if (why && !fits) /* the chain held this one, and it was not
                       * callable -- say why, not "these arguments" */
      cerrat(e, "'%s' %s", f->v.path.segs[0]->v.seg.name, why);
    cerrat(e, "no '%s' takes these arguments at compile time (08-reflection.md)",
           f->v.path.segs[0]->v.seg.name);
  }

  sig = fnsigof(pick);
  if (++calldepth > CALLDEPTH)
    cerrat(e, "the calls nest too deep: the budget is an end (08-reflection.md)");

  /* the frame: its floor above the caller's bindings, so no inner
   * push ever writes a live outer slot */
  savenlocs = nlocs;
  savelocbase = locbase;
  saveret = returning;
  savebrk = breaking;
  savecont = continuing;
  saveretv = retv;
  savefnret = fnret;
  locbase = nlocs;
  returning = breaking = continuing = 0;
  fnret = sig->t;
  for (i = 0; i < na; i++) { /* the parameters, bound */
    Ast *p = pick->decl->v.fn.params[i];
    Loc  l;

    l.name = p->v.param.name;
    l.v = valcoerce(avs[i], sig->args[i], args[i]);
    l.mut = p->v.param.mut;
    if (nlocs >= MAXLOCALS)
      cerrat(e, "too many bindings in flight (08-reflection.md)");
    locs[nlocs++] = l;
  }
  r = execblk(pick->decl->v.fn.body, envnone(), sig->t);
  if (breaking || continuing) /* the net: a jump never crosses a
                               * frame's end -- its for is inside the
                               * body, the checker says so (10) */
    cerrat(e, "a break or a continue left its fn: the checker says it cannot (10-iteration.md)");
  if (returning)
    r = retv;
  r = valcoerce(r, sig->t, e); /* the tail's answer, against the fn's
                                * own word -- a unit body under an
                                * i32 return is the mismatch it is */
  nlocs = savenlocs;
  locbase = savelocbase;
  returning = saveret;
  breaking = savebrk;
  continuing = savecont;
  retv = saveretv;
  fnret = savefnret;
  calldepth--;
  pick->evaled = 1; /* the body ran to its end: what it did not
                     * reach is a branch of it, not a misuse of
                     * @compileError the body check reports (08) */
  return r;
}
