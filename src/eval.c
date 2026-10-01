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

#include "body.h"  /* the frame a name argument reads a round's value
                   * against (bltname) */
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

/* a binding a frame made: a parameter, a let. The frames stack
 * upward in one array -- locbase marks the current frame's floor,
 * so an inner frame never writes a slot a live outer one owns */
typedef struct
{
  char *name;
  Val   v;
  int   mut;
} Loc;

/* the caller's frame, held whole around a call: its bindings' top
 * and floor, its loop marks, its return state -- saved as one and
 * put back as one, so a new context cannot forget its pair */
typedef struct
{
  usize nlocs, locbase;
  int   returning, breaking, continuing;
  Val   retv;
  Type *fnret;
} FrSaved;

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

/* the lets a const for's unroll bound, round by round: the body's
 * own pass walks them in order, so an inner const for -- iterating
 * the outer's variable -- reads the value of the round it stands
 * in, the binding's own one, held as it was bound */
static struct
{
  char *name;
  Val   v; /* the round's value itself, not the literal that spelled
            * it: the inner loop's iterable reads it without
            * re-deriving the type (10-iteration.md) */
} cforlets[256];
static usize ncforlets;

/* a walk met a generic's own parameter where it asked for an answer:
 * the black box the declaration's walk holds T under. The flag rises
 * in the evaluator, and the unroll that caused it reads it back and
 * stops -- its rounds are the instance's own, and the re-check under
 * the binding walks them (04-generics.md) */
int evalblackbox;

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

Val
valint(u64 v, Type *t)
{
  Val r;

  r.t = t;
  r.i = v;
  r.f = 0;
  r.tag = 0;
  r.tyval = 0;
  r.elems = 0;
  r.len = 0;
  return r;
}

/* a type's reference, as a value: the lift's own answer, and
 * @typeof's (08-reflection.md) */
static Val
valtype(Type *t)
{
  Val r;

  r.t = tytype();
  r.i = 0;
  r.f = 0;
  r.tag = 0;
  r.tyval = t;
  r.elems = 0;
  r.len = 0;
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
evlocfind(char *name)
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
  if (a.elems || b.elems || isaggty(a.t) || isaggty(b.t)) /* an
                                                           * aggregate's words carry nothing to
                                                           * compare -- the elements are the value,
                                                           * and the operators over aggregates are
                                                           * the trait table's, a later milestone
                                                           * (07) */
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

Val        ceval(Ast *e, Env env, Type *want);
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
  r.i = 0;
  r.f = 0;
  r.tag = v->disc; /* the discriminant: @cast reads it out (01) */
  r.tyval = 0;
  r.len = 0;
  r.elems = np ? arenaalloc(np * sizeof *r.elems) : 0;
  for (i = 0; i < np; i++) {
    r.elems[i] = valcoerce(ceval(args[i], env, ps[i]), ps[i], args[i]);
  }
  return r;
}

/* -- @typeinfo's model, built (08-reflection.md) -------------------------
 *
 * The answer is std::meta's own data, held as the walk's values: a
 * TypeInfo variant by hand, its payload rows built beside it -- the
 * same shapes a literal's evaluation holds, one representation for
 * every consumer to read. */

static Sym *sym_field, *sym_enumfield, *sym_fnarg, *sym_attr, *sym_attrarg;

/* the model's names, found once: the prelude resolves before the
 * user's file binds anything, so the syms stand when the first
 * answer asks */
static void
metainit(void)
{
  if (sym_field)
    return;
  sym_field = symfind("Field");
  sym_enumfield = symfind("EnumField");
  sym_fnarg = symfind("FnArg");
  sym_attr = symfind("Attr");
  sym_attrarg = symfind("AttrArg");
}

/* a name as the model spells it: []u8, one byte a value -- the
 * serialization walk reads it the way it reads any slice */
static Val
strslice(char *s)
{
  Val   r;
  usize n = strlen(s), i;

  r.t = tyslice(tyint(IN_U8));
  r.i = 0;
  r.f = 0;
  r.tag = 0;
  r.tyval = 0;
  r.elems = n ? arenaalloc(n * sizeof *r.elems) : 0;
  r.len = n;
  for (i = 0; i < n; i++)
    r.elems[i] = valint((u64) (unsigned char) s[i], tyint(IN_U8));
  return r;
}

/* a name the model spells ([]u8, one byte a value) as the
 * compiler's own C string: the field a walk looks up is named in
 * the value's bytes (08-reflection.md) */
char *
cstrval(Val v, Ast *at)
{
  char *s;
  usize i;

  if (v.t->k != Tyslice || !v.t->t || v.t->t->k != Tyint || v.t->t->num != IN_U8)
    cerrat(at, "the name is not a string (08-reflection.md)");
  s = arenaalloc(v.len + 1);
  for (i = 0; i < v.len; i++)
    s[i] = (char) v.elems[i].i;
  s[v.len] = 0;
  return s;
}

/* the name a builtin's argument holds: a literal's own bytes, the
 * value the evaluator resolves -- a const, the model's own rows --
 * or a const for's round read against the body frame that walks it
 * (the value rides the binding the unroll spelled; the evaluator's
 * own listing ended with the rounds). A literal builds no value --
 * evaluation allocates nothing (08-reflection.md) -- the bytes ride
 * the lexer's token, and the compiler consumes them here */
char *
bltname(Ast *a, struct Fenv *fe)
{
  if (a->k == Nstr) {
    char *s = arenaalloc(a->v.s.len + 1);

    memcpy(s, a->v.s.s, a->v.s.len);
    s[a->v.s.len] = 0;
    return s;
  }
  if (fe) { /* a const for's round, bound by the unroll the body's
             * walk is inside: the value rides the local, and the
             * bytes are read from it, not from the world the
             * evaluator sees -- its listing ended with the rounds
             * (08-reflection.md, 10-iteration.md) */
    Ast  *base = a->k == Naccess ? a->v.fld.e : a;
    char *fld = a->k == Naccess ? a->v.fld.name : 0;

    if (base->k == Npath && vlen(base->v.path.segs) == 1 && !base->v.path.root) {
      char  *nm = base->v.path.segs[0]->v.seg.name;
      usize  ci;
      Local *l = 0;

      for (ci = fe->n; ci > 0; ci--) /* the innermost binding, as the
                                      * frame's own finder walks it */
        if (strcmp(fe->ls[ci - 1].name, nm) == 0) {
          l = &fe->ls[ci - 1];
          break;
        }
      if (l && l->cv) {
        Val  *v = l->cv;
        usize i;

        if (fld) { /* f.name: the row's own field */
          if (v->t->k != Tystruct)
            cerrat(a, "'%s' is not a row a name reads from (08-reflection.md)", nm);
          for (i = 0; i < (usize) v->t->sym->nfields; i++)
            if (strcmp(v->t->sym->fields[i].name, fld) == 0)
              break;
          if (i == (usize) v->t->sym->nfields)
            cerrat(a, "'%s' has no field '%s' (08-reflection.md)", v->t->sym->name, fld);
          v = &v->elems[i];
        }
        return cstrval(*v, a); /* n itself: the slice the round bound */
      }
      if (l && l->isconst && !l->cv) /* a const parameter this walk
                                      * holds no value for: the name
                                      * is the instance's own, and the
                                      * re-check under the binding
                                      * reads it there -- the caller
                                      * defers what it asked
                                      * (08-reflection.md) */
        return 0;
    }
  }
  return cstrval(ceval(a, envnone(), 0), a);
}

/* does this fn's parameter list mark one const? The answer rides the
 * declaration's own words, every ask reading them again -- a flag on
 * the Sym would be one more thing to keep true (08-reflection.md) */
int
fnconstparams(Sym *s)
{
  Ast **ps = s->decl->v.fn.params;
  usize i, n = vlen(ps);

  for (i = 0; i < n; i++)
    if (ps[i]->v.param.cnst)
      return 1;
  return 0;
}

/* a slice of built rows: @typeinfo is the only writer of slice
 * values -- the user's own literals make arrays, and []T borrows at
 * runtime -- so what lands here is owned data, the count on it */
static Val
sliceval(Val *els, usize n, Type *elty)
{
  Val r;

  r.t = tyslice(elty);
  r.i = 0;
  r.f = 0;
  r.tag = 0;
  r.tyval = 0;
  r.elems = els;
  r.len = n;
  return r;
}

/* a model struct's value, its rows already built in the
 * declaration's own order */
static Val
mkval(Sym *s, Val *els)
{
  Val r;

  r.t = tysym(s, 0, 0);
  r.i = 0;
  r.f = 0;
  r.tag = 0;
  r.tyval = 0;
  r.elems = els;
  r.len = 0;
  return r;
}

/* a TypeInfo variant's value: the discriminant, the payload rows
 * already built in the order the variant declared them */
static Val
mkti(char *nm, Val *els, usize n, Ast *at)
{
  Type    *et = typeinfoty();
  Variant *v = symvarfind(et->sym, nm);

  if (!v) /* the embedded source changed under the compiler */
    cerrat(at, "std::meta's TypeInfo lost its '%s' variant (08-reflection.md)", nm);
  if (n != v->nfields)
    cerrat(at, "internal: TypeInfo::%s carries %lu rows, %lu built", nm, (unsigned long) v->nfields,
           (unsigned long) n);
  { /* the variantval shape, minus the call: the rows are values
     * already, built by this file's own hands */
    Val r;

    r.t = et;
    r.i = 0;
    r.f = 0;
    r.tag = v->disc;
    r.tyval = 0;
    r.elems = els;
    r.len = 0;
    return r;
  }
}

/* an attribute's argument, as the model spells it: a name, an
 * integer, or a string -- the grammar's three (01-types.md) */
static Val
mkattrargval(Ast *g)
{
  Val   r;
  Type *at2 = tysym(sym_attrarg, 0, 0);

  switch (g->k) {
  case Npath: { /* #[build(debug)] -- debug, an identifier */
    Variant *v = symvarfind(at2->sym, "Ident");

    r.t = at2;
    r.i = 0;
    r.f = 0;
    r.tag = v->disc;
    r.tyval = 0;
    r.elems = arenaalloc(sizeof *r.elems);
    r.elems[0] = strslice(g->v.path.segs[0]->v.seg.name);
    r.len = 0;
    return r;
  }
  case Nint: {
    Variant *v = symvarfind(at2->sym, "Int");

    r.t = at2;
    r.i = 0;
    r.f = 0;
    r.tag = v->disc;
    r.tyval = 0;
    r.elems = arenaalloc(sizeof *r.elems);
    r.elems[0] = valint(g->v.i.num, tyint(IN_I64));
    r.len = 0;
    return r;
  }
  case Nstr: {
    Variant *v = symvarfind(at2->sym, "Str");
    Val     *els = arenaalloc(g->v.s.len * sizeof *els);
    usize    i;

    for (i = 0; i < g->v.s.len; i++)
      els[i] = valint((u64) (unsigned char) g->v.s.s[i], tyint(IN_U8));
    r.t = at2;
    r.i = 0;
    r.f = 0;
    r.tag = v->disc;
    r.tyval = 0;
    r.elems = arenaalloc(sizeof *r.elems);
    r.elems[0] = sliceval(els, g->v.s.len, tyint(IN_U8));
    r.len = 0;
    return r;
  }
  default: /* the grammar parses a float there too; the model
            * carries no variant for one (08-reflection.md) */
    cerrat(g, "an attribute's argument is a name, an integer, or a string: "
              "the model carries no float (08-reflection.md)");
  }
  return valint(0, tyint(IN_I32)); /* unreachable */
}

/* the attrs a declaration was marked with, one Attr a mark */
static Val
mkattrs(Ast **attrs)
{
  usize n = vlen(attrs), i;
  Val  *els = n ? arenaalloc(n * sizeof *els) : 0;

  for (i = 0; i < n; i++) {
    Ast  *a = attrs[i];
    Val  *rows = arenaalloc(2 * sizeof *rows);
    usize na = vlen(a->v.seg.args), k;
    Val  *aes = na ? arenaalloc(na * sizeof *aes) : 0;

    rows[0] = strslice(a->v.seg.name);
    for (k = 0; k < na; k++)
      aes[k] = mkattrargval(a->v.seg.args[k]);
    rows[1] = sliceval(aes, na, tysym(sym_attrarg, 0, 0));
    els[i] = mkval(sym_attr, rows);
  }
  return sliceval(els, n, tysym(sym_attr, 0, 0));
}

/* a struct's field as a Field row: the offset from the layout the
 * struct settled on, the type bound to this instance's arguments */
static Val
mkfieldval(Type *t, usize i)
{
  Sym   *s = t->sym;
  Field *f = &s->fields[i];
  Type  *ft = s->ngparams ? gsubst(f->ty, s->gparams, t->args, t->nargs) : f->ty;
  Val   *els = arenaalloc(5 * sizeof *els);

  els[0] = strslice(f->name);                         /* name */
  els[1] = valtype(ft);                               /* type */
  els[2] = valint(fieldoffof(t, i), tyint(IN_USIZE)); /* offset */
  els[3] = valint(f->mut, tybool());                  /* mutable */
  els[4] = mkattrs(f->attrs);                         /* attrs */
  return mkval(sym_field, els);
}

/* an enum's variant as an EnumField row: value the discriminant,
 * type the one payload the model's slot holds -- Some when the
 * variant carries exactly one positional type, None otherwise: the
 * spec says the None half only, and the shapes it leaves unspoken
 * (a named payload, several positional ones) hold no single type
 * for the slot to name (08-reflection.md) */
static Val
mkenumfieldval(Type *t, usize i)
{
  Sym     *s = t->sym;
  Variant *v = &s->variants[i];
  Val     *els = arenaalloc(4 * sizeof *els);
  Variant *some = symvarfind(sym_option, "Some");
  Variant *none = symvarfind(sym_option, "None");
  Val      tv;

  tv.t = tyopt(tytype());
  tv.i = 0;
  tv.f = 0;
  tv.tyval = 0;
  tv.len = 0;
  if (!v->named && v->payload && v->npayload == 1) { /* Some(the one) */
    Type *pt = s->ngparams ? gsubst(v->payload[0], s->gparams, t->args, t->nargs) : v->payload[0];

    tv.tag = some->disc;
    tv.elems = arenaalloc(sizeof *tv.elems);
    tv.elems[0] = valtype(pt);
  } else { /* None: no payload, or none the slot can name */
    tv.tag = none->disc;
    tv.elems = 0;
  }
  els[0] = strslice(v->name);              /* name */
  els[1] = valint(v->disc, tyint(IN_I64)); /* value */
  els[2] = tv;                             /* type */
  els[3] = mkattrs(v->attrs);              /* attrs */
  return mkval(sym_enumfield, els);
}

/* a type's own description: the sugar answers first -- ?T is
 * Optional whatever T is, E?T is Result (08-reflection.md) -- then
 * the shape itself, the declaration's fields and attrs along */
Val
typeinfoval(Type *t, Ast *at)
{
  metainit();
  switch (t->k) {
  case Tybool:
    return mkti("Bool", 0, 0, at);
  case Tyint: {
    Val *els;

    if (t->num == IN_F32 || t->num == IN_F64) {
      els = arenaalloc(sizeof *els);
      els[0] = valint(intwidth(t) * 8, tyint(IN_U16));
      return mkti("Float", els, 1, at);
    }
    els = arenaalloc(2 * sizeof *els);
    els[0] = valint(intwidth(t) * 8, tyint(IN_U16));
    els[1] = valint(t->num < IN_U8 || t->num == IN_ISIZE, tybool());
    return mkti("Int", els, 2, at);
  }
  case Typtr:
  case Tyslice:
  case Tyarray: { /* the child with its mutability, the wrapper's
                   * own variant above it */
    Val  *els = arenaalloc(3 * sizeof *els);
    Type *c = t->t;
    int   mut = 0;
    char *nm = t->k == Typtr ? "Pointer" : t->k == Tyslice ? "Slice" : "Array";
    usize n = t->k == Tyarray ? 3 : 2;

    if (c->k == Tymut) { /* *mut T is the wrapper around the child */
      mut = 1;
      c = c->t;
    }
    if (t->k == Tyarray) {
      if (t->gp) /* [N]T with N a const parameter: the length is
                  * the caller's to bind, not a number here */
        cerrat(at, "an array whose length is a parameter has no length to tell: "
                   "bind the parameter first (08-reflection.md)");
      els[0] = valint(t->n, tyint(IN_USIZE));
      els[1] = valtype(c);
      els[2] = valint(mut, tybool());
    } else {
      els[0] = valtype(c);
      els[1] = valint(mut, tybool());
    }
    return mkti(nm, els, n, at);
  }
  case Tystruct:
  case Tyunion: {
    Sym  *s = t->sym;
    usize n = s->nfields, i;
    Val  *fs = n ? arenaalloc(n * sizeof *fs) : 0;
    Val  *els = arenaalloc(2 * sizeof *els);

    for (i = 0; i < n; i++)
      fs[i] = mkfieldval(t, i);
    els[0] = sliceval(fs, n, tysym(sym_field, 0, 0));
    els[1] = mkattrs(s->decl->attrs);
    return mkti(t->k == Tystruct ? "Struct" : "Union", els, 2, at);
  }
  case Tyenum: {
    if (t->sym == sym_option && t->nargs == 1) { /* ?T: the sugar
                                                  * says so, whatever
                                                  * T is (08) */
      Val *els = arenaalloc(sizeof *els);

      els[0] = valtype(t->args[0]);
      return mkti("Optional", els, 1, at);
    }
    if (t->sym == sym_result && t->nargs == 2) { /* E?T on its side */
      Val *els = arenaalloc(2 * sizeof *els);

      els[0] = valtype(t->args[0]);
      els[1] = valtype(t->args[1]);
      return mkti("Result", els, 2, at);
    }
    { /* a declared enum: the tag, the variants */
      Sym  *s = t->sym;
      usize n = s->nvariants, i;
      Val  *vs = n ? arenaalloc(n * sizeof *vs) : 0;
      Val  *els = arenaalloc(2 * sizeof *els);

      for (i = 0; i < n; i++)
        vs[i] = mkenumfieldval(t, i);
      els[0] = valtype(tagtyof(t));
      els[1] = sliceval(vs, n, tysym(sym_enumfield, 0, 0));
      return mkti("Enum", els, 2, at);
    }
  }
  case Tytuple: { /* the rows as Fields: names empty, offsets the
                   * layout's own */
    usize n = t->nargs, i;
    Val  *fs = n ? arenaalloc(n * sizeof *fs) : 0;
    Val  *els = arenaalloc(sizeof *els);

    for (i = 0; i < n; i++) {
      Val  *rows = arenaalloc(5 * sizeof *rows);
      usize off = 0, k;

      for (k = 0; k < i; k++) { /* the rows before it, laid out as
                                 * the size rule lays them (02) */
        off = alignto(off, alignof_(t->args[k]));
        off += sizeof_(t->args[k]);
      }
      off = alignto(off, alignof_(t->args[i]));
      rows[0] = strslice("");
      rows[1] = valtype(t->args[i]);
      rows[2] = valint(off, tyint(IN_USIZE));
      rows[3] = valint(0, tybool());
      rows[4] = mkattrs(0);
      fs[i] = mkval(sym_field, rows);
    }
    els[0] = sliceval(fs, n, tysym(sym_field, 0, 0));
    return mkti("Tuple", els, 1, at);
  }
  case Tyfn: { /* the arguments, the return: a pointer at the
                * language level, its reflection its own variant */
    usize n = t->nargs, i;
    Val  *as = n ? arenaalloc(n * sizeof *as) : 0;
    Val  *els = arenaalloc(2 * sizeof *els);

    for (i = 0; i < n; i++) {
      Val *rows = arenaalloc(2 * sizeof *rows);

      rows[0] = strslice(""); /* a name lives in the declaration,
                               * not the type (08) */
      rows[1] = valtype(t->args[i]);
      as[i] = mkval(sym_fnarg, rows);
    }
    els[0] = sliceval(as, n, tysym(sym_fnarg, 0, 0));
    els[1] = valtype(t->t);
    return mkti("Fn", els, 2, at);
  }
  case Tyvoidptr:
    return mkti("Voidptr", 0, 0, at);
  case Tyunit: { /* () is the tuple with no rows (08) */
    Val *els = arenaalloc(sizeof *els);

    els[0] = sliceval(0, 0, tysym(sym_field, 0, 0));
    return mkti("Tuple", els, 1, at);
  }
  default: /* Tytype, Typaram, the projections: the model describes
            * the language's types; these name checking itself */
    cerrat(at,
           "%s has no variant in the model: @typeinfo describes the language's "
           "types, not checking's own (08-reflection.md)",
           tnm(t));
  }
  return valint(0, tyint(IN_I32)); /* unreachable */
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
    r.i = 0;
    r.f = 0;
    r.tyval = 0;
    r.len = 0;
    r.tag = VNONROW; /* the zeroed whole: every row reads zero */
    r.elems = arenaalloc(sizeof *r.elems);
    if (n) {
      Ast *in = inits[n - 1];

      r.i = 0;
      r.tag = (u64) seen; /* seen's row, set below with its value */
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
  r.tag = 0;
  r.tyval = 0;
  r.elems = els;
  r.len = 0;
  return r;
}

/* a binding into the frame, above the rounds and the arms that made
 * it -- a later binding of the name shadows this one, for the find
 * walks from the top */
static void
evlocpush(char *name, Val v, int mut, Ast *at)
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
int
patfits(Ast *p, Val v)
{
  switch (p->k) {
  case Npwild:
    return 1;
  case Nppath: {
    Variant *var = patvariant(p, v.t);

    if (!var)
      return 1; /* the binding form: it fits */
    if (v.t->k != Tyenum || v.tag != var->disc)
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
      evlocpush(segs[0]->v.seg.name, v, mut, p);
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
          evlocpush(pf->v.init.name, v.elems[k], mut || var->fields[k].mut, pf);
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
        evlocpush(pf->v.init.name, v.elems[k], mut || v.t->sym->fields[k].mut, pf);
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

Val
ceval(Ast *e, Env env, Type *want)
{
  tick(e);
  switch (e->k) {
  case Nint: {
    Type *t = litty(e, want);

    domcheck(e->v.i.num, t, e); /* the first step is a step too: the
                                 * literal is asked of the type it
                                 * lands in (08-reflection.md) */
    return valint(e->v.i.num, t);
  }
  case Nflt: {
    Val r;

    r.t = litty(e, want);
    r.i = 0;
    r.f = e->v.f.flt;
    r.tag = 0;
    r.tyval = 0;
    r.elems = 0;
    r.len = 0;
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
    r.tag = 0;
    r.tyval = 0;
    r.elems = els;
    r.len = 0;
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
        usize n = vlen(inits), nf = v->nfields, i, k, j;
        Ast **args;

        for (i = 0; i < n; i++) /* a name twice says so itself, not
                                 * as a missing one after it */
          for (j = i + 1; j < n; j++)
            if (strcmp(inits[i]->v.init.name, inits[j]->v.init.name) == 0)
              cerrat(inits[j], "field '%s' given twice (01-types.md)", inits[j]->v.init.name);
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
      r.tag = 0;
      r.tyval = 0;
      r.elems = els;
      r.len = 0;
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
      if (b.tag == VNONROW)
        return valint(0, b.t->sym->fields[i].ty);
      if (b.tag != i)
        cerrat(e,
               "'%s' was not the field this union's value set: that read is unspecified, and the "
               "promise is determinism (01-types.md, 08-reflection.md)",
               nm);
      return b.elems[0];
    }
    return b.elems[i];
  }
  case Nstr: { /* a string literal, the bytes the token itself holds:
                * a []u8 the const walks carry -- the values ride the
                * lexer's own spelling, evaluation allocates nothing
                * (08-reflection.md) */
    usize i;
    Val  *els = e->v.s.len ? arenaalloc(e->v.s.len * sizeof *els) : 0;

    for (i = 0; i < e->v.s.len; i++)
      els[i] = valint((u64) (unsigned char) e->v.s.s[i], tyint(IN_U8));
    return sliceval(els, e->v.s.len, tyint(IN_U8));
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
      Loc *l = evlocfind(nm);

      if (l)
        return l->v;
    }
    { /* a const for's round, bound by the unroll the body's pass is
       * walking: the literal of the round it stands in, the inner
       * loop's iterable (10-iteration.md) */
      usize ci;

      for (ci = ncforlets; ci > 0; ci--)
        if (strcmp(cforlets[ci - 1].name, nm) == 0)
          return cforlets[ci - 1].v;
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
    Val v;

    if (e->v.un.op == Tcaret2) { /* the lift: the operand is a type
                                  * spelled in a value's slot, the
                                  * answer a reference to it
                                  * (08-reflection.md) */
      Env   e2 = env;            /* rty may bind the generic names it reads */
      Type *t = rty(e->v.un.e, &e2);

      return valtype(t);
    }
    if (e->v.un.op == Tdollar2) /* a splice names a type slot; this
                                 * is a value's (08-reflection.md) */
      cerrat(e, "a splice names a type slot (08-reflection.md)");
    if (e->v.un.op == Tminus && e->v.un.e->k == Nint) { /* the sign
                                                         * rides the literal: i32's least,
                                                         * -2147483648, is spelled with it,
                                                         * and no other way is.  Folded
                                                         * before the operand's own step:
                                                         * that step's check asks the
                                                         * magnitude alone, which the sign
                                                         * may yet fit (08-reflection.md) */
      Type *lt = litty(e->v.un.e, want);
      u64   m = 0 - e->v.un.e->v.i.num;

      domcheck(m, lt, e);
      return valint(m, lt);
    }
    v = ceval(e->v.un.e, env, want);

    switch (e->v.un.op) {
    case Tminus: /* the literal fold ran above: the operand here is a
                  * value of its own, a negation of it */
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
    if (strcmp(nm, "offset") == 0) { /* a field's own place in the
                                      * whole: the layout query, the
                                      * name spelled in the value's
                                      * bytes (02-layout.md) */
      Env   e2 = env;
      Type *t;
      char *fnm;
      usize i;

      if (vlen(e->v.blt.targs) != 1 || vlen(e->v.blt.args) != 1)
        cerrat(e, "@offset takes one type argument and the field's name (02-layout.md)");
      t = rty(e->v.blt.targs[0], &e2);
      if (t->k == Typaram) { /* a generic's own parameter, the black
                              * box: the fold is the instance's, and
                              * the unroll that reached here defers
                              * to the re-check (04-generics.md) */
        evalblackbox++;
        return valint(0, tyint(IN_USIZE));
      }
      { /* the name: a literal's own bytes, or the value the frame
         * holds -- a const parameter's, when a compile-time call
         * runs the body that names it, a const for's round riding
         * the listing (08-reflection.md) */
        Val v = ceval(e->v.blt.args[0], env, 0);

        fnm = cstrval(v, e->v.blt.args[0]);
      }
      if (t->k != Tystruct && t->k != Tyunion)
        cerrat(e, "%s has no fields to offset (02-layout.md)", tnm(t));
      for (i = 0; i < t->sym->nfields; i++)
        if (strcmp(t->sym->fields[i].name, fnm) == 0)
          break;
      if (i == t->sym->nfields)
        cerrat(e->v.blt.args[0], "'%s' has no field '%s' (02-layout.md)", t->sym->name, fnm);
      return valint(fieldoffof(t, i), tyint(IN_USIZE));
    }
    if (strcmp(nm, "field") == 0) /* an address is a runtime thing:
                                   * evaluation allocates none, and
                                   * the name beside it borrows the
                                   * same (08-reflection.md) */
      cerrat(e, "@field yields an address: a runtime place, not a compile-time value "
                "(08-reflection.md)");
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
        domcheck(v.tag, t, e);
        return valint(v.tag, t);
      }
      if (v.t->k == Tyint || v.t->k == Tybool)
        domcheck(v.i, t, e); /* the widening and the narrowing both:
                              * the target's domain is the check */
      else
        cerrat(e, "this cast is not compile-time known (08-reflection.md)");
      return valint(v.i, t);
    }
    if (strcmp(nm, "typeof") == 0) { /* the value's own type, up as a
                                      * reference: the one crossing
                                      * from a value to a type, the
                                      * operand's derivation the
                                      * answer (08-reflection.md) */
      Type *held;

      if (vlen(e->v.blt.targs) || vlen(e->v.blt.args) != 1)
        cerrat(e, "@typeof takes one value");
      held = ceval(e->v.blt.args[0], env, 0).t; /* no want: the
                                                 * operand's own
                                                 * derivation, not
                                                 * the slot's */
      return valtype(held);
    }
    if (strcmp(nm, "typeinfo") == 0) { /* the description, both slots
                                        * one case: the type named in
                                        * the argument slot -- a $$
                                        * splice resolves through the
                                        * frame's own bindings -- or
                                        * the value's static type
                                        * (08-reflection.md) */
      Env   e2 = env;                  /* rty may bind the generic names it reads */
      Type *t;

      if (vlen(e->v.blt.targs) == 1 && !vlen(e->v.blt.args))
        t = rty(e->v.blt.targs[0], &e2);
      else if (!vlen(e->v.blt.targs) && vlen(e->v.blt.args) == 1)
        t = ceval(e->v.blt.args[0], env, 0).t;
      else
        cerrat(e, "@typeinfo takes one type argument or one value (08-reflection.md)");
      if (t && t->k == Typaram) { /* a generic's own parameter, the
                                   * black box: the description is the
                                   * instance's, and the unroll that
                                   * reached here defers to the
                                   * re-check (04-generics.md) */
        evalblackbox++;
        return valint(0, typeinfoty());
      }
      return typeinfoval(t, e);
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
    v.tag = s->ctag;
    v.tyval = s->ctyval; /* a type value's half rides the same memo */
    v.elems = s->celems; /* the aggregate half rides the same memo */
    v.len = s->clen;     /* a slice's length with them: @typeinfo is
                          * the one writer of slice values, and a
                          * const can hold what a match pulled out
                          * (08-reflection.md) */
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
  s->ctag = v.tag;
  s->ctyval = v.tyval;
  s->celems = v.elems;
  s->clen = v.len;
  s->cvaldone = 1;
  ninflight--;
  return v;
}

void
cevalsym(Sym *s)
{
  symval(s, s->decl);
}

/* a $$ operand's value, as the type its slot takes: the splice's own
 * half. The operand is an ordinary expression -- a const, a lift, a
 * call -- and its derivation must be `type`, or the splice names
 * nothing (08-reflection.md) */
Type *
tysplice(Ast *e, Env *env)
{
  Val v = ceval(e, *env, 0);

  if (v.t->k != Tytype)
    cerrat(e, "the value is %s, a type is (08-reflection.md)", tnm(v.t));
  return v.tyval;
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

      if (some && src.tag == some->disc) {
        tick(st);
        if (execround(body, st->v.forx.a, src.elems[0], env))
          return;
        breaking = continuing = 0; /* either jump ends the one round */
      }
      return;
    }
    if (et->k == Tyslice) { /* @typeinfo's slices: the rows owned in
                             * the value -- a round apiece, nothing
                             * lent (08-reflection.md) */
      usize n = src.len, i;

      for (i = 0; i < n; i++) {
        tick(st);
        if (!patfits(st->v.forx.a, src.elems[i]))
          return;
        if (execround(body, st->v.forx.a, src.elems[i], env))
          return;
      }
      return;
    }
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
  case Ncfor: /* the const marker changes nothing in a fn the
               * evaluator runs: every value is compile-time known
               * here or the fn would not run (08-reflection.md) --
               * but the marker promises an iteration, and the other
               * two shapes are runtime ones the body pass rejects */
    if (st->v.forx.shape != FIN)
      cerrat(st, "a const for iterates: the condition and the let forms are runtime shapes "
                 "(10-iteration.md)");
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
    loc = evlocfind(l->v.path.segs[0]->v.seg.name);
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

/* -- the const for's unroll (10-iteration.md) ---------------------------
 *
 * The iteration runs here, in the evaluator; what it spelled goes
 * back into the tree as ordinary statements -- each round's value a
 * let that spells it as a literal, the body following as itself, the
 * same tree shared between the rounds, for the passes that follow
 * read the tree without writing it. No loop survives into the
 * generated code, which is the marker's whole promise. */

/* mk with another node's position: the materialized tree reports
 * where the loop stood, not wherever the lexer happens to sit */
Ast *
mknear(Nk k, Ast *at)
{
  Ast *n = mk(k);

  n->line = at->line;
  n->col = at->col;
  return n;
}

/* an integer literal, placed like the rest of the materialized
 * tree */
static Ast *
intnear(u64 num, Ast *at)
{
  Ast *n = mknear(Nint, at);

  n->v.i.num = num;
  return n;
}

/* one path segment, or two: a scalar's keyword, a declaration's
 * name -- and a variant's, which is the second segment of its
 * enum's */
static Ast *
pathsegs(char *a, char *b, Ast *at)
{
  Ast *p = mknear(Npath, at);
  Ast *s0 = mknear(Nseg, at), *s1 = b ? mknear(Nseg, at) : 0;

  s0->v.seg.name = a;
  if (s1)
    s1->v.seg.name = b;
  p->v.path.segs = vnew(Ast *, b ? 2 : 1);
  vappend(&p->v.path.segs, &s0);
  if (s1)
    vappend(&p->v.path.segs, &s1);
  return p;
}

/* the checker's type, back to the tree that spells it: a let's
 * annotation and an array literal's element type are tree slots, so
 * a materialized value's parts walk back through here. The sugar is
 * spelled back (?T, E?T); what the tree cannot say -- a pointer, a
 * generic -- stops the unroll where it stands */
static Ast *
tytoexpr(Type *t, Ast *at)
{
  static const struct
  {
    char *n;
    int   num;
  } ps[] = {
      /* resolve.c's prim, mirrored: the names are keywords
       * in type position, so no declaration shadows them */
      {"i8", IN_I8},       {"i16", IN_I16},     {"i32", IN_I32}, {"i64", IN_I64}, {"i128", IN_I128},
      {"u8", IN_U8},       {"u16", IN_U16},     {"u32", IN_U32}, {"u64", IN_U64}, {"u128", IN_U128},
      {"isize", IN_ISIZE}, {"usize", IN_USIZE}, {"f32", IN_F32}, {"f64", IN_F64},
  };
  Ast  *p;
  usize i;

  if (!t)
    return 0;
  switch (t->k) {
  case Tyint: /* the scalars: keywords in type position (resolve.c) */
    for (i = 0; i < sizeof ps / sizeof ps[0]; i++)
      if (ps[i].num == t->num)
        return pathsegs(ps[i].n, 0, at);
    cerrat(at, "this width does not materialize (08-reflection.md)");
    return 0; /* unreachable */
  case Tybool:
    return pathsegs("bool", 0, at);
  case Tytype: /* the keyword itself: a field's or a row's own slot */
    return mknear(Nttype, at);
  case Tyarray: /* [N]T, the length from the type */
    p = mknear(Ntarray, at);
    p->v.arrlit.len = intnear(t->n, at);
    p->v.arrlit.t = tytoexpr(t->t, at);
    return p;
  case Tyslice: /* []T: the same tree, its length slot empty */
    p = mknear(Ntarray, at);
    p->v.arrlit.t = tytoexpr(t->t, at);
    return p;
  case Tytuple: /* the rows, each its own tree */
    p = mknear(Nttuple, at);
    p->v.list.ts = vnew(Ast *, t->nargs ? t->nargs : 1);
    for (i = 0; i < t->nargs; i++) {
      Ast *r = tytoexpr(t->args[i], at);

      vappend(&p->v.list.ts, &r);
    }
    return p;
  case Tystruct:
  case Tyunion:
  case Tyenum:
    if (t->sym == sym_option && t->nargs == 1) { /* ?T, spelled back */
      p = mknear(Ntopt, at);
      p->v.n1.e = tytoexpr(t->args[0], at);
      return p;
    }
    if (t->sym == sym_result && t->nargs == 2) { /* E?T likewise */
      p = mknear(Ntresult, at);
      p->v.n2.a = tytoexpr(t->args[0], at);
      p->v.n2.b = tytoexpr(t->args[1], at);
      return p;
    }
    if (t->nargs) /* a generic's rows wait for the binding a
                   * declaration's own words spell (04-generics.md) */
      cerrat(at,
             "'%s' is generic here: a generic's materialization arrives with a later milestone "
             "(04-generics.md)",
             t->sym->name);
    return pathsegs(t->sym->name, 0, at);
  default: /* pointers and the rest are runtime things (08) */
    cerrat(at, "%s does not materialize: it is not compile-time known here (08-reflection.md)",
           tnm(t));
  }
  return 0; /* unreachable */
}

/* a value the walk holds, back to the expression that spells it: a
 * literal the passes that follow read the way they read the
 * program's own. A variant's payload is spelled positionally -- the
 * body's resolver does not take the braces form yet, and the
 * payload's order is the declaration's own */
Ast *
valtoexpr(Val v, Ast *at)
{
  Ast  *n;
  usize i;

  switch (v.t->k) {
  case Tybool:
    n = mknear(Nbool, at);
    n->v.i.num = v.i != 0;
    return n;
  case Tyint:
    if (v.t->num == IN_F32 || v.t->num == IN_F64) { /* a float rides
                                                     * its own slot */
      n = mknear(Nflt, at);
      n->v.f.flt = v.f;
      return n;
    }
    n = mknear(Nint, at); /* the bits whole: the domain check at the
                           * other end reads them the way this end
                           * wrote them */
    n->v.i.num = v.i;
    return n;
  case Tyunit:
    return mknear(Nunit, at);
  case Tytype: { /* the type it holds, lifted back: ^^T spells the
                  * value the round bound (08-reflection.md) */
    Ast *n = mknear(Nun, at);

    n->v.un.op = Tcaret2;
    n->v.un.e = tytoexpr(v.tyval, at);
    return n;
  }
  case Tyenum: { /* the variant the discriminant names */
    Sym     *s = v.t->sym;
    Variant *var = 0;
    usize    np;

    for (i = 0; i < (usize) s->nvariants; i++)
      if (s->variants[i].disc == v.tag) {
        var = &s->variants[i];
        break;
      }
    if (!var) /* the declaration changed under the value: cannot be,
               * for the evaluator read it to build the value */
      cerrat(at, "'%s' holds a discriminant none of its variants own", s->name);
    np = var->named ? var->nfields : (var->payload ? var->npayload : 0);
    if (s->ngparams) { /* a generic's variant spells bare -- None,
                        * Some(x) -- the way a literal writes it: the
                        * path's own two segments cannot carry the
                        * arguments, and the want supplies them
                        * (04-generics.md) */
      if (!np)
        return pathsegs(var->name, 0, at);
      { /* Some(args...): a bare call, the want naming the enum */
        Ast *c = mknear(Ncall, at);

        c->v.call.f = pathsegs(var->name, 0, at);
        c->v.call.args = vnew(Ast *, np);
        for (i = 0; i < np; i++) {
          Ast *a = valtoexpr(v.elems[i], at);

          vappend(&c->v.call.args, &a);
        }
        return c;
      }
    }
    if (!np)
      return pathsegs(s->name, var->name, at); /* payloadless: E::B */
    { /* E::V(args...): a call the checker and the emitter read the
       * way they read the program's own */
      Ast *c = mknear(Ncall, at);

      c->v.call.f = pathsegs(s->name, var->name, at);
      c->v.call.args = vnew(Ast *, np);
      for (i = 0; i < np; i++) {
        Ast *a = valtoexpr(v.elems[i], at);

        vappend(&c->v.call.args, &a);
      }
      return c;
    }
  }
  case Tystruct: { /* every row by name: the value already holds the
                    * zero the left-out half reads as (01-types.md) */
    Sym  *s = v.t->sym;
    usize nf = s->nfields;

    n = mknear(Nstructlit, at);
    n->v.slit.path = pathsegs(s->name, 0, at);
    n->v.slit.inits = vnew(Ast *, nf ? nf : 1);
    for (i = 0; i < nf; i++) {
      Ast *in = mknear(Ninit, at);

      in->v.init.name = s->fields[i].name;
      in->v.init.e = valtoexpr(v.elems[i], at);
      vappend(&n->v.slit.inits, &in);
    }
    return n;
  }
  case Tyunion: { /* the one row the write made active -- or the
                   * zeroed whole, which is the literal with no rows
                   * at all */
    Sym *s = v.t->sym;

    n = mknear(Nstructlit, at);
    n->v.slit.path = pathsegs(s->name, 0, at);
    n->v.slit.inits = vnew(Ast *, 1);
    if (v.tag != VNONROW) {
      Ast *in = mknear(Ninit, at);

      in->v.init.name = s->fields[v.tag].name;
      in->v.init.e = valtoexpr(v.elems[0], at);
      vappend(&n->v.slit.inits, &in);
    }
    return n;
  }
  case Tytuple: /* by position, the rows' own types */
    n = mknear(Ntuple, at);
    n->v.list.ts = vnew(Ast *, v.t->nargs ? v.t->nargs : 1);
    for (i = 0; i < v.t->nargs; i++) {
      Ast *r = valtoexpr(v.elems[i], at);

      vappend(&n->v.list.ts, &r);
    }
    return n;
  case Tyarray: { /* the elements in a row, the length from the type */
    usize ne = v.t->n;

    n = mknear(Narraylit, at);
    n->v.arrlit.len = intnear(ne, at);
    n->v.arrlit.t = tytoexpr(v.t->t, at);
    n->v.arrlit.es = vnew(Ast *, ne ? ne : 1);
    for (i = 0; i < ne; i++) {
      Ast *el = valtoexpr(v.elems[i], at);

      vappend(&n->v.arrlit.es, &el);
    }
    return n;
  }
  case Tyslice: { /* @typeinfo's own slices: owned rows, spelled the
                   * slice literal's way -- no length written, the
                   * elements listed (08-reflection.md) */
    usize ne = v.len;

    n = mknear(Narraylit, at);
    n->v.arrlit.len = 0; /* NULL: []T, the length on the value */
    n->v.arrlit.mut = 0;
    n->v.arrlit.t = tytoexpr(v.t->t, at);
    n->v.arrlit.es = vnew(Ast *, ne ? ne : 1);
    for (i = 0; i < ne; i++) {
      Ast *el = valtoexpr(v.elems[i], at);

      vappend(&n->v.arrlit.es, &el);
    }
    return n;
  }
  default: /* a pointer is a runtime address: not a literal (08) */
    cerrat(at, "%s does not materialize: it is not compile-time known here (08-reflection.md)",
           tnm(v.t));
  }
  return 0; /* unreachable */
}

/* a round's binding, flattened: the pattern's names and the values
 * under them -- the whole pattern walked, a tuple by position, a
 * struct by field, a variant's payload in the declaration's order,
 * an or-pattern the branch that fits. What lands here is a plain
 * name with its value, for the let that spells a round is a flat
 * one -- the emitter's let reads a pattern as irrefutable
 * (09-match.md), and the round's own fit was proven here */
typedef struct
{
  char *name;
  Val   v;
  int   mut; /* the field's own, where the pattern reached one */
} Rbind;

static void
bindround(Ast *p, Val v, int mut, Rbind **out)
{
  usize i, k;

  switch (p->k) {
  case Npwild:
  case Nunit:
    return;
  case Nppath: {
    Ast    **segs = p->v.ppath.path->v.path.segs;
    Variant *var = patvariant(p, v.t);

    if (!var) { /* the binding form: the name, the whole value */
      Rbind b;

      b.name = segs[0]->v.seg.name;
      b.v = v;
      b.mut = mut;
      vappend(out, &b);
      return;
    }
    { /* a variant's payload, positionally in its own order */
      Ast **ps = p->v.ppath.payload;
      usize np = vlen(ps);

      for (i = 0; i < np; i++) {
        int fm = 0;
        Val pv;

        if (p->v.ppath.named) { /* by field name, the declaration's
                                 * order (01-types.md) */
          Field *f = 0;

          for (k = 0; k < var->nfields; k++)
            if (strcmp(var->fields[k].name, ps[i]->v.init.name) == 0) {
              f = &var->fields[k];
              break;
            }
          if (!f)
            cerrat(p, "'%s' has no field '%s' (01-types.md)", var->name, ps[i]->v.init.name);
          fm = f->mut;
          pv = v.elems[k];
          if (ps[i]->v.init.e) /* the sub-pattern, not the field
                                * name alone */
            bindround(ps[i]->v.init.e, pv, mut || fm, out);
          else {
            Rbind b;

            b.name = f->name;
            b.v = pv;
            b.mut = mut || fm;
            vappend(out, &b);
          }
        } else {
          pv = v.elems[i];
          bindround(ps[i], pv, mut, out);
        }
      }
    }
    return;
  }
  case Nptuple: { /* by position */
    Ast **ps = p->v.list.ts;
    usize n = vlen(ps);

    for (i = 0; i < n && i < v.t->nargs; i++)
      bindround(ps[i], v.elems[i], mut, out);
    return;
  }
  case Npstruct: { /* by field name */
    Ast **fs = p->v.pstruct.fields;
    usize n = vlen(fs);

    for (i = 0; i < n; i++) {
      usize nf = v.t->sym->nfields;

      for (k = 0; k < nf; k++)
        if (strcmp(v.t->sym->fields[k].name, fs[i]->v.init.name) == 0)
          break;
      if (k == nf)
        cerrat(p, "'%s' has no field '%s' (01-types.md)", v.t->sym->name, fs[i]->v.init.name);
      if (fs[i]->v.init.e)
        bindround(fs[i]->v.init.e, v.elems[k], mut || v.t->sym->fields[k].mut, out);
      else {
        Rbind b;

        b.name = fs[i]->v.init.name;
        b.v = v.elems[k];
        b.mut = mut || v.t->sym->fields[k].mut;
        vappend(out, &b);
      }
    }
    return;
  }
  case Npor: { /* the branch that fits -- the fit was proven above */
    Ast **alts = p->v.list.ts;
    usize n = vlen(alts);

    for (i = 0; i < n; i++)
      if (patfits(alts[i], v)) {
        bindround(alts[i], v, mut, out);
        return;
      }
    return;
  }
  default:
    cerrat(p, "this pattern does not bind a round (09-match.md)");
  }
}

/* the bindings a match's taken arm makes over its scrutinee's
 * value: the round's own spelling, a flat let each -- the value
 * kept beside its tree (a let's cv), for the walks that read a
 * name's bytes take them from it (09-match.md, 08-reflection.md) */
Ast **
cmatchlets(Ast *pat, Val sv, Ast *at)
{
  Rbind *bs = vnew(Rbind, 4);
  Ast  **lets;
  usize  nb, k;

  bindround(pat, sv, 0, &bs);
  nb = vlen(bs);
  lets = vnew(Ast *, nb ? nb : 1);
  for (k = 0; k < nb; k++) { /* the const for's own spelling, the
                              * same trick (10-iteration.md) */
    Ast *let = mknear(Nlet, at);
    Val *cv = arenaalloc(sizeof *cv);

    let->v.let.pat = pathsegs(bs[k].name, 0, at);
    let->v.let.t = tytoexpr(bs[k].v.t, at);
    let->v.let.e = valtoexpr(bs[k].v, at);
    let->v.let.mut = bs[k].mut;
    *cv = bs[k].v;
    let->v.let.cv = cv;
    vappend(&lets, &let);
  }
  return lets;
}

/* a round's binding, listed for the inner const for that iterates
 * its name: the unroll builds the rounds in order, so the listing
 * shadows the way bindings do -- each round's names over the ones
 * before them */
static void
cforlet(Ast *st, Rbind *bs, usize nb)
{
  usize i;

  for (i = 0; i < nb; i++) {
    if (ncforlets >= sizeof cforlets / sizeof cforlets[0])
      cerrat(st, "too many const for rounds in flight (10-iteration.md)");
    cforlets[ncforlets].name = bs[i].name;
    cforlets[ncforlets].v = bs[i].v; /* the binding's own value, what
                                      * the round spelled */
    ncforlets++;
  }
}

/* the body's statements, the inner const fors among them already
 * unrolled: a nested one iterates the outer's variable, and the
 * round it lands in is the round being spelled -- so it expands
 * here, inside the round, not later as a node the passes would
 * reach once per round with only one unroll slot to share. A
 * runtime for stays itself: it is a statement like any other.
 * Returns 1 when a nested loop named a generic parameter: the
 * rounds this one was spelling are the instance's own too, and the
 * caller defers the whole loop to the re-check (04-generics.md) */
static int
cforbody(Ast *body, Ast ***un, Fenv *fe)
{
  Ast **ss = body->v.blk.stmts;
  usize n = vlen(ss), i, k;

  for (i = 0; i < n; i++) {
    if (ss[i]->k == Ncfor) { /* the recursion, flattened in where
                              * it stood */
      Ast **inner = cforunroll(ss[i], fe);

      if (!inner) /* the black box reached into another's rounds:
                   * one instance per round is the unfolding of
                   * generic recursion, and that walk is the
                   * re-check's own (04-generics.md) */
        return 1;
      for (k = 0; k < vlen(inner); k++)
        vappend(un, &inner[k]);
    } else {
      /* the round's own copy: the passes write what they walk -- a
       * builtin rewrites itself into the answer it gives, @field
       * into the borrow it spells, @offset into the constant it
       * folds -- and what one round wrote the next round must not
       * read. The names a round's let spelled still resolve through
       * the frame, so the copy checks and spells the same (10) */
      Ast *c = astclone(ss[i]);

      vappend(un, &c);
    }
  }
  if (body->v.blk.tail) { /* the body's last expression, an
                           * expression's statement now -- the loop
                           * had it as its own tail, the unroll has
                           * no tail (15-grammar.md) */
    Ast *x = mknear(Nexprstmt, body);

    x->v.n1.e = astclone(body->v.blk.tail);
    vappend(un, &x);
  }
  return 0;
}

/* the const for's statements: the iteration already ran, and each
 * round binds its values as lets that spell them -- the types
 * spelled too, so the checking walks them the way it walks the
 * program's own words. The pattern is flattened into one binding a
 * let, for the round's fit was proven here and a let's pattern is
 * irrefutable where it is checked (09-match.md); a pattern the
 * element does not fit ends the loop, the way a for-in's does
 * (10-iteration.md). What is not compile-time known stops where it
 * stands, for the marker is a promise, not a hope (08-reflection.md) */
Ast **
cforunroll(Ast *st, Fenv *fe)
{
  Ast **un;
  Val   src;
  Type *et;
  Val  *rounds = 0;
  usize n = 0, i, save = ncforlets;
  int   bb = evalblackbox;

  if (st->v.forx.shape != FIN)
    cerrat(st, "a const for iterates: the condition and the let forms are runtime shapes "
               "(10-iteration.md)");
  if (st->v.forx.b->k == Npath && !st->v.forx.b->v.path.root &&
      vlen(st->v.forx.b->v.path.segs) == 1) { /* a bare name: the
                                               * frame may hold it as
                                               * a compile-time
                                               * binding -- a match's
                                               * taken arm spelled it
                                               * (09-match.md) */
    Local *l = locfind(fe, st->v.forx.b->v.path.segs[0]->v.seg.name);

    if (l && l->cv)
      src = *l->cv;
    else
      src = ceval(st->v.forx.b, fe->env, 0);
  } else /* the frame's own bindings ride along either way: a
          * generic's T resolves against them, and the walk that
          * called this holds the instance's (04-generics.md) */
    src = ceval(st->v.forx.b, fe->env, 0);
  if (evalblackbox != bb) { /* the iterable named a generic
                             * parameter, the black box: the rounds
                             * are the instance's own, this walk
                             * skips the whole loop, and the re-check
                             * under the binding walks it (04) */
    evalblackbox = bb;      /* the flag dies with the walk that raised it */
    return 0;
  }
  et = src.t;
  while (et && et->k == Tymut)
    et = et->t;
  if (et->k == Tyarray) { /* an owned array: each element itself,
                           * one round apiece */
    rounds = src.elems;
    n = et->n;
  } else if (et->k == Tyenum && et->sym == sym_option && et->nargs == 1) {
    /* ?T yields its one payload or nothing: at most one round
     * (10-iteration.md) */
    Variant *some = symvarfind(et->sym, "Some");

    if (some && src.tag == some->disc) {
      rounds = src.elems;
      n = 1;
    }
  } else if (et->k == Tyslice) /* @typeinfo's slices: owned rows in
                                * the value, nothing lent out -- the
                                * runtime's borrow is a runtime thing,
                                * and no compile-time slice is one
                                * (08-reflection.md) */
    rounds = src.elems, n = src.len;
  else
    cerrat(st->v.forx.b,
           "iterating %s is not compile-time known: its iterators arrive with a later milestone "
           "(10-iteration.md)",
           tnm(et));

  un = vnew(Ast *, n ? n * 4 : 1);
  for (i = 0; i < n; i++) {
    Rbind *bs = vnew(Rbind, 4);
    Ast  **es;
    usize  nb, k;

    if (!patfits(st->v.forx.a, rounds[i]))
      break; /* the element pattern ends the loop (10-iteration.md) */
    bindround(st->v.forx.a, rounds[i], 0, &bs);
    nb = vlen(bs);
    es = arenaalloc((nb ? nb : 1) * sizeof *es);
    for (k = 0; k < nb; k++) { /* a binding a let, each flat: the
                                * value the compiler holds, spelled */
      Ast *let = mknear(Nlet, st);
      Val *cv = arenaalloc(sizeof *cv);

      let->v.let.pat = pathsegs(bs[k].name, 0, st);
      let->v.let.t = tytoexpr(bs[k].v.t, st);
      let->v.let.e = valtoexpr(bs[k].v, st);
      let->v.let.mut = bs[k].mut;
      *cv = bs[k].v; /* the value itself, kept beside its spelled
                      * tree: the body's walk binds the local, and a
                      * name argument read against the frame finds
                      * the bytes here, long after the rounds have
                      * ended and their listing has gone back
                      * (08-reflection.md) */
      let->v.let.cv = cv;
      es[k] = let;
      vappend(&un, &let);
    }
    cforlet(st, bs, nb);                      /* the inner loops iterate this round's names */
    if (cforbody(st->v.forx.body, &un, fe)) { /* a nested loop's
                                               * black box: this
                                               * loop's rounds would
                                               * carry it, and the
                                               * whole loop defers
                                               * to the re-check
                                               * (04-generics.md) */
      ncforlets = save;                       /* the listing goes back, the rounds unspent */
      st->v.forx.unroll = 0;
      return 0;
    }
  }
  ncforlets = save; /* the loop's rounds end; the listing goes back */
  st->v.forx.unroll = un;
  return un;
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
  FrSaved     save;
  Val         r;

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
  save.nlocs = nlocs; /* the caller's frame, held whole for the way
                       * back -- one struct, one save, one restore */
  save.locbase = locbase;
  save.returning = returning;
  save.breaking = breaking;
  save.continuing = continuing;
  save.retv = retv;
  save.fnret = fnret;
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
  nlocs = save.nlocs;
  locbase = save.locbase;
  returning = save.returning;
  breaking = save.breaking;
  continuing = save.continuing;
  retv = save.retv;
  fnret = save.fnret;
  calldepth--;
  pick->evaled = 1; /* the body ran to its end: what it did not
                     * reach is a branch of it, not a misuse of
                     * @compileError the body check reports (08) */
  return r;
}
