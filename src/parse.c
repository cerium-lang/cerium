/* parse.c -- the recursive descent, one function per production.
 *
 * Everything here traces to specs/15-grammar.md; where the grammar
 * has a decision to make, the note says which production carries it:
 *
 *   - ">>" splits only at a generic-arguments closing -- nextgt()
 *   - "<" after a path segment is generic arguments only when the
 *     args parse and (, ::, or . follows -- a snapshot trial
 *   - a for head is re-read as a pattern when an "in" shows up
 *     behind the expression -- the same snapshot discipline
 *   - a block's tail expression is the one expression the "}" can
 *     close without a ";"
 *
 * Errors print path:line:col and stop the compiler (perr), the way
 * the lexer's own errors do. No recovery: a parser that continues
 * past a broken tree only stacks confusion.
 */

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "die.h"
#include "lex.h"
#include "parse.h"

static void
perr(const char *fmt, ...)
{
  va_list ap;
  Token  *t = lexcur();

  fprintf(stderr, "%s:%u:%u: ", lexpath(), t->line, t->col);
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  fputc('\n', stderr);
  exit(1);
}

/* -- small helpers ---------------------------------------------------- */

/* vappend, but a NULL vector starts itself: a fresh node's vector
 * fields are NULL (calloc), and most lists in the tree stay short */
static void
npush(Ast ***vp, Ast *n)
{
  if (!*vp)
    *vp = vnew(Ast *, 8);
  vappend(vp, &n);
}

static Tok
peekt(void)
{
  return peek();
}

/* an if/match/for head is being read: the expression's trailing "{"
 * opens the body, never a struct literal (15-grammar.md -- the spec
 * says `if (Point{...}) == q`, parenthesized, is the form that means
 * a literal). Every bracketed sub-expression saves, clears, and
 * restores this, so a literal inside the head still parses. */
static int headctx;

static int
accept(Tok t)
{
  if (peek() == t) {
    next();
    return 1;
  }
  return 0;
}

static void
want(Tok t, const char *what)
{
  if (next() != t)
    perr("expected %s", what);
}

static char *
wantident(const char *what)
{
  if (peek() != Tident)
    perr("expected %s", what);
  next();
  return mkstr();
}

/* a field's name, where a field's name is parsed: the declaration
 * (a struct's field, an enum variant's named payload), a literal's
 * init, a pattern's binding, and the access. 'type' is a keyword and
 * a field's name both -- Field carries 'type: type'
 * (08-reflection.md) -- so these four spots admit it. A keyword token
 * carries no v.str, the spelling is the keyword itself. */
static char *
fieldname(const char *what)
{
  if (peek() == Ttype) {
    char *s = arenaalloc(5);

    next();
    strcpy(s, "type");
    return s;
  }
  return wantident(what);
}

/* -- attributes -------------------------------------------------------- */

static Ast *
attrarg(void)
{
  Ast *n;

  switch (peek()) {
  case Tident: {
    next();
    n = mk(Npath);
    n->v.path.segs = vnew(Ast *, 2);
    {
      Ast *s = mk(Nseg);
      s->v.seg.name = mkstr();
      npush(&n->v.path.segs, s);
    }
    return n;
  }
  case Tint:
    next();
    n = mk(Nint);
    n->v.i.num = mknum();
    return n;
  case Tflt:
    next();
    n = mk(Nflt);
    n->v.f.flt = lexcur()->v.flt;
    return n;
  case Tstr: {
    next();
    n = mk(Nstr);
    n->v.s.s = mkstr();
    n->v.s.len = lexcur()->v.str.len;
    n->v.s.flags = lexcur()->v.str.flags;
    return n;
  }
  default:
    perr("expected an attribute argument (a name or a literal)");
  }
  return 0; /* unreachable */
}

static Ast **
attrs(void)
{
  Ast **v = 0;

  while (peek() == Thashlbracket) {
    next();    /* "#[" */
    for (;;) { /* "#[a, b]" is "#[a] #[b]" (01-types.md) */
      Ast *a = mk(Nattr);

      a->v.seg.name = wantident("attribute name");
      if (accept(Tlparen)) {
        if (peek() != Trparen) {
          for (;;) {
            Ast *g = attrarg();
            npush(&a->v.seg.args, g);
            if (peek() == Tcomma) {
              next();
              if (peek() == Trparen)
                break;
              continue;
            }
            break;
          }
        }
        want(Trparen, ")");
      }
      npush(&v, a);
      if (!accept(Tcomma))
        break;
    }
    want(Trbracket, "]");
  }
  return v;
}

/* -- types -------------------------------------------------------------- */

static Ast  *type_(void);
static Ast  *expr(void);
static Ast  *postfix(void);
static Ast  *primary(void);
static Ast  *orexpr(void);
static Ast  *pattern(void);
static Ast  *block(void);
static Ast  *statement(void);
static Ast **parameters(void);

/* a path in type position: "<" after a segment is generic arguments,
 * unconditionally -- a type grammar has no comparison to be */
static Ast *
typepath(void)
{
  Ast *n = mk(Npath);

  n->v.path.root = accept(Tcoloncolon);
  n->v.path.segs = vnew(Ast *, 4);
  for (;;) {
    Ast *s = mk(Nseg);

    s->v.seg.name = wantident("a path segment");
    if (peek() == Tlt) {
      next();
      for (;;) {
        Ast *a;

        if (peek() == Tdollar2 || peek() == Tcaret2) {
          Tok op = next();

          a = mk(Nun);
          a->v.un.op = op;
          a->v.un.e =
              op == Tcaret2 ? type_() : postfix(); /* a lift's
                                                    * operand is a type; a splice's, a value */

        } else {
          a = type_();
        }
        npush(&s->v.seg.args, a);
        if (peek() == Tcomma) {
          next();
          if (peek() == Tgt || peek() == Tshr)
            break;
          continue;
        }
        break;
      }
      if (nextgt() != Tgt)
        perr("expected > closing generic arguments");
    }
    npush(&n->v.path.segs, s);
    if (peek() == Tcoloncolon) {
      next();
      continue;
    }
    break;
  }
  return n;
}

static Ast *
prefixtype(void)
{
  switch (peek()) {
  case Tquestion: {
    Ast *n;

    next();
    n = mk(Ntopt);
    n->v.n1.e = prefixtype();
    return n;
  }
  case Tstar: {
    Ast *n;

    next();
    n = mk(Ntptr);
    if (accept(Tmut))
      n->v.un.mut = 1;
    n->v.un.e = prefixtype();
    return n;
  }
  case Tlbracket: { /* [ [length] ] [mut] T -- the length is a const
                     * expression (08-reflection.md) */
    Ast *n;

    next();
    n = mk(Ntarray);
    if (peek() != Trbracket)
      n->v.arrlit.len = expr();
    want(Trbracket, "]");
    if (accept(Tmut)) /* [N]mut T / []mut T: the elements are writable
                       * slots (01-types.md); resolve wraps the element */
      n->v.arrlit.mut = 1;
    n->v.arrlit.t = prefixtype();
    return n;
  }
  case Tdotdotdot: { /* ...Ts: the pack parameter's own rows
                      * (04-generics.md) */
    Ast *n;

    next();
    n = mk(Ntpack);
    n->v.un.e = prefixtype();
    return n;
  }
  default:
    break;
  }
  /* primary_type */
  switch (peek()) {
  case Tident:
  case Tcoloncolon:
    return typepath();
  case Tdollar2: /* $$x, the splice: a type slot's own crossing, the
                  * operand a value that holds a type
                  * (08-reflection.md); a ^^ here parses too, and the
                  * resolver says which way each crosses */
  case Tcaret2: {
    Tok  op = next();
    Ast *n = mk(Nun);

    n->v.un.op = op;
    n->v.un.e = op == Tcaret2 ? type_() : postfix(); /* a lift's
                                                      * operand is a
                                                      * type; a
                                                      * splice's, a
                                                      * value */
    return n;
  }
  case Tlparen: { /* () or a tuple type */
    Ast *n;

    next();
    if (accept(Trparen))
      return mk(Nunit);
    n = mk(Nttuple);
    n->v.list.ts = vnew(Ast *, 4);
    for (;;) {
      Ast *t;

      if (accept(Tmut)) { /* (T, mut U): a writable row (01-types.md);
                           * resolve wraps it in tymut, as [N]mut does */
        Ast *m = mk(Ntmut);

        m->v.un.mut = 1;
        m->v.un.e = type_();
        t = m;
      } else
        t = type_();
      npush(&n->v.list.ts, t);
      if (peek() == Tcomma) {
        next();
        if (peek() == Trparen)
          break;
        continue;
      }
      break;
    }
    want(Trparen, ")");
    return n;
  }
  case Tfn: { /* fn(A, B) -> T -- the types are anonymous, no names
               * (01-types.md; the 15-grammar parameters reuse is a
               * spec bug, recorded in the PR) */
    Ast *n;

    next();
    n = mk(Ntfn);
    want(Tlparen, "(");
    if (peek() != Trparen)
      for (;;) {
        npush(&n->v.fnty.args, type_());
        if (!accept(Tcomma) || peek() == Trparen)
          break; /* a trailing comma is fine */
      }
    want(Trparen, ")");
    if (accept(Tarrow))
      n->v.fnty.ret = type_();
    return n;
  }
  case Tdyn: {
    Ast *n;

    next();
    n = mk(Ntdyn);
    if (accept(Tmut)) /* dyn mut A: the writable handle (06) */
      n->v.tdyn.mut = 1;
    { /* the trait's name: "<" after it is not its arguments here --
       * a handle's "<" gives associated types (06-dispatch.md), so
       * the path is read without them */
      Ast *p = mk(Npath);

      p->v.path.segs = vnew(Ast *, 4);
      for (;;) {
        Ast *s = mk(Nseg);

        s->v.seg.name = wantident("a trait name");
        npush(&p->v.path.segs, s);
        if (!accept(Tcoloncolon))
          break;
      }
      n->v.tdyn.e = p;
    }
    if (peek() == Tlt) { /* dyn Iterator<Item = u32>: the associated
                          * types given, an Ninit list (06-dispatch.md) */
      next();
      for (;;) {
        Ast *a = mk(Ninit);

        a->v.init.name = wantident("an associated type name");
        want(Teq, "=");
        a->v.init.e = type_();
        npush(&n->v.tdyn.assocs, a);
        if (!accept(Tcomma))
          break;
      }
      want(Tgt, ">");
    }
    return n;
  }
  case Ttype:
    next();
    return mk(Nttype);
  default:
    perr("expected a type");
  }
  return 0; /* unreachable */
}

static Ast *
type_(void) /* result_type: prefix [ "?" prefix ] */
{
  Ast *t = prefixtype();

  if (accept(Tquestion)) {
    Ast *n = mk(Ntresult);

    n->v.n2.a = t;
    n->v.n2.b = prefixtype();
    return n;
  }
  return t;
}

/* -- expressions -------------------------------------------------------- */

/* can the token ahead begin a type? The generic-arguments trial
 * commits only when it can, so `a < 10` never enters it: that "<" is
 * a comparison from the start */
static int
startsatype(void)
{
  switch (peek()) {
  case Tquestion:   /* ?T */
  case Tstar:       /* *T, *mut T */
  case Tlbracket:   /* [N]T, []T */
  case Tlparen:     /* (A, B) */
  case Tfn:         /* fn(A) -> R */
  case Tdyn:        /* dyn Path */
  case Ttype:       /* the type of a type */
  case Tident:      /* a path */
  case Tcoloncolon: /* a rooted path */
  case Tdollar2:    /* $$t splice */
  case Tcaret2:
    return 1;
  default:
    return 0;
  }
}

/* "<" is peeked. Trial: parse generic arguments, then demand one of
 * ( :: . after the closing ">" -- else rewind and let the caller
 * read a comparison. This is the production the ">>" split serves. */
static Ast **
tryargs(void)
{
  LexSnap *snap;
  Ast    **args = 0;
  Tok      after;

  snap = lexsnap();
  next();               /* "<" */
  if (!startsatype()) { /* `a < 10`: a comparison from the start */
    lexunsnap(snap);
    return 0;
  }
  for (;;) {
    Ast *a;

    if (peek() == Tdollar2 || peek() == Tcaret2) {
      Tok op = next();

      a = mk(Nun);
      a->v.un.op = op;
      a->v.un.e = op == Tcaret2 ? type_() : postfix(); /* a lift's
                                                        * operand is a type; a splice's, a value */

    } else {
      a = type_();
    }
    npush(&args, a);
    if (peek() == Tcomma) {
      next();
      if (peek() == Tgt || peek() == Tshr)
        break;
      continue;
    }
    break;
  }
  if (nextgt() != Tgt) {
    lexunsnap(snap);
    return 0;
  }
  after = peek();
  if (after == Tlparen || after == Tcoloncolon || after == Tdot) {
    lexdrop(snap); /* commit: the trial stands */
    return args;
  }
  lexunsnap(snap);
  return 0;
}

/* a path in expression position: each segment's "<" is tried as
 * generic arguments, and only the trial commits */
static Ast *
exprpath(void)
{
  Ast *n = mk(Npath);

  n->v.path.root = accept(Tcoloncolon);
  n->v.path.segs = vnew(Ast *, 4);
  for (;;) {
    Ast *s = mk(Nseg);

    s->v.seg.name = wantident("a path segment");
    if (peek() == Tlt) {
      s->v.seg.args = tryargs();
      if (s->v.seg.args && peek() == Tcoloncolon) {
        next();
        npush(&n->v.path.segs, s);
        continue;
      }
      if (s->v.seg.args && peek() != Tcoloncolon) {
        npush(&n->v.path.segs, s);
        break;
      }
      /* no args: the "<" belongs to whatever follows (a
       * comparison), so the segment is done */
      npush(&n->v.path.segs, s);
      if (peek() == Tcoloncolon) {
        next();
        continue;
      }
      break;
    }
    npush(&n->v.path.segs, s);
    if (peek() == Tcoloncolon) {
      next();
      continue;
    }
    break;
  }
  return n;
}

static Ast *
ifexpr(int cnst)
{
  Ast *n = mk(cnst ? Ncif : Nif);

  next();      /* if */
  headctx = 1; /* the trailing "{" opens the then-block */
  n->v.ifx.cond = expr();
  headctx = 0;
  n->v.ifx.then = block();
  if (accept(Telse)) {
    if (peek() == Tif)
      n->v.ifx.els = ifexpr(0);
    else if (peek() == Tconst) { /* else const if ... */
      next();
      n->v.ifx.els = ifexpr(1);
    } else
      n->v.ifx.els = block();
  }
  return n;
}

static Ast *
matchexpr(void)
{
  Ast *n = mk(Nmatch);

  next();               /* match */
  headctx = 1;          /* the trailing "{" opens the arms */
  n->v.call.f = expr(); /* the scrutinee */
  headctx = 0;
  want(Tlbrace, "{");
  while (peek() != Trbrace) {
    Ast *a = mk(Narm);

    a->v.n2.a = pattern();
    want(Tfatarrow, "=>");
    if (peek() == Tlbrace)
      a->v.n2.b = block();
    else
      a->v.n2.b = expr();
    npush(&n->v.call.args, a);
    want(Tcomma, ","); /* every arm carries one, the last included */
  }
  want(Trbrace, "}");
  return n;
}

static Ast *
rangeexpr(void)
{
  Ast *l = orexpr();

  if (peek() == Tdotdot) {
    Ast *n;

    next();
    n = mk(Nrange);
    n->v.bin.l = l;
    n->v.bin.r = orexpr();
    if (n->v.bin.r->k == Nrange)
      perr("one \"..\" per expression");
    return n;
  }
  return l;
}

static Ast *
expr(void)
{
  if (peek() == Tconst) {
    next();
    if (peek() == Tif)
      return ifexpr(1);
    perr("expected if after const");
  }
  if (peek() == Tif)
    return ifexpr(0);
  if (peek() == Tmatch)
    return matchexpr();
  return rangeexpr();
}

/* the binary chain: one function per level, tightest at the bottom */

static Ast *orexpr(void);
static Ast *andexpr(void);
static Ast *cmpexpr(void);
static Ast *bitorexpr(void);
static Ast *bitxorexpr(void);
static Ast *bitandexpr(void);
static Ast *shiftexpr(void);
static Ast *addexpr(void);
static Ast *mulexpr(void);
static Ast *unary(void);

static int
isany(Tok t, Tok a, Tok b)
{
  return t == a || t == b;
}

#define BINLEVEL(name, sub, isop)                                                                  \
  static Ast *name(void)                                                                           \
  {                                                                                                \
    Ast *l = sub();                                                                                \
    for (;;) {                                                                                     \
      Tok op = peek();                                                                             \
      if (isop(op)) {                                                                              \
        Ast *n;                                                                                    \
        next();                                                                                    \
        n = mk(Nbin);                                                                              \
        n->v.bin.op = op;                                                                          \
        n->v.bin.l = l;                                                                            \
        n->v.bin.r = sub();                                                                        \
        l = n;                                                                                     \
        continue;                                                                                  \
      }                                                                                            \
      return l;                                                                                    \
    }                                                                                              \
  }

#define ISOR(t)     ((t) == Tbarbar)
#define ISAND(t)    ((t) == Tampamp)
#define ISBITOR(t)  ((t) == Tbar)
#define ISBITXOR(t) ((t) == Tcaret)
#define ISBITAND(t) ((t) == Tamp)
#define ISSHIFT(t)  isany((t), Tshl, Tshr)
#define ISADD(t)    isany((t), Tplus, Tminus)
#define ISMUL(t)    (t) == Tstar || isany((t), Tslash, Tpercent)

BINLEVEL(orexpr, andexpr, ISOR)
BINLEVEL(andexpr, cmpexpr, ISAND)
BINLEVEL(bitorexpr, bitxorexpr, ISBITOR)
BINLEVEL(bitxorexpr, bitandexpr, ISBITXOR)
BINLEVEL(bitandexpr, shiftexpr, ISBITAND)
BINLEVEL(shiftexpr, addexpr, ISSHIFT)
BINLEVEL(addexpr, mulexpr, ISADD)
BINLEVEL(mulexpr, unary, ISMUL)

static Ast *
cmpexpr(void) /* no chaining: one operator, at most */
{
  Ast *l = bitorexpr();
  Tok  op = peek();

  switch (op) {
  case Tlt:
  case Tgt:
  case Tle:
  case Tge:
  case Teqeq:
  case Tne: {
    Ast *n;

    next();
    n = mk(Nbin);
    n->v.bin.op = op;
    n->v.bin.l = l;
    n->v.bin.r = bitorexpr();
    switch (peek()) {
    case Tlt:
    case Tgt:
    case Tle:
    case Tge:
    case Teqeq:
    case Tne:
      perr("comparisons do not chain; parenthesize each side");
    default:
      break;
    }
    return n;
  }
  default:
    return l;
  }
}

static Ast *
unary(void)
{
  switch (peek()) {
  case Tstar:
  case Tamp:
  case Tbang:
  case Tminus:
  case Ttilde:
  case Tcaret2:
  case Tdollar2: {
    Tok  op = next();
    Ast *n = mk(Nun);

    n->v.un.op = op;
    if (op == Tamp && accept(Tmut))
      n->v.un.mut = 1;
    if (op == Tamp && peek() == Tdyn) { /* &dyn b, &mut dyn b: the
                                         * fat handle, its value
                                         * the operand (06-dispatch.md) */
      next();
      n->v.un.op = Tdyn; /* re-marked: not a borrow, a handle made */
    }
    /* a lift's operand is a type spelled in the value's slot
     * (08-reflection.md); a splice's is the value that holds one */
    n->v.un.e = op == Tcaret2 ? type_() : unary();
    return n;
  }
  default:
    return postfix();
  }
}

static Ast *
callargs(Ast *n, Ast ***slot) /* the "(" is consumed; the args land in
                               * *slot. Nbuiltin passes &blt.args:
                               * call.args would alias blt.targs in
                               * the union */
{
  int save = headctx;

  headctx = 0; /* inside the argument list, a "{ " is a literal */
  if (peek() != Trparen) {
    for (;;) {
      Ast *a;

      if (peek() == Tdotdotdot) {
        next();
        a = mk(Nspread);
        a->v.un.e = expr();
      } else {
        a = expr();
      }
      npush(slot, a);
      if (peek() == Tcomma) {
        next();
        if (peek() == Trparen)
          break;
        continue;
      }
      break;
    }
  }
  want(Trparen, ")");
  headctx = save;
  return n;
}

static Ast *
postfix(void)
{
  Ast *e = primary();

  for (;;) {
    switch (peek()) {
    case Tdot: {
      next();
      if (peek() == Tint) {
        Ast *n;

        next();
        n = mk(Ntupidx);
        n->v.tup.e = e;
        n->v.tup.idx = mknum();
        e = n;
      } else {
        Ast *n = mk(Naccess);

        n->v.fld.e = e;
        n->v.fld.name = fieldname("a field name");
        e = n;
      }
      continue;
    }
    case Tlbracket: { /* index, or a range index */
      int save = headctx;

      headctx = 0; /* inside the brackets, a "{ " is a literal again */
      next();
      if (peek() == Tdotdot) { /* [..hi] or [..] */
        Ast *n = mk(Nrangeindex);

        next();
        n->v.ridx.e = e;
        if (peek() != Trbracket)
          n->v.ridx.hi = orexpr();
        want(Trbracket, "]");
        headctx = save;
        e = n;
        continue;
      }
      {
        Ast *ix = orexpr(); /* not expr(): a ".." here closes the
                             * range, it is not a range operator */
        Ast *n;

        if (peek() == Tdotdot) { /* [lo..hi] or [lo..] */
          next();
          n = mk(Nrangeindex);
          n->v.ridx.e = e;
          n->v.ridx.lo = ix;
          if (peek() != Trbracket)
            n->v.ridx.hi = orexpr();
          want(Trbracket, "]");
        } else {
          want(Trbracket, "]");
          n = mk(Nindex);
          n->v.n2.a = e;
          n->v.n2.b = ix;
        }
        headctx = save;
        e = n;
      }
      continue;
    }
    case Tlparen: {
      Ast *n = mk(Ncall);

      next();
      n->v.call.f = e;
      e = callargs(n, &n->v.call.args);
      continue;
    }
    case Tquestion: {
      Ast *n;

      next();
      n = mk(Ntry);
      n->v.n1.e = e;
      e = n;
      continue;
    }
    default:
      return e;
    }
  }
}

static Ast *
closure(void) /* the "fn" is peeked */
{
  Ast *n = mk(Nclosure);

  next(); /* fn */
  want(Tlbracket, "[");
  if (peek() != Trbracket) {
    for (;;) {
      Ast *c = mk(Ncap);

      if (accept(Tamp)) {
        c->v.cap.byref = 1;
        if (accept(Tmut))
          c->v.cap.mut = 1;
      } else if (accept(Tmut)) {
        c->v.cap.mut = 1;
      }
      c->v.cap.name = wantident("a capture");
      npush(&n->v.clos.caps, c);
      if (peek() == Tcomma) {
        next();
        if (peek() == Trbracket)
          break;
        continue;
      }
      break;
    }
  }
  want(Trbracket, "]");
  want(Tlparen, "(");
  if (peek() != Trparen)
    n->v.clos.params = parameters();
  want(Trparen, ")");
  if (accept(Tarrow))
    n->v.clos.ret = type_();
  n->v.clos.body = block();
  return n;
}

/* the ten the language provides (08-reflection.md) -- a misspelling
 * parses silently otherwise, and a golden test froze one for a week */
static int
isbuiltin(const char *name)
{
  static const char *const names[] = {"sizeof",   "alignof",      "offset", "cast",
                                      "typeinfo", "typeof",       "field",  "count",
                                      "take",     "compileError", 0};
  usize                    i;

  for (i = 0; names[i]; i++)
    if (strcmp(name, names[i]) == 0)
      return 1;
  return 0;
}

static Ast *
builtin(void) /* the "@" is peeked */
{
  Ast *n = mk(Nbuiltin);

  next(); /* @ */
  n->v.blt.name = wantident("a builtin name");
  if (!isbuiltin(n->v.blt.name))
    perr("unknown builtin '@%s' (08-reflection.md lists the ten)", n->v.blt.name);
  if (peek() == Tlt) {
    next();
    for (;;) {
      Ast *a;

      if (peek() == Tdollar2 || peek() == Tcaret2) {
        Tok op = next();

        a = mk(Nun);
        a->v.un.op = op;
        a->v.un.e =
            op == Tcaret2 ? type_() : postfix(); /* a lift's
                                                  * operand is a type; a splice's, a value */

      } else {
        a = type_();
      }
      npush(&n->v.blt.targs, a);
      if (peek() == Tcomma) {
        next();
        if (peek() == Tgt || peek() == Tshr)
          break;
        continue;
      }
      break;
    }
    if (nextgt() != Tgt)
      perr("expected > closing generic arguments");
  }
  want(Tlparen, "(");
  callargs(n, &n->v.blt.args); /* the argument shape of a call, in the
                                * builtin's own slot */
  return n;
}

static Ast *
arraylit(void) /* the "[" is peeked */
{
  Ast *n = mk(Narraylit);
  int  save = headctx;

  headctx = 0; /* inside the literal, a "{ " is a literal again */
  next();
  if (peek() != Trbracket) /* the length is a const expression, as a
                            * type's own is (08-reflection.md) */
    n->v.arrlit.len = expr();
  want(Trbracket, "]");
  if (accept(Tmut))
    n->v.arrlit.mut = 1;
  n->v.arrlit.t = type_();
  want(Tlbrace, "{");
  if (peek() != Trbrace) {
    for (;;) {
      Ast *e = expr();

      npush(&n->v.arrlit.es, e);
      if (peek() == Tcomma) {
        next();
        if (peek() == Trbrace)
          break;
        continue;
      }
      break;
    }
  }
  want(Trbrace, "}");
  headctx = save;
  return n;
}

static Ast *
fieldinits(Ast *n, Ast ***slot) /* "{ f: e, ... }" is peeked; the
                                 * vector slot is the caller's to name */
{
  int save = headctx;

  headctx = 0; /* inside the braces, a "{ " is a literal again */
  next();      /* { */
  if (peek() != Trbrace) {
    for (;;) {
      Ast *fi = mk(Ninit);

      fi->v.init.name = fieldname("a field name");
      want(Tcolon, ":");
      fi->v.init.e = expr();
      npush(slot, fi);
      if (peek() == Tcomma) {
        next();
        if (peek() == Trbrace)
          break;
        continue;
      }
      break;
    }
  }
  want(Trbrace, "}");
  headctx = save;
  return n;
}

static Ast *
primary(void)
{
  switch (peek()) {
  case Ttrue:
  case Tfalse: {
    Ast *n;

    n = mk(Nbool);
    n->v.i.num = peek() == Ttrue;
    next();
    return n;
  }
  case Tint: {
    Ast *n;

    next();
    n = mk(Nint);
    n->v.i.num = mknum();
    return n;
  }
  case Tflt: {
    Ast *n;

    next();
    n = mk(Nflt);
    n->v.f.flt = lexcur()->v.flt;
    return n;
  }
  case Tbyte: {
    Ast *n;

    next();
    n = mk(Nbyte);
    n->v.s.s = mkstr();
    n->v.s.len = 1;
    return n;
  }
  case Tstr: {
    Ast *n;

    next();
    n = mk(Nstr);
    n->v.s.s = mkstr();
    n->v.s.len = lexcur()->v.str.len;
    n->v.s.flags = lexcur()->v.str.flags;
    return n;
  }
  case Tident:
  case Tcoloncolon: {
    Ast *p = exprpath();

    if (peek() == Tlbrace && !headctx) { /* struct literal */
      Ast *n = mk(Nstructlit);

      n->v.slit.path = p;
      return fieldinits(n, &n->v.slit.inits);
    }
    return p;
  }
  case Tlparen: {
    Ast *n;
    int  save = headctx;

    headctx = 0; /* inside the parens, a "{ " is a literal again */
    next();
    if (accept(Trparen)) {
      headctx = save;
      return mk(Nunit);
    }
    {
      Ast *e1;

      if (peek() == Tdotdotdot) { /* (...Ts): the pack's rows, a
                                   * grouping of the spread -- the
                                   * impl target's own spelling
                                   * (04-generics.md) */
        next();
        e1 = mk(Nspread);
        e1->v.un.e = expr();
      } else
        e1 = expr();

      if (peek() == Tcomma) { /* a tuple */
        n = mk(Ntuple);
        n->v.list.ts = vnew(Ast *, 4);
        npush(&n->v.list.ts, e1);
        while (accept(Tcomma)) {
          if (peek() == Trparen)
            break;
          {
            Ast *e = expr();

            npush(&n->v.list.ts, e);
          }
        }
        want(Trparen, ")");
        headctx = save;
        return n;
      }
      want(Trparen, ")");
      headctx = save;
      return e1; /* (e) is e */
    }
  }
  case Tlbracket:
    return arraylit();
  case Tlbrace: { /* bare struct literal */
    Ast *n = mk(Nbarestructlit);

    return fieldinits(n, &n->v.list.ts);
  }
  case Tfn:
    return closure();
  case Tat:
    return builtin();
  default:
    perr("expected an expression");
  }
  return 0; /* unreachable */
}

/* -- statements --------------------------------------------------------- */

static Ast **
parameters(void)
{
  Ast **v = vnew(Ast *, 4);

  for (;;) {
    Ast *p = mk(Nparam);

    p->attrs = attrs();
    if (accept(Tconst))
      p->v.param.cnst = 1;
    if (accept(Tmut))
      p->v.param.mut = 1;
    p->v.param.name = wantident("a parameter name");
    want(Tcolon, ":");
    p->v.param.t = type_();
    if (p->v.param.t->k == Ntpack && peek() == Tcomma) /* the pack
                                                        * parameter swallows what is left; nothing
                                                        * follows it (04-generics.md) */
      perr("a pack parameter comes last");
    npush(&v, p);
    if (peek() == Tcomma) {
      next();
      if (peek() == Trparen)
        break;
      continue;
    }
    break;
  }
  return v;
}

static Ast *
letstmt(void)
{
  Ast *n = mk(Nlet);

  next(); /* let */
  if (accept(Tmut))
    n->v.let.mut = 1;
  n->v.let.pat = pattern();
  if (accept(Tcolon))
    n->v.let.t = type_();
  want(Teq, "=");
  n->v.let.e = expr();
  want(Tsemi, ";");
  return n;
}

static Ast *
jumpstmt(void)
{
  Ast *n;

  switch (peek()) {
  case Treturn:
    next();
    n = mk(Nreturn);
    if (peek() != Tsemi)
      n->v.n1.e = expr();
    break;
  case Tbreak:
    next();
    n = mk(Nbreak);
    break;
  default:
    next();
    n = mk(Ncontinue);
    break;
  }
  want(Tsemi, ";");
  return n;
}

static Ast *
forstmt(int cnst)
{
  Ast *n = mk(cnst ? Ncfor : Nfor);

  next();               /* for */
  if (peek() == Tlet) { /* for let PAT = e */
    next();
    n->v.forx.shape = FLET;
    n->v.forx.a = pattern();
    want(Teq, "=");
    headctx = 1; /* the trailing "{" opens the body */
    n->v.forx.b = expr();
    headctx = 0;
  } else {
    LexSnap *s = lexsnap();

    headctx = 1; /* the trailing "{" opens the body */
    {
      Ast *e = expr();

      if (peek() == Tin) { /* it was a pattern after all */
        headctx = 0;
        lexunsnap(s);
        n->v.forx.shape = FIN;
        n->v.forx.a = pattern();
        want(Tin, "in");
        headctx = 1;
        n->v.forx.b = expr();
        headctx = 0;
      } else { /* the condition form */
        lexdrop(s);
        n->v.forx.shape = FCOND;
        n->v.forx.a = e;
        headctx = 0;
      }
    }
  }
  n->v.forx.body = block();
  return n;
}

static Ast *
constitem(Ast **at, int pub) /* the "const" is peeked; block-level
                              * has no attrs and no pub */
{
  Ast *n = mk(Nconst);

  n->attrs = at;
  n->pub = pub;
  next();
  n->v.cst.name = wantident("a const name");
  want(Tcolon, ":");
  n->v.cst.t = type_();
  want(Teq, "=");
  n->v.cst.e = expr();
  want(Tsemi, ";");
  return n;
}

static int
isassignop(Tok t)
{
  switch (t) {
  case Teq:
  case Tpluseq:
  case Tminuseq:
  case Tstareq:
  case Tslasheq:
  case Tshleq:
  case Tshreq:
    return 1;
  default:
    return 0;
  }
}

static Ast *
statement(void)
{
  switch (peek()) {
  case Tlet:
    return letstmt();
  case Treturn:
  case Tbreak:
  case Tcontinue:
    return jumpstmt();
  case Tfor:
    return forstmt(0);
  case Tconst: { /* const-if, const-for, or a block-level const item */
    LexSnap *s = lexsnap();

    next();
    if (peek() == Tif) {
      lexunsnap(s);
      next(); /* const */
      return ifexpr(1);
    }
    if (peek() == Tfor) {
      lexunsnap(s);
      next(); /* const */
      return forstmt(1);
    }
    lexunsnap(s);
    return constitem(0, 0);
  }
  default:
    perr("internal: statement dispatch");
  }
  return 0; /* unreachable */
}

static Ast *
block(void)
{
  Ast *n = mk(Nblock);
  int  done = 0;

  want(Tlbrace, "{");
  for (;;) {
    if (peek() == Trbrace || peek() == Teof)
      break;
    if (peekt() == Tconst) { /* const-if is an expression; the
                              * const-for and const-item are not */
      LexSnap *s = lexsnap();

      next(); /* const */
      if (peek() == Tif)
        lexunsnap(s); /* falls to the expression arm: expr() reads
                       * the "const if" itself */
      else {
        lexunsnap(s);
        npush(&n->v.blk.stmts, statement());
        continue;
      }
    } else if (peekt() == Tlet || peekt() == Treturn || peekt() == Tbreak || peekt() == Tcontinue ||
               peekt() == Tfor) {
      npush(&n->v.blk.stmts, statement());
      continue;
    }
    { /* an expression: statement, assignment, or the tail */
      Ast *e = expr();

      if (isassignop(peek())) {
        Ast *a = mk(Nassign);

        a->v.bin.op = next();
        a->v.bin.l = e;
        a->v.bin.r = expr();
        want(Tsemi, ";");
        npush(&n->v.blk.stmts, a);
        continue;
      }
      if (peek() == Tsemi) {
        Ast *s = mk(Nexprstmt);

        next();
        s->v.n1.e = e;
        npush(&n->v.blk.stmts, s);
        continue;
      }
      if (peek() == Trbrace) {
        n->v.blk.tail = e; /* the block's value */
        done = 1;
        break;
      }
      perr("expected \";\" or \"}\" after the expression");
    }
  }
  (void) done;
  want(Trbrace, "}");
  return n;
}

/* -- patterns ------------------------------------------------------------ */

static Ast *
pfield(void)
{
  Ast *n = mk(Npfield);

  n->v.init.name = fieldname("a field");
  if (accept(Tcolon))
    n->v.init.e = pattern();
  return n;
}

static Ast *
unitpattern(void)
{
  if (peek() == Tident && strcmp(lexcur()->v.str.s, "_") == 0) {
    next();
    return mk(Npwild);
  }
  switch (peek()) {
  case Tident:
  case Tcoloncolon: {
    Ast *n = mk(Nppath);

    n->v.ppath.path = typepath();
    if (peek() == Tlparen) { /* positional payload */
      next();
      if (peek() != Trparen) {
        for (;;) {
          Ast *p = pattern();

          npush(&n->v.ppath.payload, p);
          if (peek() == Tcomma) {
            next();
            if (peek() == Trparen)
              break;
            continue;
          }
          break;
        }
      }
      want(Trparen, ")");
      return n;
    }
    if (peek() == Tlbrace) { /* named payload */
      n->v.ppath.named = 1;
      next();
      if (peek() == Tdotdot) {
        next();
        n->v.ppath.rest = 1;
      } else if (peek() != Trbrace) {
        for (;;) {
          Ast *f = pfield();

          npush(&n->v.ppath.payload, f);
          if (peek() == Tcomma) {
            next();
            if (peek() == Trbrace || peek() == Tdotdot)
              break;
            continue;
          }
          break;
        }
        if (accept(Tdotdot))
          n->v.ppath.rest = 1;
      }
      want(Trbrace, "}");
      return n;
    }
    return n;
  }
  case Tlparen: { /* tuple pattern, () included */
    Ast *n = mk(Nptuple);

    next();
    if (peek() != Trparen) {
      for (;;) {
        Ast *p = pattern();

        npush(&n->v.list.ts, p);
        if (peek() == Tcomma) {
          next();
          if (peek() == Trparen)
            break;
          continue;
        }
        break;
      }
    }
    want(Trparen, ")");
    return n;
  }
  case Tlbrace: { /* bare struct pattern */
    Ast *n = mk(Npstruct);

    next();
    if (peek() == Tdotdot) {
      next();
      n->v.pstruct.rest = 1;
    } else if (peek() != Trbrace) {
      for (;;) {
        Ast *f = pfield();

        npush(&n->v.pstruct.fields, f);
        if (peek() == Tcomma) {
          next();
          if (peek() == Trbrace || peek() == Tdotdot)
            break;
          continue;
        }
        break;
      }
      if (accept(Tdotdot))
        n->v.pstruct.rest = 1;
    }
    want(Trbrace, "}");
    return n;
  }
  default:
    perr("expected a pattern");
  }
  return 0; /* unreachable */
}

static Ast *
pattern(void)
{
  Ast *u = unitpattern();

  if (peek() != Tbar)
    return u;
  {
    Ast *n = mk(Npor);

    n->v.list.ts = vnew(Ast *, 4);
    npush(&n->v.list.ts, u);
    while (accept(Tbar)) {
      Ast *v = unitpattern();

      npush(&n->v.list.ts, v);
    }
    return n;
  }
}

/* -- items --------------------------------------------------------------- */

static Ast **
genericparams(void) /* the "<" is peeked */
{
  Ast **v = vnew(Ast *, 4);

  next();
  for (;;) {
    Ast *g = mk(Ngparam);

    if (accept(Tdotdotdot)) {
      g->v.gp.pack = 1;
    } else if (peek() == Tconst) { /* a value parameter, an array
                                    * length (08-reflection.md) */
      next();
      g->v.gp.cnst = 1;
      g->v.gp.name = wantident("a generic parameter");
      want(Tcolon, ":");
      g->v.gp.t = type_();
      npush(&v, g);
      if (peek() == Tcomma) {
        next();
        if (peek() == Tgt || peek() == Tshr)
          break;
        continue;
      }
      break;
    }
    g->v.gp.name = wantident("a generic parameter");
    if (accept(Tcolon)) {
      for (;;) {
        Ast *b = typepath(); /* a bound is a path */

        npush(&g->v.gp.bounds, b);
        if (accept(Tplus))
          continue;
        break;
      }
    }
    if (accept(Teq))
      g->v.gp.dflt = type_();
    if (g->v.gp.pack && peek() == Tcomma) /* a pack stands for zero or
                                           * more types and must come
                                           * last (04-generics.md) */
      perr("a pack parameter comes last");
    npush(&v, g);
    if (peek() == Tcomma) {
      next();
      if (peek() == Tgt || peek() == Tshr)
        break;
      continue;
    }
    break;
  }
  if (nextgt() != Tgt)
    perr("expected > closing generic parameters");
  return v;
}

static Ast *
fnitem(Ast **at, int pub) /* the "fn" is peeked; trait and impl
                           * members carry no pub of their own */
{
  Ast *n = mk(Nfn);

  n->attrs = at;
  n->pub = pub;
  next();
  n->v.fn.name = wantident("a function name");
  if (peek() == Tlt)
    n->v.fn.gparams = genericparams();
  want(Tlparen, "(");
  if (peek() != Trparen)
    n->v.fn.params = parameters();
  want(Trparen, ")");
  if (accept(Tarrow))
    n->v.fn.ret = type_();
  if (peek() == Tlbrace)
    n->v.fn.body = block();
  else
    want(Tsemi, "; or a body");
  return n;
}

static Ast *
fieldnode(void) /* attributes [mut] ident : type */
{
  Ast *n = mk(Nfield);

  n->attrs = attrs();
  if (accept(Tmut))
    n->v.variant.mut = 1;
  n->v.variant.name = fieldname("a field name");
  want(Tcolon, ":");
  n->v.variant.t = type_();
  return n;
}

static Ast *
fields(void) /* inside the braces, peeked at "{" */
{
  Ast *n = mk(Nstruct); /* carrier; the caller lifts v.ty.fields */
  Ast *f;

  next();
  while (peek() != Trbrace && peek() != Teof) {
    f = fieldnode();
    npush(&n->v.ty.fields, f);
    if (peek() == Tcomma) {
      next();
      continue;
    }
    break;
  }
  want(Trbrace, "}");
  return n;
}

static Ast *
variants(void)
{
  Ast *n = mk(Nenum); /* carrier; the caller lifts v.en.variants */

  next(); /* { */
  while (peek() != Trbrace && peek() != Teof) {
    Ast *v = mk(Nvariant);

    v->attrs = attrs();
    v->v.variant.name = wantident("a variant name");
    if (accept(Teq)) { /* "= const expr" (08-reflection.md) */
      v->v.variant.hasdisc = 1;
      v->v.variant.discexpr = expr();
    } else if (peek() == Tlparen) { /* positional payload: types */
      next();
      if (peek() != Trparen) {
        for (;;) {
          Ast *t = type_();

          npush(&v->v.variant.payload, t);
          if (peek() == Tcomma) {
            next();
            if (peek() == Trparen)
              break;
            continue;
          }
          break;
        }
      }
      want(Trparen, ")");
    } else if (peek() == Tlbrace) { /* named payload: fields */
      Ast *f;

      v->v.variant.named = 1;
      next();
      while (peek() != Trbrace && peek() != Teof) {
        f = fieldnode();
        npush(&v->v.variant.payload, f);
        if (peek() == Tcomma) {
          next();
          continue;
        }
        break;
      }
      want(Trbrace, "}");
    }
    npush(&n->v.en.variants, v);
    if (peek() == Tcomma) {
      next();
      continue;
    }
    break;
  }
  want(Trbrace, "}");
  return n;
}

static Ast *
traitmember(void)
{
  switch (peek()) {
  case Ttype: { /* type X; */
    Ast *n;

    next();
    n = mk(Ntypedef);
    n->v.td.name = wantident("an associated type name");
    want(Tsemi, ";");
    return n;
  }
  case Tconst: { /* const X: T; */
    Ast *n;

    next();
    n = mk(Nconst);
    n->v.cst.name = wantident("a const name");
    want(Tcolon, ":");
    n->v.cst.t = type_();
    want(Tsemi, ";");
    return n;
  }
  default:
    return fnitem(attrs(), 0);
  }
}

static Ast *
implmember(void)
{
  switch (peek()) {
  case Tconst: { /* const X: T = e; */
    Ast *n;

    next();
    n = mk(Nconst);
    n->v.cst.name = wantident("a const name");
    want(Tcolon, ":");
    n->v.cst.t = type_();
    want(Teq, "=");
    n->v.cst.e = expr();
    want(Tsemi, ";");
    return n;
  }
  case Ttype: { /* type X = T; */
    Ast *n;

    next();
    n = mk(Ntypedef);
    n->v.td.name = wantident("a type name");
    want(Teq, "=");
    n->v.td.t = type_();
    want(Tsemi, ";");
    return n;
  }
  default:
    return fnitem(attrs(), 0);
  }
}

static Ast *
usetree(Ast *n) /* fills v.use; the leading path is next */
{
  Ast *p = mk(Npath);

  p->v.path.root = accept(Tcoloncolon);
  p->v.path.segs = vnew(Ast *, 4);
  for (;;) {
    Ast *s = mk(Nseg);

    s->v.seg.name = wantident("a path segment");
    npush(&p->v.path.segs, s);
    if (peek() != Tcoloncolon)
      break;
    next();                /* "::" */
    if (peek() == Tstar) { /* ::* */
      next();
      n->v.use.path = p;
      n->v.use.star = 1;
      return n;
    }
    if (peek() == Tlbrace) { /* ::{ tree, ... } */
      n->v.use.path = p;
      next();
      while (peek() != Trbrace) {
        Ast *sub = mk(Nuse);

        usetree(sub);
        npush(&n->v.use.subs, sub);
        if (peek() == Tcomma) {
          next();
          if (peek() == Trbrace)
            break; /* the trailing comma is fine */
          continue;
        }
        break;
      }
      want(Trbrace, "}");
      return n;
    }
    /* another segment follows */
  }
  n->v.use.path = p;
  return n;
}

Ast *
parseitem(void)
{
  Ast **at = attrs();
  int   pub;
  Ast  *n;

  pub = accept(Tpub); /* after the attributes (01-types.md:
                       * "#[extern(C)] pub fn triple") */
  switch (peek()) {
  case Tfn:
    return fnitem(at, pub);
  case Tstruct:
  case Tunion: {
    Ast *car;

    n = mk(peek() == Tstruct ? Nstruct : Nunion);
    n->attrs = at;
    n->pub = pub;
    next();
    n->v.ty.name = wantident("a type name");
    if (peek() == Tlt)
      n->v.ty.gparams = genericparams();
    car = fields();
    n->v.ty.fields = car->v.ty.fields;
    return n;
  }
  case Tenum: {
    Ast *car;

    n = mk(Nenum);
    n->attrs = at;
    n->pub = pub;
    next();
    n->v.en.name = wantident("an enum name");
    if (peek() == Tlt)
      n->v.en.gparams = genericparams();
    if (accept(Tlparen)) { /* the tag type: enum X(u32) */
      n->v.en.tag = type_();
      want(Trparen, ")");
    }
    car = variants();
    n->v.en.variants = car->v.en.variants;
    return n;
  }
  case Ttrait: {
    n = mk(Ntrait);
    n->attrs = at;
    n->pub = pub;
    next();
    n->v.ty.name = wantident("a trait name");
    if (peek() == Tlt)
      n->v.ty.gparams = genericparams();
    want(Tlbrace, "{");
    while (peek() != Trbrace && peek() != Teof) {
      Ast *m = traitmember();

      npush(&n->v.ty.members, m);
    }
    want(Trbrace, "}");
    return n;
  }
  case Timpl: {
    n = mk(Nimpl);
    n->attrs = at;
    n->pub = pub;
    next();
    if (peek() == Tlt)
      n->v.impl.gparams = genericparams();
    n->v.impl.path = typepath();
    if (accept(Tfor))
      n->v.impl.fort = type_();
    want(Tlbrace, "{");
    while (peek() != Trbrace && peek() != Teof) {
      Ast *m = implmember();

      npush(&n->v.impl.members, m);
    }
    want(Trbrace, "}");
    return n;
  }
  case Ttype: { /* type X = T; */
    n = mk(Ntypedef);
    n->attrs = at;
    n->pub = pub;
    next();
    n->v.td.name = wantident("a type name");
    if (peek() == Tlt)
      n->v.td.gparams = genericparams();
    want(Teq, "=");
    n->v.td.t = type_();
    want(Tsemi, ";");
    return n;
  }
  case Tuse: {
    n = mk(Nuse);
    n->attrs = at;
    n->pub = pub;
    next();
    usetree(n);
    want(Tsemi, ";");
    return n;
  }
  case Tconst:
    return constitem(at, pub);
  case Tstatic: {
    n = mk(Nstatic);
    n->attrs = at;
    n->pub = pub;
    next();
    if (accept(Tmut))
      n->v.cst.mut = 1;
    n->v.cst.name = wantident("a static name");
    want(Tcolon, ":");
    n->v.cst.t = type_();
    want(Teq, "=");
    n->v.cst.e = expr();
    want(Tsemi, ";");
    return n;
  }
  default:
    perr("expected an item (a declaration)");
  }
  return 0; /* unreachable */
}
