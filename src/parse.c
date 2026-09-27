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
  Token *t = lexcur();

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
npush(Node ***vp, Node *n)
{
  if (!*vp)
    *vp = vnew(Node *, 8);
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

/* -- attributes -------------------------------------------------------- */

static Node *
attrarg(void)
{
  Node *n;

  switch (peek()) {
  case Tident: {
    next();
    n = mk(Npath);
    n->v.path.segs = vnew(Node *, 2);
    {
      Node *s = mk(Nseg);
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

static Node **
attrs(void)
{
  Node **v = 0;

  while (peek() == Thashlbracket) {
    Node *a;

    next(); /* "#[" */
    a = mk(Nattr);
    a->v.seg.name = wantident("attribute name");
    if (accept(Tlparen)) {
      if (peek() != Trparen) {
        for (;;) {
          Node *g = attrarg();
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
    want(Trbracket, "]");
    npush(&v, a);
  }
  return v;
}

/* -- types -------------------------------------------------------------- */

static Node *type_(void);
static Node *expr(void);
static Node *postfix(void);
static Node *primary(void);
static Node *orexpr(void);
static Node *pattern(void);
static Node *block(void);
static Node *statement(void);
static Node **parameters(void);

/* a path in type position: "<" after a segment is generic arguments,
 * unconditionally -- a type grammar has no comparison to be */
static Node *
typepath(void)
{
  Node *n = mk(Npath);

  n->v.path.root = accept(Tcoloncolon);
  n->v.path.segs = vnew(Node *, 4);
  for (;;) {
    Node *s = mk(Nseg);

    s->v.seg.name = wantident("a path segment");
    if (peek() == Tlt) {
      next();
      for (;;) {
        Node *a;

        if (peek() == Tdollar2 || peek() == Tcaret2) {
          Tok op = next();

          a = mk(Nun);
          a->v.un.op = op;
          a->v.un.e = postfix();
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

static Node *
prefixtype(void)
{
  switch (peek()) {
  case Tquestion: {
    Node *n;

    next();
    n = mk(Ntopt);
    n->v.n1.e = prefixtype();
    return n;
  }
  case Tstar: {
    Node *n;

    next();
    n = mk(Ntptr);
    if (accept(Tmut))
      n->v.un.mut = 1;
    n->v.un.e = prefixtype();
    return n;
  }
  case Tlbracket: { /* [ [integer] ] [mut] T */
    Node *n;

    next();
    n = mk(Ntarray);
    if (peek() == Tint) {
      Node *l;

      next();
      l = mk(Nint);
      l->v.i.num = mknum();
      n->v.arrlit.len = l;
    }
    want(Trbracket, "]");
    if (accept(Tmut))
      n->v.arrlit.mut = 1;
    n->v.arrlit.t = prefixtype();
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
  case Tlparen: { /* () or a tuple type */
    Node *n;

    next();
    if (accept(Trparen))
      return mk(Nunit);
    n = mk(Nttuple);
    n->v.list.ts = vnew(Node *, 4);
    for (;;) {
      Node *t = type_();

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
    Node *n;

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
    Node *n;

    next();
    n = mk(Ntdyn);
    n->v.n1.e = typepath();
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

static Node *
type_(void) /* result_type: prefix [ "?" prefix ] */
{
  Node *t = prefixtype();

  if (accept(Tquestion)) {
    Node *n = mk(Ntresult);

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
static Node **
tryargs(void)
{
  LexSnap *snap;
  Node **args = 0;
  Tok after;

  snap = lexsnap();
  next();               /* "<" */
  if (!startsatype()) { /* `a < 10`: a comparison from the start */
    lexunsnap(snap);
    return 0;
  }
  for (;;) {
    Node *a;

    if (peek() == Tdollar2 || peek() == Tcaret2) {
      Tok op = next();

      a = mk(Nun);
      a->v.un.op = op;
      a->v.un.e = postfix();
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
static Node *
exprpath(void)
{
  Node *n = mk(Npath);

  n->v.path.root = accept(Tcoloncolon);
  n->v.path.segs = vnew(Node *, 4);
  for (;;) {
    Node *s = mk(Nseg);

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

static Node *
ifexpr(int cnst)
{
  Node *n = mk(cnst ? Ncif : Nif);

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

static Node *
matchexpr(void)
{
  Node *n = mk(Nmatch);

  next();               /* match */
  headctx = 1;          /* the trailing "{" opens the arms */
  n->v.call.f = expr(); /* the scrutinee */
  headctx = 0;
  want(Tlbrace, "{");
  while (peek() != Trbrace) {
    Node *a = mk(Narm);

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

static Node *
rangeexpr(void)
{
  Node *l = orexpr();

  if (peek() == Tdotdot) {
    Node *n;

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

static Node *
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

static Node *orexpr(void);
static Node *andexpr(void);
static Node *cmpexpr(void);
static Node *bitorexpr(void);
static Node *bitxorexpr(void);
static Node *bitandexpr(void);
static Node *shiftexpr(void);
static Node *addexpr(void);
static Node *mulexpr(void);
static Node *unary(void);

static int
isany(Tok t, Tok a, Tok b)
{
  return t == a || t == b;
}

#define BINLEVEL(name, sub, isop)                                                                  \
  static Node *name(void)                                                                          \
  {                                                                                                \
    Node *l = sub();                                                                               \
    for (;;) {                                                                                     \
      Tok op = peek();                                                                             \
      if (isop(op)) {                                                                              \
        Node *n;                                                                                   \
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

static Node *
cmpexpr(void) /* no chaining: one operator, at most */
{
  Node *l = bitorexpr();
  Tok op = peek();

  switch (op) {
  case Tlt:
  case Tgt:
  case Tle:
  case Tge:
  case Teqeq:
  case Tne: {
    Node *n;

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

static Node *
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
    Tok op = next();
    Node *n = mk(Nun);

    n->v.un.op = op;
    if (op == Tamp && accept(Tmut))
      n->v.un.mut = 1;
    n->v.un.e = unary();
    return n;
  }
  default:
    return postfix();
  }
}

static Node *
callargs(Node *n) /* the "(" is consumed; fills n->v.call.args */
{
  int save = headctx;

  headctx = 0; /* inside the argument list, a "{ " is a literal */
  if (peek() != Trparen) {
    for (;;) {
      Node *a;

      if (peek() == Tdotdotdot) {
        next();
        a = mk(Nspread);
        a->v.un.e = expr();
      } else {
        a = expr();
      }
      npush(&n->v.call.args, a);
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

static Node *
postfix(void)
{
  Node *e = primary();

  for (;;) {
    switch (peek()) {
    case Tdot: {
      next();
      if (peek() == Tint) {
        Node *n;

        next();
        n = mk(Ntupidx);
        n->v.tup.e = e;
        n->v.tup.idx = mknum();
        e = n;
      } else {
        Node *n = mk(Naccess);

        n->v.fld.e = e;
        n->v.fld.name = wantident("a field name");
        e = n;
      }
      continue;
    }
    case Tlbracket: { /* index, or a range index */
      int save = headctx;

      headctx = 0; /* inside the brackets, a "{ " is a literal again */
      next();
      if (peek() == Tdotdot) { /* [..hi] or [..] */
        Node *n = mk(Nrangeindex);

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
        Node *ix = orexpr(); /* not expr(): a ".." here closes the
                              * range, it is not a range operator */
        Node *n;

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
      Node *n = mk(Ncall);

      next();
      n->v.call.f = e;
      e = callargs(n);
      continue;
    }
    case Tquestion: {
      Node *n;

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

static Node *
closure(void) /* the "fn" is peeked */
{
  Node *n = mk(Nclosure);

  next(); /* fn */
  want(Tlbracket, "[");
  if (peek() != Trbracket) {
    for (;;) {
      Node *c = mk(Ncap);

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

static Node *
builtin(void) /* the "@" is peeked */
{
  Node *n = mk(Nbuiltin);

  next(); /* @ */
  n->v.blt.name = wantident("a builtin name");
  if (peek() == Tlt) {
    next();
    for (;;) {
      Node *a;

      if (peek() == Tdollar2 || peek() == Tcaret2) {
        Tok op = next();

        a = mk(Nun);
        a->v.un.op = op;
        a->v.un.e = postfix();
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
  callargs(n); /* shares the argument shape with a call */
  return n;
}

static Node *
arraylit(void) /* the "[" is peeked */
{
  Node *n = mk(Narraylit);
  int save = headctx;

  headctx = 0; /* inside the literal, a "{ " is a literal again */
  next();
  if (peek() == Tint) {
    Node *l;

    next();
    l = mk(Nint);
    l->v.i.num = mknum();
    n->v.arrlit.len = l;
  }
  want(Trbracket, "]");
  if (accept(Tmut))
    n->v.arrlit.mut = 1;
  n->v.arrlit.t = type_();
  want(Tlbrace, "{");
  if (peek() != Trbrace) {
    for (;;) {
      Node *e = expr();

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

static Node *
fieldinits(Node *n, Node ***slot) /* "{ f: e, ... }" is peeked; the
                                   * vector slot is the caller's to name */
{
  int save = headctx;

  headctx = 0; /* inside the braces, a "{ " is a literal again */
  next();      /* { */
  if (peek() != Trbrace) {
    for (;;) {
      Node *fi = mk(Ninit);

      fi->v.init.name = wantident("a field name");
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

static Node *
primary(void)
{
  switch (peek()) {
  case Ttrue:
  case Tfalse: {
    Node *n;

    n = mk(Nbool);
    n->v.i.num = peek() == Ttrue;
    next();
    return n;
  }
  case Tint: {
    Node *n;

    next();
    n = mk(Nint);
    n->v.i.num = mknum();
    return n;
  }
  case Tflt: {
    Node *n;

    next();
    n = mk(Nflt);
    n->v.f.flt = lexcur()->v.flt;
    return n;
  }
  case Tbyte: {
    Node *n;

    next();
    n = mk(Nbyte);
    n->v.s.s = mkstr();
    n->v.s.len = 1;
    return n;
  }
  case Tstr: {
    Node *n;

    next();
    n = mk(Nstr);
    n->v.s.s = mkstr();
    n->v.s.len = lexcur()->v.str.len;
    n->v.s.flags = lexcur()->v.str.flags;
    return n;
  }
  case Tident:
  case Tcoloncolon: {
    Node *p = exprpath();

    if (peek() == Tlbrace && !headctx) { /* struct literal */
      Node *n = mk(Nstructlit);

      n->v.slit.path = p;
      return fieldinits(n, &n->v.slit.inits);
    }
    return p;
  }
  case Tlparen: {
    Node *n;
    int save = headctx;

    headctx = 0; /* inside the parens, a "{ " is a literal again */
    next();
    if (accept(Trparen)) {
      headctx = save;
      return mk(Nunit);
    }
    {
      Node *e1 = expr();

      if (peek() == Tcomma) { /* a tuple */
        n = mk(Ntuple);
        n->v.list.ts = vnew(Node *, 4);
        npush(&n->v.list.ts, e1);
        while (accept(Tcomma)) {
          if (peek() == Trparen)
            break;
          {
            Node *e = expr();

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
    Node *n = mk(Nbarestructlit);

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

static Node **
parameters(void)
{
  Node **v = vnew(Node *, 4);

  for (;;) {
    Node *p = mk(Nparam);

    p->attrs = attrs();
    if (accept(Tconst))
      p->v.param.cnst = 1;
    if (accept(Tmut))
      p->v.param.mut = 1;
    p->v.param.name = wantident("a parameter name");
    want(Tcolon, ":");
    p->v.param.t = type_();
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

static Node *
letstmt(void)
{
  Node *n = mk(Nlet);

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

static Node *
jumpstmt(void)
{
  Node *n;

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

static Node *
forstmt(int cnst)
{
  Node *n = mk(cnst ? Ncfor : Nfor);

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
      Node *e = expr();

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

static Node *
constitem(Node **at) /* the "const" is peeked; block-level has no attrs */
{
  Node *n = mk(Nconst);

  n->attrs = at;
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

static Node *
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
    return constitem(0);
  }
  default:
    perr("internal: statement dispatch");
  }
  return 0; /* unreachable */
}

static Node *
block(void)
{
  Node *n = mk(Nblock);
  int done = 0;

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
      Node *e = expr();

      if (isassignop(peek())) {
        Node *a = mk(Nassign);

        a->v.bin.op = next();
        a->v.bin.l = e;
        a->v.bin.r = expr();
        want(Tsemi, ";");
        npush(&n->v.blk.stmts, a);
        continue;
      }
      if (peek() == Tsemi) {
        Node *s = mk(Nexprstmt);

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

static Node *
pfield(void)
{
  Node *n = mk(Npfield);

  n->v.init.name = wantident("a field");
  if (accept(Tcolon))
    n->v.init.e = pattern();
  return n;
}

static Node *
unitpattern(void)
{
  if (peek() == Tident && strcmp(lexcur()->v.str.s, "_") == 0) {
    next();
    return mk(Npwild);
  }
  switch (peek()) {
  case Tident:
  case Tcoloncolon: {
    Node *n = mk(Nppath);

    n->v.ppath.path = typepath();
    if (peek() == Tlparen) { /* positional payload */
      next();
      if (peek() != Trparen) {
        for (;;) {
          Node *p = pattern();

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
          Node *f = pfield();

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
    Node *n = mk(Nptuple);

    next();
    if (peek() != Trparen) {
      for (;;) {
        Node *p = pattern();

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
    Node *n = mk(Npstruct);

    next();
    if (peek() == Tdotdot) {
      next();
      n->v.pstruct.rest = 1;
    } else if (peek() != Trbrace) {
      for (;;) {
        Node *f = pfield();

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

static Node *
pattern(void)
{
  Node *u = unitpattern();

  if (peek() != Tbar)
    return u;
  {
    Node *n = mk(Npor);

    n->v.list.ts = vnew(Node *, 4);
    npush(&n->v.list.ts, u);
    while (accept(Tbar)) {
      Node *v = unitpattern();

      npush(&n->v.list.ts, v);
    }
    return n;
  }
}

/* -- items --------------------------------------------------------------- */

static Node **
genericparams(void) /* the "<" is peeked */
{
  Node **v = vnew(Node *, 4);

  next();
  for (;;) {
    Node *g = mk(Ngparam);

    if (accept(Tdotdotdot))
      g->v.gp.pack = 1;
    g->v.gp.name = wantident("a generic parameter");
    if (accept(Tcolon)) {
      for (;;) {
        Node *b = typepath(); /* a bound is a path */

        npush(&g->v.gp.bounds, b);
        if (accept(Tplus))
          continue;
        break;
      }
    }
    if (accept(Teq))
      g->v.gp.dflt = type_();
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

static Node *
fnitem(Node **at) /* the "fn" is peeked */
{
  Node *n = mk(Nfn);

  n->attrs = at;
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

static Node *
fieldnode(void) /* attributes [mut] ident : type */
{
  Node *n = mk(Nfield);

  n->attrs = attrs();
  if (accept(Tmut))
    n->v.variant.mut = 1;
  n->v.variant.name = wantident("a field name");
  want(Tcolon, ":");
  n->v.variant.t = type_();
  return n;
}

static Node *
fields(void) /* inside the braces, peeked at "{" */
{
  Node *n = mk(Nstruct); /* carrier; the caller lifts v.ty.fields */
  Node *f;

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

static Node *
variants(void)
{
  Node *n = mk(Nenum); /* carrier; the caller lifts v.en.variants */

  next(); /* { */
  while (peek() != Trbrace && peek() != Teof) {
    Node *v = mk(Nvariant);

    v->attrs = attrs();
    v->v.variant.name = wantident("a variant name");
    if (accept(Teq)) {
      if (peek() != Tint)
        perr("expected an integer after =");
      next();
      v->v.variant.hasdisc = 1;
      v->v.variant.disc = mknum();
    } else if (peek() == Tlparen) { /* positional payload: types */
      next();
      if (peek() != Trparen) {
        for (;;) {
          Node *t = type_();

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
      Node *f;

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

static Node *
traitmember(void)
{
  switch (peek()) {
  case Ttype: { /* type X; */
    Node *n;

    next();
    n = mk(Ntypedef);
    n->v.td.name = wantident("an associated type name");
    want(Tsemi, ";");
    return n;
  }
  case Tconst: { /* const X: T; */
    Node *n;

    next();
    n = mk(Nconst);
    n->v.cst.name = wantident("a const name");
    want(Tcolon, ":");
    n->v.cst.t = type_();
    want(Tsemi, ";");
    return n;
  }
  default:
    return fnitem(attrs());
  }
}

static Node *
implmember(void)
{
  switch (peek()) {
  case Tconst: { /* const X: T = e; */
    Node *n;

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
    Node *n;

    next();
    n = mk(Ntypedef);
    n->v.td.name = wantident("a type name");
    want(Teq, "=");
    n->v.td.t = type_();
    want(Tsemi, ";");
    return n;
  }
  default:
    return fnitem(attrs());
  }
}

static Node *
usetree(Node *n) /* fills v.use; the leading path is next */
{
  Node *p = mk(Npath);

  p->v.path.root = accept(Tcoloncolon);
  p->v.path.segs = vnew(Node *, 4);
  for (;;) {
    Node *s = mk(Nseg);

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
        Node *sub = mk(Nuse);

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

Node *
parseitem(void)
{
  Node **at = attrs();
  Node *n;

  switch (peek()) {
  case Tfn:
    return fnitem(at);
  case Tstruct:
  case Tunion: {
    Node *car;

    n = mk(peek() == Tstruct ? Nstruct : Nunion);
    n->attrs = at;
    next();
    n->v.ty.name = wantident("a type name");
    if (peek() == Tlt)
      n->v.ty.gparams = genericparams();
    car = fields();
    n->v.ty.fields = car->v.ty.fields;
    return n;
  }
  case Tenum: {
    Node *car;

    n = mk(Nenum);
    n->attrs = at;
    next();
    n->v.en.name = wantident("an enum name");
    if (peek() == Tlt)
      n->v.en.gparams = genericparams();
    car = variants();
    n->v.en.variants = car->v.en.variants;
    return n;
  }
  case Ttrait: {
    n = mk(Ntrait);
    n->attrs = at;
    next();
    n->v.ty.name = wantident("a trait name");
    if (peek() == Tlt)
      n->v.ty.gparams = genericparams();
    want(Tlbrace, "{");
    while (peek() != Trbrace && peek() != Teof) {
      Node *m = traitmember();

      npush(&n->v.ty.members, m);
    }
    want(Trbrace, "}");
    return n;
  }
  case Timpl: {
    n = mk(Nimpl);
    n->attrs = at;
    next();
    if (peek() == Tlt)
      n->v.impl.gparams = genericparams();
    n->v.impl.path = typepath();
    if (accept(Tfor))
      n->v.impl.fort = type_();
    want(Tlbrace, "{");
    while (peek() != Trbrace && peek() != Teof) {
      Node *m = implmember();

      npush(&n->v.impl.members, m);
    }
    want(Trbrace, "}");
    return n;
  }
  case Ttype: { /* type X = T; */
    n = mk(Ntypedef);
    n->attrs = at;
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
    next();
    usetree(n);
    want(Tsemi, ";");
    return n;
  }
  case Tconst:
    return constitem(at);
  case Tstatic: {
    n = mk(Nstatic);
    n->attrs = at;
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
