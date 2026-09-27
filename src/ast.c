/* ast.c -- node building and the S-expression dump.
 *
 * The dump is the golden-test contract (tests/parse/ok): one node a
 * line, children indented two under their parent, leaf values inline.
 * Atoms print as themselves: "mut", "const", "let" for the for-head
 * shapes, "_" for a missing range end, "=" before a generic default
 * (it would else read as a bound), "<...>" around a builtin's type
 * arguments (they are types, and a type node may print as a bare
 * path, just like an expression reference).
 */

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "die.h"

/* -- building --------------------------------------------------------- */

/* the arena: nodes and strings are never freed, the process is, so
 * mk/mkstr bump a pointer through big chunks instead of paying a
 * calloc each. 64 KiB a chunk; the last one's slack is lost, which
 * the arena bargain already allows. */
struct chunk
{
  struct chunk *next;
  usize used; /* bytes handed out of mem */
  char mem[64 * 1024];
};
static struct chunk *chunks;

static void *
bump(usize n)
{
  struct chunk *c = chunks;
  char *p;

  n = (n + 7u) & ~(usize) 7u; /* 8-byte alignment: nodes carry
                               * pointers and doubles */
  if (!c || c->used + n > sizeof c->mem) {
    c = malloc(sizeof *c);
    if (!c)
      die("out of memory");
    c->used = 0;
    c->next = chunks;
    chunks = c;
  }
  p = c->mem + c->used;
  c->used += n;
  return p;
}

Ast *
mk(Nk k)
{
  Ast *n = bump(sizeof *n);

  memset(n, 0, sizeof *n); /* the calloc semantics callers rely on:
                            * unset slots read as NULL/0 */
  n->k = k;
  n->line = lexcur()->line;
  n->col = lexcur()->col;
  return n;
}

char *
mkstr(void)
{
  Token *t = lexcur();
  char *s = bump(t->v.str.len + 1);

  memcpy(s, t->v.str.s, t->v.str.len);
  s[t->v.str.len] = 0;
  return s;
}

u64
mknum(void)
{
  return lexcur()->v.num;
}

/* -- the dump --------------------------------------------------------- */

const char *
nkname(Nk k)
{
  static const char *names[] = {
#define X(kind, name) name,
      XYZ_NODES(X)
#undef X
  };
  return (unsigned) k < NK_N ? names[k] : "?";
}

/* the operator of a binop/assign, as spelled */
static const char *
optext(Tok op)
{
  switch (op) {
  case Tplus:
    return "+";
  case Tminus:
    return "-";
  case Tstar:
    return "*";
  case Tslash:
    return "/";
  case Tpercent:
    return "%";
  case Tshl:
    return "<<";
  case Tshr:
    return ">>";
  case Tamp:
    return "&";
  case Tbar:
    return "|";
  case Tcaret:
    return "^";
  case Tlt:
    return "<";
  case Tgt:
    return ">";
  case Tle:
    return "<=";
  case Tge:
    return ">=";
  case Teqeq:
    return "==";
  case Tne:
    return "!=";
  case Tampamp:
    return "&&";
  case Tbarbar:
    return "||";
  case Teq:
    return "=";
  case Tpluseq:
    return "+=";
  case Tminuseq:
    return "-=";
  case Tstareq:
    return "*=";
  case Tslasheq:
    return "/=";
  case Tshleq:
    return "<<=";
  case Tshreq:
    return ">>=";
  case Tbang:
    return "!";
  case Ttilde:
    return "~";
  case Tcaret2:
    return "^^";
  case Tdollar2:
    return "$$";
  default:
    return "?";
  }
}

/* a byte string, escaped with the lexer's own closed set -- shared
 * with the token dump in main.c so the two read alike */
void
dumpstr(const char *s, usize n)
{
  usize i;

  for (i = 0; i < n; i++) {
    unsigned char c = (unsigned char) s[i];

    switch (c) {
    case '\n':
      printf("\\n");
      continue;
    case '\t':
      printf("\\t");
      continue;
    case '\r':
      printf("\\r");
      continue;
    case 0:
      printf("\\0");
      continue;
    case '\'':
      printf("\\'");
      continue;
    case '"':
      printf("\\\"");
      continue;
    case '\\':
      printf("\\\\");
      continue;
    }
    if (c < 0x20 || c > 0x7e)
      printf("\\x%02x", c);
    else
      putchar(c);
  }
}

static void dumpnode(Ast *n, int ind);

static void
ind(int n)
{
  while (n-- > 0)
    putchar(' ');
}

/* C89 printf has no %llu; the digits are spelled out */
void
dumpu64(u64 v)
{
  char d[20]; /* 2^64-1 is 20 digits */
  int n = 0;

  do {
    d[n++] = (char) ('0' + (int) (v % 10));
    v /= 10;
  } while (v != 0);
  while (n > 0)
    putchar(d[--n]);
}

static void
dumpu64sp(u64 v) /* with the leading space */
{
  putchar(' ');
  dumpu64(v);
}

/* a child on its own line; NULL prints nothing -- optional slots
 * (an else, a tail expression, a type annotation) vanish silently */
static void
child(Ast *n, int i)
{
  if (!n)
    return;
  putchar('\n');
  dumpnode(n, i + 2);
}

/* a placeholder slot: NULL prints "_", a missing range end */
static void
opt(Ast *n, int i)
{
  if (!n) {
    printf(" _");
    return;
  }
  putchar('\n');
  dumpnode(n, i + 2);
}

/* the STRF_* flags, as "c", "r", "ml" */
static void
dumpflags(unsigned f)
{
  if (f & STRF_C)
    printf(" c");
  if (f & STRF_RAW)
    printf(" r");
  if (f & STRF_ML)
    printf(" ml");
}

/* a path: bare segments inline, [::] first when rooted; a segment
 * with generic args is (seg name args...) */
static void
dumppath(Ast *n, int i)
{
  Ast **s;
  usize j;

  printf("(path");
  if (n->v.path.root)
    printf(" ::");
  for (s = n->v.path.segs, j = 0; s && j < vlen(s); j++) {
    Ast *seg = s[j];

    if (seg->v.seg.args) {
      child(seg, i);
    } else {
      printf(" %s", seg->v.seg.name);
    }
  }
  printf(")");
}

/* attrs inline, each (attr name args...) -- literal arguments stay
 * on the line; called at the head of every arm that carries them */
static void
putattrs(Ast *n)
{
  Ast **a;
  usize i;

  for (a = n->attrs, i = 0; a && i < vlen(a); i++) {
    Ast *at = a[i];
    Ast **args;
    usize j;

    printf(" (attr %s", at->v.seg.name);
    for (args = at->v.seg.args, j = 0; args && j < vlen(args); j++) {
      putchar(' ');
      dumpnode(args[j], 0); /* literals: inline, one line */
    }
    printf(")");
  }
}

static void
dumpnode(Ast *n, int i)
{
  Ast **v;
  usize j;

  if (!n) {
    printf("_");
    return;
  }
  ind(i);
  if (n->k == Npath) { /* dumppath owns the "(path" paren */
    dumppath(n, i);
    return;
  }
  printf("(%s", nkname(n->k));
  switch (n->k) {
  case Nint:
    dumpu64sp(n->v.i.num);
    break;
  case Nbool:
    printf(" %s", n->v.i.num ? "true" : "false");
    break;
  case Nflt:
    printf(" %g", n->v.f.flt);
    break;
  case Nbyte:
    printf(" '");
    dumpstr(n->v.s.s, 1);
    putchar('\'');
    break;
  case Nstr:
    printf(" %lu \"", (unsigned long) n->v.s.len);
    dumpstr(n->v.s.s, n->v.s.len);
    putchar('"');
    dumpflags(n->v.s.flags);
    break;
  case Nunit:
  case Nbreak:
  case Ncontinue:
  case Npwild:
  case Nttype:
    break;
  case Ntuple:
  case Npor:
  case Nptuple:
  case Nbarestructlit:
    for (v = n->v.list.ts, j = 0; v && j < vlen(v); j++)
      child(v[j], i);
    break;
  case Nseg: /* only reached as a generic-args carrier */
    printf(" %s", n->v.seg.name);
    for (v = n->v.seg.args, j = 0; v && j < vlen(v); j++)
      child(v[j], i);
    break;
  case Nbin:
  case Nassign:
    printf(" %s", optext(n->v.bin.op));
    child(n->v.bin.l, i);
    child(n->v.bin.r, i);
    break;
  case Nrange:
    child(n->v.bin.l, i);
    child(n->v.bin.r, i);
    break;
  case Nun:
    printf(" %s", n->v.un.mut ? "&mut" : optext(n->v.un.op));
    child(n->v.un.e, i);
    break;
  case Nspread:
    child(n->v.un.e, i);
    break;
  case Ncall:
    child(n->v.call.f, i);
    for (v = n->v.call.args, j = 0; v && j < vlen(v); j++)
      child(v[j], i);
    break;
  case Nindex:
    child(n->v.n2.a, i);
    child(n->v.n2.b, i);
    break;
  case Nrangeindex:
    child(n->v.ridx.e, i);
    opt(n->v.ridx.lo, i);
    opt(n->v.ridx.hi, i);
    break;
  case Naccess:
    child(n->v.fld.e, i);
    printf(" %s", n->v.fld.name);
    break;
  case Ntupidx:
    child(n->v.tup.e, i);
    dumpu64sp(n->v.tup.idx);
    break;
  case Ntry:
  case Nreturn:
  case Nexprstmt:
  case Ntopt:
    child(n->v.n1.e, i);
    break;
  case Nif:
  case Ncif:
    child(n->v.ifx.cond, i);
    child(n->v.ifx.then, i);
    child(n->v.ifx.els, i);
    break;
  case Nmatch:
    child(n->v.call.f, i);
    for (v = n->v.call.args, j = 0; v && j < vlen(v); j++)
      child(v[j], i);
    break;
  case Narm:
    child(n->v.n2.a, i);
    child(n->v.n2.b, i);
    break;
  case Nblock:
    for (v = n->v.blk.stmts, j = 0; v && j < vlen(v); j++)
      child(v[j], i);
    child(n->v.blk.tail, i);
    break;
  case Narraylit:
    opt(n->v.arrlit.len, i);
    if (n->v.arrlit.mut)
      printf(" mut");
    child(n->v.arrlit.t, i);
    for (v = n->v.arrlit.es, j = 0; v && j < vlen(v); j++)
      child(v[j], i);
    break;
  case Nstructlit:
    child(n->v.slit.path, i);
    for (v = n->v.slit.inits, j = 0; v && j < vlen(v); j++)
      child(v[j], i);
    break;
  case Ninit:
  case Npfield:
    printf(" %s", n->v.init.name);
    child(n->v.init.e, i);
    break;
  case Nclosure:
    if (!n->v.clos.caps || !vlen(n->v.clos.caps))
      printf(" ()");
    for (v = n->v.clos.caps, j = 0; v && j < vlen(v); j++)
      child(v[j], i);
    if (!n->v.clos.params || !vlen(n->v.clos.params))
      printf(" ()");
    for (v = n->v.clos.params, j = 0; v && j < vlen(v); j++)
      child(v[j], i);
    child(n->v.clos.ret, i);
    child(n->v.clos.body, i);
    break;
  case Ncap:
    printf(" %s%s", n->v.cap.byref ? (n->v.cap.mut ? "&mut " : "&") : (n->v.cap.mut ? "mut " : ""),
           n->v.cap.name);
    break;
  case Nbuiltin:
    printf(" @%s", n->v.blt.name);
    if (n->v.blt.targs)
      printf(" <");
    for (v = n->v.blt.targs, j = 0; v && j < vlen(v); j++) {
      if (j > 0)
        putchar(' ');
      dumpnode(v[j], 0); /* types print inline */
    }
    if (n->v.blt.targs)
      printf(">");
    for (v = n->v.blt.args, j = 0; v && j < vlen(v); j++)
      child(v[j], i);
    break;
  case Ntresult:
    child(n->v.n2.a, i);
    child(n->v.n2.b, i);
    break;
  case Ntptr:
    if (n->v.un.mut)
      printf(" mut");
    child(n->v.un.e, i);
    break;
  case Ntarray:
    opt(n->v.arrlit.len, i);
    if (n->v.arrlit.mut)
      printf(" mut");
    child(n->v.arrlit.t, i);
    break;
  case Nttuple:
    for (v = n->v.list.ts, j = 0; v && j < vlen(v); j++)
      child(v[j], i);
    break;
  case Ntfn:
    if (!n->v.fnty.args || !vlen(n->v.fnty.args))
      printf(" ()");
    for (v = n->v.fnty.args, j = 0; v && j < vlen(v); j++)
      child(v[j], i);
    child(n->v.fnty.ret, i);
    break;
  case Ntdyn:
    child(n->v.n1.e, i);
    break;
  case Nfn:
    putattrs(n);
    printf(" %s", n->v.fn.name);
    for (v = n->v.fn.gparams, j = 0; v && j < vlen(v); j++)
      child(v[j], i);
    if (!n->v.fn.params || !vlen(n->v.fn.params))
      printf(" ()");
    for (v = n->v.fn.params, j = 0; v && j < vlen(v); j++)
      child(v[j], i);
    child(n->v.fn.ret, i);
    child(n->v.fn.body, i);
    break;
  case Nstruct:
  case Nunion:
  case Ntrait:
    putattrs(n);
    printf(" %s", n->v.ty.name);
    for (v = n->v.ty.gparams, j = 0; v && j < vlen(v); j++)
      child(v[j], i);
    for (v = n->k == Ntrait ? n->v.ty.members : n->v.ty.fields, j = 0; v && j < vlen(v); j++)
      child(v[j], i);
    break;
  case Nenum:
    putattrs(n);
    printf(" %s", n->v.en.name);
    for (v = n->v.en.gparams, j = 0; v && j < vlen(v); j++)
      child(v[j], i);
    for (v = n->v.en.variants, j = 0; v && j < vlen(v); j++)
      child(v[j], i);
    break;
  case Nvariant:
    putattrs(n);
    printf(" %s", n->v.variant.name);
    if (n->v.variant.hasdisc)
      dumpu64sp(n->v.variant.disc);
    for (v = n->v.variant.payload, j = 0; v && j < vlen(v); j++)
      child(v[j], i);
    break;
  case Nfield:
    putattrs(n);
    if (n->v.variant.mut)
      printf(" mut");
    printf(" %s", n->v.variant.name);
    child(n->v.variant.t, i);
    break;
  case Nimpl:
    putattrs(n);
    for (v = n->v.impl.gparams, j = 0; v && j < vlen(v); j++)
      child(v[j], i);
    child(n->v.impl.path, i);
    if (n->v.impl.fort) {
      printf(" (for");
      child(n->v.impl.fort, i + 2);
      printf(")");
    }
    for (v = n->v.impl.members, j = 0; v && j < vlen(v); j++)
      child(v[j], i);
    break;
  case Ntypedef:
    putattrs(n);
    printf(" %s", n->v.td.name);
    for (v = n->v.td.gparams, j = 0; v && j < vlen(v); j++)
      child(v[j], i);
    child(n->v.td.t, i);
    break;
  case Nuse:
    putchar(' ');
    dumppath(n->v.use.path, i);
    if (n->v.use.star)
      printf(" *");
    for (v = n->v.use.subs, j = 0; v && j < vlen(v); j++)
      child(v[j], i);
    break;
  case Nconst:
  case Nstatic:
    putattrs(n);
    if (n->k == Nstatic && n->v.cst.mut)
      printf(" mut");
    printf(" %s", n->v.cst.name);
    child(n->v.cst.t, i);
    child(n->v.cst.e, i);
    break;
  case Nattr:
    printf(" %s", n->v.seg.name);
    for (v = n->v.seg.args, j = 0; v && j < vlen(v); j++)
      child(v[j], i);
    break;
  case Nparam:
    putattrs(n);
    if (n->v.param.cnst)
      printf(" const");
    if (n->v.param.mut)
      printf(" mut");
    printf(" %s", n->v.param.name);
    child(n->v.param.t, i);
    break;
  case Ngparam:
    if (n->v.gp.pack)
      printf(" ...");
    printf(" %s", n->v.gp.name);
    for (v = n->v.gp.bounds, j = 0; v && j < vlen(v); j++)
      child(v[j], i);
    if (n->v.gp.dflt)
      printf(" =");
    child(n->v.gp.dflt, i);
    break;
  case Nlet:
    if (n->v.let.mut)
      printf(" mut");
    child(n->v.let.pat, i);
    child(n->v.let.t, i);
    child(n->v.let.e, i);
    break;
  case Nfor:
  case Ncfor:
    if (n->v.forx.shape == FLET)
      printf(" let");
    else if (n->v.forx.shape == FIN)
      printf(" in");
    child(n->v.forx.a, i);
    child(n->v.forx.b, i);
    child(n->v.forx.body, i);
    break;
  case Nppath:
    putchar(' ');
    dumppath(n->v.ppath.path, i);
    for (v = n->v.ppath.payload, j = 0; v && j < vlen(v); j++)
      child(v[j], i);
    if (n->v.ppath.rest)
      printf(" ..");
    break;
  case Npstruct:
    for (v = n->v.pstruct.fields, j = 0; v && j < vlen(v); j++)
      child(v[j], i);
    if (n->v.pstruct.rest)
      printf(" ..");
    break;
  default:
    die("internal: dump: unhandled kind %s", nkname(n->k));
  }
  printf(")");
}

void
dumpast(Ast *n)
{
  dumpnode(n, 0);
  putchar('\n');
}
