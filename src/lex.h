/* lex.h -- the lexical grammar, single-pass streaming.
 *
 * The design follows qbe's parse.c: one token slot, one lookahead
 * (peek/next), a growable buffer reused across tokens, and errors that
 * stop the compiler on the spot. No token vector, no whole-file buffer;
 * the file is read as a stream.
 *
 * The token set is specs/15-grammar.md's "Punctuation" production,
 * verbatim. Longest match applies throughout: "::" is never two ":",
 * "a..b" reads a, "..", b.
 *
 * Contract: ">>" always lexes as one token. The parser splits it into
 * two ">" only at a generic-arguments closing (05-traits.md) -- the
 * lexer never decides this.
 *
 * "#" has exactly one destination: "#[" as a single token. A "#" not
 * followed by "[" is a lexical error.
 */

#ifndef LEX_H
#define LEX_H

/* die.h: die() -- one exit path for the whole compiler */
#include "die.h"

#include "vec.h"

/* C89 has no long long; every compiler we build with (gcc, clang)
 * ships it as an extension, and __extension__ keeps -pedantic quiet. */
__extension__ typedef unsigned long long u64;

/* the token list, one place: the enum and the name table share it.
 * clang-format mangles a backslash macro this wide into a staircase
 * it then disagrees with itself about; the rows are hand-grouped. */
/* clang-format off */
#define XYZ_TOKS(X)                                                          \
  X(Txxx) /* empty slot; zero-initialized state is legal */                  \
  X(Teof) /* end of file; next() at Teof stays at Teof */                    \
  X(Tint) /* v.num; unsigned, a sign is an operator elsewhere */             \
  X(Tflt) /* v.flt; decimals only, digits on both sides of the dot */        \
  X(Tbyte) /* v.str; one byte, '\xNN' takes any value through 0xff */        \
  X(Tstr) /* v.str; v.str.flags carry the c/r/multiline forms */             \
  X(Tident) /* v.str; c, r, cr followed by '"' are prefixes, not idents */   \
  /* keywords: 22 + 2 reserved (specs/15-grammar.md). The table there\n   * omits else, but the if production needs it as a keyword -- a spec\n   * gap, noted in the PR. */                      \
  X(Tfn) X(Tstruct) X(Tenum) X(Tunion) X(Ttrait) X(Timpl) X(Ttype) X(Tuse)   \
  X(Tpub) X(Tlet) X(Tconst) X(Tstatic) X(Tmut) X(Tdyn) X(Ttrue) X(Tfalse)    \
  X(Tif) X(Telse) X(Tmatch) X(Tfor) X(Tin) X(Tbreak) X(Tcontinue)              \
  X(Treturn)                                                                  \
  X(Tmacro) X(Tdefer) /* reserved, unused */                                 \
  /* punctuation */                                                          \
  X(Tlparen) X(Trparen) X(Tlbracket) X(Trbracket) X(Tlbrace) X(Trbrace)      \
  X(Tcomma) X(Tsemi) X(Tcolon) X(Tcoloncolon) X(Tdot) X(Tdotdot)             \
  X(Tdotdotdot) X(Tarrow) X(Tfatarrow) X(Tquestion) X(Tat) X(Tdollar2)       \
  X(Tcaret2)                                                                   \
  X(Thashlbracket) X(Tplus) X(Tminus) X(Tstar) X(Tslash) X(Tpercent)         \
  X(Ttilde) X(Tcaret) X(Tamp) X(Tbar) X(Tbang) X(Tshl) X(Tshr) X(Tlt)        \
  X(Tgt) X(Tle) X(Tge) X(Teqeq) X(Tne) X(Tampamp) X(Tbarbar) X(Teq)          \
  X(Tpluseq) X(Tminuseq) X(Tstareq) X(Tslasheq) X(Tshleq) X(Tshreq)
/* clang-format on */

typedef enum
{
#define X(name) name,
  XYZ_TOKS(X)
#undef X
      TOKKIND_N
} Tok;

/* string literal form flags -- a prefix is part of the literal only
 * when it touches it: an identifier c, r, or cr immediately followed
 * by '"' is the prefixed literal, and that is the whole rule. */
enum
{
  STRF_C = 1,   /* c: the value ends with a '\0' */
  STRF_RAW = 2, /* r: every '\' is a plain byte, no escapes */
  STRF_ML = 4   /* """ multiline: the closing indentation is stripped */
};

typedef struct Token Token;
struct Token
{
  Tok      t;         /* Txxx until peek/next fills it */
  unsigned line, col; /* 1-based; a literal reports where it opens */
  union
  {
    u64    num; /* Tint */
    double flt; /* Tflt */
    struct
    {
      char    *s;     /* in the lexer's reused buffer, NUL-terminated */
      usize    len;   /* value bytes; STRF_C's terminator is counted */
      unsigned flags; /* STRF_* */
    } str;            /* Tstr, Tbyte, Tident */
  } v;
};

/* one lexer per process, like qbe: the state is static in lex.c */
void        lexinit(const char *path); /* NULL reads stdin */
const char *lexpath(void);

Tok    peek(void);   /* look at the next token without consuming */
Tok    next(void);   /* consume it; the token lands in lexcur() */
Token *lexcur(void); /* the token peek/next last produced */

/* the generic-arguments closing: reads one token, but a ">>" reads as
 * its left half -- a ">" -- and leaves the right half peeked. That is
 * the whole ">>" split (the header contract above): it happens here,
 * only where a ">" is wanted. */
Tok nextgt(void);

/* parser backtracking: a snapshot of everything that reads forward.
 * lexsnap() saves, lexunsnap() rewinds and frees, and lexdrop()
 * abandons the rewind right (a trial that committed). Snapshots
 * nest -- a stack, released innermost first either way. The
 * character log itself never rewinds: it is the record of what the
 * FILE has already given, so the file never moves either; only the
 * cursor does. */
typedef struct LexSnap LexSnap;
LexSnap               *lexsnap(void);
void                   lexunsnap(LexSnap *s);
void                   lexdrop(LexSnap *s);

/* v.str.s lives in a buffer reused across tokens: its content is valid
 * until the next peek/next. Copy it out if it must outlive that. */

/* diagnostic: "path:line:col: message", then exit(1) */
void lexerr(const char *fmt, ...);

/* the enum name for a kind, for the token dump ("Tcomma" etc.) */
const char *tokname(Tok t);

#endif
