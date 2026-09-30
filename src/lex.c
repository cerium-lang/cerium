/* lex.c -- the lexer. One token slot, one lookahead, a reused buffer.
 *
 * Every production here traces to specs/15-grammar.md's lexical pass.
 * The shape follows qbe's parse.c: statics for state, peek()/next()
 * around a single thead slot, errors print and exit.
 */

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lex.h"

static FILE       *inf;
static const char *inpath = "<stdin>";
static unsigned    line = 1, col = 1; /* position of the NEXT character */

/* the embedded source, when one is bound: std's prelude is a string
 * in the binary (08-reflection.md's std::meta parses like any other
 * source), and C89 has no fmemopen to serve it as a FILE */
static const char *srcbuf;
static usize       srcpos;

static Token cur;   /* the token peek/next last produced */
static Token thead; /* peeked token; t == Txxx means empty */

/* the kind of the last token the parser was handed; lexnumber reads
 * it -- a number right after a Tdot is a tuple index, not a float */
static Tok   prevtok = Txxx;
static char *buf; /* reused value buffer, NUL-terminated */

/* -- character layer ------------------------------------------------
 *
 * A log of folded characters with a cursor into it: gc hands out
 * clog[npos] and ungc steps the cursor back, so any character -- a
 * newline, a blank, an EOF included -- is unread with its position
 * intact. Lookahead across a line boundary is what needs this ("1.\nx":
 * the dot must go back behind the newline) and it is why this is a
 * log, not qbe's one-char pushback.
 *
 * CRLF folds to '\n' as characters are logged; a lone '\r' keeps its
 * column and stays a plain byte.
 */

static struct
{
  int      c;
  unsigned line, col; /* the seat this character sits at */
} clog[64];
static int nread; /* characters pulled from the file */
static int npos;  /* the cursor; clog[npos] is the next character */
static int eofseen;

/* the backtracking stack (defined far below) keeps rewind points in
 * the log's coordinates; a compaction shifts them along */
static void snapshiftdown(int off);
static int  snapshotkeep(int floor);

static void
clogput(int c)
{
  unsigned l, cl;

  if (nread == 0) {
    l = 1;
    cl = 1;
  } else if (clog[nread - 1].c == '\n') {
    l = clog[nread - 1].line + 1;
    cl = 1;
  } else {
    l = clog[nread - 1].line;
    cl = clog[nread - 1].col + 1;
  }
  clog[nread].c = c;
  clog[nread].line = l;
  clog[nread].col = cl;
  nread++;
}

static int
rdc(void) /* the next raw character: the file's, or the embedded
           * source's -- the prelude's text is NUL-terminated, so the
           * NUL is the EOF and the cursor stops on it */
{
  if (srcbuf)
    return srcbuf[srcpos] ? (unsigned char) srcbuf[srcpos++] : EOF;
  return fgetc(inf);
}

static void
fill(void) /* pull one folded character from the source into the log */
{
  int c, c2;

  if (eofseen || nread >= (int) (sizeof clog / sizeof clog[0]))
    return; /* defensive: the cursor keeps gc() below refilling */
  c = rdc();
  if (c == '\r') {
    for (;;) {
      c2 = rdc();
      if (c2 == '\n') { /* the fold sits where the '\r' sat */
        clogput('\n');
        return;
      }
      clogput('\r'); /* a lone '\r' takes its column, stays a byte */
      if (c2 != '\r') {
        c = c2;
        break;
      }
      /* the next '\r' starts the fold test over */
    }
  }
  if (c == EOF) {
    eofseen = 1;
    clogput(EOF); /* the EOF has a seat too: it can be unread */
    return;
  }
  clogput(c);
}

static int
gc(void)
{
  int c;

  if (npos == nread) {
    /* the log is exhausted: all logged characters are consumed. When
     * it is also near full, compact: keep the tail -- an ungc chain
     * reaches back through it, clogput chains seats off it, and every
     * live backtracking snapshot rewinds into it -- and refill. The
     * keep covers the deepest rewind point, so a snapshot never ages
     * out of the log. */
    if (nread >= (int) (sizeof clog / sizeof clog[0]) - 2) {
      int keep = snapshotkeep(nread > 8 ? 8 : nread);

      if (keep > nread)
        keep = nread; /* defensive */
      memmove(clog, clog + nread - keep, (size_t) keep * sizeof clog[0]);
      snapshiftdown(nread - keep);
      nread = keep;
      npos = keep;
    }
    fill();
  }
  if (npos == nread)
    return EOF;
  c = clog[npos].c;
  if (c == '\n') {
    line = clog[npos].line + 1;
    col = 1;
  } else if (c != EOF) {
    line = clog[npos].line;
    col = clog[npos].col + 1;
  }
  npos++;
  return c;
}

/* undo the last gc; the character goes back with its seat */
static void
ungc(void)
{
  if (npos == 0)
    die("internal: ungc underflow");
  npos--;
  line = clog[npos].line;
  col = clog[npos].col;
}

/* -- diagnostics ---------------------------------------------------- */

void
lexerr(const char *fmt, ...)
{
  va_list ap;

  fprintf(stderr, "%s:%u:%u: ", inpath, line, col);
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  fputc('\n', stderr);
  exit(1);
}

/* -- buffer --------------------------------------------------------- */

static void
bufclear(void)
{
  vclear(buf);
}

static void
bufput(int c)
{
  char b;

  b = (char) c;
  vappend(&buf, &b);
}

/* buf's len counts the NUL terminator: strcmp/strtod read the C string,
 * vlen(buf)-1 is the byte count the token wants */
static void
bufnul(void)
{
  bufput(0);
}

/* -- character classes ---------------------------------------------- */
/* hand-rolled, locale-free, and safe for any byte value */

static int
isletter(int c)
{
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

static int
isdig(int c)
{
  return c >= '0' && c <= '9';
}

static int
isxdig(int c)
{
  return isdig(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
}

static int
hexval(int c)
{
  if (isdig(c))
    return c - '0';
  if (c >= 'a' && c <= 'f')
    return c - 'a' + 10;
  return c - 'A' + 10;
}

static int
isidentchar(int c)
{
  return isletter(c) || isdig(c) || c == '_';
}

static int
isbasedig(int c, int base)
{
  switch (base) {
  case 16:
    return isxdig(c);
  case 8:
    return c >= '0' && c <= '7';
  case 2:
    return c == '0' || c == '1';
  }
  return isdig(c);
}

/* -- u64 parse: C89 has no strtoull, and it must catch overflow ----- */

static u64
parseu64(const char *s, int base, int *ovf)
{
  u64 v = 0, max = (u64) -1;

  *ovf = 0;
  for (; *s; s++) {
    u64 d = (u64) hexval(*s);
    if (v > (max - d) / (u64) base) {
      *ovf = 1;
      return max;
    }
    v = v * (u64) base + d;
  }
  return v;
}

/* -- keywords -------------------------------------------------------- */

static struct
{
  const char *name;
  Tok         t;
} kwtab[] = {
    {"fn", Tfn},       {"struct", Tstruct}, {"enum", Tenum},         {"union", Tunion},
    {"trait", Ttrait}, {"impl", Timpl},     {"type", Ttype},         {"use", Tuse},
    {"pub", Tpub},     {"let", Tlet},       {"const", Tconst},       {"static", Tstatic},
    {"mut", Tmut},     {"dyn", Tdyn},       {"true", Ttrue},         {"false", Tfalse},
    {"if", Tif},       {"else", Telse},     {"match", Tmatch},       {"for", Tfor},
    {"in", Tin},       {"break", Tbreak},   {"continue", Tcontinue}, {"return", Treturn},
    {"macro", Tmacro}, {"defer", Tdefer}, /* reserved, unused */
};

static Tok
kwlook(const char *s)
{
  size_t i;

  for (i = 0; i < sizeof kwtab / sizeof kwtab[0]; i++)
    if (strcmp(kwtab[i].name, s) == 0)
      return kwtab[i].t;
  return Txxx;
}

/* -- escapes: \n \t \r \0 \' \" \\ \xNN, that is the whole set -------- */

static int
lexescape(void) /* the '\' is consumed */
{
  int c, d;

  c = gc();
  switch (c) {
  case 'n':
    return '\n';
  case 't':
    return '\t';
  case 'r':
    return '\r';
  case '0':
    return 0;
  case '\'':
    return '\'';
  case '"':
    return '"';
  case '\\':
    return '\\';
  case 'x':
    d = gc();
    if (!isxdig(d))
      lexerr("escape \\x needs two hex digits");
    c = hexval(d) << 4;
    d = gc();
    if (!isxdig(d))
      lexerr("escape \\x needs two hex digits");
    return c | hexval(d);
  case EOF:
    lexerr("unterminated escape");
    break; /* lexerr does not return; this only quiets -Wfallthrough */
  default:
    lexerr("unknown escape '\\%c'", c);
  }
  return 0; /* unreachable */
}

/* -- comments -------------------------------------------------------- */

static void
linecomment(void)
{
  int c;

  do
    c = gc();
  while (c != '\n' && c != EOF);
}

static void
blockcomment(void)
{
  int c, depth = 1;

  for (;;) {
    c = gc();
    if (c == EOF)
      lexerr("unterminated block comment");
    if (c == '*' && (c = gc()) == '/') {
      if (--depth == 0)
        return;
      continue;
    }
    if (c == '/' && (c = gc()) == '*')
      depth++;
  }
}

/* -- identifiers ------------------------------------------------------ */

/* the string scanners live below; ident scanning reaches them through
 * the prefix rule */
static void lexstring(int cflag, int rflag);
static void lexmultiline(int flags);

static void
lexident(int c0)
{
  Tok k;
  int c = c0;

  bufclear();
  do {
    bufput(c);
    c = gc();
  } while (isidentchar(c));
  ungc();
  bufnul();

  /* the prefix rule: c, r, cr touching a quote is the prefixed
   * literal, and that is the whole rule */
  if (strcmp(buf, "c") == 0 || strcmp(buf, "r") == 0 || strcmp(buf, "cr") == 0) {
    c = gc();
    if (c == '"') {
      /* buf[1] == 'r' is true only for the two-letter spelling */
      lexstring(buf[0] == 'c', buf[0] == 'r' || buf[1] == 'r');
      return;
    }
    ungc();
  }

  k = kwlook(buf);
  if (k != Txxx) {
    cur.t = k;
    return;
  }
  cur.t = Tident;
  cur.v.str.s = buf;
  cur.v.str.len = vlen(buf) - 1;
  cur.v.str.flags = 0;
}

/* -- numbers ---------------------------------------------------------- */

static void
lexnumber(int c0) /* c0 is a digit */
{
  int    base = 10, c = c0, isflt = 0, ovf;
  char  *end;
  double d;

  bufclear();

  if (c0 == '0') {
    c = gc();
    if (c == 'x' || c == 'o' || c == 'b') {
      /* the prefix does not enter buf: parseu64 sees digits only.
       * (hexval('x') would read 'A'-relative -- 0x41 -- as a digit.) */
      base = c == 'x' ? 16 : c == 'o' ? 8 : 2;
      c = gc();
      if (c == EOF || !isbasedig(c, base))
        lexerr("digit expected after the base prefix");
    } else {
      bufput(c0); /* plain decimal: "0" and "007" are decimal forms */
    }
  }

  /* digit sequence; an underscore needs a digit on each side */
  for (;;) {
    if (c == EOF)
      break;
    if (c == '_') {
      c = gc();
      if (!isbasedig(c, base))
        lexerr("underscore separates digits -- it needs one on each side");
    }
    if (!isbasedig(c, base))
      break;
    bufput(c);
    c = gc();
  }

  /* a dot: float if a digit follows -- "1..3" and "1.x" are not.
   * Not after a postfix dot either: the digits are a tuple index
   * and this dot opens the next one ("t.1.0") */
  if (base == 10 && c == '.' && prevtok != Tdot) {
    int c2 = gc();
    if (isdig(c2)) {
      isflt = 1;
      bufput('.');
      bufput(c2);
      c = gc();
      for (;;) {
        if (c == '_') {
          c = gc();
          if (!isbasedig(c, 10))
            lexerr("underscore separates digits -- it needs one on each side");
        }
        if (!isbasedig(c, 10))
          break;
        bufput(c);
        c = gc();
      }
    } else {
      ungc();
      c = '.';
    }
  }

  /* exponent: decimals only, digits required */
  if (base == 10 && (c == 'e' || c == 'E')) {
    isflt = 1;
    bufput('e');
    c = gc();
    if (c == '+' || c == '-') {
      bufput(c);
      c = gc();
    }
    if (!isdig(c))
      lexerr("exponent needs digits");
    for (;;) {
      if (c == '_') {
        c = gc();
        if (!isdig(c))
          lexerr("underscore separates digits -- it needs one on each side");
      }
      if (!isdig(c))
        break;
      bufput(c);
      c = gc();
    }
  }

  ungc();
  bufnul();

  /* a literal does not run into identifier characters: 0x1G, 0b12,
   * 1000abc are errors, not two tokens. c is the first character the
   * literal did not take. */
  if (isidentchar(c) && c != '_')
    lexerr("invalid digit '%c' in the literal", c);

  if (isflt) {
    d = strtod(buf, &end);
    if (*end != 0)
      lexerr("malformed float literal");
    cur.t = Tflt;
    cur.v.flt = d;
    return;
  }
  cur.t = Tint;
  cur.v.num = parseu64(buf, base, &ovf);
  if (ovf)
    lexerr("integer literal out of range");
}

/* -- strings ----------------------------------------------------------- */

static void
lexstring(int cflag, int rflag) /* the quote is consumed */
{
  int c, c2, flags = 0;

  if (cflag)
    flags |= STRF_C;
  if (rflag)
    flags |= STRF_RAW;

  c = gc();
  if (c == '"') {
    c2 = gc();
    if (c2 == '"') {
      lexmultiline(flags);
      return;
    }
    ungc();
    /* "" -- the empty string */
    bufclear();
    cur.t = Tstr;
    cur.v.str.s = buf;
    cur.v.str.flags = flags;
    cur.v.str.len = 0;
    if (flags & STRF_C) {
      bufput(0);
      cur.v.str.len = 1; /* the terminator is the whole value */
    }
    bufnul();
    return;
  }
  ungc();

  bufclear();
  for (;;) {
    c = gc();
    if (c == EOF || c == '\n')
      lexerr("unterminated string literal");
    if (c == '"')
      break;
    if (c == '\\' && !(flags & STRF_RAW)) {
      bufput(lexescape());
      continue;
    }
    bufput(c);
  }
  if (flags & STRF_C)
    bufput(0);
  bufnul();
  cur.t = Tstr;
  cur.v.str.s = buf;
  cur.v.str.len = vlen(buf) - 1;
  cur.v.str.flags = flags;
}

static void
lexbyte(void) /* the '\'' is consumed */
{
  int c;

  bufclear();
  c = gc();
  if (c == EOF || c == '\n')
    lexerr("unterminated byte literal");
  if (c == '\'')
    lexerr("empty byte literal");
  if (c == '\\')
    bufput(lexescape());
  else
    bufput(c);
  c = gc();
  if (c != '\'')
    lexerr("unterminated byte literal");
  bufnul();
  cur.t = Tbyte;
  cur.v.str.s = buf;
  cur.v.str.len = 1;
  cur.v.str.flags = 0;
}

/* -- multiline strings -------------------------------------------------
 * The opening """ and its newline are consumed. The closing """ stands
 * at the head of its own line; its indentation is stripped from every
 * content line, matched one character for one character. An empty line
 * is exempt. The last content line's newline is the one before the
 * closing quotes -- it is not content. */

static void
lexmultiline(int flags)
{
  usize *lstarts = vnew(usize, 0); /* buf offset where each content line starts */
  usize  lineoff, nlines = 0, i, w, clen;
  char  *closing;
  int    c, c2, c3;

  c = gc();
  if (c != '\n')
    lexerr("a newline must follow the opening \"\"\"");

  bufclear();
  for (;;) {
    lineoff = vlen(buf);

    /* leading blanks, then the closing test */
    for (;;) {
      c = gc();
      if (c == ' ' || c == '\t') {
        bufput(c);
        continue;
      }
      break;
    }
    if (c == '"' && (c2 = gc()) == '"' && (c3 = gc()) == '"') {
      clen = vlen(buf) - lineoff;
      closing = malloc(clen ? clen : 1);
      if (!closing)
        lexerr("out of memory");
      memcpy(closing, buf + lineoff, clen);
      vhdr(buf)->len = lineoff; /* the closing line is not content */
      break;
    }
    /* a content line: recorded only now, so lstops never holds the
     * closer -- the last entry must be the last content line */
    vappend(&lstarts, &lineoff);
    nlines++;

    if (c == '"') {
      if (c2 == '"') { /* "" opens a line, checked above it was not """ */
        ungc();
        bufput('"');
      } else {
        ungc();
      }
      bufput('"');
      goto content;
    }
    if (c == '\n') {
      bufput('\n');
      continue; /* an empty line contributes just its newline */
    }
    if (c == EOF)
      lexerr("unterminated multiline string");
    if (c == '\\' && !(flags & STRF_RAW)) {
      bufput(lexescape());
      goto content;
    }
    bufput(c);

  content:
    for (;;) {
      c = gc();
      if (c == '\n') {
        bufput('\n');
        break;
      }
      if (c == EOF)
        lexerr("unterminated multiline string");
      if (c == '"') { /* three in a row cannot appear -- escape or
                         split; one or two are content */
        c2 = gc();
        if (c2 == '"') {
          c3 = gc();
          if (c3 == '"')
            lexerr("three quotes in a row -- escape them or split the line");
          ungc();
          bufput('"');
        } else {
          ungc();
        }
        bufput('"');
        continue;
      }
      if (c == '\\' && !(flags & STRF_RAW)) {
        bufput(lexescape());
        continue;
      }
      bufput(c);
    }
  }

  /* strip: every non-empty line must carry the closing indentation,
   * character for character -- a tab under a space closer is an error.
   * An empty line is exempt; it contributes just its newline. */
  w = 0;
  for (i = 0; i < nlines; i++) {
    usize s = lstarts[i];
    usize e = (i + 1 < nlines) ? lstarts[i + 1] : (usize) vlen(buf);

    if (i + 1 == nlines) /* the newline before the closing """ is not
                            content */
      e--;
    if (e > s && !(e - s == 1 && buf[s] == '\n')) {
      if (e - s < clen || memcmp(buf + s, closing, clen) != 0)
        lexerr("line under-indented, or indentation mixes tabs and spaces");
      s += clen;
    }
    memmove(buf + w, buf + s, e - s);
    w += e - s;
  }
  vhdr(buf)->len = w;
  free(closing);
  vfree(lstarts);

  if (flags & STRF_C)
    bufput(0);
  bufnul();
  cur.t = Tstr;
  cur.v.str.s = buf;
  cur.v.str.len = vlen(buf) - 1;
  cur.v.str.flags = flags | STRF_ML;
}

/* -- the main dispatch ------------------------------------------------- */

static Token
lex(void)
{
  int      c, c2, c3;
  unsigned sl, sc;

  /* blanks and comments are equivalent */
  for (;;) {
    sl = line;
    sc = col;
    c = gc();
    if (c == EOF) {
      cur.t = Teof;
      cur.line = sl;
      cur.col = sc;
      return cur;
    }
    if (c == ' ' || c == '\t' || c == '\n')
      continue;
    if (c == '/') {
      c2 = gc();
      if (c2 == '/') {
        linecomment();
        continue;
      }
      if (c2 == '*') {
        blockcomment();
        continue;
      }
      ungc();
      /* fall through: the slash is a token */
    }
    ungc();
    break;
  }

  cur.line = line;
  cur.col = col;

  c = gc();
  switch (c) {
  case '(':
    cur.t = Tlparen;
    return cur;
  case ')':
    cur.t = Trparen;
    return cur;
  case '[':
    cur.t = Tlbracket;
    return cur;
  case ']':
    cur.t = Trbracket;
    return cur;
  case '{':
    cur.t = Tlbrace;
    return cur;
  case '}':
    cur.t = Trbrace;
    return cur;
  case ',':
    cur.t = Tcomma;
    return cur;
  case ';':
    cur.t = Tsemi;
    return cur;
  case ':':
    cur.t = (c2 = gc()) == ':' ? Tcoloncolon : (ungc(), Tcolon);
    return cur;
  case '.':
    if ((c2 = gc()) == '.')
      cur.t = (c2 = gc()) == '.' ? Tdotdotdot : (ungc(), Tdotdot);
    else {
      ungc();
      cur.t = Tdot;
    }
    return cur;
  case '-':
    if ((c2 = gc()) == '>')
      cur.t = Tarrow;
    else if (c2 == '=')
      cur.t = Tminuseq;
    else {
      ungc();
      cur.t = Tminus;
    }
    return cur;
  case '?':
    cur.t = Tquestion;
    return cur;
  case '@':
    cur.t = Tat;
    return cur;
  case '$':
    if ((c2 = gc()) != '$') {
      ungc();
      lexerr("'$' is only '$$'");
    }
    cur.t = Tdollar2;
    return cur;
  case '^':
    if ((c2 = gc()) == '^')
      cur.t = Tcaret2;
    else {
      ungc();
      cur.t = Tcaret;
    }
    return cur;
  case '#':
    if ((c2 = gc()) != '[') {
      ungc();
      lexerr("'#' is only '#['");
    }
    cur.t = Thashlbracket;
    return cur;
  case '+':
    cur.t = (c2 = gc()) == '=' ? Tpluseq : (ungc(), Tplus);
    return cur;
  case '*':
    cur.t = (c2 = gc()) == '=' ? Tstareq : (ungc(), Tstar);
    return cur;
  case '/':
    cur.t = (c2 = gc()) == '=' ? Tslasheq : (ungc(), Tslash);
    return cur;
  case '%':
    cur.t = Tpercent;
    return cur;
  case '~':
    cur.t = Ttilde;
    return cur;
  case '&':
    if ((c2 = gc()) == '&')
      cur.t = Tampamp;
    else {
      ungc();
      cur.t = Tamp;
    }
    return cur;
  case '|':
    if ((c2 = gc()) == '|')
      cur.t = Tbarbar;
    else {
      ungc();
      cur.t = Tbar;
    }
    return cur;
  case '!':
    cur.t = (c2 = gc()) == '=' ? Tne : (ungc(), Tbang);
    return cur;
  case '=':
    c2 = gc();
    if (c2 == '=')
      cur.t = Teqeq;
    else if (c2 == '>') /* the match arm's fat arrow */
      cur.t = Tfatarrow;
    else {
      ungc();
      cur.t = Teq;
    }
    return cur;
  case '<':
    if ((c2 = gc()) == '<') {
      if ((c3 = gc()) == '=')
        cur.t = Tshleq;
      else {
        ungc();
        cur.t = Tshl;
      }
    } else if (c2 == '=')
      cur.t = Tle;
    else {
      ungc();
      cur.t = Tlt;
    }
    return cur;
  case '>':
    if ((c2 = gc()) == '>') {
      if ((c3 = gc()) == '=')
        cur.t = Tshreq;
      else {
        ungc();
        cur.t = Tshr;
      }
    } else if (c2 == '=')
      cur.t = Tge;
    else {
      ungc();
      cur.t = Tgt;
    }
    return cur;
  case '"':
    lexstring(0, 0);
    return cur;
  case '\'':
    lexbyte();
    return cur;
  default:
    if (isletter(c) || c == '_') { /* "_1000 is an identifier" --
                                      the letter production, plus the
                                      underscore the text grants */
      lexident(c);
      return cur;
    }
    if (isdig(c)) {
      lexnumber(c);
      return cur;
    }
    lexerr("invalid character 0x%02x", (unsigned char) c);
  }
  return cur; /* unreachable */
}

/* -- the qbe peek/next pair --------------------------------------------- */

Tok
peek(void)
{
  if (thead.t == Txxx) {
    prevtok = cur.t; /* cur holds the previous token until lex()
                      * overwrites it -- read it just before */
    thead = lex();
  }
  cur = thead;
  return thead.t;
}

Tok
next(void)
{
  Tok t;

  t = peek();
  thead.t = Txxx;
  return t;
}

Token *
lexcur(void)
{
  return &cur;
}

/* -- the ">>" split ------------------------------------------------------
 * Only where a ">" is wanted (the generic-arguments closings). A Tshr
 * stands for two closers touching; this hands out the left one and
 * leaves the right one peeked. The two halves share the seat. */

Tok
nextgt(void)
{
  if (peek() == Tshr) {
    thead = cur;   /* cur is the Tshr peek just produced */
    thead.t = Tgt; /* the right half stays peeked */
    cur.t = Tgt;   /* and the left half is what was consumed */
    return Tgt;
  }
  return next();
}

/* -- backtracking --------------------------------------------------------
 * The parser's generic-arguments trial reads forward, then either
 * commits or rewinds. The character log is never rewound: it is the
 * record of everything the FILE has already given, and the cursor
 * (npos) alone moves back -- rewinding nread would strand characters
 * the FILE has passed but the log no longer claims. A snapshot is the
 * cursor, the position, the lookahead, the current token, and the
 * value buffer's contents.
 *
 * Snapshots nest (a for-head snapshot survives an inner trial), so
 * they form a stack. A log compaction keeps every live snapshot's
 * point by shifting the stack's saved cursors along with the log. */

struct LexSnap
{
  LexSnap *prev;       /* the stack, innermost first */
  int      npos;       /* the cursor, in the log's current coordinates */
  unsigned line, col;  /* where the snapshot was taken */
  Token    thead, cur; /* the saved slots */
  usize    blen;       /* vlen(buf) at the snapshot point */
  char     bcont[1];   /* [vlen(buf)] follows */
};

static LexSnap *live;

static void
snapshiftdown(int off) /* a compaction moved the log's seats */
{
  LexSnap *p;

  for (p = live; p; p = p->prev)
    p->npos -= off;
}

static int
snapshotkeep(int floor) /* the compaction must keep at least this much */
{
  LexSnap *p;

  for (p = live; p; p = p->prev)
    if (p->npos > floor)
      floor = p->npos; /* no snapshot may age out of the log */
  return floor;
}

LexSnap *
lexsnap(void)
{
  LexSnap *s;
  usize    n = vlen(buf);

  s = malloc(sizeof *s + n);
  if (!s)
    die("out of memory");
  s->prev = live;
  s->npos = npos;
  s->line = line;
  s->col = col;
  s->thead = thead;
  s->cur = cur;
  s->blen = n;
  memcpy(s->bcont, buf, n);
  live = s;
  return s;
}

void
lexunsnap(LexSnap *s)
{
  usize i;

  npos = s->npos;
  line = s->line;
  col = s->col;
  thead = s->thead;
  cur = s->cur;
  vclear(buf);
  for (i = 0; i < s->blen; i++) {
    char c = s->bcont[i];

    vappend(&buf, &c);
  }
  live = s->prev;
  free(s);
}

void
lexdrop(LexSnap *s) /* the trial stands: give up the rewind right */
{
  LexSnap **pp;

  for (pp = &live; *pp; pp = &(*pp)->prev)
    if (*pp == s) {
      *pp = s->prev;
      free(s);
      return;
    }
  die("internal: lexdrop: not a live snapshot");
}

/* -- setup -------------------------------------------------------------- */

/* every slot back to its birth state. One lexer now serves two
 * sources per compilation -- the prelude's embedded text first, then
 * the user's file -- and the second must not inherit the first's
 * seats. lexinit never reset these before: one file per process was
 * the assumption, and the prelude ended it. */
static void
lexreset(void)
{
  cur.t = Txxx;
  thead.t = Txxx;
  prevtok = Txxx;
  nread = 0;
  npos = 0;
  eofseen = 0;
  line = 1;
  col = 1;
  live = 0; /* a parse that finished unwound them all; strays here
             * are dropped, a leak only in a path that is exiting */
  srcbuf = 0;
  srcpos = 0;
  buf = vnew(char, 64);
}

void
lexinit(const char *path)
{
  lexreset();
  inpath = path ? path : "<stdin>";
  inf = path ? fopen(path, "r") : stdin;
  if (!inf)
    die("cannot open %s", inpath);
  /* a byte order mark at the very start is skipped; the three bytes
   * are logged first so a short file is left exactly as it was */
  fill();
  fill();
  fill();
  if (nread == 3 && clog[0].c == 0xef && clog[1].c == 0xbb && clog[2].c == 0xbf)
    nread = 0; /* dropped; the cursor at 0 reads what follows */
}

void
lexsrc(const char *text) /* the embedded source, errors name it "<std>" */
{
  lexreset();
  inpath = "<std>";
  inf = 0;
  srcbuf = text;
}

const char *
lexpath(void)
{
  return inpath;
}

/* -- names ----------------------------------------------------------- */

const char *
tokname(Tok t)
{
  static const char *names[] = {
#define X(name) #name,
      XYZ_TOKS(X)
#undef X
  };
  return (unsigned) t < TOKKIND_N ? names[t] : "?";
}
