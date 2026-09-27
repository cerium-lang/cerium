/* ast.h -- the tree, tagged-union style.
 *
 * One Node struct, one kind enum, a union branch per kind. The
 * shape is chosen for the self-hosting port: a match over Nk maps
 * onto xyz's own E?T one arm per kind, so what is exhaustive here
 * stays exhaustive there.
 *
 * Nodes are never freed: mk() allocates, the process is the arena.
 * Names and literal bytes are copied out of the lexer's reused
 * buffer at build time (mkstr) -- nothing in the tree points into
 * token scratch space.
 *
 * Node vectors are vec.h vectors (vlen/vappend); NULL is the empty
 * vector everywhere vlen tolerates it.
 *
 * The dump (ast.c) is S-expressions and a golden-test contract:
 * (kind field ...), one node a line, children indented. Changing it
 * rewrites every .golden under tests/parse/.
 */

#ifndef AST_H
#define AST_H

#include "lex.h"

/* the node list, one place: the enum and the dump names share it.
 * Each entry: kind, then the dump name it prints as. */
/* clang-format off */
#define XYZ_NODES(X)                                                           \
  /* expressions */                                                            \
  X(Nint, "int") X(Nflt, "flt") X(Nbyte, "byte") X(Nstr, "str")                \
  X(Nbool, "bool") X(Nunit, "unit") X(Ntuple, "tuple")                         \
  X(Npath, "path") X(Nseg, "seg")                                              \
  X(Nbin, "bin") X(Nrange, "range") X(Nun, "un")                               \
  X(Ncall, "call") X(Nspread, "spread")                                        \
  X(Nindex, "index") X(Naccess, "access") X(Nrangeindex, "rangeidx")           \
  X(Ntupidx, "tupidx") X(Ntry, "try")                                          \
  X(Nif, "if") X(Ncif, "const-if") X(Nmatch, "match") X(Narm, "arm")           \
  X(Nblock, "block")                                                           \
  X(Narraylit, "arraylit") X(Nstructlit, "structlit")                          \
  X(Nbarestructlit, "barestructlit") X(Ninit, "init")                          \
  X(Nclosure, "closure") X(Ncap, "cap") X(Nbuiltin, "builtin")                 \
  /* types */                                                                  \
  X(Ntopt, "topt") X(Ntresult, "tresult") X(Ntptr, "tptr")                     \
  X(Ntarray, "tarray") X(Nttuple, "ttuple") X(Ntfn, "tfn")                     \
  X(Ntdyn, "tdyn") X(Nttype, "ttype")                                          \
  /* items */                                                                  \
  X(Nfn, "fn") X(Nstruct, "struct") X(Nunion, "union") X(Nenum, "enum")        \
  X(Ntrait, "trait") X(Nimpl, "impl") X(Ntypedef, "typedef")                   \
  X(Nuse, "use") X(Nconst, "const") X(Nstatic, "static")                       \
  X(Nattr, "attr") X(Nfield, "field") X(Nvariant, "variant")                   \
  X(Nparam, "param") X(Ngparam, "gparam")                                      \
  /* statements */                                                             \
  X(Nlet, "let") X(Nassign, "assign") X(Nreturn, "return")                     \
  X(Nbreak, "break") X(Ncontinue, "continue")                                  \
  X(Nfor, "for") X(Ncfor, "const-for") X(Nexprstmt, "expr")                    \
  /* patterns */                                                               \
  X(Npor, "por") X(Npwild, "wild") X(Nppath, "ppath")                          \
  X(Nptuple, "ptuple") X(Npstruct, "pstruct") X(Npfield, "pfield")
/* clang-format on */

typedef enum
{
#define X(k, dumpname) k,
  XYZ_NODES(X)
#undef X
      NK_N
} Nk;

/* the dump names, one per kind -- "let", "if", "const-if", ... */
const char *nkname(Nk k);

typedef struct Node Node;
struct Node
{
  Nk k;
  unsigned line, col;
  Node **attrs; /* items, variants, fields, parameters -- the four
                 * slots the grammar gives attributes to; else NULL */
  union
  {
    struct
    {
      u64 num; /* Nint, Nvariant's = */
    } i;
    struct
    {
      Node *e; /* the tuple */
      u64 idx; /* the index */
    } tup;     /* Ntupidx -- a pointer and a u64, so it cannot borrow
                * n1/i: the union would overlap them */
    struct
    {
      double flt;
    } f;
    struct
    {
      char *s;        /* value bytes, NUL-terminated for the dump */
      usize len;      /* byte count */
      unsigned flags; /* Nstr: STRF_* */
    } s;
    struct
    {
      char *name; /* Nseg, Nattr, Ncap, Ninit, Npfield: an identifier */
    } nm;
    struct
    {
      Node **segs; /* Npath: Nseg vector */
      int root;    /* leading "::" */
    } path;
    struct
    {
      char *name;
      Node **args; /* generic args: types, or $$/^^ expressions */
    } seg;         /* Nseg, Nattr */
    struct
    {
      Node *l, *r;
      Tok op; /* Nbin, Nassign; unused for Nrange */
    } bin;
    struct
    {
      Node *e;
      Tok op;  /* Nun: the operator token */
      int mut; /* Nun: &mut; Ntptr: *mut */
    } un;
    struct
    {
      Node *f;     /* Ncall, Nbuiltin: the callee */
      Node **args; /* Ncall, Nbuiltin, Nmatch: args or arms */
    } call;
    struct
    {
      Node *a, *b; /* Nindex: e, i; Ntresult: E, T; Narm: pat, body;
                      Ntuple pairs never land here */
    } n2;
    struct
    {
      Node *e; /* Ntry, Nreturn, Nexprstmt, Ntopt, Ninit's value,
                  Npfield's sub-pattern */
    } n1;
    struct
    {
      Node *e;       /* the indexed/ranged/... base */
      Node *lo, *hi; /* either may be NULL */
    } ridx;
    struct
    {
      Node *e;
      char *name;
    } fld; /* Nfield */
    struct
    {
      int cnst; /* const if/match/for */
      Node *cond;
      Node *then; /* Nblock */
      Node *els;  /* Nif or Nblock, or NULL */
    } ifx;
    struct
    {
      Node **stmts;
      Node *tail; /* the block's value, or NULL */
    } blk;
    struct
    {
      Node *len; /* Nint, or NULL for [] */
      int mut;
      Node *t;
      Node **es;
    } arrlit;
    struct
    {
      Node *path;   /* Npath */
      Node **inits; /* Ninit vector */
    } slit;
    struct
    {
      Node **caps;   /* Ncap vector */
      Node **params; /* Nparam vector */
      Node *ret;     /* or NULL */
      Node *body;    /* Nblock */
    } clos;
    struct
    {
      int byref; /* & or &mut capture */
      int mut;   /* mut x, or &mut x */
      char *name;
    } cap;
    struct
    {
      char *name;     /* Nfn, Nstruct, ...: the declared name */
      Node **gparams; /* Ngparam vector, or NULL */
      Node **params;  /* Nparam vector, or NULL */
      Node *ret;      /* or NULL */
      Node *body;     /* Nblock, or NULL for the ";" form */
    } fn;
    struct
    {
      char *name;
      Node **gparams;
      Node **fields;  /* Nfield vector */
      Node **members; /* Ntrait/Nimpl: fn, const, typedef items */
    } ty;             /* Nstruct, Nunion, Ntrait */
    struct
    {
      char *name;
      Node **gparams;
      Node **variants;
    } en;
    struct
    {
      Node **attrs;
      char *name; /* Nvariant, Nfield */
      u64 disc;   /* Nvariant: "= integer", when hasdisc */
      int hasdisc;
      Node **payload; /* Nvariant: types or Nfield list, or NULL */
      int named;      /* payload braces rather than parens */
      int mut;        /* Nfield */
      Node *t;        /* Nfield's type */
    } variant;        /* Nvariant, Nfield */
    struct
    {
      Node **gparams; /* impl's own */
      Node *path;     /* the trait or the type */
      Node *fort;     /* impl ... for T, or NULL (inherent) */
      Node **members;
    } impl;
    struct
    {
      char *name;
      Node **gparams;
      Node *t; /* the aliased type */
    } td;
    struct
    {
      Node *path;  /* Npath; a nested Nuse hangs off subs */
      Node **subs; /* nested use trees */
      int star;    /* ::* */
    } use;
    struct
    {
      Node **attrs;
      int mut;    /* static mut, let mut */
      char *name; /* const/static: the name */
      Node *t;
      Node *e;
    } cst; /* Nconst, Nstatic */
    struct
    {
      int mut;
      int cnst; /* parameter: the argument is compile-time known */
      char *name;
      Node *t;
    } param; /* Nparam, and let's shape below is close enough */
    struct
    {
      char *name;    /* the generic parameter, or the pack's */
      Node **bounds; /* Npath vector */
      Node *dflt;    /* = T, or NULL */
      int pack;      /* ...name */
    } gp;
    struct
    {
      int mut;
      Node *pat;
      Node *t; /* : T, or NULL */
      Node *e; /* = e */
    } let;
    struct
    {
      int cnst;
      int shape;   /* FCOND, FLET, FIN (below) */
      Node *a, *b; /* per shape: the condition; the pattern and the
                      source; the pattern and the iterable */
      Node *body;
    } forx;
    struct
    {
      Node *path;     /* Npath */
      Node **payload; /* patterns or Npfield list, or NULL */
      int named;      /* braces */
      int rest;       /* ".." */
    } ppath;
    struct
    {
      Node **fields; /* Npfield vector */
      int rest;      /* ".." */
    } pstruct;
    struct
    {
      char *name;
      Node *e; /* Ninit: the value; Npfield: the sub-pattern, or NULL */
    } init;    /* Ninit, Npfield -- name and child must coexist */
    struct
    {
      char *name;   /* @name */
      Node **targs; /* generic args, or NULL */
      Node **args;
    } blt; /* Nbuiltin */
    struct
    {
      Node **args; /* the parameter types, anonymous (01-types.md) */
      Node *ret;
    } fnty; /* Ntfn */
    struct
    {
      Node **ts; /* Nttuple elements */
    } list;      /* Ntuple, Npor, Nptuple, Nbarestructlit */
  } v;
};

/* the three for heads (15-grammar.md, "The for shapes") */
enum
{
  FCOND, /* for cond */
  FLET,  /* for let pat = e */
  FIN    /* for pat in e */
};

/* node building: mk copies nothing but the kind and the position;
 * the caller fills the union. Anything set to NULL where a vector is
 * wanted means "empty". */
Node *mk(Nk k);

/* copy the lexer's current string value out of its reused buffer */
char *mkstr(void);
u64 mknum(void);

/* the S-expression dump: one node, then its children indented */
void dumpast(Node *n);

/* dump helpers shared with the token dump: a byte string escaped
 * with the lexer's own closed set, and a u64 in decimal (C89
 * printf has no %llu) */
void dumpstr(const char *s, usize len);
void dumpu64(u64 v);

#endif
