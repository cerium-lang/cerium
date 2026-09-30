/* ast.h -- the tree, tagged-union style.
 *
 * One Ast struct, one kind enum, a union branch per kind. The
 * shape is chosen for the self-hosting port: a match over Nk maps
 * onto xyz's own E?T one arm per kind, so what is exhaustive here
 * stays exhaustive there.
 *
 * Nodes are never freed: mk() allocates, the process is the arena.
 * Names and literal bytes are copied out of the lexer's reused
 * buffer at build time (mkstr) -- nothing in the tree points into
 * token scratch space.
 *
 * Ast vectors are vec.h vectors (vlen/vappend); NULL is the empty
 * vector everywhere vlen tolerates it.
 *
 * The dump (ast.c) is S-expressions and a golden-test contract:
 * (kind field ...), one node a line, children indented. Changing it
 * rewrites every .golden under tests/parse/ok/.
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
  X(Ntmut, "tmut") X(Ntarray, "tarray") X(Nttuple, "ttuple") X(Ntfn, "tfn")    \
  X(Ntdyn, "tdyn") X(Nttype, "ttype") /* mut: dyn mut A */                      \
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

typedef struct Ast  Ast;
typedef struct Sym  Sym;  /* sym.h, one step later in the includes */
typedef struct Type Type; /* type.h, one step later in the includes */

struct Ast
{
  Nk       k;
  unsigned line, col;
  int      pub; /* items only: visible outside the namespace
                 * (11-namespaces.md) */
  Ast **attrs;  /* items, variants, fields, parameters -- the four
                 * slots the grammar gives attributes to; else NULL */
  Type *ty;     /* what checking made of this node, written back for
                 * the passes that follow (the emitter); NULL until
                 * then -- the -a dump prints before it exists */
  union
  {
    struct
    {
      u64 num; /* Nint, Nvariant's = */
    } i;
    struct
    {
      Ast *e;   /* the tuple */
      u64  idx; /* the index */
    } tup;      /* Ntupidx -- a pointer and a u64, so it cannot borrow
                 * n1/i: the union would overlap them */
    struct
    {
      double flt;
    } f;
    struct
    {
      char    *s;     /* value bytes, NUL-terminated for the dump */
      usize    len;   /* byte count */
      unsigned flags; /* Nstr: STRF_* */
    } s;
    struct
    {
      char *name; /* Nseg, Nattr, Ncap, Ninit, Npfield: an identifier */
    } nm;
    struct
    {
      Ast  **segs; /* Npath: Nseg vector */
      int    root; /* leading "::" */
      Sym   *sym;  /* Npath: the fn it names as a value, when it does */
      Type **tys;  /* Npath: the instantiation the want picked (04) */
    } path;
    struct
    {
      char *name;
      Ast **args; /* generic args: types, or $$/^^ expressions */
    } seg;        /* Nseg, Nattr */
    struct
    {
      Ast *l, *r;
      Tok  op; /* Nbin, Nassign; unused for Nrange */
    } bin;
    struct
    {
      Ast *e;
      Tok  op;  /* Nun: the operator token */
      int  mut; /* Nun: &mut; Ntptr: *mut; Ntmut: a tuple row's slot */
    } un;
    struct
    {
      Ast  *e;      /* the trait path */
      int   mut;    /* dyn mut A */
      Ast **assocs; /* <Item = u32>: Ninit list, name + the type (06) */
    } tdyn;         /* Ntdyn */
    struct
    {
      Ast  *f;    /* Ncall, Nbuiltin: the callee */
      Ast **args; /* Ncall, Nbuiltin, Nmatch: args or arms */
      Sym  *sym;  /* Ncall: the overload the checker picked, so the
                   * emitter need not guess by name */
      Type **tys; /* Ncall: the generic bindings it picked -- NULL
                   * when the fn is not generic */
    } call;
    struct
    {
      Ast *a, *b; /* Nindex: e, i; Ntresult: E, T; Narm: pat, body;
                      Ntuple pairs never land here */
    } n2;
    struct
    {
      Ast *e; /* Ntry, Nreturn, Nexprstmt, Ntopt, Ninit's value,
                  Npfield's sub-pattern */
    } n1;
    /* Ntdyn lives in v.tdyn (below) */
    struct
    {
      Ast *e;       /* the indexed/ranged/... base */
      Ast *lo, *hi; /* either may be NULL */
    } ridx;
    struct
    {
      Ast  *e;
      char *name;
    } fld; /* Nfield */
    struct
    {
      int  cnst; /* const if/match/for */
      Ast *cond;
      Ast *then; /* Nblock */
      Ast *els;  /* Nif or Nblock, or NULL */
    } ifx;
    struct
    {
      Ast **stmts;
      Ast  *tail; /* the block's value, or NULL */
    } blk;
    struct
    {
      Ast *len; /* Nint, or an Npath naming a const parameter,
                   or NULL for [] */
      int   mut;
      Ast  *t;
      Ast **es;
    } arrlit;
    struct
    {
      Ast  *path;  /* Npath */
      Ast **inits; /* Ninit vector */
    } slit;
    struct
    {
      Ast **caps;   /* Ncap vector */
      Ast **params; /* Nparam vector */
      Ast  *ret;    /* or NULL */
      Ast  *body;   /* Nblock */
    } clos;
    struct
    {
      int   byref; /* & or &mut capture */
      int   mut;   /* mut x, or &mut x */
      char *name;
    } cap;
    struct
    {
      char *name;    /* Nfn, Nstruct, ...: the declared name */
      Ast **gparams; /* Ngparam vector, or NULL */
      Ast **params;  /* Nparam vector, or NULL */
      Ast  *ret;     /* or NULL */
      Ast  *body;    /* Nblock, or NULL for the ";" form */
    } fn;
    struct
    {
      char *name;
      Ast **gparams;
      Ast **fields;  /* Nfield vector */
      Ast **members; /* Ntrait/Nimpl: fn, const, typedef items */
    } ty;            /* Nstruct, Nunion, Ntrait */
    struct
    {
      char *name;
      Ast **gparams;
      Ast  *tag; /* "enum X(u32)": the tag type, or NULL */
      Ast **variants;
    } en;
    struct
    {
      Ast **attrs;
      char *name;     /* Nvariant, Nfield */
      Ast  *discexpr; /* Nvariant: "= const expr", when hasdisc */
      int   hasdisc;
      Ast **payload; /* Nvariant: types or Nfield list, or NULL */
      int   named;   /* payload braces rather than parens */
      int   mut;     /* Nfield */
      Ast  *t;       /* Nfield's type */
    } variant;       /* Nvariant, Nfield */
    struct
    {
      Ast **gparams; /* impl's own */
      Ast  *path;    /* the trait or the type */
      Ast  *fort;    /* impl ... for T, or NULL (inherent) */
      Ast **members;
    } impl;
    struct
    {
      char *name;
      Ast **gparams;
      Ast  *t; /* the aliased type */
    } td;
    struct
    {
      Ast  *path; /* Npath; a nested Nuse hangs off subs */
      Ast **subs; /* nested use trees */
      int   star; /* ::* */
    } use;
    struct
    {
      Ast **attrs;
      int   mut;  /* static mut, let mut */
      char *name; /* const/static: the name */
      Ast  *t;
      Ast  *e;
    } cst; /* Nconst, Nstatic */
    struct
    {
      int   mut;
      int   cnst; /* parameter: the argument is compile-time known */
      char *name;
      Ast  *t;
    } param; /* Nparam, and let's shape below is close enough */
    struct
    {
      char *name;   /* the generic parameter, or the pack's */
      Ast **bounds; /* Npath vector */
      Ast  *dflt;   /* = T, or NULL */
      int   pack;   /* ...name */
      int   cnst;   /* const N: T -- a value parameter (08) */
      Ast  *t;      /* its type, cnst only */
    } gp;
    struct
    {
      int  mut;
      Ast *pat;
      Ast *t; /* : T, or NULL */
      Ast *e; /* = e */
    } let;
    struct
    {
      int  shape; /* FCOND, FLET, FIN (below) */
      Ast *a, *b; /* per shape: the condition; the pattern and the
                      source; the iterable */
      Ast  *body;
      Ast **unroll; /* Ncfor only: the statements the iteration
                     * spelled, each round's let and the body --
                     * shared between the rounds, for the passes read
                     * it without writing it (10-iteration.md) */
    } forx;
    struct
    {
      Ast  *path;    /* Npath */
      Ast **payload; /* patterns or Npfield list, or NULL */
      int   named;   /* braces */
      int   rest;    /* ".." */
    } ppath;
    struct
    {
      Ast **fields; /* Npfield vector */
      int   rest;   /* ".." */
    } pstruct;
    struct
    {
      char *name;
      Ast  *e; /* Ninit: the value; Npfield: the sub-pattern, or NULL */
    } init;    /* Ninit, Npfield -- name and child must coexist */
    struct
    {
      char *name;  /* @name */
      Ast **targs; /* generic args, or NULL */
      Ast **args;
    } blt; /* Nbuiltin */
    struct
    {
      Ast **args; /* the parameter types, anonymous (01-types.md) */
      Ast  *ret;
    } fnty; /* Ntfn */
    struct
    {
      Ast **ts; /* Nttuple elements */
    } list;     /* Ntuple, Npor, Nptuple, Nbarestructlit */
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
Ast *mk(Nk k);

/* raw arena bytes for the passes after the tree: types and symbols
 * outlive it, and nothing is freed either */
void *arenaalloc(usize n);

/* copy the lexer's current string value out of its reused buffer */
char *mkstr(void);
u64   mknum(void);

/* the S-expression dump: one node, then its children indented */
void dumpast(Ast *n);

/* dump helpers shared with the token dump: a byte string escaped
 * with the lexer's own closed set, and a u64 in decimal (C89
 * printf has no %llu) */
void dumpstr(const char *s, usize len);
void dumpu64(u64 v);

#endif
