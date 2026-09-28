/* sym.h -- a declaration, and what checking made of it.
 *
 * One namespace for the whole single-file root (11-namespaces.md);
 * directories-as-namespaces arrive as their own feature. A Sym is
 * what pass 1 declares and pass 2 fills: the fields and variants of
 * a type, the target of an alias, the fn type of a function. Impls
 * carry no name -- they wait for the pass that collects them.
 *
 * Field/Variant are flattened out of the Ast on purpose: checking
 * walks them constantly, and the tree stays the parser's contract.
 */

#ifndef SYM_H
#define SYM_H

#include "ast.h"
#include "type.h"

/* this header's own types, typedef'd in one place so use sites drop
 * the struct -- the pattern ast.h and type.h set. Sym's typedef
 * lives in type.h, which needs the forward reference. */
typedef struct Field   Field;
typedef struct Variant Variant;
typedef struct Member  Member;

enum
{
  Snone,
  Stype, /* struct, union, enum, or alias -- tykind below */
  Sfn,
  Sconst,
  Sstatic,
  Strait,
  Simpl /* an impl: nameless, kept in the pass-3 list only */
};

enum
{
  TYstruct,
  TYunion,
  TYenum,
  TYalias
};

/* a field of a struct or union, or of a named enum payload */
struct Field
{
  char *name; /* as written in the declaration */
  Type *ty;   /* its resolved type */
  int   mut;  /* struct fields; a union rejects it (01-types.md) */
};

/* an enum variant */
struct Variant
{
  char *name;      /* as written */
  u64   disc;      /* the assigned value, written or filled in by the
                    * ordering rule (01-types.md) */
  int    hasdisc;  /* "= integer" was written */
  int    named;    /* named payload rather than positional */
  Field *fields;   /* named payload, or NULL */
  usize  nfields;  /* the named payload's count */
  Type **payload;  /* positional payload types, or NULL */
  usize  npayload; /* the positional payload's count */
};

/* a member of a trait or an impl: a method, an associated type, or
 * an associated constant (05-traits.md). In a trait, Mfn's ty is
 * the declared signature -- Self a parameter, Self::Item a Typroj;
 * in an impl, everything is resolved against the impl's own types. */
enum
{
  Mfn,
  Mtype,
  Mconst
};

struct Member
{
  char *name; /* the member's own name */
  int   kind; /* Mfn, Mtype, or Mconst */
  Ast  *decl; /* the declaring item, or NULL for the prelude */
  Type *ty;   /* Mfn: the fn type; Mconst: the type; Mtype: unused */
  Type *val;  /* Mtype in an impl: the supplied type */
};

/* the shape follows kind: Stype carries the fields, variants, tag, or
 * alias target below; Sfn the fn type; Strait and Simpl the members
 * -- a trait's carry the declared signatures, an impl's the resolved
 * ones -- and an impl its resolved head too, per the last fields. */
struct Sym
{
  char    *name;
  int      kind;          /* one of Snone..Simpl above */
  int      pub;           /* unused until namespaces land */
  Ast     *decl;          /* the declaring item, or NULL for the prelude */
  Ast    **gparams;       /* the Ngparam nodes */
  usize    ngparams;      /* their count */
  Sym     *next;          /* same-name overloads, fn only (04-generics.md) */
  int      tykind;        /* Stype: one of TYstruct..TYalias above */
  Field   *fields;        /* Stype: a struct or union's, or NULL */
  usize    nfields;       /* their count */
  Variant *variants;      /* Stype: an enum's, or NULL */
  usize    nvariants;     /* their count */
  Type    *tagty;         /* Stype: the enum tag, or NULL when compiler-picked */
  Type    *aliasty;       /* Stype: an alias's resolved target */
  int      resolving;     /* Stype: alias cycle detection, during pass 2 */
  Type    *fnty;          /* Sfn: the resolved fn type */
  Type    *cty;           /* Sconst, Sstatic: the resolved type */
  Member  *members;       /* Strait, Simpl: in declaration order */
  usize    nmembers;      /* their count */
  Type    *ipath, *ifort; /* Simpl: the head. ipath is the trait (a trait
                             impl) or the type itself (an inherent one);
                             ifort, what a trait impl is for */
};

void syminit(void);
Sym *symdecl(const char *name, int kind, Ast *decl, Ast **gparams, usize ngparams);
Sym *symfind(const char *name);

/* -- names in scope while a type resolves ------------------------------
 * Shared by resolve.c's passes and body.c's pass 4: a binding is a
 * generic parameter bound to itself, an alias argument bound to its
 * type, or Self bound to what implements it. */

typedef struct Bind Bind;
struct Bind
{
  char *name; /* the bound name */
  Type *t;    /* what it is bound to */
};

typedef struct Env Env;
struct Env
{
  Bind *b;      /* the bindings, innermost last */
  usize n;      /* their count */
  Sym  *strait; /* resolving a trait's members: Self::X projects */
  Sym  *impl;   /* resolving an impl's: Self::X is the supplied type */
};

Env   envnone(void);
Env   envpush(Env *e, char *name, Type *t);
Env   envgparams(Env *outer, Ast **gps, usize n); /* outer may be NULL */
Type *envfind(Env *env, char *name);

/* pass 3's impl table, built by checkfile: pass 4 reads it for
 * inherent methods and Drop checks */
extern Sym **chk_impls;
extern usize chk_nimpls;

/* Self's one generic parameter, built by syminit (sym.c) */
extern Ast *sym_selfgp;

/* the prelude's own declarations (prelude.c) */
void prelude(void);

/* the prelude enums the sugar builds on, and the exclusion pair
 * (prelude.c). The operator traits (07-operators.md) join them as
 * their passes arrive. */
extern Sym *sym_option, *sym_result, *sym_copy, *sym_drop;

/* a variant by name; the enum a bare variant name belongs to --
 * the checker's patterns and the emitter's construction share them */
Variant *symvarfind(Sym *s, const char *name);
Sym     *symvariantowner(char *name);

#endif
