/* sym.h -- a declaration, and what checking made of it.
 *
 * The namespaces the files' directories spell (11-namespaces.md):
 * one tree from the root, a declaration landing in the namespace of
 * the directory its file sits in. Each namespace holds its own
 * table; the bare-name lookup walks the tree the spec's order
 * spells. A Sym is what pass 1 declares and pass 2 fills: the
 * fields and variants of a type, the target of an alias, the fn
 * type of a function. Impls carry no name -- they wait for the pass
 * that collects them.
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
 * lives in ast.h, which every sym.h reader has. */
typedef struct Field   Field;
typedef struct Variant Variant;
typedef struct Member  Member;
typedef struct Ns      Ns;

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
  char *name;  /* as written in the declaration */
  Type *ty;    /* its resolved type */
  int   mut;   /* struct fields; a union rejects it (01-types.md) */
  Ast **attrs; /* the field's own, flattened out of the tree with
                * the rest -- the reflection walks them back out
                * as Attr values (08-reflection.md) */
};

/* an enum variant */
struct Variant
{
  char *name;      /* as written */
  u64   disc;      /* the assigned value, written or filled in by the
                    * ordering rule (01-types.md) */
  int    hasdisc;  /* "= integer" was written */
  int    named;    /* named payload rather than positional */
  Ast  **attrs;    /* the variant's own, for the same walk (08) */
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
  Sym  *sym;  /* Mfn in an impl: its fn Sym -- the call sites'
               * writeback, and the emitter's handle (05-traits.md) */
};

/* the shape follows kind: Stype carries the fields, variants, tag, or
 * alias target below; Sfn the fn type; Strait and Simpl the members
 * -- a trait's carry the declared signatures, an impl's the resolved
 * ones -- and an impl its resolved head too, per the last fields. */
struct Sym
{
  char    *name;
  int      kind;       /* one of Snone..Simpl above */
  int      pub;        /* visible outside its namespace (11) */
  Ns      *ownns;      /* the namespace it was declared in */
  Ast     *decl;       /* the declaring item, or NULL for the prelude */
  Ast    **gparams;    /* the Ngparam nodes */
  usize    ngparams;   /* their count */
  Sym     *next;       /* same-name overloads, fn only (04-generics.md) */
  int      tykind;     /* Stype: one of TYstruct..TYalias above */
  Field   *fields;     /* Stype: a struct or union's, or NULL */
  usize    nfields;    /* their count */
  Variant *variants;   /* Stype: an enum's, or NULL */
  usize    nvariants;  /* their count */
  Type    *tagty;      /* Stype: the enum tag, or NULL when compiler-picked */
  Type    *aliasty;    /* Stype: an alias's resolved target */
  int      resolving;  /* Stype: alias cycle detection, during pass 2 */
  Type    *fnty;       /* Sfn: the resolved fn type */
  Sym     *impl;       /* Sfn: the impl a method's Sym belongs to,
                        * or NULL for a plain fn */
  int evaled;          /* Sfn: the evaluator ran this body to its end
                        * -- an @compileError it did not reach is a
                        * branch of it, not a report the body check
                        * makes (08-reflection.md) */
  Type *cty;           /* Sconst, Sstatic: the resolved type */
  u64   cval;          /* Sconst, Sstatic: the evaluated value -- a
                        * const's, or a static's first one (08) */
  double cflt;         /* the float's own bits, when cty is one */
  u64    ctag;         /* the aggregate half's own metadata: an enum's
                        * discriminant, a union's active row (08) */
  Type *ctyval;        /* a type value's own half, when cty is `type`:
                        * the type it holds (08-reflection.md) */
  void *celems;        /* an aggregate's elements, when cty is an
                        * array: eval.c's Val vector, memoized beside
                        * cval the way it is (08-reflection.md) */
  usize clen;          /* a slice's own length, when cty is one: the
                        * elements a slice holds are the value's, not
                        * the type's -- @typeinfo's are the only
                        * slices a const can hold (08-reflection.md) */
  int cvaldone;        /* the value is in: the chain may land here
                        * again, and read it (08-reflection.md) */
  Member *members;     /* Strait, Simpl: in declaration order */
  usize   nmembers;    /* their count */
  int     traitdone;   /* Strait: the member table is built -- pass 3
                        * builds it, and a signature read that needs
                        * it earlier builds it then (05-traits.md) */
  Type *ipath, *ifort; /* Simpl: the head. ipath is the trait (a trait
                          impl) or the type itself (an inherent one);
                          ifort, what a trait impl is for */
};

void syminit(void);
Sym *symdecl(const char *name, int kind, Ast *decl, Ast **gparams, usize ngparams);
Sym *symfind(const char *name);
void nscur(Ns *ns); /* the namespace whose file the checker is in:
                     * symfind reads its table first (11) */

/* -- the use environment -------------------------------------------------
 * What a `use` brought into scope: an item's Sym, or a namespace
 * itself (11-namespaces.md). The bare name's lookup reads the
 * root's table first, then these -- the prelude-era fallthrough
 * retired, the use's own bindings taking its place. */

typedef struct Use Use;
struct Use
{
  char *name; /* the name in scope: the item's, or the ns's own */
  Ns   *ns;   /* the namespace the use brought in, or NULL */
  Sym  *sym;  /* the item the use brought in, or NULL */
  Ast  *at;   /* the use that wrote it: a collision's position */
};

void  useclear(void);     /* per compilation, from syminit */
Use **usenew(void);       /* a fresh file's own, empty */
void  usecur(Use **uses); /* the checker's switch, a file at a time --
                           * the uses are a file's own (11) */
Use *usebind(const char *name, Sym *sym, Ns *ns, Ast *at); /* the
                                                            * binding made, or the one that held
                                                            * the name first -- the caller
                                                            * reports the collision */
Use *usefind(const char *name);                            /* a binding by name, or NULL */
Use *usefindns(const char *name);                          /* a binding that is a namespace */

/* -- the namespace tree -------------------------------------------------
 * A directory is a namespace (11-namespaces.md); the root is the
 * project's. Each holds its own declarations and its
 * sub-namespaces. The bare name's lookup reads the root's table and
 * then what `use` brought in; a namespaced path walks the tree. */

struct Ns
{
  char *name; /* the last segment; the root's is "" */
  Sym **tbl;  /* this namespace's own declarations */
  usize cap, n;
  Ns  **subs; /* the sub-namespaces, a vec */
  Ns   *parent;
};

Ns *nsroot(void);
Ns *nsmk(Ns *parent, const char *name); /* a sub-namespace, named */
Ns *nschild(Ns *ns, const char *name);  /* a sub-namespace by name, or NULL */
Ns *nssubfind(const char *name);        /* one by name, the lookup chain the
                                         * bare name's own walks: the file's,
                                         * the root's, a use's (11) */
Sym  *nsitem(Ns *ns, const char *name); /* a declaration of this one */
char *nsname(Ns *ns);                   /* its full path, std::meta */
Sym **nstable(Ns *ns, usize *np);       /* every declaration of it,
                                         * a glob's walk (11) */
Sym *nsdecl(Ns *ns, const char *name, int kind, Ast *decl, Ast **gparams,
            usize ngparams); /* declare into it -- symdecl's own, one
                              * namespace over */

/* -- names in scope while a type resolves ------------------------------
 * Shared by resolve.c's passes and body.c's pass 4: a binding is a
 * generic parameter bound to itself, an alias argument bound to its
 * type, or Self bound to what implements it. */

typedef struct Bind Bind;
struct Bind
{
  char *name; /* the bound name */
  Type *t;    /* what it is bound to */
  Val  *cv;   /* the value, when the binding is a const generic
               * parameter's: the re-check's env carries it, and a
               * length spelled off the name reads it there
               * (08-reflection.md) */
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
Bind *envbind(Env *env, char *name); /* the binding whole: a const generic
                                      * parameter's value rides it (08) */

/* pass 3's impl table, built by checkfile: pass 4 reads it for
 * inherent methods and Drop checks */
extern Sym **chk_impls;
extern usize chk_nimpls;

/* Self's one generic parameter, built by syminit (sym.c) */
extern Ast *sym_selfgp;

/* the prelude's own declarations (prelude.c): the hand-built pair,
 * and std's embedded source -- parsed before the user's file (the
 * lexer is one global), resolved once the table is clean */
void prelude(void);
void preludeparse(void);
void preludefile(void);

/* std::meta's TypeInfo, from the embedded source: the type every
 * @typeinfo answers with (prelude.c, 08-reflection.md) */
extern Sym *sym_typeinfo;
Type       *typeinfoty(void);

/* the prelude enums the sugar builds on, and the exclusion pair
 * (prelude.c). The operator traits (07-operators.md) join them as
 * their passes arrive. */
extern Sym *sym_option, *sym_result, *sym_copy, *sym_drop;

/* a variant by name; the enum a bare variant name belongs to --
 * the checker's patterns and the emitter's construction share them */
Variant *symvarfind(Sym *s, const char *name);
Sym     *symvariantowner(char *name);

#endif
