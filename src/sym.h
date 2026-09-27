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
  char *name;
  Type *ty;
  int mut; /* struct fields; a union rejects it (01-types.md) */
};

/* an enum variant: disc is the assigned value, whether written or
 * filled in by the ordering rule (01-types.md) */
struct Variant
{
  char *name;
  u64 disc;
  int hasdisc;
  int named;            /* named payload rather than positional */
  struct Field *fields; /* named payload, or NULL */
  usize nfields;        /* the named payload's count */
  Type **payload;       /* positional payload types, or NULL */
  usize npayload;       /* the positional payload's count */
};

struct Sym
{
  char *name;
  int kind;
  int pub;       /* unused until namespaces land */
  Ast *decl;     /* the declaring item, or NULL for the prelude */
  Ast **gparams; /* the Ngparam nodes, ngparams of them */
  usize ngparams;
  struct Sym *next; /* same-name overloads, fn only (04-generics.md) */

  /* Stype */
  int tykind;
  struct Field *fields;
  usize nfields;
  struct Variant *variants;
  usize nvariants;
  Type *tagty;   /* the enum tag type, or NULL when compiler-picked */
  Type *aliasty; /* an alias: the resolved target */
  int resolving; /* alias cycle detection, during pass 2 */

  /* Sfn */
  Type *fnty;
  /* Sconst, Sstatic */
  Type *cty;
  /* Simpl: the resolved head. ipath is the trait (a trait impl) or
   * the type itself (an inherent one); ifort is what a trait impl
   * is for. Shape-pattern meaning waits for pass 3. */
  Type *ipath, *ifort;
};

void syminit(void);
Sym *symdecl(const char *name, int kind, Ast *decl, Ast **gparams, usize ngparams);
Sym *symfind(const char *name);

/* the prelude's own declarations (prelude.c) */
void prelude(void);

/* the prelude enums the sugar builds on (prelude.c). Copy/Drop and
 * the operator traits join them as their passes arrive. */
extern Sym *sym_option, *sym_result;

#endif
