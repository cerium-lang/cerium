/* type.h -- the checker's types, hash-consed.
 *
 * The representation is the spec's type system, not its reflection
 * data: TypeInfo (08-reflection.md) is what a program sees of a type
 * at run time, and it is built from these, later. Three decisions
 * shape everything here:
 *
 *   - interning: every Type is unique by content, so type equality
 *     is pointer equality, and an alias never exists as one -- it
 *     resolves to its target and disappears (01-types.md).
 *   - mut is a slot property carried by a wrapper: Tymut wraps the
 *     child of a pointer, slice, array, or tuple element. *mut T is
 *     Typtr(Tymut(T)) -- never a standalone type, for there is no
 *     standalone mut i32.
 *   - ?T and E?T are Option<T> and Result<T,E>: sugar at the AST
 *     level, ordinary enum instances here (01-types.md). tyfmt
 *     prints the sugar back; is_same sees through nothing.
 *
 * args arrays are bare arena arrays with a count, not vec.h vectors:
 * a Type outlives the pass that builds it and never grows.
 */

#ifndef TYPE_H
#define TYPE_H

#include "ast.h"

/* Sym and Type are typedef'd in ast.h, which every type.h reader has */

typedef unsigned char u8; /* the tree's u64 (lex.h) has no smaller kin */

/* the kinds */
enum
{
  Tyunit, /* () */
  Tybool,
  Tyint, /* num: one of IN_* below */
  Tyvoidptr,
  Typaram,  /* a generic parameter in scope; gp is its Ngparam */
  Typtr,    /* t: the pointee; Tymut(t) for *mut */
  Tyslice,  /* t: the element; Tymut(t) for []mut */
  Tyarray,  /* t: the element; n the length, or gp a const parameter */
  Tytuple,  /* args: the elements */
  Tystruct, /* sym, args */
  Tyunion,  /* sym, args */
  Tyenum,   /* sym, args -- ?T and E?T included */
  Tytrait,  /* sym, args -- a trait named in type position */
  Tydyn,    /* sym, args, mut -- a handle (06-dispatch.md) */
  Tyfn,     /* args: the parameters, t: the return */
  Tytype,   /* the type `type` -- of a type value ($$t, 08) */
  Typroj,   /* Self::Item inside a trait: sym the trait, t the Self,
             * name the associated type (05-traits.md) */
  Tymut,    /* a writable slot inside ptr/slice/array/tuple; never alone */
  TYK_N
};

/* the integer widths and flavors, Tyint's num */
enum
{
  IN_I8,
  IN_I16,
  IN_I32,
  IN_I64,
  IN_I128,
  IN_U8,
  IN_U16,
  IN_U32,
  IN_U64,
  IN_U128,
  IN_ISIZE,
  IN_USIZE,
  IN_F32,
  IN_F64,
  IN_N
};

struct Type
{
  u8 k;          /* one of Ty* above */
  u8 num;        /* Tyint: IN_* */
  u8 mut;        /* Tydyn: dyn mut A */
  u8 copyknown;  /* iscopy's answer, memoized: types are interned,
                  * a tuple's rows ask once however often read */
  u8    copyval; /* the answer copyknown holds */
  u64   n;       /* Tyarray: the length, when it is a number */
  usize nargs;   /* the args count below */
  Sym  *sym;     /* Tystruct/Tyunion/Tyenum/Tytrait/Tydyn/Typroj: the declaration */
  Ast  *gp;      /* Typaram: the Ngparam; Tyarray: the length, when it is a
                  * const-parameter reference */
  Type **args;   /* nargs slots, or NULL when none */
  Type  *t;      /* Typtr/Tyslice/Tyarray/Tymut: the child; Tyfn/Typroj: the
                  * return / the Self */
  char *name;    /* Typroj: the associated type's name */
};

/* an args array of n slots, zeroed, on the arena */
Type **tyargs(usize n);

/* the singletons, interned once at first use */
Type *tyunit(void);
Type *tybool(void);
Type *tyvoidptr(void);
Type *tytype(void);
Type *tyint(int num); /* IN_* */

Type *typaram(Ast *gp); /* the Ngparam node */
Type *tymut(Type *t);   /* the writable-slot wrapper */
Type *typtr(Type *t);   /* *T; *mut T is typtr(tymut(t)) */
Type *tyslice(Type *t); /* []T; []mut T likewise */
Type *tyarray(u64 n, Type *t);
Type *tyarrayp(Ast *gp, Type *t); /* [N]T, N a const parameter */
Type *tytuple(Type **ts, usize n);
Type *tyfn(Type **args, usize n, Type *ret);
Type *tysym(Sym *s, Type **args, usize n); /* struct/union/enum/trait */
Type *tydyn(Sym *s, Type **args, usize n, int mut);
Type *typroj(Sym *s, Type *self, char *name); /* Self::Item, in a trait */

/* a family's signature read through a handle's own words: the pack's
 * parameter bound the whole tuple spelled out one row a parameter
 * (04-generics.md, 06-dispatch.md) */
Type *tyfnspread(Type *t, Ast **gp, Type **ty, usize n);

/* the sugar constructors: prelude enums, spelled as themselves */
Type *tyopt(Type *t);          /* ?T */
Type *tyres(Type *t, Type *e); /* E?T is Result<T, E> */

/* equality: interned, so this is a == b -- spelled out for the
 * places that talk about it */
int tysame(Type *a, Type *b);

/* substitute generic parameters by the types a call or an instance
 * bound them to: every Typaram that names one of gps reads as the
 * tys slot in step with it */
Type *gsubst(Type *t, Ast **gps, Type **tys, usize n);
Type *gsubstv(Type *t, Ast **gps, Type **tys, Val **gcvals,
              usize n); /* the
                         * const generic parameters' baked
                         * numbers ride along: [N]T's N
                         * answers with them, and a black-box
                         * one stays the box (08) */

/* a pointer dereferenced as far as it needs to reach a member:
 * sp.b is (*sp).b, and a *mut T's pointee is the mut slot mut T */
Type *derefthrough(Type *t);

/* the printable form, expanded: aliases are already gone, and the
 * sugar is spelled back -- ?T, E?T, [3]mut u8. tysprint writes a
 * NUL-terminated string into buf and returns it; tyfmt prints to
 * stdout, the dump's sink. */
char *tysprint(char *buf, usize n, Type *t);
void  tyfmt(Type *t);

/* an IN_* number's own word -- i8, u64, f32 -- the mangler's code for
 * a primitive says the same thing (12-projects.md, Symbols) */
const char *inname(int num);

#endif
