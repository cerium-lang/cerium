/* body.h -- the shared face of pass 4: the flow environment and the
 * helpers the body walk calls. flow.c owns them; body.c walks. */

#ifndef BODY_H
#define BODY_H

#include "sym.h" /* Sym, Member, Variant, Env -- and, through it, Ast and Type */

/* the FZ_* borrow states stay private to flow.c: the walk goes
 * through freeze and touchconflict, never at the bits */

typedef struct Local Local;
struct Local
{
  char       *name;    /* the binding's name */
  Type       *ty;      /* its declared type, never narrowed away */
  int         mut;     /* let mut */
  int         dead;    /* moved from: unusable until its scope ends */
  int         frz;     /* FZ_*: what a live borrow forbids */
  int         frzby;   /* the borrowing binding's index, to thaw when it dies */
  char       *frzpath; /* the borrowed field chain, ".a.b"; NULL is the root */
  Type       *cur;     /* the narrowed type, ty until a check narrows it */
  struct Val *cv;      /* a const for round's value, when the unroll spelled
                        * the binding: a name's bytes are read from it
                        * (08-reflection.md) */
};

typedef struct Fenv Fenv;
struct Fenv
{
  Local *ls;       /* the bindings, innermost last */
  usize  n;        /* their count */
  Env    env;      /* the type-level names: Self, generics (sym.h) */
  int    loopd;    /* for's depth: break/continue, moves in a loop */
  usize  loopbase; /* bindings alive when the outermost loop began: a
                    * move of one of those repeats every round (03) */
  Type *fnret;     /* the enclosing fn's return, for return and ? */
  int   nofreeze;  /* an inline borrow the deref below is spending
                    * whole: it reserves nothing past the expression,
                    * so freeze holds its hand (01-types.md) */
};

/* what a call's receiver borrow displaced, and its way back */
typedef struct Frzsave Frzsave;
struct Frzsave
{
  Local *root; /* NULL: the call froze nothing */
  int    frz;
  int    frzby;
  char  *frzpath;
};

/* the flow environment (03-move.md) */
Local *locfind(Fenv *fe, char *name);
usize  locfindi(Fenv *fe, char *name);
void   locpush(Fenv *fe, char *name, Type *ty, int mut);
void   locpop(Fenv *fe, usize nbase);
Fenv   fefork(Fenv *fe);
void   locnarrow(Fenv *fe, char *name, Type *t);

/* copy and drop (03-move.md) */
int iscopy(Type *t);

/* diagnostics */
void  berr(Ast *a, const char *fmt, ...);
char *btys(Type *t);

/* small type helpers */
Type *reschild(Type *t, Type **ok);
int   isintty(Type *t);
int   isnumty(Type *t);
int   fitsv(u64 v, Type *t);

/* places and borrows (01-types.md, 03-move.md) */
Type  *slicefield(Type *t, char *name);
int    isplace(Ast *e);
Local *placeroot(Ast *e, Fenv *fe, char *path, usize psz);
void   freeze(Ast *place, Fenv *fe, int mut, int by);
void   frzrestore(Frzsave *sv);
int    touchconflict(Ast *place, Fenv *fe, int writing);

/* narrowing (01-types.md, Nullability) */
int narrowcond(Ast *cond, Fenv *fe, char **name, Type **child);

/* joins (03-move.md, Branches) */
void     fejoin(Fenv *fe, Fenv *a, Fenv *b);
void     unreach(Fenv *fe);
int      mustexit(Ast *st);
Variant *varfind(Sym *s, const char *name);
int      gunify(Type *sig, Type *arg, Ast **gps, Type **tys, usize n);

/* inherent impl members: *imp receives the supplying impl, for the
 * caller's genericity gate */
Member *inherentfind(Sym *s, const char *name, Sym **imp);
Member *inherentfindt(Type *t, const char *name, Sym **imp, Type ***tysp);
Member *implfind(Sym *trait, Type *t, const char *name, Sym **imp, Type ***tysp);
Member *traitfindt(Type *t, const char *name, Sym **imp, Type ***tysp);
Sym    *implfor(Sym *trait, Type *t, Type ***tysp);
int     implsatisfies(Sym *trait, Type *t);

#endif
