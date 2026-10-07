/* body.h -- the shared face of pass 4. flow.c the environment and
 * its helpers, body.c the walk, operators.c the spelled surface,
 * patterns.c what a match arms: the four meet here. */

#ifndef BODY_H
#define BODY_H

#include "lex.h" /* Tok: the operator table reads the token's own kind */
#include "sym.h" /* Sym, Member, Variant, Env -- and, through it, Ast and Type */

/* the FZ_* borrow states stay private to flow.c: the walk goes
 * through freeze and touchconflict, never at the bits */

typedef struct Local Local;
struct Local
{
  char *name;    /* the binding's name */
  Type *ty;      /* its declared type, never narrowed away */
  int   mut;     /* let mut */
  int   dead;    /* moved from: unusable until its scope ends */
  int   frz;     /* FZ_*: what a live borrow forbids */
  int   frzby;   /* the borrowing binding's index, to thaw when it dies */
  char *frzpath; /* the borrowed field chain, ".a.b"; NULL is the root */
  Type *cur;     /* the narrowed type, ty until a check narrows it */
  Val  *cv;      /* a const for round's value, when the unroll spelled
                  * the binding: a name's bytes are read from it
                  * (08-reflection.md) */
  int isconst;   /* a const parameter: its value rides cv when the
                  * instance's walk brings it, and a compile-time read
                  * before that is the black box -- the re-check under
                  * the binding answers (08-reflection.md) */
};

typedef struct Fenv Fenv;
struct Fenv
{
  Local *ls;        /* the bindings, innermost last */
  usize  n;         /* their count */
  Env    env;       /* the type-level names: Self, generics (sym.h) */
  int    loopd;     /* for's depth: break/continue, moves in a loop */
  usize  loopbase;  /* bindings alive when the outermost loop began: a
                     * move of one of those repeats every round (03) */
  usize loopbs[32]; /* each for's own base, innermost last: what a
                     * break or a continue's destructors cover, the
                     * pattern's bindings with the body's (03) */
  int   nloopbs;
  usize fnbase;   /* the bindings the fn's own frame owns: a return's
                   * destructors cover these, nothing above them -- a
                   * closure returns out of its own frame only (03) */
  Type *fnret;    /* the enclosing fn's return, for return and ? */
  int   nofreeze; /* an inline borrow the deref below is spending
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
int hasdrop(Type *t); /* a destructor the type owns, its own row or a
                       * field's inherited: what a scope's end runs
                       * and what Copy's exclusion reads (03) */

/* diagnostics */
void  berr(Ast *a, const char *fmt, ...) __attribute__((__noreturn__));
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
int    spentborrow(Ast *operand); /* the deref -- or the @take -- around
                                   * it spends the borrow whole: the walk
                                   * tells freeze to hold its hand */

/* narrowing (01-types.md, Nullability) */
int narrowcond(Ast *cond, Fenv *fe, char **name, Type **child);

/* the move picture a trial takes with it: an argument walk moves
 * what it reads (03-move.md), and a row that did not take the call
 * cannot leave its moves behind -- the next row's walk would read
 * them as gone. One int per binding, the arena's own. */
int *movsnap(Fenv *fe);
void movrestore(Fenv *fe, int *snap);

/* joins (03-move.md, Branches) */
void     fejoin(Fenv *fe, Fenv *a, Fenv *b);
void     unreach(Fenv *fe);
int      mustexit(Ast *st);
Variant *varfind(Sym *s, const char *name);
int      gunify(Type *sig, Type *arg, Ast **gps, Type **tys, usize n);
int      gunifyv(Type *sig, Type *arg, Ast **gps, Type **tys, Val **gcvals,
                 usize n); /* the const generic parameters bind their
                            * numbers beside the types (08) */

/* a trait's rows for one type, the most specific first: the rows
 * the receiver alone cannot order by their signature, the call's
 * own arguments walk against each in turn and the first that takes
 * them is the call's (07-operators.md). */
typedef struct Implcand Implcand;
struct Implcand
{
  Sym    *imp; /* the row's impl */
  Member *m;   /* its member the call names */
  Type  **tys; /* the receiver's binding of the impl's variables */
};

/* inherent impl members: *imp receives the supplying impl, for the
 * caller's genericity gate */
Member *inherentfind(Sym *s, const char *name, Sym **imp);
Member *inherentfindt(Type *t, const char *name, Sym **imp, Type ***tysp);
Member *implfind(Sym *trait, Type *t, const char *name, Sym **imp, Type ***tysp);
usize   implcands(Sym *trait, Type *t, const char *name, Implcand *cs, usize cap);
usize   traitcands(Type *t, const char *name, Implcand *cs, usize cap);
Sym    *implfor(Sym *trait, Type *t, Type ***tysp);
int     implsatisfies(Sym *trait, Type *t, Type **targs, usize ntargs, Ast **pins, Type **ptys,
                      usize npins);
int     boundsatisfies(Ast *b, Type *t, Ast **gps, Type **tys, usize n, Type ***ta, Ast **ig,
                       Type **itys, usize ni);
int     boundsok(Sym *im, Type **tys); /* the impl's own bounds, every
                                        * slot landed: the trial's ask
                                        * once the arguments bound the
                                        * rest (07-operators.md) */

/* the walk itself (body.c). The spelled surface and the match route
 * back into these: a builtin's or an operator's operand is a walk
 * of its own, a match arm's body a block. bodyfn is the fn whose
 * body pass 4 is walking -- the @compileError it holds reports only
 * when the evaluator never ran it (08-reflection.md). */
Type       *rexpr(Ast *e, Fenv *fe, Type *want);
Type       *rplace(Ast *e, Fenv *fe);
void        rstmt(Ast *st, Fenv *fe);
Type       *recoerce(Ast *e, Type *t, Fenv *fe);
int         placewritable(Ast *p, Fenv *fe);
extern Sym *bodyfn;

/* the drop calls and the closure ground, their own file:
 * bodymove.c. A scope's drops run in reverse when it leaves; a
 * closure literal becomes the fn and the env its captures ride in.
 * The blocks walk these -- the statements are body.c's above. */
Type *rblock(Ast *b, Fenv *fe, Type *want);
void  dropcalls(Ast *place, Type *t, Ast ***out, Ast *at);
Ast **scopedrops(Fenv *fe, usize from, Ast *at);
Type *rclosure(Ast *c, Fenv *fe);

/* the spelled surface (07-operators.md, 08-reflection.md), its own
 * file: operators.c. The walk hands the shapes over, and the
 * rewrite hands its pieces back: the comparison chain checks the
 * table itself, the in-place rewrites borrow and append with the
 * constructors below. */
Type       *rbuiltin(Ast *e, Fenv *fe, Type *want);
int         binop(Tok op, Type *a, Type *b, Type **res);
int         optrait(Ast *e, Fenv *fe);
void        opuntrait(Ast *e, Fenv *fe, const char *tr, const char *mth);
void        opunmove(Ast *e, Fenv *fe);
Ast        *opnode(Nk k, Ast *at);
void        opvpush(Ast ***vp, Ast *n);
Ast        *opseg(const char *nm, Ast *at);
Ast        *oppath(const char *x, Ast *at);
Ast        *opborrow(Ast *v, int mut, Ast *at);
int         opbarelocal(Ast *e);
int         opscalar1(Type *t);
int         opscalars(Type *a, Type *b);
const char *opname(Tok op);

/* patterns and match (09-match.md), its own file: patterns.c. A
 * pattern fits or reports; the match that arms them and the const
 * route a scrutinee may take are its too. */
void  rpat(Ast *p, Type *t, Fenv *fe, int mut);
Type *rmatch(Ast *e, Fenv *fe, Type *want);

#endif
