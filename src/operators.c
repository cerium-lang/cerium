/* operators.c -- pass 4's spelled surface: the @ builtins and the
 * operator table. Every shape the language spells itself -- @cast,
 * -a, a + b -- the checker reads by hand and rewrites into the call
 * it answers: a builtin into its own node, an operator into its
 * trait's method (07-operators.md, 08-reflection.md). The walk in
 * body.c hands these shapes over. */

#include <string.h>

#include "ast.h"
#include "body.h"
#include "check.h"
#include "eval.h"
#include "layout.h" /* fieldoffof: @offsetof reads the layout (02-layout.md) */
#include "lex.h"
#include "type.h"
#include "vec.h"

/* -- the @ builtins (08-reflection.md) ----------------------------------- */

Type *
rbuiltin(Ast *e, Fenv *fe, Type *want)
{
  char *nm = e->v.blt.name;
  Ast **targs = e->v.blt.targs;
  Ast **args = e->v.blt.args;
  usize nt = vlen(targs), na = vlen(args);

  if (strcmp(nm, "sizeof") == 0 || strcmp(nm, "alignof") == 0) {
    if (nt != 1 || na != 0)
      berr(e, "@%s takes one type argument and no value", nm);
    targs[0]->ty = rty(targs[0], &fe->env);
    return tyint(IN_USIZE);
  }
  if (strcmp(nm, "cast") == 0) {
    Type *to;

    if (nt != 1 || na != 1)
      berr(e, "@cast takes one type argument and one value");
    to = rty(targs[0], &fe->env);
    targs[0]->ty = to;
    rexpr(args[0], fe, 0);
    return to;
  }
  if (strcmp(nm, "take") == 0) {
    Type *pt;

    if (nt != 0 || na != 1)
      berr(e, "@take takes one place");
    if (gatedargs) /* the move a mode-gated call would make in one
                    * mode and skip in the other: neither mode may
                    * make it, the kept one holds the ban
                    * (01-types.md, Mode-gated functions) */
      berr(e, "@take cannot ride a gated fn's arguments: the modes that remove"
              " the call never make the move (01-types.md)");
    {
      int spent = spentborrow(args[0]);

      if (spent) /* the take spends the borrow whole, as a deref does:
                  * the pointer dies the moment it is made -- take(x)
                  * is @take(&mut x), and the sugar is useless if the
                  * borrow outlives it (01, 03) */
        fe->nofreeze++;
      pt = rexpr(args[0], fe, 0);
      if (spent)
        fe->nofreeze--;
    }
    if (!pt || pt->k != Typtr || pt->t->k != Tymut)
      berr(args[0], "@take wants a *mut T place, this is %s", btys(pt));
    return pt->t->t;
  }
  if (strcmp(nm, "slice") == 0) { /* the way a slice is born: the
                                   * pointer's reach and the length
                                   * together, one step -- no
                                   * half-built view ever stands
                                   * (01-types.md). The pointee goes
                                   * in whole: a *mut T, whose pointee
                                   * is the mut slot itself, answers
                                   * a []mut T */
    Type *pt;

    if (nt != 0 || na != 2)
      berr(e, "@slice takes a pointer and a length");
    pt = rexpr(args[0], fe, 0);
    if (!pt || pt->k != Typtr)
      berr(args[0], "@slice wants a *T here, this is %s", btys(pt));
    rexpr(args[1], fe, tyint(IN_USIZE)); /* the length a usize, a
                                          * literal coerced where it
                                          * stands */
    return tyslice(pt->t);
  }
  if (strcmp(nm, "len") == 0) { /* the count half of the fat pointer,
                                 * read where it sits (01-types.md) --
                                 * every file's own door, the count no
                                 * secret: @slice writes it, @len reads
                                 * it back */
    Type *st;

    if (nt != 0 || na != 1)
      berr(e, "@len takes one slice (01-types.md)");
    st = rexpr(args[0], fe, 0);
    if (!st || st->k != Tyslice)
      berr(args[0], "@len wants a []T here, this is %s", btys(st));
    return tyint(IN_USIZE);
  }
  if (strcmp(nm, "ptr") == 0) { /* the reach half -- the std library's
                                 * own door: the raw pointer is what
                                 * this round closed, and the surface
                                 * every other file takes is ptr()
                                 * (01-types.md) */
    Type *st;

    if (nt != 0 || na != 1)
      berr(e, "@ptr takes one slice (01-types.md)");
    if (!nsinstd(nscuring()))
      berr(e, "@ptr is the std library's own: the door every other file takes is "
              "ptr() (01-types.md)");
    st = rexpr(args[0], fe, 0);
    if (!st || st->k != Tyslice)
      berr(args[0], "@ptr wants a []T here, this is %s", btys(st));
    return typtr(st->t); /* *mut T for a []mut T: the mut layer the
                          * slice carries stays on (01-types.md) */
  }
  if (strcmp(nm, "compileError") == 0) {
    Type *st;

    if (na != 1)
      berr(e, "@compileError takes the message");
    st = rexpr(args[0], fe, 0);
    if (!st || st->k != Tyslice || !st->t || st->t->k != Tyint || st->t->num != IN_U8)
      berr(args[0], "@compileError takes a string");
    if (bodyfn && bodyfn->evaled)
      return tyunit(); /* the evaluator ran this body and did not
                        * reach here: the branch stands, and emit
                        * gives it no runtime behavior (08) */
    berr(e, "%.*s", (int) args[0]->v.s.len, args[0]->v.s.s);
  }
  if (strcmp(nm, "count") == 0) { /* the pack's own length, a
                                   * compile-time constant against
                                   * the binding (04-generics.md) */
    Type *pt;

    if (nt != 0 || na != 1 || args[0]->k != Nspread)
      berr(e, "@count takes one pack (...Ts) (04-generics.md)");
    pt = rty(args[0]->v.un.e, &fe->env);
    if (pt && pt->k == Typaram) { /* the declaration's own walk: the
                                   * number is the instance's, and
                                   * what stands on it defers (08) */
      if (!pt->gp->v.gp.pack)
        berr(args[0], "'%s' is not a pack; @count wants one (04-generics.md)", pt->gp->v.gp.name);
      evalblackbox++;
      return tyint(IN_USIZE);
    }
    if (pt && (pt->k == Tytuple || pt->k == Tyunit)) { /* the binding:
                                                        * fold to the number, every
                                                        * pass below reads a literal */
      e->k = Nint;
      e->v.i.num = pt->k == Tytuple ? pt->nargs : 0;
      return tyint(IN_USIZE);
    }
    berr(args[0], "@count takes a pack (...Ts) (04-generics.md)");
  }
  if (strcmp(nm, "typeof") == 0) { /* the value's own type, as a
                                    * reference: $$ puts it back into
                                    * a slot (08-reflection.md) */
    if (nt != 0 || na != 1)
      berr(e, "@typeof takes one value");
    rexpr(args[0], fe, 0); /* the operand is checked for its own
                            * sake; the reference names its type, and
                            * the evaluator reads that when a splice
                            * asks */
    return tytype();
  }
  if (strcmp(nm, "typeinfo") == 0) { /* the description, both slots
                                      * one case: the type named in
                                      * the argument slot, or the
                                      * value's static type
                                      * (08-reflection.md) */
    Type *t;

    if (nt == 1 && na == 0) {
      if (targs[0]->k == Nun && targs[0]->v.un.op == Tdollar2) {
        /* the deferred splice: the operand's value arrives with a
         * frame a compile-time call builds, and this walk holds no
         * frame -- the node stands for the evaluator to answer
         * there (08-reflection.md). The fn that holds it runs at
         * compile time or not at all: a runtime call would pass
         * the type's word, and the word holds nothing to describe */
        if (bodyfn && bodyfn->evaled)
          return typeinfoty();
        berr(e, "the splice names a value this walk holds no frame for -- a parameter's, a "
                "local's: name a const, or call the fn at compile time (08-reflection.md)");
      }
      t = rty(targs[0], &fe->env);
      if (t->k == Typaram)   /* a generic's own parameter, met on the
                              * declaration's walk: the description is
                              * the instance's, and the node stands for
                              * the re-check under the binding to
                              * rewrite, on the clone the emitter hands
                              * that walk (04-generics.md) */
        return typeinfoty(); /* the shape alone: this walk checks the
                              * world around it, the instance's fills
                              * the answer in */
      targs[0]->ty = t;
    } else if (nt == 0 && na == 1)
      t = rexpr(args[0], fe, 0); /* the value's own derivation, not
                                  * the slot's want */
    else
      berr(e, "@typeinfo takes one type argument or one value (08-reflection.md)");
    { /* the answer is data the checker already holds: built here,
       * the node rewritten as the literal that spells it, and the
       * walk re-entered reads its own words (08-reflection.md) */
      Ast *x = valtoexpr(typeinfoval(t, e), e);

      memset(&e->v, 0, sizeof e->v);
      e->k = x->k;
      memcpy(&e->v, &x->v, sizeof e->v);
      return rexpr(e, fe, want);
    }
  }
  if (strcmp(nm, "offset") == 0) { /* a field's own place in the type's
                                    * whole: the layout query, folded
                                    * where it stands (02-layout.md) */
    Type *t;
    char *fnm;
    usize i;

    if (nt != 1 || na != 1)
      berr(e, "@offset takes one type argument and the field's name (02-layout.md)");
    if (targs[0]->k == Nun && targs[0]->v.un.op == Tdollar2) {
      /* the deferred splice, @typeinfo's own shape: the answer typed
       * and zeroed where no runtime read reaches it (08) */
      if (bodyfn && bodyfn->evaled)
        return tyint(IN_USIZE);
      berr(e, "the splice names a value this walk holds no frame for -- a parameter's, a "
              "local's: name a const, or call the fn at compile time (08-reflection.md)");
    }
    t = rty(targs[0], &fe->env);
    if (t->k == Typaram) /* the black box again: the fold is the
                          * instance's, and the node stands for the
                          * re-check to fold it there
                          * (04-generics.md) */
      return tyint(IN_USIZE);
    targs[0]->ty = t;
    fnm = bltname(args[0], fe); /* the name: a literal's bytes, a
                                 * const for's round, a const
                                 * parameter -- whatever the
                                 * evaluator resolves, and nothing
                                 * else */
    if (!fnm)                   /* the name named a const parameter this walk holds no
                                 * value for: the fold is the instance's own, and the
                                 * node stands for the re-check to fold it there --
                                 * the same deferral the black-box type took above
                                 * (08-reflection.md) */
      return tyint(IN_USIZE);
    if (t->k != Tystruct && t->k != Tyunion)
      berr(e, "%s has no fields to offset (02-layout.md)", btys(t));
    for (i = 0; i < t->sym->nfields; i++)
      if (strcmp(t->sym->fields[i].name, fnm) == 0)
        break;
    if (i == t->sym->nfields)
      berr(args[0], "'%s' has no field '%s' (02-layout.md)", t->sym->name, fnm);
    { /* the layout's own answer, a constant the walks read as one */
      Ast *x = valtoexpr(valint(fieldoffof(t, i), tyint(IN_USIZE)), e);

      memset(&e->v, 0, sizeof e->v);
      e->k = x->k;
      memcpy(&e->v, &x->v, sizeof e->v);
      return rexpr(e, fe, want);
    }
  }
  if (strcmp(nm, "field") == 0) { /* the field's address, the name
                                   * spelled in the value's bytes:
                                   * rewritten the hand's own borrow,
                                   * every rule the access has the
                                   * rewrite's (08-reflection.md) */
    Type  *vt;
    char  *fnm;
    Field *f;
    usize  i;
    Ast   *acc;

    if (nt != 0 || na != 2)
      berr(e, "@field takes the value and the field's name (08-reflection.md)");
    vt = rplace(args[0], fe); /* a place, read: the borrow this rewrite
                               * spells is of the place's own field,
                               * and borrowing reads, never moves --
                               * a global or a computed base falls to
                               * the value walk (03-move.md) */
    if (!vt)
      vt = rexpr(args[0], fe, 0);
    while (vt && vt->k == Tymut) /* the permission, not the shape */
      vt = vt->t;
    fnm = bltname(args[1], fe); /* the name: a literal's bytes, a const
                                 * for's round (10-iteration.md is what
                                 * makes one compile-time known), a
                                 * const parameter's value (08) */
    if (!fnm) {                 /* the name named a const parameter this walk holds
                                 * no value for: the borrow the rewrite spells is the
                                 * instance's own, and the re-check under the binding
                                 * writes it -- a typed slot or the tail keeps its
                                 * shape here, an untyped let meets the answer's at
                                 * its use (08-reflection.md) */
      if (!want)
        berr(e, "the field's name is a const parameter this walk holds no value for: "
                "the instance's own -- spell the slot's type, or call the fn at compile time "
                "(08-reflection.md)");
      return want;
    }
    if (!vt || vt->k == Typaram) /* a generic's own parameter: the
                                  * fields are the instance's, and
                                  * the borrow this rewrite spells is
                                  * theirs to spell -- outside a walk
                                  * over the type's fields there is
                                  * no name to read, and inside one
                                  * the re-check does the rewrite
                                  * (04-generics.md) */
      berr(args[0], "@field of a generic parameter is the instance's: walk its type's "
                    "fields with a const for, inside the instance (04-generics.md)");
    if (vt->k != Tystruct && vt->k != Tyunion)
      berr(args[0],
           "@field reads a struct's or a union's field: %s is neither "
           "(08-reflection.md)",
           btys(vt));
    for (i = 0; i < vt->sym->nfields; i++)
      if (strcmp(vt->sym->fields[i].name, fnm) == 0)
        break;
    if (i == vt->sym->nfields)
      berr(args[1], "'%s' has no field '%s' (08-reflection.md)", vt->sym->name, fnm);
    f = &vt->sym->fields[i];
    { /* &v.name -- &mut where the field is mut, so a write through
       * the address is governed by the field's own mut, exactly as
       * v.name's is. The borrow checks, the packed rule, the place
       * itself: the access's own, unchanged */
      acc = mknear(Naccess, e);
      acc->v.fld.e = args[0];
      acc->v.fld.name = f->name;
      memset(&e->v, 0, sizeof e->v);
      e->k = Nun;
      e->v.un.op = Tamp;
      e->v.un.mut = f->mut;
      e->v.un.e = acc;
      return rexpr(e, fe, want);
    }
  }
  /* count: packs' own (04-generics.md) */
  berr(e, "@%s arrives with reflection (08-reflection.md)", nm);
  return 0; /* unreachable */
}

/* -- the operator table (07-operators.md) --------------------------------- */

/* what a binary operator does with two operand types, or NULL when
 * they do not fit it. *res gets the result type. */
int
binop(Tok op, Type *a, Type *b, Type **res)
{
  switch (op) {
  case Tplus:
  case Tminus:
  case Tstar:
  case Tslash:
    if (isnumty(a) && tysame(a, b)) {
      *res = a;
      return 1;
    }
    if ((op == Tplus || op == Tminus) && a && a->k == Typtr && isintty(b)) {
      *res = a; /* pointer arithmetic (01-types.md) */
      return 1;
    }
    return 0;
  case Tpercent:
  case Tamp:
  case Tbar:
  case Tcaret:
  case Tshl:
  case Tshr:
    if (isintty(a) && tysame(a, b)) {
      *res = a;
      return 1;
    }
    if ((op == Tamp || op == Tbar || op == Tcaret) && a && a->k == Tybool &&
        tysame(a, b)) { /* a bool is the one-bit integer: and, or,
                         * xor take it whole (07-operators.md), the
                         * rows beside them for the spelled call */
      *res = a;
      return 1;
    }
    if ((op == Tshl || op == Tshr) && isintty(a) && isintty(b)) {
      *res = a; /* any integer shifts (07-operators.md) */
      return 1;
    }
    return 0;
  case Teqeq:
  case Tne:
    if (a && b && tysame(a, b) &&
        (isnumty(a) || a->k == Typtr || a->k == Tybool || a->k == Tyenum || a->k == Tyunit)) {
      *res = tybool();
      return 1;
    }
    return 0;
  case Tlt:
  case Tgt:
  case Tle:
  case Tge:
    if (isnumty(a) && tysame(a, b)) {
      *res = tybool();
      return 1;
    }
    if (a && a->k == Typtr && tysame(a, b)) { /* a walk's stop (01) */
      *res = tybool();
      return 1;
    }
    return 0;
  case Tampamp:
  case Tbarbar:
    if (a && a->k == Tybool && b && b->k == Tybool) {
      *res = tybool();
      return 1;
    }
    return 0;
  default:
    return 0;
  }
}

/* a node the operator's rewrite makes, placed where the operator
 * stood: the operands keep their own places, the wrappers take the
 * operator's, so a rejection names the line the operator was on */
Ast *
opnode(Nk k, Ast *at)
{
  Ast *n = mk(k);

  n->line = at->line;
  n->col = at->col;
  return n;
}

/* the vector append the rewrite's own spelling: n a local, its
 * address the append takes (vec.h) */
void
opvpush(Ast ***vp, Ast *n)
{
  vappend(vp, &n);
}

/* one segment of a path the rewrite spells: a name alone, no generic
 * arguments -- the default and the call's own unifier carry the
 * binding (04-generics.md) */
Ast *
opseg(const char *nm, Ast *at)
{
  Ast *s = opnode(Nseg, at);

  s->v.seg.name = (char *) nm;
  return s;
}

/* std::ops::<x>, three segments so far -- the trait, or Ordering --
 * the whole path spelled, so the operator needs no use
 * (11-namespaces.md). Rooted, ::spelled: the rewrite's own words are
 * the compiler's, not the file's -- a std file sits in its own
 * namespace, where a bare std names nothing, and the root's door is
 * the one path every file reads the same (11-namespaces.md) */
Ast *
oppath(const char *x, Ast *at)
{
  Ast *p = opnode(Npath, at);

  p->v.path.root = 1;
  p->v.path.segs = vnew(Ast *, 4);
  opvpush(&p->v.path.segs, opseg("std", at));
  opvpush(&p->v.path.segs, opseg("ops", at));
  opvpush(&p->v.path.segs, opseg(x, at));
  return p;
}

/* &v, the shared borrow -- the compound's own rewrite still
 * spells one: `a += b` is AddAssign::add_assign(&mut a, b), the
 * left borrowed for the write (07-operators.md) */
Ast *
opborrow(Ast *v, int mut, Ast *at)
{
  Ast *b = opnode(Nun, at);

  b->v.un.op = Tamp;
  b->v.un.mut = mut;
  b->v.un.e = v;
  return b;
}

/* the operand read as a value moves what it holds when the type is
 * not Copy -- and the rewrite below passes the operand itself, so
 * the re-entered walk reads it again and moves it once whole: the
 * move the entry read made unwinds here, the second read the one
 * that stays (07-operators.md). */
void
opunmove(Ast *e, Fenv *fe)
{
  char   buf[256];
  Local *root = placeroot(e, fe, buf, sizeof buf);

  if (root)
    root->dead = 0;
}

/* is the place a bare local -- `a`, not `a.n` or `*p` -- so an
 * assignment's read of it unwinds whole: a field chain may carry a
 * partial move the store must not erase (03-move.md) */
int
opbarelocal(Ast *e)
{
  return e->k == Npath && vlen(e->v.path.segs) == 1;
}

/* the operator as its trait call, what a non-scalar side makes of it
 * (07-operators.md): `a + b` becomes Add::add(a, b), `a != b`
 * becomes !Eq::eq(a, b), and an ordering compares the answer
 * against the end it names -- `a < b` is cmp == Ordering::Less, `a
 * >= b` is cmp != Ordering::Less -- the variants read as themselves,
 * no discriminant spelled anywhere. The operands enter as
 * themselves: by value, the left one moving where its type is not
 * Copy, the right one a value the parameter's own slot takes
 * whole. The node is rewritten in place, the walk re-entered
 * reads its own words. The answer says the operator had a trait
 * to spell: every operator here has one, the remainder and the
 * bitwise and the shifts among them (07-operators.md), the
 * built-in table asked first so a scalar pair never arrives. */
int
optrait(Ast *e, Fenv *fe)
{
  Tok         op = e->v.bin.op;
  Ast        *l = e->v.bin.l, *r = e->v.bin.r;
  const char *tr = 0, *mth = 0;
  const char *is = 0, *isnot = 0;
  Ast        *top;

  switch (op) {
  case Tplus:
    tr = "Add";
    mth = "add";
    break;
  case Tminus:
    tr = "Sub";
    mth = "sub";
    break;
  case Tstar:
    tr = "Mul";
    mth = "mul";
    break;
  case Tslash:
    tr = "Div";
    mth = "div";
    break;
  case Tpercent:
    tr = "Rem";
    mth = "rem";
    break;
  case Tamp:
    tr = "BitAnd";
    mth = "bitand";
    break;
  case Tbar:
    tr = "BitOr";
    mth = "bitor";
    break;
  case Tcaret:
    tr = "BitXor";
    mth = "bitxor";
    break;
  case Tshl:
    tr = "Shl";
    mth = "shl";
    break;
  case Tshr:
    tr = "Shr";
    mth = "shr";
    break;
  case Teqeq:
  case Tne:
    tr = "Eq";
    mth = "eq";
    break;
  case Tlt:
    tr = "Ord";
    mth = "cmp";
    is = "Less";
    break;
  case Tgt:
    tr = "Ord";
    mth = "cmp";
    is = "Greater";
    break;
  case Tle:
    tr = "Ord";
    mth = "cmp";
    isnot = "Greater";
    break;
  case Tge:
    tr = "Ord";
    mth = "cmp";
    isnot = "Less";
    break;
  default:
    return 0;
  }
  opunmove(l, fe);
  opunmove(r, fe);
  { /* the call: std::ops::<Trait>::<method>(l, r) */
    Ast *f = oppath(tr, e);
    Ast *c = opnode(Ncall, e);

    opvpush(&f->v.path.segs, opseg(mth, e));
    c->v.call.f = f;
    c->v.call.args = vnew(Ast *, 2);
    opvpush(&c->v.call.args, l);
    opvpush(&c->v.call.args, r);
    top = c;
    if (is || isnot) { /* the ordering's read: == the end it names, !=
                        * the far one */
      Ast *v = oppath("Ordering", e);
      Ast *b = opnode(Nbin, e);

      opvpush(&v->v.path.segs, opseg(is ? is : isnot, e));
      b->v.bin.op = is ? Teqeq : Tne;
      b->v.bin.l = c;
      b->v.bin.r = v;
      top = b;
    } else if (op == Tne) { /* the negation: != is !eq */
      Ast *n = opnode(Nun, e);

      n->v.un.op = Tbang;
      n->v.un.e = c;
      top = n;
    }
  }
  memset(&e->v, 0, sizeof e->v);
  e->k = top->k;
  memcpy(&e->v, &top->v, sizeof e->v);
  return 1;
}

/* is one side in the domain the built-in operators take -- the
 * numbers, the bool, the pointers (07-operators.md)? A scalar
 * pair the language holds no row of its own for still finds its
 * trait's (bool < bool is Ord's, std carrying the scalar impls),
 * but a scalar pair of mixed types finds nothing: std's scalar
 * rows are all Rhs = Self, so the language's own error answers
 * there -- the rewrite's words would only misdirect it (a
 * method whose parameters it wants otherwise, the report naming
 * the call and not the operator). A pointer's Add<usize>, the
 * spec's own mixed
 * Rhs, makes this worth another look the day its casts land
 * (01-types.md). */
int
opscalar1(Type *t)
{
  return isnumty(t) || (t && (t->k == Tybool || t->k == Typtr));
}

int
opscalars(Type *a, Type *b)
{
  return opscalar1(a) && opscalar1(b);
}

/* the unary operator as its trait call, what a non-scalar operand
 * makes of it (07-operators.md): `-v` becomes Neg::neg(v), the
 * operand entering by value, moving where its type is not Copy.
 * The same in-place rewrite the binary one is: the node becomes
 * the call, the walk re-entered reads its own words. A scalar
 * the checker's own unary does not take never arrives -- the
 * language's own error answers for it, the trait's words would
 * only misdirect the report. */
void
opuntrait(Ast *e, Fenv *fe, const char *tr, const char *mth)
{
  Ast *f = oppath(tr, e);
  Ast *c = opnode(Ncall, e);

  opunmove(e->v.un.e, fe);
  opvpush(&f->v.path.segs, opseg(mth, e));
  c->v.call.f = f;
  c->v.call.args = vnew(Ast *, 1);
  opvpush(&c->v.call.args, e->v.un.e);
  memset(&e->v, 0, sizeof e->v);
  e->k = c->k;
  memcpy(&e->v, &c->v, sizeof e->v);
}

/* the operator's spelling, for diagnostics */
const char *
opname(Tok op)
{
  switch (op) {
  case Tplus:
    return "+";
  case Tminus:
    return "-";
  case Tstar:
    return "*";
  case Tslash:
    return "/";
  case Tpercent:
    return "%";
  case Tamp:
    return "&";
  case Tbar:
    return "|";
  case Tcaret:
    return "^";
  case Tshl:
    return "<<";
  case Tshr:
    return ">>";
  case Teqeq:
    return "==";
  case Tne:
    return "!=";
  case Tlt:
    return "<";
  case Tgt:
    return ">";
  case Tle:
    return "<=";
  case Tge:
    return ">=";
  case Tampamp:
    return "&&";
  case Tbarbar:
    return "||";
  default:
    return "?";
  }
}

/* -- the walk ------------------------------------------------------------- */
