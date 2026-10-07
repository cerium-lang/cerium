/* emitdata.c -- the data segment and the vtables.
 *
 * A literal with fields left out reads as zero: zeroblk grows one
 * zero block per size, and dswrite spells a const's leaves into
 * the segment it rides, dsvariant giving the tag a variant's row
 * starts with (08-reflection.md). The fns a const evaluates to
 * take a symbol of their own: constsym, one per fn, its lines held
 * for the passes after. A trait's impls print their vtables at
 * the end, one row per method in declaration order: vtname names
 * a pair, printvts writes the lot (06-dispatch.md).
 */

#include <stdio.h>
#include <string.h>

#include "abi.h"
#include "ast.h"
#include "check.h"
#include "die.h"
#include "emit.h"
#include "eval.h"
#include "layout.h"
#include "sym.h"
#include "type.h"
#include "vec.h"

/* a zero block in the data segment, one per size: a literal's
 * left-out fields and elements read as zero (01-types.md), so the
 * storage starts zeroed and what is written lands on top. Per fn:
 * the strings grow the same way, one symbol each */
char *
zeroblk(Em *em, usize sz)
{
  char *d, *base;
  usize i;

  for (i = 0; i < em->nzeros; i++)
    if (em->zerosz[i] == sz)
      return em->zeros[i];
  if (!em->zeros) {
    em->zeros = vnew(char *, 8);
    em->zerosz = vnew(usize, 8);
  }
  base = arenaalloc(16);
  sprintf(base, "$z.%lu", (unsigned long) ++dsn);
  d = arenaalloc(48);
  sprintf(d, "data %s = { z %lu }", base, (unsigned long) sz);
  vappend(&em->datas, &d);
  vappend(&em->zeros, &base);
  vappend(&em->zerosz, &sz);
  em->nzeros = vlen(em->zeros);
  return base;
}

/* -- the segment a const's aggregate rides (08-reflection.md) -----------
 *
 * A const has no address to lend (01-types.md), but its value has
 * bytes, and a body's read copies them: the evaluator's memoized
 * Val spelled as qbe data items, the layout tables' offsets walked
 * and the padding zero. A static's first value rides the same way,
 * its symbol its own slot -- whole-program, writable where mut
 * marked it (01-types.md). */

/* the variant a discriminant names: the value's own half */
static Variant *
dsvariant(Type *t, u64 disc)
{
  usize i;

  for (i = 0; i < t->sym->nvariants; i++)
    if (t->sym->variants[i].disc == disc)
      return &t->sym->variants[i];
  return 0;
}

/* the bytes a value takes, as qbe data items: the offsets the
 * layout tables spell, the evaluator's value folded in, the
 * padding zero. *p is the write cursor; the bytes written return */
static char **dssubs; /* the child lines the value being spelled
                       * holds, collected as dswrite meets the
                       * slices; constsym owns the set */
static usize dsleaves(Type *t);
static usize
dswrite(char **p, Type *t, Val *v)
{
  switch (t->k) {
  case Tybool:
    *p += sprintf(*p, "b %lu, ", (unsigned long) (v ? v->i : 0));
    return 1;
  case Tyint:
    if (t->num >= IN_F32) { /* 9 and 17 significant digits: the
                             * least that round-trips */
      char c = t->num == IN_F32 ? 's' : 'd';

      *p += sprintf(*p, "%c %c_%.*g, ", c, c, c == 's' ? 9 : 17, v ? v->f : 0.0);
      return intwidth(t);
    }
    {
      char c = intwidth(t) == 1 ? 'b' : intwidth(t) == 2 ? 'h' : intwidth(t) == 4 ? 'w' : 'l';

      *p += sprintf(*p, "%c %lu, ", c, (unsigned long) (v ? v->i : 0));
      return intwidth(t);
    }
  case Typtr:
  case Tyvoidptr: /* a compile-time pointer is the null one, but the
                   * walk stays general */
    *p += sprintf(*p, "l %lu, ", (unsigned long) (v ? v->i : 0));
    return WORD;
  case Tystruct: {
    Sym  *s = t->sym;
    int   packed;
    usize alignk, off = 0, cur = 0, i;

    layoutattrs(s->decl, &packed, &alignk);
    for (i = 0; i < s->nfields; i++) { /* the walk foffset takes */
      Type *ft = gsubst(s->fields[i].ty, s->gparams, t->args, t->nargs);
      usize w;

      if (!packed)
        off = alignto(off, alignof_(ft));
      if (off > cur) {
        *p += sprintf(*p, "z %lu, ", (unsigned long) (off - cur));
        cur = off;
      }
      w = dswrite(p, ft, v ? &v->elems[i] : 0);
      cur += w;
      off += sizeof_(ft);
    }
    if (sizeof_(t) > cur) { /* the tail: the symbol's size is the
                             * type's, the reads walk it all */
      *p += sprintf(*p, "z %lu, ", (unsigned long) (sizeof_(t) - cur));
      cur = sizeof_(t);
    }
    return cur;
  }
  case Tyunion: {
    usize sz = sizeof_(t), w = 0;

    if (v && v->tag != VNONROW) { /* the one row a literal wrote; the
                                   * zeroed whole reads zero every
                                   * row (01-types.md) */
      Type *ft = gsubst(t->sym->fields[v->tag].ty, t->sym->gparams, t->args, t->nargs);

      w = dswrite(p, ft, &v->elems[0]);
    }
    if (sz > w) /* the rows overlap: what the active one left is zero */
      *p += sprintf(*p, "z %lu, ", (unsigned long) (sz - w));
    return sz;
  }
  case Tytuple: {
    usize i, off = 0, cur = 0;

    for (i = 0; i < t->nargs; i++) { /* the walk Ntupidx takes */
      usize w;

      off = alignto(off, alignof_(t->args[i]));
      if (off > cur) {
        *p += sprintf(*p, "z %lu, ", (unsigned long) (off - cur));
        cur = off;
      }
      w = dswrite(p, t->args[i], v ? &v->elems[i] : 0);
      cur += w;
      off += sizeof_(t->args[i]);
    }
    if (sizeof_(t) > cur) {
      *p += sprintf(*p, "z %lu, ", (unsigned long) (sizeof_(t) - cur));
      cur = sizeof_(t);
    }
    return cur;
  }
  case Tyarray: {
    usize i, cur = 0;

    for (i = 0; i < t->n; i++) /* the elements packed, no padding
                                * between them (02-layout.md) */
      cur += dswrite(p, t->t, v ? &v->elems[i] : 0);
    if (sizeof_(t) > cur) {
      *p += sprintf(*p, "z %lu, ", (unsigned long) (sizeof_(t) - cur));
      cur = sizeof_(t);
    }
    return cur;
  }
  case Tyslice: { /* the two words a slice is: the child symbol its
                   * rows ride, the count after it. @typeinfo's are
                   * the only slices a value holds (08-reflection.md)
                   * -- the user's own literals make arrays, []T
                   * borrows at runtime -- so the rows here are owned
                   * data, spelled beside the parent as the child's
                   * own data line */
    char *base, *line, *q;
    usize n = v ? v->len : 0, cur = 0, i;

    if (!n) { /* empty: no rows anywhere, the reads walk none */
      *p += sprintf(*p, "z %lu, ", (unsigned long) (2 * WORD));
      return 2 * WORD;
    }
    base = arenaalloc(16);
    sprintf(base, "$s.%lu", (unsigned long) ++dsn);
    line = arenaalloc(dsleaves(t->t) * n * 32 + 2 * 32 + 64);
    q = line + sprintf(line, "data %s = { ", base);
    for (i = 0; i < n; i++) /* the rows packed, the array rule */
      cur += dswrite(&q, t->t, &v->elems[i]);
    if (n * sizeof_(t->t) > cur) {
      q += sprintf(q, "z %lu, ", (unsigned long) (n * sizeof_(t->t) - cur));
      cur = n * sizeof_(t->t);
    }
    q -= 2; /* the last item's ", ": the line's own close */
    sprintf(q, "}");
    vappend(&dssubs, &line); /* the parent's set: replayed with it */
    *p += sprintf(*p, "l %s, l %lu, ", base, (unsigned long) n);
    return 2 * WORD;
  }
  case Tyenum: {
    usize sz = sizeof_(t);

    if (nicheness(t) != NICHE_NONE) { /* the one word it is: the unit
                                       * side the null, the other
                                       * side the value (02) */
      Variant *vr = v ? dsvariant(t, v->tag) : 0;
      usize    np = 0;

      if (vr)
        np = vr->named ? vr->nfields : vr->npayload;
      if (np && v) /* the value's own bits, the pointer itself */
        *p += sprintf(*p, "l %lu, ", (unsigned long) v->elems[0].i);
      else
        *p += sprintf(*p, "z %lu, ", (unsigned long) sz);
      return sz;
    }
    { /* the discriminant, then the payloads packed after it, the
       * walk emavariant takes */
      Type    *tt = tagtyof(t);
      Variant *vr = v ? dsvariant(t, v->tag) : 0;
      char   c = intwidth(tt) == 1 ? 'b' : intwidth(tt) == 2 ? 'h' : intwidth(tt) == 4 ? 'w' : 'l';
      usize  off, i, np = 0, cur;
      Type **ps = 0;

      *p += sprintf(*p, "%c %lu, ", c, (unsigned long) (v ? v->tag : 0));
      cur = intwidth(tt);
      off = payloadoff(t);
      if (off > cur) {
        *p += sprintf(*p, "z %lu, ", (unsigned long) (off - cur));
        cur = off;
      }
      if (vr) { /* the active variant's own payload types */
        if (vr->named) {
          ps = tyargs(vr->nfields);
          for (i = 0; i < vr->nfields; i++)
            ps[i] = gsubst(vr->fields[i].ty, t->sym->gparams, t->args, t->nargs);
          np = vr->nfields;
        } else if (vr->payload && vr->npayload) {
          ps = tyargs(vr->npayload);
          for (i = 0; i < vr->npayload; i++)
            ps[i] = gsubst(vr->payload[i], t->sym->gparams, t->args, t->nargs);
          np = vr->npayload;
        }
        for (i = 0; i < np; i++)
          cur += dswrite(p, ps[i], v ? &v->elems[i] : 0);
      }
      if (sz > cur) {
        *p += sprintf(*p, "z %lu, ", (unsigned long) (sz - cur));
        cur = sz;
      }
      return cur;
    }
  }
  default: /* unit and the rest: nothing rides, no bytes taken */
    return 0;
  }
}

/* an upper bound on the items the value spells, for the line's
 * allocation: each takes its literal's worst case, a float's 17
 * digits among the letters */
static usize
dsleaves(Type *t)
{
  usize i, n = 0;

  switch (t->k) {
  case Tystruct:
    for (i = 0; i < t->sym->nfields; i++)
      n += dsleaves(gsubst(t->sym->fields[i].ty, t->sym->gparams, t->args, t->nargs));
    return n;
  case Tyunion:
    for (i = 0; i < t->sym->nfields; i++) /* the widest row would do;
                                           * the sum is simpler */
      n += dsleaves(gsubst(t->sym->fields[i].ty, t->sym->gparams, t->args, t->nargs));
    return n;
  case Tytuple:
    for (i = 0; i < t->nargs; i++)
      n += dsleaves(t->args[i]);
    return n;
  case Tyarray:
    return t->n ? dsleaves(t->t) * t->n : 1;
  case Tyslice: /* the reference's two words: the child symbol's
                 * spelling, the count */
    return 2;
  case Tyenum: {
    usize m = 0;

    if (nicheness(t) != NICHE_NONE)
      return 1;
    for (i = 0; i < t->sym->nvariants; i++) { /* the widest payload */
      Variant *vr = &t->sym->variants[i];
      usize    k = 0, j;

      if (vr->named)
        for (j = 0; j < vr->nfields; j++)
          k += dsleaves(gsubst(vr->fields[j].ty, t->sym->gparams, t->args, t->nargs));
      else
        for (j = 0; j < vr->npayload; j++)
          k += dsleaves(gsubst(vr->payload[j], t->sym->gparams, t->args, t->nargs));
      if (k > m)
        m = k;
    }
    return 2 + m;
  }
  default:
    return 1;
  }
}

/* the data symbol a const's or a static's value rides: the line
 * printed after the fns with the rest, the symbol handed to whoever
 * reads the value -- a blit's source, a place's base. The symbols
 * are the compilation's own -- a name's line goes out once a pass,
 * with whichever fn first read it, and the naming pass's text goes
 * nowhere, so the real pass takes it again */
static Sym   **conss;     /* the ones already named, per compilation */
static char  **conssyms;  /* their symbols, parallel */
static char  **conslines; /* their data lines, for the passes after */
static char ***conssubs;  /* their slices' child lines, parallel: the
                           * rows a slice's words point at, spelled as
                           * data lines of their own -- replayed beside
                           * the parent's, or a later pass reads a
                           * symbol nothing spelled */
static int  *conspass;    /* the pass each line last went out in */
static usize nconss;

char *
constsym(Em *em, Sym *s)
{
  char *base, *line, *p;
  usize i, sz;

  for (i = 0; i < nconss; i++)
    if (conss[i] == s) {
      if (conspass[i] == ipass)
        return conssyms[i]; /* this pass already carries the line */
      vappend(&em->datas, &conslines[i]);
      {
        usize j;

        for (j = 0; j < vlen(conssubs[i]); j++)
          vappend(&em->datas, &conssubs[i][j]);
      }
      conspass[i] = ipass;
      return conssyms[i];
    }
  /* the symbol: the kind, the namespace's own path, then the
   * name -- the same chain a fn's mangle walks (projhead),
   * spelled in dots, for the identifier's own law keeps a dot
   * out of a name. The path a fn carries and this once did not
   * was the bug: two namespaces' same-named slots met at the
   * assembler, one symbol for two values. A root slot keeps the
   * old shape -- the path nothing, the dots none */
  {
    Ns   *chain[64];
    Ns   *ns = s->ownns;
    usize d = 0, len;

    while (ns && ns->parent) {
      if (d == 64)
        die("a namespace nesting too wide for the emitter's names");
      chain[d++] = ns;
      ns = ns->parent;
    }
    len = strlen(s->name) + 16;
    for (i = 0; i < d; i++)
      len += strlen(chain[i]->name) + 1;
    base = arenaalloc(len);
    p = base + sprintf(base, "$%s.", s->kind == Sconst ? "const" : "static");
    for (i = d; i-- > 0;) /* outermost first, the path's own order */
      p += sprintf(p, "%s.", chain[i]->name);
    sprintf(p, "%s", s->name);
  }
  sz = sizeof_(s->cty);
  line = arenaalloc(dsleaves(s->cty) * 32 + 2 * 32 + 64);
  p = line + sprintf(line, "data %s = { ", base);
  dssubs = vnew(char *, 8);
  if (sz) {
    Val top; /* the Sym's halves, as the walk's one value: cval the
              * tag half -- an enum's discriminant, a union's active
              * row -- celems the elements beside it, clen the length
              * a slice's own (@typeinfo's are the only slices a
              * const holds -- 08-reflection.md); tyval the one half
              * no bytes ride: a type's value carries none */

    top.t = s->cty;
    top.i = s->cval;
    top.f = s->cflt;
    top.tag = s->ctag;
    top.tyval = 0;
    top.elems = s->celems;
    top.len = s->clen;
    dswrite(&p, s->cty, &top);
    p -= 2; /* the last item's ", ": the line's own close */
    sprintf(p, "}");
  } else /* a sizeless aggregate: a byte the symbol wants, nothing
          * reads it */
    sprintf(p, "z 1 }");
  vappend(&em->datas, &line);
  {
    usize j;

    for (j = 0; j < vlen(dssubs); j++)
      vappend(&em->datas, &dssubs[j]);
  }
  if (!conss) {
    conss = vnew(Sym *, 8);
    conssyms = vnew(char *, 8);
    conslines = vnew(char *, 8);
    conssubs = vnew(char **, 8);
    conspass = vnew(int, 8);
  }
  vappend(&conss, &s);
  vappend(&conssyms, &base);
  vappend(&conslines, &line);
  vappend(&conssubs, &dssubs);
  {
    int pi = ipass;

    vappend(&conspass, &pi);
  }
  nconss = vlen(conss);
  return base;
}

/* -- vtables (06-dispatch.md) -------------------------------------------- */

/* One table per trait and concrete type, an entry a method in the
 * trait's declaration order. The construction site names its pair;
 * the tables print after every fn, the instances they name already
 * emitted -- a generic impl's method lands here as an instance the
 * no static call ever found, so the print is what ensures it. */
typedef struct Vt Vt;
struct Vt
{
  Sym  *tr;    /* the trait */
  Type *ty;    /* the concrete type behind the handle */
  char *name;  /* the data symbol, $-less */
  char *tramp; /* the pointer row's trampoline, its own fn, named
                * once and shared by the passes (06-dispatch.md) */
};

static Vt   *vts;       /* every pair named, in first-seen order */
static usize vtprinted; /* how many of them the text already holds */
static usize ntramp;    /* the trampolines named, in the same order */

/* the table's own name: the trait's whole path, the concrete type's
 * code beside it -- the pair the construction site names, spelled
 * so two pairs fold the same only by being the same pair (06-dispatch.md) */
char *
vtname(Sym *tr, Type *ty)
{
  char  buf[1024];
  usize i, o;

  for (i = 0; i < vlen(vts); i++)
    if (vts[i].tr == tr && vts[i].ty == ty)
      return vts[i].name;
  if (2 >= sizeof buf)
    die("a name too wide for the emitter's names");
  memcpy(buf, "vt", 3);
  o = 2;
  if (tr->ownns && tr->ownns->parent)
    o = tymns(tr->ownns, buf, o, sizeof buf);
  o = segput(buf, o, sizeof buf, tr->name);
  o = tymang2(ty, buf, o, sizeof buf);
  buf[o] = 0;
  {
    Vt    vt;
    char *n = arenaalloc(o + 1);

    memcpy(n, buf, o + 1);
    memset(&vt, 0, sizeof vt); /* the fields a pair never fills hold
                                * zero, not the stack's leftovers */
    vt.tr = tr;
    vt.ty = ty;
    vt.name = n;
    if (!vts)
      vts = vnew(Vt, 8);
    vappend(&vts, &vt);
    return n;
  }
}

/* the pointer's own row, laid as a fn of its own: the fat's first
 * word is the fn the handle holds, the slot's ABI hands it %self,
 * and the call drops it -- every argument forwarded as it arrived,
 * the aggregate addresses included, qbe lowering the rest
 * (06-dispatch.md). The scratch pass names the aggregate types this
 * signature wears, so the declarations print ahead of the text */
static void
printtramp(FILE *o, Vt *vt)
{
  Type *t = vt->ty;
  usize i;

  if (t->k != Tyfn) /* unreachable: the caller checked */
    return;
  if (!vt->tramp) {
    vt->tramp = arenaalloc(32);
    sprintf(vt->tramp, "vttramp.%lu", (unsigned long) ++ntramp);
  }
  fprintf(o, "function %s $%s(l %%self", sigty(t->t, 0), vt->tramp);
  for (i = 0; i < t->nargs; i++)
    fprintf(o, ", %s %%a.%lu", sigty(t->args[i], 0), (unsigned long) i);
  fprintf(o, ") {\n@start\n\t%%r =%s call %%self(", sigty(t->t, 0));
  for (i = 0; i < t->nargs; i++)
    fprintf(o, "%s%s %%a.%lu", i ? ", " : "", sigty(t->args[i], 0), (unsigned long) i);
  fprintf(o, ")\n\tret %%r\n}\n");
}

/* the tables not yet printed: one line each, its entries the impl
 * the pair picked -- a pattern impl's methods as instances, ensured
 * here so the drain that follows emits them */
void
printvts(FILE *o)
{
  usize i, j;

  for (i = vtprinted; i < vlen(vts); i++) {
    Vt    *vt = &vts[i];
    Sym   *im;
    Type **tys = 0;
    char  *line;
    char  *p;
    char **nms = vnew(char *, 8); /* the entries' names, spelled
                                   * ahead of the line: its length
                                   * is what they say, not a guess
                                   * a longer mangle outgrows */
    usize len, nn = 0;

    im = implfor(vt->tr, vt->ty, &tys);
    if (!im && vt->ty->k != Tyfn) /* the fn pointer's own row: no
                                   * impl a file spells, the
                                   * trampoline the entry names
                                   * (05-traits.md, 06) */
      cerrat(vt->tr->decl, "unreachable: the construction site checked");
    for (j = 0; j < vt->tr->nmembers; j++) {
      Member *tm = &vt->tr->members[j];
      Member *fm = 0;
      usize   k;
      char   *nm;

      if (tm->kind != Mfn)
        continue;          /* a handle exposes the methods (06-dispatch.md) */
      if (!im) {           /* the pointer's own row: the trampoline its one
                            * entry (06-dispatch.md) */
        printtramp(o, vt); /* names it, and the types it wears */
        nm = vt->tramp;
        vappend(&nms, &nm);
        nn++;
        continue;
      }
      for (k = 0; k < im->nmembers; k++)
        if (strcmp(im->members[k].name, tm->name) == 0) {
          fm = &im->members[k];
          break;
        }
      if (!fm || fm->kind != Mfn)
        cerrat(vt->tr->decl, "unreachable: the impl supplies it");
      if (!fm->sym) { /* the closure's own fn: the literal's own
                       * name, the one the drain gave it
                       * (05-traits.md) */
        if (!fm->decl || fm->decl->k != Nclosure || !fm->decl->v.clos.sym)
          cerrat(vt->tr->decl, "unreachable: the impl supplies it");
        nm = fm->decl->v.clos.sym;
      } else if (tys)
        nm = instensure(fm->sym, tys, 0, 0)->name; /* the instance this
                                                    * table names */
      else
        nm = fsymname(fm->sym, fm->sym->decl);
      vappend(&nms, &nm);
      nn++;
    }
    len = strlen(vt->name) + 16; /* data $ = { } and the NUL */
    for (j = 0; j < nn; j++)
      len += strlen(nms[j]) + 8; /* the entry's own: l $ NAME,  */
    line = arenaalloc(len);
    p = line + sprintf(line, "data $%s = { ", vt->name);
    for (j = 0; j < nn; j++)
      p += sprintf(p, "l $%s, ", nms[j]);
    sprintf(p, "}");
    if (ipass == 2)
      fprintf(o, "%s\n", line);
  }
  vtprinted = vlen(vts);
}

/* the tables all printed: the driver's loops wait on this between
 * the drains -- a literal's body names instances and tables both,
 * so the drains walk together until this holds */
int
vtssettled(void)
{
  return vtprinted == vlen(vts);
}

/* the naming pass burned numbers, the text restarts with it: the
 * driver's own reset between the passes */
void
vtsreset(void)
{
  vtprinted = 0;
}
