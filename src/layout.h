/* layout.h -- the layout tables of 02-layout.md: how a struct is
 * padded, how an enum carries its tag, what a niche saves. */

#ifndef LAYOUT_H
#define LAYOUT_H

#include "sym.h"
#include "type.h"
#include "vec.h"

#define WORD ((usize) 8)

/* an enum's niche encoding, if it has one (01-types.md, 02-layout.md) */
enum
{
  NICHE_NONE,   /* a tag: the ordinary enum */
  NICHE_OPT,    /* ?T, T a pointer family: Some is the value, None the null */
  NICHE_OKUNIT, /* E?T, T=(): Ok is the null, Err the value */
  NICHE_ERRUNIT /* E?T, E=(): Err is the null, Ok the value */
};

usize alignto(usize off, usize a);
usize intwidth(Type *t); /* an integer's width in bytes, its alignment with it */
Ast  *attrfind(Ast **attrs, const char *name); /* #[name] or #[name(arg)], or NULL */
int   declmodes(Ast *decl);                    /* does its #[cfg] name a mode? */
int   modegated(Sym *s, int rel);              /* is the fn held out of this mode? */
void  modewords(Sym *s, char *buf, usize sz);  /* the modes its #[cfg] names, as words */
void  layoutattrs(Ast *decl, int *packed, usize *alignk);
int   nicheness(Type *t);
usize alignof_(Type *t);
usize sizeof_(Type *t);
usize payloadoff(Type *t);          /* where a tagged enum's payloads begin */
usize fieldoffof(Type *t, usize i); /* a struct's i-th field's offset,
                                     * a union's every one 0 (08) */
Type *tagtyof(Type *t);             /* a variant's tag width, as a type */

#endif
