/* cfg.c -- pass zero: the conditions' own cull.
 *
 * A #[cfg] names the conditions a declaration lives under -- three
 * dimensions, the system (linux, darwin) and the machine (amd64,
 * arm64) and the mode (debug, release, -r's own word), the words
 * and-ed within a pair of parentheses and across attributes, no
 * negation anywhere (12-projects.md). The platform words cull in
 * this pass, the checker's first walk: before a single name is
 * declared, every file's items are rebuilt without the ones whose
 * words are false, so a culled item is not hidden but absent -- not
 * its declare, not its uses, not its impls, not its bodies -- and
 * the Syms pass 1 returns stay parallel to what is left. A mode
 * word culls every item but a fn the same way; a fn it hands to the
 * call-site removal instead, the body's pass owning the door
 * (01-types.md, Mode-gated functions). The platform the words read
 * is the one this compiler runs on, host and target the same
 * machine; a uname the tables cannot name keeps every word false,
 * the honest answer for a system the compiler was never told about.
 */

#include <string.h>
#include <sys/utsname.h> /* uname: the platform the words read */

#include "cfg.h" /* this file's own face */
#include "lex.h" /* lexsetpath: the cull's errors name the file, and
                   * the vec.h a vec wants */
#include "sym.h" /* chk_rel: the mode this compile runs, a mode
                  * dimension's own answer (12-projects.md) */

static char cfgos[16];   /* "linux" or "darwin", or "" -- a system the
                          * tables do not know (12-projects.md) */
static char cfgarch[16]; /* "amd64" or "arm64", or "" -- a machine the
                          * tables do not know */

static const char *const cfgsystems[] = {"linux", "darwin"};
static const char *const cfgmachines[] = {"amd64", "arm64"};
static const char *const cfgmodes[] = {"debug", "release"};

/* the mode this compile runs in, the word the mode dimension's own
 * words read: -r's say, held where the flags are (sym.h), debug every
 * compile without it (12-projects.md) */
static const char *
cfgmode(void)
{
  return chk_rel ? "release" : "debug";
}

/* the platform this compiler runs on, the one it compiles for: the
 * two are the same machine, the honest shape for a compiler without a
 * cross target (12-projects.md). A uname the tables cannot name leaves
 * both empty -- and every named condition false, the honest answer
 * for a system the tables do not know */
static void
cfginit(void)
{
  struct utsname u;
  static int     done;

  if (done)
    return;
  done = 1;
  if (uname(&u) != 0)
    return;
  if (strcmp(u.sysname, "Linux") == 0)
    strcpy(cfgos, "linux");
  else if (strcmp(u.sysname, "Darwin") == 0)
    strcpy(cfgos, "darwin");
  if (strcmp(u.machine, "x86_64") == 0 || strcmp(u.machine, "amd64") == 0)
    strcpy(cfgarch, "amd64");
  else if (strcmp(u.machine, "arm64") == 0 || strcmp(u.machine, "aarch64") == 0)
    strcpy(cfgarch, "arm64");
}

/* 1 the systems' dimension, 2 the machines', 3 the modes', 0 a name
 * no table holds -- the unknown-name error the cull reads is the typo
 * guard every dimension shares (12-projects.md) */
static int
cfgdim(const char *name)
{
  usize i;

  for (i = 0; i < sizeof cfgsystems / sizeof cfgsystems[0]; i++)
    if (strcmp(name, cfgsystems[i]) == 0)
      return 1;
  for (i = 0; i < sizeof cfgmachines / sizeof cfgmachines[0]; i++)
    if (strcmp(name, cfgmachines[i]) == 0)
      return 2;
  for (i = 0; i < sizeof cfgmodes / sizeof cfgmodes[0]; i++)
    if (strcmp(name, cfgmodes[i]) == 0)
      return 3;
  return 0;
}

/* does one #[cfg]'s arguments hold a mode word? -- a fn's modes
 * belong in one #[cfg], never two: the words one pair of parentheses
 * meets with and the call-site removal the mode words hand a fn to
 * do not compose (12-projects.md) */
static int
cfgmodesin(Ast *at)
{
  usize i;

  for (i = 0; i < vlen(at->v.seg.args); i++) {
    Ast *a = at->v.seg.args[i];

    if (a->k == Npath && !a->v.path.root && vlen(a->v.path.segs) == 1 &&
        cfgdim(a->v.path.segs[0]->v.seg.name) == 3)
      return 1;
  }
  return 0;
}

/* one #[cfg(...)]'s word: its arguments and-ed, a name from each
 * dimension at most -- the clash error, two words from one dimension
 * in one pair of parentheses, guarding the hand that meant one of
 * each. The parser made each argument a single-segment path; a
 * literal is a misplaced one, a path with segments left names a
 * namespace the conditions do not take (12-projects.md).
 *
 * A mode word is a fn's own door on this walk: the fn is not culled
 * but handed to the call-site removal the body's pass owns
 * (01-types.md, Mode-gated functions) -- so the word holds no say
 * over a fn's keep here, while on every other item it is the
 * platform words' own cull, the item absent in the modes it does not
 * name (12-projects.md) */
static int
cfgattr(Ast *at, int isfn)
{
  Ast       **as = at->v.seg.args;
  usize       i;
  const char *sys = 0, *mac = 0, *mod = 0;
  int         keep = 1;

  if (!vlen(as))
    cerrat(at, "#[cfg] takes a condition -- linux, darwin, amd64, arm64, debug or"
               " release (12-projects.md)");
  for (i = 0; i < vlen(as); i++) {
    Ast        *a = as[i];
    const char *nm;

    if (a->k != Npath || a->v.path.root || vlen(a->v.path.segs) != 1)
      cerrat(a, "#[cfg] takes a name -- linux, darwin, amd64, arm64, debug or"
                " release (12-projects.md)");
    nm = a->v.path.segs[0]->v.seg.name;
    if (cfgdim(nm) == 1) {
      if (sys && strcmp(sys, nm) != 0)
        cerrat(a,
               "'%s' and '%s' are both systems -- one #[cfg] takes one of each"
               " dimension (12-projects.md)",
               sys, nm);
      sys = nm;
      if (strcmp(nm, cfgos) != 0)
        keep = 0;
    } else if (cfgdim(nm) == 2) {
      if (mac && strcmp(mac, nm) != 0)
        cerrat(a,
               "'%s' and '%s' are both machines -- one #[cfg] takes one of"
               " each dimension (12-projects.md)",
               mac, nm);
      mac = nm;
      if (strcmp(nm, cfgarch) != 0)
        keep = 0;
    } else if (cfgdim(nm) == 3) {
      if (mod && strcmp(mod, nm) != 0)
        cerrat(a,
               "'%s' and '%s' are both modes -- one #[cfg] takes one of each"
               " dimension (12-projects.md)",
               mod, nm);
      mod = nm;
      if (!isfn && strcmp(nm, cfgmode()) != 0)
        keep = 0;
    } else
      cerrat(a,
             "unknown condition '%s' -- linux, darwin, amd64, arm64, debug or"
             " release (12-projects.md)",
             nm);
  }
  return keep;
}

/* the words of one dimension never hold together, and several
 * #[cfg]s meet with and: the same dimension across attributes is as
 * false as within one pair of parentheses -- the same hand, the same
 * guard, stopped the same way (12-projects.md). The shape's own
 * errors and the unknown names are cfgattr's to say; this walk only
 * carries each attribute's words to the next one's */
static void
cfgcrossdim(Ast *at, const char **sys, const char **mac, const char **mod)
{
  usize i;

  for (i = 0; i < vlen(at->v.seg.args); i++) {
    Ast        *a = at->v.seg.args[i];
    const char *nm;

    if (a->k != Npath || a->v.path.root || vlen(a->v.path.segs) != 1)
      return;
    nm = a->v.path.segs[0]->v.seg.name;
    if (cfgdim(nm) == 1) {
      if (*sys && strcmp(*sys, nm) != 0)
        cerrat(a,
               "'%s' and '%s' are both systems -- several #[cfg]s meet with and,"
               " and the words of one dimension never hold together"
               " (12-projects.md)",
               *sys, nm);
      *sys = nm;
    } else if (cfgdim(nm) == 2) {
      if (*mac && strcmp(*mac, nm) != 0)
        cerrat(a,
               "'%s' and '%s' are both machines -- several #[cfg]s meet with"
               " and, and the words of one dimension never hold together"
               " (12-projects.md)",
               *mac, nm);
      *mac = nm;
    } else if (cfgdim(nm) == 3) {
      if (*mod && strcmp(*mod, nm) != 0)
        cerrat(a,
               "'%s' and '%s' are both modes -- several #[cfg]s meet with and,"
               " and the words of one dimension never hold together"
               " (12-projects.md)",
               *mod, nm);
      *mod = nm;
    }
  }
}

/* does an item live here? Every #[cfg] it carries and-ed, and no
 * negation: a library lists the platforms it supports rather than the
 * ones it does not -- "not on this one" is every other platform
 * written out (12-projects.md). A word already false does not stop
 * the walk: the later arguments, and the later attributes, still read
 * their checks -- a typo on a culled branch as loud as one on a kept
 * one.
 *
 * A fn carries at most one #[cfg] that holds a mode word: the words
 * of two would meet with and -- a fn neither mode holds -- while the
 * call-site removal reads them as the one door's own words, a fn
 * every mode holds. The two shapes do not compose, so the hand is
 * stopped (12-projects.md) */
static int
itemcfg(Ast *it)
{
  usize       i;
  int         keep = 1;
  int         isfn = it->k == Nfn;
  int         modes = 0;
  const char *sys = 0, *mac = 0, *mod = 0;

  if (!it->attrs)
    return 1;
  for (i = 0; i < vlen(it->attrs); i++)
    if (strcmp(it->attrs[i]->v.seg.name, "cfg") == 0) {
      if (isfn && cfgmodesin(it->attrs[i]) && modes++)
        cerrat(it->attrs[i], "a fn's modes belong in one #[cfg]: #[cfg(debug)] or #[cfg(release)],"
                             " never two (12-projects.md)");
      if (!cfgattr(it->attrs[i], isfn))
        keep = 0;
      cfgcrossdim(it->attrs[i], &sys, &mac, &mod);
    }
  return keep;
}

/* pass zero, before the four: every file's items rebuilt without the
 * ones the platform culls. The cull is why a culled item never
 * exists -- not its declare, not its uses, not its impls, not its
 * bodies; the Syms stay parallel to what is left, and no later pass
 * walks a list the culled entered (12-projects.md) */
void
cfgcull(Srcfile **files, usize nfiles)
{
  usize f;

  cfginit();
  for (f = 0; f < nfiles; f++) {
    Srcfile *sf = files[f];
    Ast    **keep = vnew(Ast *, 8);
    usize    i;

    lexsetpath(sf->path); /* the cull's errors name the file the item
                           * sits in, like every pass's */
    for (i = 0; i < vlen(sf->items); i++)
      if (itemcfg(sf->items[i]))
        vappend(&keep, &sf->items[i]);
    sf->items = keep;
  }
}
