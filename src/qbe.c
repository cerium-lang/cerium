/* qbe.c -- the backend in the process: qbe's passes behind
 * cerium's own door.
 *
 * The tool's main.c drove its passes over a file the shell handed
 * it; the driver here drives the same passes over streams
 * compile() owns -- the .ssa text in memory, the .s text on its
 * way to the system cc -- the pipe and the subprocess gone, the
 * backend one call away. The pass pipeline and the callbacks are
 * qbe's own words, verbatim: pinned by the submodule, the one
 * place the backend may change from. What the tool's main.o held
 * that the passes reach for -- the target the config picked, the
 * debug words -- the driver holds instead, the debug all zero:
 * qbe's -d hands stayed behind with its main.
 *
 * The file speaks qbe's own dialect (C99, its headers' words --
 * the die macro among them): built by its own rule, the one
 * cerium file outside the C89 the rest hold.
 */

#include "../qbe/all.h"
#include "../qbe/config.h"

/* the targets the config may pick, declared in qbe's own main.c
 * alone -- the driver holds its own words for them */
extern Target T_amd64_sysv;
extern Target T_amd64_apple;
extern Target T_amd64_win;
extern Target T_arm64;
extern Target T_arm64_apple;
extern Target T_rv64;

Target T;              /* main.o's own globals, the driver's now */
char   debug['Z' + 1]; /* all zero: no -d of qbe's rides along */

static FILE *outf; /* the .s the passes write, compile()'s own say */

/* the table rows qbe's parse hands up, verbatim from its main.c:
 * data through emitdat, a fn through the pass pipeline in qbe's
 * own order, the arena freed between as its main did */

static void
data(Dat *d)
{
  emitdat(d, outf);
  if (d->type == DEnd) {
    fputs("/* end data */\n\n", outf);
    freeall();
  }
}

static void
func(Fn *fn)
{
  uint n;

  T.abi0(fn);
  fillcfg(fn);
  filluse(fn);
  promote(fn);
  filluse(fn);
  ssa(fn);
  filluse(fn);
  ssacheck(fn);
  fillalias(fn);
  loadopt(fn);
  filluse(fn);
  fillalias(fn);
  coalesce(fn);
  filluse(fn);
  filldom(fn);
  ssacheck(fn);
  gvn(fn);
  fillcfg(fn);
  simplcfg(fn);
  filluse(fn);
  filldom(fn);
  gcm(fn);
  filluse(fn);
  ssacheck(fn);
  if (T.cansel) {
    ifconvert(fn);
    fillcfg(fn);
    filluse(fn);
    filldom(fn);
    ssacheck(fn);
  }
  T.abi1(fn);
  simpl(fn);
  fillcfg(fn);
  filluse(fn);
  T.isel(fn);
  fillcfg(fn);
  filllive(fn);
  fillloop(fn);
  fillcost(fn);
  spill(fn);
  rega(fn);
  fillcfg(fn);
  simpljmp(fn);
  fillcfg(fn);
  assert(fn->rpo[0] == fn->start);
  for (n = 0;; n++)
    if (n == fn->nblk - 1) {
      fn->rpo[n]->link = 0;
      break;
    } else
      fn->rpo[n]->link = fn->rpo[n + 1];
  T.emitfn(fn, outf);
  fprintf(outf, "/* end function %s */\n\n", fn->name);
  freeall();
}

static void
dbgfile(char *fn)
{
  emitdbgfile(fn, outf);
}

/* the whole tool in one call: .ssa text in, .s text out, the
 * passes between -- nothing on the disk the .s does not touch. A
 * malformed .ssa dies inside qbe's own words, the process with
 * it: the same exit the tool's own had */
void
qberun(FILE *inf, FILE *out)
{
  T = Deftgt;
  outf = out;
  parse(inf, "cerium", dbgfile, data, func);
  T.emitfin(outf);
}
