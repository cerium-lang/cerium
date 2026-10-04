# Cerium

A system programming language: Rust's moves, Zig's pointers and
compile-time execution, C++'s generics — in a compiler that is **here
and running**, stage 0 written in **C89**, codegen through
[QBE](https://c9x.me/compile/).

There is no borrow checker and no reference type — one pointer family,
lifetimes nowhere. Ownership is Rust's: moves, `Copy`, `Drop`,
destructors inserted statically, and what aliasing would prove is
proven only where the compiler can see it, within a function — the
guarantees are uneven [on purpose](./specs/README.md#what-cerium-guarantees).
Compile-time execution is Zig's answer standing in for a macro system:
a `const` initializer runs in the compiler, reflection reads the type
tables, nothing rewrites tokens. The generics are C++'s — variadic
packs, specialization ordered by shape pattern — with a check none of
the three has: overlap is ruled out where the impls are declared, not
where they are instantiated.

```ce
struct Vec2 { mut x: i32, mut y: i32 }

impl Vec2 {
  fn len2(self: *Vec2) -> i32 { self.x * self.x + self.y * self.y }
  fn scale(self: *mut Vec2, k: i32) { self.x = self.x * k; self.y = self.y * k; }
}

trait Show { fn show(self: *Self) -> i32; }

impl Show for Vec2 {
  fn show(self: *Self) -> i32 { self.len2() }
}

const BASE: Vec2 = Vec2{ x: 3, y: 4 };  /* evaluated in the compiler */

fn main() -> i32 {
  let mut v = BASE;           /* a const copies into a mut slot */
  v.scale(2);                 /* the &mut ends with the call */
  let h: dyn Show = &dyn v;   /* the fat handle freezes what it holds */
  h.show()                    /* 100, the tail expression returns */
}
```

## Building it

```
git clone --recurse-submodules https://github.com/cerium-lang/cerium
cd cerium && make             # gcc; make CC=clang works too
make qbe/qbe                  # the backend, on demand
make test                     # golden tests + codegen's run tests
```

The `qbe/` submodule is the backend, pointing at a
[fork](https://github.com/mivinci/qbe) that tracks upstream master —
`git submodule update --init -- qbe` pulls it on demand, and without
it `make test` skips the run section. Run `make hooks` once per
checkout so commits format what they stage. Stage 0 carries no host
framework: one bare Makefile, `src/vec.h` the container layer, the
compiler emitting `.ssa` text that `qbe` lowers and the system `cc`
links. The goal is self-hosting, with LLVM a v1+ backend rather than
a v0 dependency.

Five flags, each a pass's dump: `cerium -t file.ce` the token stream,
a line a token; `-a` the parse tree as S-expressions; `-T` what
checking made of every item — declarations, types, each declaration's
resolved shape. The last three read a directory as a project, every
`.ce` under it a file of it, each in the namespace its path spells
(`12-projects.md`): `-s` prints the whole unit's `.ssa` text, and
`-c file.ce -o out` runs the pipeline — emit, `qbe` as a subprocess,
the system `cc` to link. `-r` rides `-s` and `-c`: release, the
runtime checks out, the wraps a release owns (`01-types.md`).

## What the compiler carries

- **the front end** — the grammar whole (`15-grammar.md`): expressions
  through statements, the method sugar, packs, type values, the
  attribute forms
- **the checker** — namespaces and `use` in its four shapes, generics
  with defaults and const parameters, trait impls ordered by
  specificity, operators as trait calls — `a + b` is `Add::add(a,
  b)`, both operands by value, the traits and the built-in rows
  in `std::ops`, the compound assignments their own, the borrowed
  pairs one generic row — `&a + &b` over any Copy element through
  `T::Output`, a bound carrying the trait's own arguments —
  `T: Add<usize>` answered only by the rows that take them — and
  the call picking the row itself: the rows the receiver fits
  walk in specificity order and the first whose signature takes
  the arguments answers, spelled or sugar, a row whose variables
  the receiver alone cannot land waiting on the arguments the
  trial binds as it walks (`07-operators.md`) —
  the flow rules — moves, frozen borrows,
  `mut`'s two levels, narrowing, a temporary's `&` its own
  materialised place, reassignment running the old value's
  destructor before the store — structural, the rows a type
  inherits walked in reverse — `@take` the sanctioned move-out —
  the value out, the zero value back — a scope's close running
  its live bindings' destructors in reverse declaration order,
  the early exits carrying theirs, a bare temporary's at its
  statement's end, a `for`'s pattern bindings once a round,
  panic running none (`03-move.md`) — and
  compile-time
  evaluation: const and static initializers, plain-fn calls,
  aggregate values, the const `for`, `$$` splices, `@typeinfo` and
  the field walk (`08-reflection.md`)
- **emit** — qbe `.ssa`, the `cerium_` mangle folding namespaces, the
  data segment under const values, `@take` the value out and the
  zero block back, an assignment's destructor after the new value
  is fixed to its own storage — even `x = x` — the scope drops
  the checker pre-made — a block's close, the early exits, a
  temporary's statement end, an enum's rows through a pre-built
  `match`, a `?`'s propagation spelled the same way
  (`03-move.md`) — and a debug build's four runtime
  checks — index, arithmetic overflow, shift, cast — every failure one
  call into std's panic (`01-types.md`)
- **std** — a directory the compiler reads as the project's first
  files (`CERIUM_SYSROOT` names where it lives), the prelude —
  `Option`, `Result`, `Copy`, `Drop`, `panic` — bound without a use,
  `pub use` the re-export (`11-namespaces.md`), `std::ops` the
  operator traits (`07-operators.md`), `std::fmt` the `Fmt` a type
  implements to print itself and the `Writer` that joins a sink to
  the typed writes, `std::io` the `Write` anything that takes bytes
  implements, the two the process was born with, and the prints —
  `print`, `eprint`, the general `fmt_to` (`01-types.md`)

### Testing

Four golden suites: `tests/lex`, `tests/parse` and `tests/check`, each
split `ok/` against `err/` — a dump must reproduce its `.golden`
exactly, a rejection must say why — and `tests/run`, where every `.ce`
compiles to a binary whose exit, stdout and stderr the files beside it
name, an optional `.release` building it with `-r`
(`tools/run-tests.sh`). 477 green at the time of writing.

## The specification

Sixteen chapters and a grammar, `specs/` — [the spec's own
README](./specs/README.md) is the index, and holds what the language
guarantees, what it deliberately does not, where the design comes
from, and what is still open. `review/` holds one file per design
review, `review-YYYYMMDD-NN.md`.

## Status

v0, end to end and moving: `?` propagation landed — the checker
spells the match it is, the emitter none the wiser — and main's
`E?()` with Err's printing are next (`12-projects.md`), the test
runner behind them (`13-testing.md`). How the compiler got here — one
stretch a milestone, in the order they landed — is
[docs/PROGRESS.md](./docs/PROGRESS.md).
