# Cerium

A system programming language: Rust's moves, Zig's pointers and
compile-time execution, C++'s generics. The compiler works today —
stage 0 written in C89, codegen through
[QBE](https://c9x.me/compile/).

No borrow checker and no reference type — one pointer family, lifetimes
nowhere. Ownership is Rust's: moves, `Copy`, `Drop`, destructors
inserted statically. Compile-time execution stands in for a macro
system: a `const` initializer runs in the compiler, reflection reads the
type tables, nothing rewrites tokens. The generics are C++'s — variadic
packs, specialization ordered by shape — with a check none of the three
has: overlap is ruled out where the impls are declared, not where they
are instantiated.

```rust
use std::fmt::print;

fn main() {
  print("hello cerium\n");
}
```

## Building it

```bash
git clone --recurse-submodules https://github.com/cerium-lang/cerium
cd cerium && make             # gcc; make CC=clang works too
make test                     # the goldens and the run tests
```

`qbe/` is the backend submodule, [a fork](https://github.com/mivinci/qbe)
tracking upstream master, its objects linked into the compiler whole:
the backend one call away (`src/qbe.c`), no binary beside, no pipe
between. It is [Quentin Carbonneaux](https://c9x.me/compile/)'s, MIT.
Stage 0 carries no host framework: one bare Makefile, `src/vec.h` the
container layer, the compiler emitting `.ssa` that the linked-in `qbe`
lowers and the system `cc` links. The goal is self-hosting, with LLVM
a v1+ backend rather than a v0 dependency.

Five flags, one pass each: `-l` the token stream, `-a` the parse tree,
`-T` what checking made of every item, `-s` a project's whole `.ssa`,
and `-c file.ce -o out` the pipeline end to end. `-r` rides `-s` and
`-c`: release, the runtime checks out.

Highlighting for VS Code lives in [`editors/vscode`](./editors/vscode).

## The specification

Sixteen chapters and a grammar, `specs/` — [the spec's own
README](./specs/README.md) is the index, and holds what the language
guarantees, what it deliberately does not, and what is still open.
`review/` holds one file per design review.

## Status

v0, end to end and moving. How the compiler got here — one stretch a
milestone, in the order they landed — is
[docs/PROGRESS.md](./docs/PROGRESS.md).
