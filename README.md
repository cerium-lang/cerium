# xyz

A system programming language, still being designed. This is its
**specification draft** — not a tutorial, and not an implementation.

`xyz` is the working codename for the project; the language has no final name
yet. The `.xyz` file extension used throughout `11-namespaces.md` is a
placeholder for the same reason.

## Chapters

Each chapter opens by naming what it continues. The dependencies are not a chain
— `09` and `11` follow from `01` alone, and `10` needs both `05` and `09` — but
the order below is one valid way through them. `00-preliminaries.md` states the
notation and the terms every chapter uses.

| file | continues | what it defines |
| --- | --- | --- |
| [00-preliminaries.md](./specs/00-preliminaries.md) | — | notation; `slot`, `place`, compile-time known |
| [01-types.md](./specs/01-types.md) | `00` | types and `mut`, arrays, slices, structs, pointers, enums, strings, attributes, panic |
| [02-layout.md](./specs/02-layout.md) | `01` | size and alignment, layout attributes |
| [03-move.md](./specs/03-move.md) | `02` | move semantics, `Copy`, `Drop`, `@take` |
| [04-generics.md](./specs/04-generics.md) | `03` | generics, specialization, shape patterns, variadics |
| [05-traits.md](./specs/05-traits.md) | `04` | traits, associated items, inherent impls, `Option` |
| [06-dispatch.md](./specs/06-dispatch.md) | `05` | `dyn A`, dynamic dispatch, object safety |
| [07-operators.md](./specs/07-operators.md) | `05` | operators as trait methods, `Add`/`Ord`/`Eq` |
| [08-reflection.md](./specs/08-reflection.md) | `07` | compile-time execution, `TypeInfo`, the builtin table |
| [09-match.md](./specs/09-match.md) | `01` | pattern matching |
| [10-iteration.md](./specs/10-iteration.md) | `05`, `09` | `Iterator`, `for`, `if`, `return` |
| [11-namespaces.md](./specs/11-namespaces.md) | `01` | a directory is a namespace, `use`, name resolution |
| [12-projects.md](./specs/12-projects.md) | `11` | compilation unit, one artifact, `main` and exit codes, what v0 does not carry |
| [13-testing.md](./specs/13-testing.md) | `12` | `#[test]`, the test artifact and its runner |
| [14-macros.md](./specs/14-macros.md) | `08` | the case against a user-defined macro system (not settled) |
| [15-grammar.md](./specs/15-grammar.md) | — | the grammar in EBNF, closed: lexing, expressions, types, declarations, statements, patterns |

## What xyz guarantees

There is no borrow checker and no reference type — one pointer family, and
lifetimes nowhere — so the guarantees are uneven on purpose:

- **Guaranteed statically.** No use after move, no double free from a move, no
  write through a shared pointer, no move out of a borrowed place, no dropping
  through a pointer that is not known to be exclusive.
- **Not proven.** Exclusivity and non-dangling hold only where the compiler can
  see them — within a function. Across a function boundary there is no lifetime
  information, so a dangling use or an aliasing violation is undefined
  behaviour. A `debug` build may catch some of these; the language does not
  define a mechanism, and promises nothing. This is the same bargain a slice
  makes (`01-types.md`).
- **Deliberately unchecked.** A `static mut` is the one place aliasing is
  allowed without any check: any function may write it, and nothing proves only
  one does. Statics are also never destructed (`01-types.md`).
- **Not addressed.** Data races, iterator invalidation, and anything else that
  would need aliasing to be tracked across the whole program.

## Where it comes from

The shape is Rust's ownership — moves, `Copy`, `Drop`, destructors inserted
statically — with Zig's answer to aliasing and to compile-time execution, which
is what lets reflection stand in for a macro system. The generics are C++'s:
variadic packs, specialization ordered by shape pattern, and a type predicate
such as `is_same` written as a struct with two impls. What none of them has is
how the pieces are pinned down: overlap is checked where the impls are declared,
not where they are instantiated, and `mut`, though part of the type, is not deep.

## Implementation

Stage 0 of the compiler is **C89** — no host framework, one bare Makefile,
`src/vec.h` as the container layer. Codegen goes through
[QBE](https://c9x.me/compile/): the compiler emits `.ssa` text, runs `qbe`
as a subprocess, and the system `cc` links. The submodule at `qbe/` points
at a [fork](https://github.com/mivinci/qbe) that tracks upstream master —
pinned by commit, moved by `tools/sync-qbe.sh`. The goal is self-hosting:
the compiler rewritten in xyz itself, with LLVM held as a v1+ release
backend rather than a v0 dependency.

A fresh clone builds with:

```
git clone --recurse-submodules <url>
cd xyz && make          # gcc; make CC=clang works too
make test               # the golden tests: lexer, parser, checker, rejections
```

The `qbe/` submodule only matters once codegen lands —
`git submodule update --init -- qbe` pulls it on demand. Run `make hooks`
once per checkout so commits format what they stage.

The lexer and the parser are in: `xyz -t file.xyz` dumps the token
stream — position, kind, value — and `xyz -a file.xyz` dumps the parse
tree as S-expressions, one node a line, children indented. The type
checker's first two passes are in: `xyz -T file.xyz` declares every
name in the file, resolves every type a declaration carries — aliases
expand to their targets, `?T`/`E?T` build on the prelude's
`Option`/`Result`, `dyn A` names its trait — then resolves trait and
impl members against their `Self`, checks a trait impl supplies
exactly what the trait declares, and orders every pair of impls by
shape-pattern specificity or the `Copy`/`Drop` exclusion
(`04-generics.md`), rejecting the rest on the spot. `tests/lex`,
`tests/parse` and `tests/check` hold the golden tests, split by pass:
`ok/` has one `.golden` per `.xyz` that the dumps must reproduce
exactly, `err/` has inputs that must be rejected (`tools/run-tests.sh`).

## Status

A design in progress. The specification is internally consistent at the moment;
`review/` holds one file per review, named `review-YYYYMMDD-NN.md`.

Still open:

- Whether a macro system should exist at all — `14-macros.md` currently argues
  against one, but the conclusion is deliberately left open
- The one open item left at the end of `12-projects.md`: external libraries,
  and how their paths enter the root — they will distribute as source
  (`12-projects.md`), but dependency declaration waits for a manifest
- The chapter-level items deferred with their chapters: `dyn A + B` and a
  `@typeinfo<dyn A>` variant (`06-dispatch.md`); an `Output` associated type
  and traits for `%`, the bitwise operators, and shifts (`07-operators.md`);
  the compile-time `assert` (`14-macros.md`, deferred); concurrency, atomics,
  `volatile`, and inline assembly are v1+ work, behind `#[extern(C)]` until
  then (`12-projects.md`)
