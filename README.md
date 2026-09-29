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
make qbe/qbe            # the backend, on demand
make test               # golden tests + codegen's run tests
```

The `qbe/` submodule is the backend: `git submodule update --init -- qbe`
pulls it on demand, and without it `make test` skips the run section. Run
`make hooks` once per checkout so commits format what they stage.

The lexer and the parser are in: `xyz -t file.xyz` dumps the token
stream — position, kind, value — and `xyz -a file.xyz` dumps the parse
tree as S-expressions, one node a line, children indented. The type
checker is in through the bodies: `xyz -T file.xyz` declares every
name in the file, resolves every type a declaration carries — aliases
expand to their targets, `?T`/`E?T` build on the prelude's
`Option`/`Result`, `dyn A` names its trait — then resolves trait and
impl members against their `Self`, checks a trait impl supplies
exactly what the trait declares, and orders every pair of impls by
shape-pattern specificity or the `Copy`/`Drop` exclusion
(`04-generics.md`), rejecting the rest on the spot. It then walks
every function body: expressions get their types — the method sugar
adapts its receiver, a trait method found by matching impl patterns
against the receiver's type (`Trait::member(&p)` picks its impl the
same way), a generic fn's bounds checked where the call binds its
parameters, and every borrow a call writes — an argument's
as much as the receiver's — ends with that call, `None` takes its
`?T` from the other side, a `match` is exhaustive variant by
variant — and the flow rules hold:
a move kills its binding downstream, a borrow freezes what it
touched, and `mut` stays two orthogonal levels, the slot and the
field, with a pointer gating everything it lends (`01-types.md`,
`03-move.md`, `09-match.md`, `10-iteration.md`).

Codegen has begun: `xyz -s file.xyz` prints the `.ssa` text — qbe's
input — and `xyz -c file.xyz -o out` runs the pipeline, qbe as a
subprocess and the system `cc` linking. Checking writes each node's
type back into the tree, so the emitter never re-derives one. What
it emits covers the expression language: arithmetic and comparisons
with the signedness picked from the operands, `@cast` across the
int/float/bool matrix (`u16`→`i64` widens by the source, a float
narrows by the target), `@sizeof`/`@alignof` folding over the
layout tables of `02-layout.md` in full — struct padding in
declaration order, unions, enums as a tag and a payload union, the
`?T` niche, `#[packed]` and `#[align(N)]` — calls through a name or
a fn value, an impl's methods emitted as the fns they are, inherent
or trait, exact or a pattern the receiver instantiates (the sugar's
receiver adapted at the call, a `Type::member` call, a
`Trait::member(&p)` one resolving its impl by the receiver, and a
method held as a value included), `#[extern(C)]` imports and
exports keeping their symbols, and the aggregate half: a struct literal (a nested one
`blit`s into its field), a string landing in the data segment with
its slice on the stack, `.ptr`/`.len` reads, and field/deref/slot
places to read and write through, `mut` permitting. A literal's
left-out half reads as zero — a struct's missing fields, an
array's shorter tail, a bare `{}` — the storage starts zeroed
(`01-types.md`). An overload
chain resolves at the checker — the call site writes its pick back,
and one instantiation is re-checked per binding (M3e); a fn value
carries the same resolution, a generic one instantiated from the
type expected of it, and a generic struct's literal instantiates
the same way: the expected type first, the field values binding
what it leaves open, the declaration's defaults covering the rest.

Control flow is M3c's, and in: `if` is an expression — value form,
else-if chains, nesting — `&&`/`||` skip the right side, and the
three `for` shapes of `10-iteration.md` run: a condition re-checked
every round, a `let` pattern re-fitted every round (a misfit stops
the loop), and `in` — an Option yields its one payload, niche or
tagged, one round at most, a `continue` ending it like a `break`
would, and a slice lends each element out as a pointer, so the
binding is a `*T`. `match` destructures per `09-match.md`:
positional and named payloads, nested struct patterns with `..`,
wildcards, or-patterns whose shared bindings are pre-bound before
the alternatives — the join must read what every path defined —
and the short variant name, which the scrutinee's enum settles
against the bare binding. Arm order decides. Compound assignment
covers the operators' full set with their signedness, and
assigning an aggregate blits.

Aggregates cross calls on the platform's C convention — that is
what `01-types.md` made xyz's own convention, and qbe lowers it:
register eightbytes, stack order, an sret, all of it. Every
aggregate that crosses a call is named in a `:type` registry —
structs, tuples, unions, enums as tag plus payload union, slices
as two words, arrays — registered recursively, the declarations
printed innermost-first ahead of the functions, the order qbe
reads. A niche `?ptr`/`E?ptr` stays the one scalar it is at the
boundary, loaded out and stored back on either side. A `#[packed]`
shape, or one whose natural layout C would pad differently, rides
an opaque `align N { size }` — memory carries it, correctness
over speed. Parameters arrive as the copies qbe makes (C
semantics); a shared borrow may stack on a live shared one, the
checker now reading `01-types.md`'s "a `*T` is not exclusive"
as written. Array literals and indexing, tuple expressions and
row writes — `(T, mut U)`, one row its own slot — and
monomorphization are in (M3d–M3f): one copy per binding, the
same instance emitted once, all of it static.

The layout and the behavior are tested by running them:
`tests/run` holds one `.xyz` per binary with an `.expect` naming
its exit code, an optional `.stdout` holding the bytes it must
print — `#[extern(C)] fn write` is how the language prints for now
(`tools/run-tests.sh`); the section runs only when `qbe/qbe`
is built.
`tests/lex`, `tests/parse` and `tests/check` hold the golden tests,
split by pass: `ok/` has one `.golden` per `.xyz` that the dumps must
reproduce exactly, `err/` has inputs that must be rejected
(`tools/run-tests.sh`).

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
