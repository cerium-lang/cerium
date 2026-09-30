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
`Option`/`Result`, `dyn A` names its trait and gives its
associated types — `dyn Iterator<Item = u32>`, the slots aligned to
the trait's declaration order — then resolves trait and
impl members against their `Self`, checks a trait impl supplies
exactly what the trait declares, and orders every pair of impls by
shape-pattern specificity, bounds inclusion, or the `Copy`/`Drop`
exclusion (`04-generics.md`), rejecting the rest on the spot. It then walks
every function body: expressions get their types — the method sugar
adapts its receiver and names any receiver an impl's pattern can
match, scalars included, a trait method found by matching impl
patterns against the receiver's type with every bound answered
there — a bound the receiver cannot answer keeps its impl out —
and the most specific match winning, an exact target above
patterns and bounds ordering equal shapes (`Trait::member(&p)` and
a handle's vtable pick the same one), a generic fn's bounds
checked where the call binds its parameters — `Copy` read
structurally, a `Drop` impl anywhere in the type excluding it
(`03-move.md`) — an associated type read outside an impl —
`Counter::Item` — taking what the impl supplies, and every borrow a
call writes — an argument's
as much as the receiver's — ends with that call, `None` takes its
`?T` from the other side, a `match` is exhaustive variant by
variant — and the flow rules hold:
a move kills its binding downstream, a borrow freezes what it
touched, and `mut` stays two orthogonal levels, the slot and the
field, with a pointer gating everything it lends (`01-types.md`,
`03-move.md`, `09-match.md`, `10-iteration.md`).

A declaration may close its own tail: a type parameter's default
fills the arguments left out — at a path, in a literal's inference,
on an impl's head — naming only the parameters before it, and a
trait's may be `Self`, so `impl Add for V3` is `Add<V3>` for `V3`
and the member's `Rhs` is that argument (`04-generics.md`,
`07-operators.md`). A fn's parameters come from its arguments and
an impl's from the trait it implements: neither carries one.

Compile-time evaluation has begun, the leaf layer: a `const` and a
`static` initializer run in the compiler — literals, arithmetic and
the operators over them, the signedness read from the type, the
short circuit kept, `@sizeof`/`@alignof` over the layout tables,
`@cast` across the matrix, a float truncating as `01-types.md` says
— and the const references that chain them, lazy: each value is
kept once it lands, so a chain resolves once however many read it.
`08-reflection.md`'s four promises hold — every step checked
against the type it lands in, so overflow and a division by zero
are compile errors; the in-flight stack is the cycle check, a
const that depends on itself an error, not a hang; a step budget
ends what would not end; nothing is observed, so nothing varies.
The sign rides a literal: `-2147483648` is i32's least, spelled
the only way it can be — the const's evaluator folds it, and the
body's inference does too, at every place a want arrives: a
binding, a call's argument, a field, an element. The domain check
reads the signed whole, so the least passes and one past it does
not, and a negative never fits an unsigned.
An array's length and a variant's discriminant are const
expressions now, not only literals — `[N * 2 + 1]u8` and
`enum E(u8) { A = D, B }` both evaluate, `B` counting from `D` —
and a body reads a const as an immediate, a float riding the data
segment, the literal's ride. An fn call in an initializer waits
for the passes that know the bodies.

The wait is over for the plain fns: a call whose arguments are
compile-time known runs the callee in the compiler, no annotation
asked — `08-reflection.md`'s own words. Its body walks a statement
subset: a `let` that binds one name, an assignment to a local —
the compound six included — an `if` in either shape with the
else-if chain, an early `return`, and the tail. Each call gets a
frame — a floor on the binding stack, so an inner call never
writes a live outer slot — and the depth has a budget (128) of
its own, before the shared step budget has to speak. A forward
reference resolves lazily, the signature read on demand; an
overload chain picks by the arguments, and a name that cannot run
says why — extern, generic, no body — instead of a shrug. A call
lands anywhere a const does, a length included. `@compileError`
is the report: reached, it is the compile error itself; a branch
a finished run did not take stands as a branch — the body check
leaves it, and emit gives it no runtime behavior, for panic
belongs to the running program — while a fn the evaluator never
ran still owns every `@compileError` it holds, a misuse. Loops,
`match`, and aggregate values are the next layer.

The arrays are values now: a literal's elements evaluate — the
zero fill behind them, a const chain inside them, a call as the
length, the nesting an element at a time — and an index reads one,
checked against the length the initializer spelled. A `mut` in the
type is the slot's permission, not the element's own, so the reads
are the same either way. A slice literal is a borrow, and
evaluation allocates nothing; no operator spans aggregates yet —
the words an array's value sits in are an address here, and
comparing those compares nothing. An array crosses a frame as a
parameter and comes back as an answer; a `const`'s value keeps its
elements memoized beside its scalar half, and a body reading one
still waits — emit has no aggregate immediate, and its data segment
arrives with the milestone that gives it storage.

The signed steps work through the negatives now too: the domain
check asks the type's question, not the u64's — a borrow was an
overflow, a negative product wrapped the wrong domain — and the
division rounds the way the running program's does, toward zero,
not the floor the host's C89 might have chosen. An i64's own ends
are the signs' to catch: operands agreeing, an answer that flipped
is past them, and the least times minus one is the one quotient no
division holds.

The aggregates are values in full now: a struct's rows evaluate by
name with the ones left out zero, a tuple's by position, and the
reading matches — a field by name, a row by number. A union keeps
one row active — the last name a literal wrote, or the zeroed whole
reading zero every row — and a read of any other is the unspecified
thing the spec says not to rely on, which the promise of determinism
turns into a report. An enum constructs in every shape it parses:
positional, named, payloadless by path, and the prelude's bare
`Some`/`None`; the discriminant rides the value, `@cast<u32>` reads
it out — at compile time and in a body now, the tag loaded at its
own width — and the payload waits for `match`. The nesting recurses,
a struct in a struct, an array of them; the frames carry them as
parameters and answers; a const's Sym keeps the elements beside the
scalar half. The operators stay the trait table's — `==` is `Eq`'s,
and that table arrives with dispatch — and a generic's rows wait
for the binding a call's own words spell. A body reading a const
aggregate still waits for the data segment emit does not have.

The match runs, and the loops with it. A pattern destructures; it
does not test — so an arm's turn is a discriminant's compare, the
patterns that miss naming another variant, and the bindings an arm
spelled land in the frame and end with it. A `let` takes a pattern
now, the tuple by position, the struct by field, a variant's
payload under it — irrefutable, and a value that says otherwise is
the abort it would be where it runs, reported here. The three `for`
heads unwind: a condition while it holds, a `let` while its
pattern fits the value re-read every round, an `in` over an owned
array's elements or the `?T` niche — one round at most, so a
`continue` ends it like a `break` would — and a refutable element
pattern ends the loop the way a `for let`'s misfit does. `break`
and `continue` unwind one level, a `return` everything; a round
costs a step, so a body that empties still meets the budget, and
what would not end ends. Iterating a borrow stays out — the
pointer a slice lends is a runtime thing — and so does an iterator
the method table would pick. A call's arguments take their wants
from the one plain overload that takes them, so `Some(3)`
constructs in an argument's place, and a match lands at a const's
own initializer, as the `if` already did. The const `for`'s unroll
waits with its own milestone.

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
method held as a value included) — and a method may take parameters
of its own, the trait declaring the family: the impl's parameters
and the method's bind together at the call, the arguments picking
the member, one instance per binding, a generic caller's re-check
carrying both (04-generics.md) — `dyn A` handles — `&dyn b` builds
the fat where the impl is known, one vtable per trait and concrete
type in the data segment — the most specific impl's, as every
call-site pick — the call reading its slot through the
table (`dyn mut A` writing too), and a handle's spelling giving the
associated types — a projected `?Self::Item` return takes its type
from the spelling, not from the impl the vtable erased — `#[extern(C)]` imports and
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

A binding's stack is the fn's frame, asked for at the entry: every
slot the body wants — a let, a pattern's, a value's, an
iteration's — goes out at `@start`, once, the rounds of a loop
reusing what they were given. An alloc left inside a loop is bytes
taken again every round, and a long enough loop walks its frame
off the guard page; the entry ask is Clang's alloca discipline,
and the emitter holds the fn's text back so the asks can go ahead
of it.

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
