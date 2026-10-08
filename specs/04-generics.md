# Generics

This chapter continues `03-move.md`. Traits themselves are defined in
`05-traits.md`; here a bound is just a name for a constraint on `T`.

## Type Parameters

Functions and types can be parameterized over types:

```rust
fn id<T>(x: T) -> T {
  x
}

struct Box<T> {
  inner: *mut T,
}

enum Option<T> {
  None,
  Some(T),
}
```

A generic type instantiated with different arguments is a different type:
`Box<u32>` and `Box<File>` share nothing but spelling. `Box<u32>` is an
ordinary type — usable as a field, a parameter, a return value.

A type parameter may be used by value (`Some(T)`) or behind a pointer
(`*mut T`); the layout of the containing type is derived per instantiation,
so no special mechanism is needed.

### Defaults

A type parameter may be given a default, used whenever the argument is left out:

```rust
trait Add<Rhs = Self> { ... }

struct Table<K, V = u32> { ... }
```

A default may name a parameter declared before it, and in a trait it may be
`Self`. Defaults belong to the declaration: a trait, a struct, an enum, a union,
or a type alias. A function and an `impl` do not carry them — a function's type
parameters come from its arguments, and an `impl` only repeats the shape of the
trait it implements.

```rust
impl Add for Vec3 { ... }        // Add<Vec3> — the default is Self
impl Add<u32> for Vec3 { ... }   // given explicitly
```

`07-operators.md` is written against this: `Add<Rhs = Self>` is what lets an impl
say `impl Add for Vec3`.

## Monomorphization

A generic function is instantiated as a separate copy for every concrete
type it is called with:

```rust
let a = id(3);            // instantiates id<i32>
let b = id(File{ fd: 0 }); // instantiates id<File>
```

`id<i32>` and `id<File>` are distinct functions — different addresses, not
interchangeable as function pointers. The same instantiation is emitted once
globally, no matter how many namespaces call it.

A generic fn used as a value instantiates from the type expected of it: `let
f: fn(i32) -> i32 = id;` is `id<i32>`, and an argument slot expecting
`fn(i32) -> i32` instantiates `id` the same way. Without an expected type
there is nothing to instantiate from — `let g = id;` does not compile. Generic
arguments cannot be spelled in expression position (`id<i32>` parses as a
comparison chain); the expected type is the only spelling.

A generic struct's literal instantiates the same way: the type expected of it
binds first (`let b: Box<i32> = Box{ v: p };`), the field values bind what
that leaves open, and a parameter nothing names falls to its default, as
ever. With no evidence and no default the literal does not compile —
`Empty{ z: 1 }` says nothing about `T`. Generic arguments cannot be spelled
in expression position here either; the expected type and the fields are the
only spellings.

Dispatch is fully static: no vtables, no trait objects, no runtime cost. `dyn`
(`06-dispatch.md`) is the opt-in dynamic path, and it is written in the type
rather than inferred from it.

## Bounds

A bound restricts what `T` can be, and everything the function body does to
`T` must be justified by a bound — checked once, at the declaration, not
per-instantiation:

```rust
fn max<T: Ord>(a: T, b: T) -> T {
  if a > b { a } else { b }   // `>` is legal because T: Ord
}
```

Without a bound, `T` is a black box: it can be moved, stored, and passed on,
but not copied, compared, or called. Copying requires `T: Copy`:

```rust
fn dup<T: Copy>(x: T) -> [2]T {
  [2]T{ x, x }
}
```

Multiple bounds are joined with `+`, and bounds participate in specialization
matching (see below):

```rust
fn sort<T: Ord + Show>(a: []mut T) { ... }
```

A bound names a trait, and the trait's own parameters may be spelled in the
name (`05-traits.md`): the constraint is then the trait at those arguments,
and only a row that takes them answers it.

```rust
trait Marks<Rhs = Self> {
  fn mark(self: Self, other: Rhs) -> i32;
}

fn drain<T: Marks<usize>>(a: T, n: usize) -> i32 { Marks::mark(a, n) }
```

A `Marks` row over any other `Rhs` does not answer `T` here, however well its
receiver fits. The tail the bound left unspelled is the trait's own defaults,
`Self` the type under the bound: `T: Marks` asks `Marks<T>` — the row's own
`Self`, what `Rhs` defaults to. A parameter past the spelled ones with no
default is a bound no ask can complete, and it is rejected where the bound is
written.

A bound may also pin a trait's associated types, the pin joining the
arguments inside the angle brackets and standing behind every one of them:

```rust
fn each<F: Fn<i32, Output = u32>>(f: F) -> u32
```

The `=` tells the two apart: a name followed by one is an associated type
being pinned, a name followed by a comma or the closing bracket a type
argument — the same spelling a declaration's defaults take
(`Add<Rhs = Self>` above), and read the same way: the position says which it
is, a declaration naming its own, a use naming the trait's. The pins stand
last, the order an impl's block spells them in (`05-traits.md`).

## Specialization

Multiple impls of the same trait are ordered by how specific their bounds are.
For a given concrete type, the most specific matching impl wins.

The receiver alone reads that order; a row's signature is a claim about the
call's other arguments too. A call — spelled or sugar — walks the rows the
receiver fits in specificity order and takes the first whose signature accepts
its arguments; a row that does not take them steps aside, unwinding what its
argument walks froze and moved, and the report when no row takes them is the
first row's own, the pick the receiver would have made alone
(`07-operators.md`).

```rust
impl<T: Copy>      Show for T { /* bitwise dump */ }
impl               Show for File { /* detailed report */ }
```

`File` has a destructor, so it can never satisfy `T: Copy` — the two impls
are provably disjoint and both are legal. For `u32`, only the first matches.
A type that is neither `Copy` nor `File` matches neither impl, and the call is a
compile error.

The rules, precisely:

| relation between two impls | outcome |
| --------------------------- | ------- |
| bounds A ⊃ bounds B (`{Copy, Ord}` vs `{Ord}`) | A is more specific; A wins where both match |
| nonempty vs empty bounds (`{Copy}` vs `{}`) | the nonempty one is more specific |
| provably disjoint (`{Copy}` vs `{Drop}`) | no conflict; each covers its own types |
| incomparable, possibly both matching (`{Ord}` vs `{Show}`) | ambiguous — compile error where they overlap |

### Shape Patterns

The type position of an `impl` is a pattern, not just a name. A pattern may
match structure, and patterns are ordered by specificity — pattern A is more
specific than pattern B when every type matching A also matches B, but not
vice versa. Specificity is determined jointly by bounds and by the type
pattern:

```rust
impl<T> Show for *T { /* show the pointee */ }
impl<T> Show for T  { /* fallback */ }
```

Every type matching `*T` also matches `T`, so the pointer impl wins for
pointers and the fallback covers the rest:

| pattern A | pattern B | more specific |
| --------- | --------- | ------------- |
| `*T` | `T` | `*T` |
| `*mut T` | `*T` | `*mut T` |
| `[N]T` | `T` | `[N]T` |
| `[]mut T` | `[]T` | `[]mut T` |
| `*mut [N]T` | `*[N]T` | `*mut [N]T` |
| `(Head, ...Rest)` | `(...Ts)` | `(Head, ...Rest)` |
| `(T, T)` | `(A, B)` | `(T, T)` |

Mutability orders the same way shape does: every type matching the mutable
pattern also matches the immutable one — a value of a mutable type is usable
where the immutable one is expected (`01-types.md`) — but not the other way
around.

Equal specificity over the same types is a compile error.

### Struct Specialization

A struct declaration takes the same shape patterns as an impl. Two structs
with one name are ordered by specificity, and the most specific match wins for
a given instantiation — this is what a type predicate such as `is_same` is
built from (`08-reflection.md`):

```rust
struct is_same<A, B> {}   // shape only — members via inherent impl (05-traits.md)
struct is_same<T, T> {}
```

A struct's type parameters need not appear in its fields: `is_same` uses `A` and
`B` only in the pattern, and that is enough — the type is never instantiated.

The `(T, T)` pattern — one type variable appearing twice — constrains the two
positions to be the same type. It is more specific than `(A, B)`: every type
matching `(T, T)` also matches `(A, B)`, but not the other way around. A
repeated variable is the only new shape; pointers, arrays and packs behave
exactly as they do in an impl.

A specialized struct declares its shape here; its members — associated
constants and methods — are supplied by inherent impls, one per
specialization, whose target repeats the same pattern (`05-traits.md`).

### Disjointness

Disjointness is proven from the exclusion table — a fixed list of trait pairs
that can never be implemented by the same type:

| trait A | trait B |
| ------- | ------- |
| `Copy`  | `Drop`  |

`Copy` and `Drop` are mutually exclusive by construction (`03-move.md`), which
makes the most common specialization — cheap-copy types versus
resource-owning types — require no new machinery.

### Overlap Is Checked at Declaration

When an impl is declared, the compiler compares it against every existing impl
of the same trait: it is accepted only if the two are provably disjoint or
strictly ordered by specificity. Anything else is rejected on the spot.

A row's identity is its trait's arguments beside the type it is for.
`Add for S` — whose `Rhs` defaulted to `Self` — and `Add<usize> for S` are
disjoint rows: no call takes both, and both are legal. An argument a variable
can bind orders instead: `impl<T> Add<T> for S` stands under `Add<usize> for
S`, the specific row answering the call that spells it. But two rows whose
arguments are the same shape, a variable either one can bind, are one row
written twice.

The consequence is stability: adding an impl can never change how existing
code resolves, because a conflicting impl never gets in. This is what makes
specialization safe to build on without a runtime or a fixed link order.

### Layout Specialization

One specialization happens below the trait system: the compiler tailors the
layout of `Option` specifically (see `05-traits.md`). When `T` has an unused
bit pattern — a nonnull pointer, say — `?T` reuses it and keeps the size of
`T`; otherwise `?T` grows by a tag. This is not a general mechanism, and user
code cannot define new layout specializations.

## Overloading

Functions may share a name when their signatures differ by arity or by type
pattern shape. Resolution follows the same partial order as impl selection:
the most specific matching signature wins, and two signatures that match
equally specifically is a compile error. A call that matches no signature is,
of course, also a compile error.

```rust
fn show(p: *Point) { ... }
fn show<T>  (t: T)  { ... }

show(&p);   // *Point pattern is more specific than T
show(3);    // only the fallback matches
```

This is the mechanism variadic recursion peels with — `sum()` and
`sum<First, ...Rest>` are two overloads of one name.

## Variadics

A pack stands for zero or more types. It is declared with `...` and must come
last in both parameter lists:

```rust
fn sum<...Ts>(ts: ...Ts) -> i64;
```

`ts` is a tuple of type `(...Ts)`. Passing a tuple to a pack parameter
unifies the pack with the tuple's elements — `sum(t)` where `t: (i32, u8)`
instantiates `Ts = (i32, u8)`.

The pack's rows can carry a shape of their own: `(...mut Ts)` spells the
tuple a row per type, each row the template's own — a writable slot, the
mut beside the template, the way a hand-written `(mut i32, mut u8)` row
carries it (`01-types.md`). The mut never wraps the pack itself:
`(mut ...Ts)` is a different shape, and the checker names the right one.

The gather runs where such a tuple meets an argument: the spread walks the
tail alone — a tuple type with `...τ` among its rows meets an argument
tuple of any width at least the rows before it, those rows meeting one a
one, every row after meeting τ alone, the pack bound to the tuple the
meetings leave. `fn f<...Ts: Iter>(its: *mut (...mut Ts))` called on
`&mut t` where `t: (mut []i32, mut []i32)` binds `Ts = ([]i32, []i32)`,
the rows the meetings strip — the template's own `mut` is the slot's, put
back where the instantiation spells the rows out.

A trait's type parameters may be a pack too, and a pack the whole of them —
`Fn<...Args>` takes its arguments that way (`05-traits.md`) — the last-word
rule holding the same: a pack that is the list's only word breaks nothing.
At an impl the pack is spelled out, the parameters the elements one for one
(`05-traits.md`'s Scale); a bound feeds it types one for one as well, `Fn<E>`
and `Fn<A, B>` both binding `Args` whole.

A pack is manipulated with ordinary indexing and slicing, plus `@count`:

| expression | meaning |
| ---------- | ------- |
| `@count(...Ts)` | the number of types in the pack, a compile-time constant |
| `ts[0]` | the first element; an empty pack is a compile error |
| `ts[1..]` | the tuple without its first element — a place of its own when every spanned row is a writable slot, the tail handed on whole and written through the slice |

`...` in expression position expands a tuple or a slice into individual
arguments:

```rust
sum(...ts)   // passes every element of ts as one argument each
```

The same `...` groups the pack's rows as a value: `(...its)` inside a
generic body is the pack spelled as a tuple — the declaration's walk
takes it on faith, the shape riding the binding, and the
instantiation's re-check expands the rows where they stand, the same
reads the `(x, ...rest)` spelling takes, each row read where it is.

A bound on a pack applies to every element: `<...Ts: Show>` requires each
type in the pack to implement `Show`.

### Compile-Time Recursion

`const if` is the compile-time conditional: an ordinary `if`
(`10-iteration.md`) whose condition must be compile-time known. The untaken
block is discarded before type checking, so it may contain code that only
compiles for some instantiations:

```rust
fn sum<...Ts>(ts: ...Ts) -> i64 {
  const if @count(...Ts) == 0 {
    0
  } else {
    @cast<i64>(ts[0]) + sum(...ts[1..])
  }
}
```

`sum(1, 2, 3)` unfolds at compile time into `1 + 2 + 3 + 0`. The recursion
terminates at the empty pack, whose tuple is `()`.

A pack can also be peeled by the type parameter list itself — `First, Rest...`
matches one element plus the rest, so the recursive step never sees an empty
pack and the count check disappears:

```rust
fn sum() -> i64 {
  0
}

fn sum<First, ...Rest>(first: First, rest: ...Rest) -> i64 {
  @cast<i64>(first) + sum(...rest)
}
```

Overload resolution picks `sum<First, ...Rest>` for any nonempty argument
list and `sum()` for the empty one. The `...` in `rest: ...Rest` says the
parameter takes the pack — the name is ordinary. At the call site,
`sum(...rest)` expands the pack back into individual arguments. This is
the same shape pattern as `(Head, ...Rest)` in impls, applied to functions.

Recursion depth is capped at 256 unfoldings; exceeding the cap is a compile
error.

### Instantiation-Time Checking

Ordinary generics are checked once, at the declaration. Variadic code cannot
be: `const if` branches and pack builtins only make sense against a concrete
pack.
A variadic function is therefore type-checked per instantiation, and an error
inside the recursion is reported at the call site, with the chain of
unfoldings that led there. This is the deliberate price of type-level
recursion.

### Pack Impls

A pack can implement a trait for tuples of every arity:

```rust
impl<...Ts: Show> Show for (...Ts) { ... }
```

The method bodies recurse the same way `sum` does — `const if` on the count,
`ts[0]` and `...ts[1..]` to peel, the empty tuple to stop.
