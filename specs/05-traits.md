# Traits

This chapter continues `04-generics.md`.

## Trait

A trait is a set of function signatures. A type implements the trait by
providing them:

```rust
trait Show {
  fn show(self: *Self) -> ();
}

impl Show for Point {
  fn show(self: *Self) -> () {
    print_one(@cast<voidptr>(self));
  }
}
```

`print_one` below is an ordinary function from `std::fmt`, brought into
scope with `use std::fmt::print_one;` (`11-namespaces.md`) — not an
`@` builtin, which are listed in `08-reflection.md`.

`self` is an ordinary parameter — `*Self` for a read-only method, `*mut Self`
for a mutating one, `Self` for one that consumes the value. It is written with
its type like any other parameter; there is no bare `self`:

```rust
fn show(self: *Self) -> ();
fn next(self: *mut Self) -> ?Self::Item;
fn into_iter(self: Self) -> Self::Iter;
```

A method call is sugar: the receiver is adapted to the `self` the method
declares, so `p.len()` is `Point::len(&p)` and `it.next()` is `It::next(&mut it)`.
Taking `&mut` still requires a `mut` slot, exactly as `&mut` does anywhere else
(`01-types.md`). Where the receiver is a pointer it is dereferenced first, so
`sp.len()` with `sp: *Point` is `Point::len(&*sp)` — which is `sp` again. A
receiver that names no place of its own — a literal, a call's answer — is given
one: the call borrows a nameless slot the statement's own block holds, dying
with it, so the borrow never outlives the call; a `*mut Self` receiver keeps
the refusal an `&mut` keeps, for a writable temporary has no honest reader. A
`dyn A` handle is passed the same way (`06-dispatch.md`). The explicit form
stays available — `Show::show(&p)` is the same call written out.

Everywhere else there is no implicit borrowing: an argument list passes exactly
what you wrote (`03-move.md`).

`Self` refers to the implementing type. Inside an `impl`, it is a synonym for
the name in `for`.

## Associated Items

A trait declares associated types and constants, which an impl supplies
alongside the functions:

```rust
trait Iter {
  type Item;                          // associated type
  fn next(self: *mut Self) -> ?Self::Item;
}

impl<T> Iter for []mut T {
  type Item = T;                      // impl supplies the type
  fn next(self: *mut Self) -> ?T { ... }
}
```

`Self::Item` names it inside the trait; outside, `Iter::Item` names it for
the trait as a whole, and `It::Item` for a type `It` that implements it.

The same `type` keyword names a type at the top level — a transparent type
alias, generic or not (`01-types.md`).

A trait can also declare an associated constant, read with `::` and supplied
by the impl. A struct or enum supplies its associated constants and methods
through an inherent impl — the next section.

## Inherent Impl

An `impl` without a trait name attaches methods and associated constants
directly to a type. No trait needs to be in scope to call them:

```rust
impl Point {
  fn len(self: *Self) -> usize { ... }
}

let p = Point{ x: 0, y: 0 };
p.len();            // inherent method — no trait import
```

The target is a shape pattern, exactly as in a struct declaration
(`04-generics.md`), or a built-in shape spelled whole — `impl<T> []T` attaches
to every slice, the mut slices riding the same row with `T` bound the mut
layer (`01-types.md`). A specialized struct gets one inherent impl per
specialization, each target repeating the same pattern:

```rust
// in std::meta — the predicate struct (04-generics.md)
struct is_same<A, B> {}          // shape only
struct is_same<T, T> {}

impl<A, B> is_same<A, B> { const value: bool = false; }
impl<T>    is_same<T, T> { const value: bool = true;  }

assert(is_same<i32, i32>::value);
assert(!is_same<i32, u32>::value);
```

An inherent impl supplies associated constants and methods, but not fields —
fields live in the struct declaration. The `(T, T)` repeated-variable pattern
makes the two impls target disjoint instantiations, so they do not conflict.

An associated constant lives in the type, not the instance: reading it takes
no space — `@sizeof<is_same<i32, i32>>()` is `0`.

Inherent impls and trait impls are separate namespaces: `p.len()` calls the
inherent method, `Show::show(&p)` the trait one. The specificity and
disjointness rules that order trait impls (`04-generics.md`) apply to
inherent impls of a specialized struct unchanged.

## Generics

How generic functions are instantiated, how bounds are checked, and how
multiple impls of a trait are ordered — see `04-generics.md`.

## Operators

`+`, `<` and `==` are trait methods as well — `Add`, `Ord` and `Eq` — and an
operator is sugar for the call. See `07-operators.md`.

## Fn

A call is not a builtin either: `f(x)` is sugar for a trait method, exactly as
an operator is — `f(a, b)` supplies `call`'s pack two elements, `f()` none.
Three traits, and the receiver is what tells them apart:

| trait | receiver | the body |
| --- | --- | --- |
| `Fn<...Args>` | `self: *Self` | reads the captures; may also write **through** a captured `*mut T` |
| `FnMut<...Args>` | `self: *mut Self` | writes a captured slot |
| `FnOnce<...Args>` | `self: Self` | moves a capture out |

The line between `Fn` and `FnMut` is where the write goes: through a captured
pointer, or through `self`. Writing through a captured `*mut T` is permitted by
that pointer's own type, so reading it out of a `*Self` is enough; writing
`self.n` needs `*mut Self` (`01-types.md`). Rust draws this line differently
because there a `&mut` must be reborrowed out of the closure, which needs
`&mut self`.

The arguments arrive as one pack, the result leaves as one associated type —
the arguments are what a bound names, the result what the implementation
decides (`std/ops/fn.ce`):

```rust
pub trait Fn<...Args> {
  type Output;
  fn call(self: *Self, args: ...Args) -> Self::Output;
}
```

In an impl the pack is spelled out, the parameters the pack's elements one
for one, and `Output` given inside the block — a struct that implements one
directly is a callable with named state:

```rust
struct Scale { factor: u32 }

impl Fn<u32> for Scale {
  type Output = u32;
  fn call(self: *Self, x: u32) -> u32 { self.factor * x }
}
```

A bound names the arguments and may pin the result; unpinned, the result
reads `F::Output` and inference takes it the rest of the way:

```rust
impl<T, E> Result<T, E> {
  fn map_err<F: Fn<E>>(self: Self, f: F) -> Result<T, F::Output> { ... }
}
fn each<F: Fn<i32, Output = u32>>(f: F) -> u32
```

A generic function takes a callable by value and monomorphizes, as it does
for any bound; `dyn Fn<i32, Output = u32>` is the type-erased form, `dyn mut
Fn<...>` the writable one, and both are values like any other `dyn A`
(`06-dispatch.md`). `FnOnce` has no `dyn`: a handle that may be called once
is not a handle.

A closure implements whichever of the three its body needs — the least
demanding one that works (`01-types.md`). A function pointer implements
`Fn` for its own signature (`std/ops/fn.ce`), which is how a named fn meets
a bound a closure also answers.

## Copy

```rust
trait Copy { }
```

This is std's declaration — `std::ops`'s own, the compiler reading it
by pointer (`12-projects.md`); a project may declare its own `Copy`, and the
exclusion checks still read std's.

`Copy` is a marker trait — a trait with no functions, which is why its impl is
empty. The compiler accepts an impl only when every field (or element) is itself
`Copy` and no destructor exists (see `Drop` below).

```rust
struct Point {
  mut x: u32,
  y: u32,
}

impl Copy for Point { }
```

See `03-move.md` for what `Copy` does on assignment.

## Drop

```rust
trait Drop {
  fn drop(mut self: Self) -> ();
}
```

std's own declaration, in `std::ops` — the exclusion pair's other half, read
by pointer like `Copy` (`12-projects.md`).

`Drop` has a single function, `drop`, which receives the value by ownership.
It runs when the binding that owns the value reaches the end of its scope —
see `03-move.md` for the timing rules.

```rust
impl Drop for File {
  fn drop(mut self: Self) -> () {
    close(self.fd);
  }
}
```

`mut self: Self` — a parameter is a slot like any other (`01-types.md`), so
`mut` marks it writable; taking `Self` by value is what transfers ownership.
The body must treat the zero value as a no-op — this is what makes `@take` sound, and it
forbids types whose zero value is a live resource:

```rust
impl Drop for BadFd {
  fn drop(mut self: Self) -> () {
    close(self.fd);   // ❌ if zero means fd 0, closing stdin
  }
}
```

`Copy` and `Drop` are mutually exclusive. The compiler rejects `impl Copy`
when any field is not `Copy` or a destructor exists.

Both are ordinary traits — declared once, implemented by hand like any other.
What sets them apart is that the compiler knows their names: it checks a `Copy`
impl against the type's fields, and it inserts the `Drop` call.

## Option

`?T` is `Option<T>`, a regular enum in the standard library:

```rust
enum Option<T> {
  None,
  Some(T),
}

// Some(3): ?u32
```

`Some` and `None` are written without a prefix — that is part of the `?T` sugar,
not of `use` (`11-namespaces.md`). How the type is laid out depends on `T`
(`01-types.md`), but nothing about that shows up in the enum itself.

## Result

`Result<T, E>` is the error-carrying counterpart of `Option<T>`, and like it an
ordinary enum in the standard library:

```rust
enum Result<T, E> {
  Ok(T),
  Err(E),
}
```

Its sugar is `E?T` (`01-types.md`), which reads as "a `T` or an `E`" — the same
`?`, with the other case named in front of it instead of left empty.

There is no `try`, no exception and no `catch`: an error is a value, and `f()?`
hands it back instead of branching on it. When there is nothing sensible to hand
back, `panic` — an ordinary function in `std`, not a builtin — ends the program.
It is `#[noreturn]`: a call to it never produces a value (`10-iteration.md`).

Niche optimization is a compiler specialization for `Option` specifically,
not a trait-system feature: when `T` has an unused bit pattern, `@sizeof(?T)`
equals `@sizeof(T)`; otherwise `?T` grows by a tag. See `04-generics.md` for the
specialization rules.

## Coherence

At most one `impl` may exist for a given trait and concrete type, globally —
generic impls with disjoint or strictly ordered bounds are the exception, see
`04-generics.md`. To keep two libraries from colliding, an `impl` is rejected
unless the trait or the type was defined in the current namespace — the orphan
rule:

```rust
impl Show for u32 { ... }   // ❌ neither Show nor u32 is ours
```
