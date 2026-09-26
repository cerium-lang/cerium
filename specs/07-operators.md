# Operators

This chapter continues `05-traits.md`. An operator is sugar for a trait method
call. There is no operator overloading of the C++ kind, and no way to define a
new operator.

## The traits

They live in `std::ops`.

| operator | trait | method |
| --- | --- | --- |
| `a + b` | `Add<Rhs>` | `fn add(self: *Self, other: *Rhs) -> Self` |
| `a - b` | `Sub<Rhs>` | `fn sub(self: *Self, other: *Rhs) -> Self` |
| `a * b` | `Mul<Rhs>` | `fn mul(self: *Self, other: *Rhs) -> Self` |
| `a / b` | `Div<Rhs>` | `fn div(self: *Self, other: *Rhs) -> Self` |
| `a < b`, `a > b`, `a <= b`, `a >= b` | `Ord` | `fn cmp(self: *Self, other: *Self) -> Ordering` |
| `a == b`, `a != b` | `Eq` | `fn eq(self: *Self, other: *Self) -> bool` |

```rust
enum Ordering {
  Less,
  Equal,
  Greater,
}
```

There is one method per trait, not one per operator: `a <= b` is `cmp` read the
other way round, and `a != b` is `!eq`.

`Rhs` is a type parameter and defaults to `Self`, so `impl Add for Vec3` means
`impl Add<Vec3> for Vec3`. It need not be `Self` — that is what pointer
arithmetic is (`01-types.md`):

```rust
impl<T> Add<usize> for *T { ... }
```

The result is always `Self`. There is no `Output` associated type: an operation
that would return something else — a matrix times a vector — is a method.

## Desugaring

`a + b` is `Add::add(&a, &b)`. Both operands are passed by pointer, which is why
the methods take `*Self` and `*Rhs`; nothing is moved or copied. This is the one
place besides a method call where an address is taken for you
(`05-traits.md`) — an operator is sugar for a call, not an implicit conversion.

Compound assignment needs no trait of its own:

```rust
a += b;   // a = a + b — requires Add, and a to be a mut slot
```

## Who implements them

The built-in types come with impls provided by the compiler — integers, floats,
`bool`, pointers, and so on. A type of your own implements them like any other
trait, subject to the orphan rule (`05-traits.md`):

```rust
struct Vec3 { x: f32, y: f32, z: f32 }

impl Add for Vec3 {
  fn add(self: *Self, other: *Self) -> Vec3 {
    Vec3{ x: self.x + other.x, y: self.y + other.y, z: self.z + other.z }
  }
}
```

An operator needs no `use` — the compiler finds the trait. Writing the call out,
`Add::add(&a, &b)`, does need one, like any trait method
(`11-namespaces.md`).

A bound is what makes an operator available in generic code:

```rust
fn max<T: Ord>(a: T, b: T) -> T {
  if a > b { a } else { b }   // `>` is legal because T: Ord
}
```

## What is not a trait

Some things are language, not traits:

- `&&` and `||` — they short-circuit, which a call cannot
- `?`, `!`, `...`, and every `@` builtin — syntax and builtins, not operators
- `%`, the bitwise operators, shifts, and unary `-` and `~` are built in for
  integers; they have no trait

### Shifts

`a << n` and `a >> n` are built in for integers, with any integer type on
the right. A signed `>>` is an arithmetic shift — the sign bit repeats — and
an unsigned `>>` is a logical one; there is no `>>>`, because the unsigned
types already say which shift is meant. A shift amount at or above the
operand's width panics at run time — "shift amount out of range" — and is a
compile error when the amount is a compile-time known constant, the same
bargain a constant index out of range makes (`01-types.md`).

### Why indexing is not a trait

An index yields a **place**, and a method returns a **value** (`03-move.md`).
Three things go through an index, and a value can only do the first of them:

```rust
let x = a[0];              // read
a[0] = v;                  // write
let p = &a[0];             // address of
```

If `Index` returned `T`, `a[0] = v` would assign to a temporary and `&a[0]` would
take the address of a copy — a move out of a place, which is rejected.

If it returned `*T` or `*mut T`, both would work once `a[0]` is desugared to
`*a.index(0)`, but the price is too high:

- a constant index out of range stops being a compile error, because the check
  would sit in an impl body instead of in the language (`01-types.md`)
- writability is part of the type here — `[N]T` and `[N]mut T` are two types —
  so one method cannot serve both, and `Index` would have to split in two
- it still would not reach tuples and packs: `ts[0]` and `ts[1]` have different
  types (`04-generics.md`), and a method has one return type per `Self`

As a language rule, indexing is uniform and can be checked at compile time.

## Open items

- An `Output` associated type, for operations that return something other than
  `Self` — a matrix times a vector.
- Traits for `%`, the bitwise operators, shifts, unary `-`, and in-place
  operations that would avoid building a new value.
