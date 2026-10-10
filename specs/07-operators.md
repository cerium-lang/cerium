# Operators

This chapter continues `05-traits.md`. An operator is sugar for a trait method
call. There is no operator overloading of the C++ kind, and no way to define a
new operator.

## The traits

They live in `std::ops`.

| operator | trait | method |
| --- | --- | --- |
| `a + b` | `Add<Rhs>` | `fn add(self: Self, other: Rhs) -> Self::Output` |
| `a - b` | `Sub<Rhs>` | `fn sub(self: Self, other: Rhs) -> Self::Output` |
| `a * b` | `Mul<Rhs>` | `fn mul(self: Self, other: Rhs) -> Self::Output` |
| `a / b` | `Div<Rhs>` | `fn div(self: Self, other: Rhs) -> Self::Output` |
| `a % b` | `Rem<Rhs>` | `fn rem(self: Self, other: Rhs) -> Self::Output` |
| `a & b` | `BitAnd<Rhs>` | `fn bitand(self: Self, other: Rhs) -> Self::Output` |
| `a | b` | `BitOr<Rhs>` | `fn bitor(self: Self, other: Rhs) -> Self::Output` |
| `a ^ b` | `BitXor<Rhs>` | `fn bitxor(self: Self, other: Rhs) -> Self::Output` |
| `a << b` | `Shl<Rhs>` | `fn shl(self: Self, other: Rhs) -> Self::Output` |
| `a >> b` | `Shr<Rhs>` | `fn shr(self: Self, other: Rhs) -> Self::Output` |
| `-a` | `Neg` | `fn neg(self: Self) -> Self::Output` |
| `a += b` | `AddAssign<Rhs>` | `fn add_assign(self: *mut Self, other: Rhs)` |
| `a <<= b` | `ShlAssign<Rhs>` | `fn shl_assign(self: *mut Self, other: Rhs)` |
| `a >>= b` | `ShrAssign<Rhs>` | `fn shr_assign(self: *mut Self, other: Rhs)` |
| `a < b`, `a > b`, `a <= b`, `a >= b` | `Ord` | `fn cmp(self: Self, other: Self) -> Ordering` |
| `a == b`, `a != b` | `Eq` | `fn eq(self: Self, other: Self) -> bool` |

```rust
enum Ordering {
  Less,
  Equal,
  Greater,
}
```

There is one method per trait, not one per operator: `a <= b` is `cmp` read the
other way round, and `a != b` is `!eq`.

The compound family is closed by the language's own tokens: `+=` `-=` `*=` `/=`
`<<=` `>>=` are what the lexer spells, so `RemAssign`, `BitAndAssign`, and the
rest have no operator to sugar — a trait without its operator is a trait nobody
calls, and std writes none. `Rem` itself is an integer's own idea — no float
row exists, a float beside a float keeps the language's error.

`Rhs` is a type parameter and defaults to `Self`, so `impl Add for Vec3` means
`impl Add<Vec3> for Vec3`. It need not be `Self` — that is what pointer
arithmetic is (`01-types.md`), and the row spells it in `std::ops`:

```rust
impl<T> Add<usize> for *T {
  type Output = *T;
  fn add(self: *T, other: usize) -> *T { self + other }
}
```

The body is the language's own pair — a pointer beside an integer, the
built-in table answering it, so the row never answers itself. A plain `p + n`
never comes here, any integer the table's to take; the spelled call arrives
with `usize` in hand — and the spelled call picks the row itself: the rows the
receiver fits walk in specificity order, and the first whose signature takes
the call's arguments answers (`Add::add(p, n)` with `n: usize` takes this row
over the borrowed one's, `Add::add(p, q)` the borrowed one's).

The answer is the `Output` associated type (`05-traits.md`), so an operation
may return something other than either operand — the arithmetic four carry it,
and a comparison does not: `cmp` and `eq` decide, they do not produce, so their
answer is `Ordering` and `bool` outright.

## Desugaring

`a + b` is `Add::add(a, b)`. Both operands enter by value, the left one moving
where its type is not Copy, the right one a value the parameter's own slot
takes whole. Nothing is borrowed on the way in — an operator is sugar for a
call, and the call's own signature says what moves.

A comparison is a read, not a consumption: `a < b` is
`Ord::cmp(&a, &b)` and `a == b` is `Eq::eq(&a, &b)`, both operands entering as
borrows — the value compared where it stands, nothing moved, and a `max` over
`T: Ord` needs no `Copy`. `cmp` and `eq` take `*Self` receivers; a comparison
through a pointer is the deref the caller spells (`*p < *q`), not a row of the
pointer's own.

A borrowed pair is the arithmetic's to spell, and it is an impl of the
operator's trait over the pointer. The std writes one generic row per
arithmetic operator, and the row names the projection its answer rides on:

```rust
impl<T: Add + Copy> Add for *T {   // &a + &b, the row the pointers carry
  type Output = T::Output;
  fn add(self: *T, other: *T) -> T::Output { Add::add(*self, *other) }
}
```

The `Copy` bound is the row's own law: reading `*self` out of a shared borrow
is a copy or it is a move out of one, and the latter is not a thing to write
(`03-move.md`). A type that is not Copy borrows no row — its own impl spells
its fields, or takes the values. One generic row covers every Copy element:
where another language writes a macro over its matrix, Cerium writes this
(`14-macros.md`).

Compound assignment is its own trait — the left is borrowed for the write, the
right enters by value:

```rust
a += b;   // AddAssign::add_assign(&mut a, b) — requires the row, and a mut slot
```

Unary `-` is the same sugar with one operand: `-a` is `Neg::neg(a)`, the value
entering whole. A borrowed operand reaches the row by the spelled call alone —
`-&a` keeps the language's error, the pointer a scalar to the checker's unary,
and `Neg::neg(&a)` is what a library writes instead.

## Who implements them

The scalar rows live in `std::ops` as source, spelled like any other impl —
a plain `n + m` never reaches them, the built-in table answering first, but
the spelled call `Add::add(n, m)` is theirs to answer, and a generic's bound
reads them through the same rows. A type of your own implements them like any
other trait, subject to the orphan rule (`05-traits.md`):

```rust
struct Vec3 { x: f32, y: f32, z: f32 }

impl Add for Vec3 {
  type Output = Vec3;
  fn add(self: Vec3, other: Vec3) -> Vec3 {
    Vec3{ x: self.x + other.x, y: self.y + other.y, z: self.z + other.z }
  }
}
```

An operator needs no `use` — the compiler finds the trait. Writing the call out,
`Add::add(a, b)`, does need one, like any trait method
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
- `~` — it has no trait, an integer's own complement
- `%`, the bitwise operators, shifts, and unary `-` are traits now, the rows in
  `std::ops` — but a scalar pair never reaches them: the built-in table answers
  first, an integer beside an integer and a float beside a float, and what the
  table does not take is the trait's to answer. `bool` is the one-bit integer
  for `&`, `|`, and `^` — the table takes it whole, the rows beside them for
  the spelled call — while `%` and the shifts keep their integer sense and no
  bool row exists.

### Shifts

`a << n` and `a >> n` take any integer type beside any integer type — a
shift's amount is its own width, not the shifted's — the built-in table
answering that pair first, the `Shl` and `Shr` rows waiting for what is left.
A signed `>>` is an arithmetic shift — the sign bit repeats — and an unsigned
`>>` is a logical one; there is no `>>>`, because the unsigned types already
say which shift is meant. A shift amount at or above the operand's width
panics at run time — "shift amount out of range" — and is a compile error when
the amount is a compile-time known constant, the same bargain a constant index
out of range makes (`01-types.md`). The compounds `<<=` and `>>=` carry their
own traits, `ShlAssign` and `ShrAssign`, beside `AddAssign`'s.

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

- The destructor half is written whole. The assignment side: the left of a
  store is a place, so a non-Copy `AddAssign` spells its body — `*self =
  ...` — and reassignment runs the old value's destructors before the
  store, `@take` emitted with the zero value written back (`03-move.md`).
  The scope side: a binding that still owns a value runs its destructors
  at the closing brace its block reaches on its own — reverse declaration
  order, structural (a field's, an element's, a row's inherited, an
  enum's payload behind a match the checker spells) — an early exit
  (`return`, `break`, `continue`) carries the destructors of the bindings
  its path owns, a bare temporary dies at its statement's end, a fn's
  parameters at the return its body reaches, and a `for`'s pattern
  bindings at every round's end. An abort runs none, by `03-move.md`'s
  own words.
- A row whose variables live only in the trait's arguments (`impl<T> Add<T>
  for S`) joins the walk with its slots open: the receiver lands what it
  can, the call's own arguments bind the rest — the sugar, the spelled
  call and the method sugar walk the same rows — a variable no argument
  rides is the call's error to name, the bounds wait until every slot
  lands, and a row whose trait argument is a shape (`Add<Wrap<T>>`)
  binds through the literal's fields, a want's parameters lending their
  slots without holding them.
