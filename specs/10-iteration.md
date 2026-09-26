# Iteration

This chapter continues `05-traits.md` (traits and associated items) and
`09-match.md` (match). It defines the `Iterator` and `IntoIterator` traits and
the loops built on them.

## Iterator

An iterator yields a sequence of values. The trait carries one associated
type — what it yields — and one method — advance and yield the next value:

```rust
trait Iterator {
  type Item;
  fn next(self: *mut Self) -> ?Self::Item;   // None ends the sequence
}
```

`next` borrows `self` mutably (`*mut Self`): advancing the iterator moves its
position, so the borrow must be mutable. It returns `?Self::Item` —
`Option<Self::Item>` — and `None` marks the end. `Iterator` pairs with `match`
(`09-match.md`): every loop is a `match` on `next`'s result.

A slice is an iterator over its elements. A slice borrows what it points at
(`01-types.md`), so iterating lends each element out — it neither moves nor
copies it. What comes out is therefore a pointer:

```rust
fn sum(s: []u32) -> u32 {          // a slice, straight from the parameter
  let mut total = 0;
  for x in s {
    total += *x;                   // x: *u32
  }
  total
}
```

```rust
impl<T> Iterator for []T {
  type Item = *T;
  fn next(self: *mut Self) -> ?*T {
    if self.len == 0 {
      None
    } else {
      let p = self.ptr;
      *self = (*self)[1..];
      Some(p)
    }
  }
}
```

A mutable slice lends a writable pointer:

```rust
fn zero(s: []mut u32) {
  for x in s {
    *x = 0;                        // x: *mut u32
  }
}

impl<T> Iterator for []mut T {
  type Item = *mut T;
  fn next(self: *mut Self) -> ?*mut T {
    if self.len == 0 {
      None
    } else {
      let p = self.ptr;
      *self = (*self)[1..];
      Some(p)
    }
  }
}
```

`self` is `*mut Self`, so the iterator advances itself by slicing —
`*self = (*self)[1..]` moves the start and shortens the length in one step,
which is all a slice iterator is. `ptr` and `len` are a slice's two fields
(`01-types.md`). The `[]mut T` impl
is more specific and wins for a mutable slice — the same rule that makes `*T`
win over `T` (`04-generics.md`).

Because a slice only borrows, `T` needs no `Copy`: nothing is moved or copied,
only pointed at.

## IntoIterator

A container is not itself an iterator: it may be iterated several ways, and
iterating needs a cursor. `IntoIterator` turns a value into its iterator,
consuming it:

```rust
trait IntoIterator {
  type Item;
  type IntoIter: Iterator<Item = Self::Item>;
  fn into_iter(self: Self) -> Self::IntoIter;
}
```

The bound `Iterator<Item = Self::Item>` ties the two associated types: what
the iterator yields is exactly what the container holds. `Iterator<Item = T>`
is an associated-type equality bound — `IntoIter` implements `Iterator`, and
its `Item` is `T`.

An iterator is its own `IntoIterator`, so `for` over a slice needs no
conversion:

```rust
impl<I: Iterator> IntoIterator for I {
  type Item = I::Item;
  type IntoIter = I;
  fn into_iter(self: Self) -> I { self }
}
```

An owned array is different. It owns its elements, so iterating it moves them
out and uses the array up. That needs an iterator holding the array and an
index — a slice would borrow from a value that is about to die, and returning a
slice of a local array is already a compile error (`01-types.md`):

```rust
struct ArrayIter<T, const N: usize> {
  arr: [N]mut T,    // moved in — a fresh slot may raise writability (01-types.md)
  mut index: usize,
}

impl<T, const N: usize> IntoIterator for [N]T {
  type Item = T;
  type IntoIter = ArrayIter<T, N>;
  fn into_iter(self: Self) -> ArrayIter<T, N> {
    ArrayIter<T, N>{ arr: self, index: 0 }
  }
}

impl<T, const N: usize> Iterator for ArrayIter<T, N> {
  type Item = T;
  fn next(self: *mut Self) -> ?T {
    if self.index == N {
      None
    } else {
      let i = self.index;
      self.index = self.index + 1;
      Some(@take(&mut self.arr[i]))   // the element leaves, a zero stays behind
    }
  }
}
```

Every element leaves through `@take` (`03-move.md`) — the one way a value comes
out of a place — so the array is used up element by element and a zero value
stays behind in each slot. A `T` with a destructor is fine: the zeros left
behind are destructed as no-ops, and whatever the loop did not take is
destructed with `ArrayIter` itself.

To iterate an array without using it up, iterate a pointer to it — which is
what `&arr` is, since an array never decays (`01-types.md`). The iterator is
then a slice over the array's own storage, so nothing moves:

```rust
impl<T, const N: usize> IntoIterator for *[N]T {
  type Item = *T;
  type IntoIter = []T;
  fn into_iter(self: Self) -> []T {
    []T { ptr: &(*self)[0], len: N }
  }
}

impl<T, const N: usize> IntoIterator for *mut [N]mut T {
  type Item = *mut T;
  type IntoIter = []mut T;
  fn into_iter(self: Self) -> []mut T {
    []mut T { ptr: &mut (*self)[0], len: N }
  }
}
```

The length comes from the type: `N` is a `const` value parameter
(`08-reflection.md`), and `ptr` and `len` are a slice's two fields
(`01-types.md`).

## For

`for` is the only loop. Which of its three shapes it takes is decided by what
follows the keyword — `let` marks the pattern shape, `in` the iteration shape,
and anything else is a condition:

| shape | runs its body while |
| ----- | ------------------- |
| `for cond { }` | the condition holds |
| `for let PAT = e { }` | the pattern fits |
| `for PAT in c { }` | the iterator yields |

`in` appears nowhere else in the language, so one token is enough to tell the
three apart. The first two shapes are below; the third has a section of its
own.

### Condition

`for cond` runs its body for as long as the condition holds:

```rust
for cond {
  body
}
```

The condition is re-evaluated before every iteration. `break` exits the loop
and `continue` skips to the next evaluation:

```rust
for cond {
  if skip {
    continue;
  }
  if done {
    break;
  }
}
```

`continue` skips to the condition — it skips everything else in the body
too, so a loop that steps at its tail (`for node != None { ...; node =
next(node); }`) does not step when a path through the body ends in
`continue`. This is the `while` bargain of C, not a new trap; a loop whose
rhythm is that intricate is an `Iterator` (`09-match.md`, `04-generics.md`)
— that is what the shape is for, and why the language does not carry a
third loop.

### For let

`for let` matches a value against a pattern and runs the body while the
pattern fits:

```rust
for let Some(x) = it.next() {
  use(x);
}
```

desugars to the condition shape over a `match`:

```rust
for true {
  match it.next() {
    Some(x) => use(x),
    None => break,
  }
}
```

A value the pattern does not fit ends the loop — the arm written `None` above.
`for x in c` is this shape over `into_iter` (`For in` below), so the two are
one construct.

## For in

`for x in c` iterates a container and is sugar for `for let` over its iterator:

```rust
for x in c {
  body
}
```

desugars to:

```rust
{
  let mut it = c.into_iter();
  for let Some(x) = it.next() {
    body
  }
}
```

The binding is a pattern
(`09-match.md`), so an iterator that yields tuples is destructured in place:

```rust
for (x, y) in zip(xs, ys) {
  use(*x + *y);          // x, y: *u32
}
```

`x` is a fresh binding per iteration; its type is the container's `Item`.
Whether the container is used up depends on what is iterated:

| loop | iterated thing | `x` | used up? |
| ---- | -------------- | --- | -------- |
| `for x in arr` | `[N]T` — an owned array | `T` | ✅ the array is consumed |
| `for x in &arr` | `*[N]T` | `*T` | ❌ |
| `for x in &mut arr` | `*mut [N]mut T` | `*mut T` | ❌ |
| `for x in s`, `s: []T` | `[]T` | `*T` | ❌ |
| `for x in s`, `s: []mut T` | `[]mut T` | `*mut T` | ❌ |

Iterating a borrow yields a pointer, so a read takes `*x` and a field takes
`x.f`. Iterating an owned array yields the value itself and consumes the array
— the move semantics of `03-move.md`, applied to iteration:

```rust
let arr = [3]u32{1, 2, 3};

for a in arr { use(a); }     // a: u32 — arr is consumed
for a in &arr { use(*a); }   // a: *u32 — arr is still there
```

Mutable iteration yields `*mut T`, and a mutable pointer cannot grant permission
the type does not give (`01-types.md`), so the elements have to be `mut` — the
`let mut a = [3]mut u32{}` row of the table there:

```rust
let mut arr = [3]mut u32{1, 2, 3};

for a in &mut arr { *a = 0; }   // a: *mut u32
```

With a plain `[3]u32`, `*mut [N]mut T` does not match, and since `*mut [N]T` also
matches `*[N]T` (`04-generics.md`) the read-only impl is what applies: `x` is
`*u32` and `*x = v` is rejected — the same answer `arr[0] = v` gets.

### Range

`a..b` is an expression — one `..`, both ends required (`15-grammar.md`) —
and its value is a `Range<T>` for the integer type of the ends:

```rust
// in std
struct Range<T> { start: T, end: T }

impl<T: Copy> Iterator for Range<T> {
  type Item = T;
  fn next(self: *mut Self) -> ?T {
    if self.start < self.end {
      const v = self.start;
      self.start = self.start + 1;
      return Some(v);
    }
    return None;
  }
}
```

The interval is half-open: `0..3` yields 0, 1, 2. A range with `start >= end`
yields nothing — the loop body never runs, and no error is raised. The most
common consumer is the loop the range was made for:

```rust
for i in 0..len {
  visit(a[i]);
}
```

There is no `..=`: a closed end is written `0..(n + 1)`. An open end is not
an expression — `..n` and `n..` index a slice's length, and they stay inside
index brackets (`01-types.md`), where the end they mean exists.

`a[1..3]` is not sugar for passing a range value: slicing borrows the array,
and the bracket form says so where it stands. A `Range` value is a value —
stored, passed, returned — and the two agree only in appearance.

### Const For

`const for` requires the iterated value to be compile-time known and
unrolls the loop during compilation:

```rust
const for f in fields {
  ...
}
```

Each iteration is emitted once, with the loop variable replaced by its value,
so the variable — and everything reached through it, such as `f.name` — is
compile-time known. That is what lets a reflected name feed `@field`
(`08-reflection.md`), which requires one.

What is unrolled is the iteration, not the body: each copy of the body is
emitted where the loop stood, as ordinary runtime code. `const for` is
therefore not sugar for the pattern shape — no loop survives into the
generated code — and a `const for` over a value that is not compile-time
known is a compile error.

## If

`if` is an expression: its value is the value of the branch taken, and both
branches must have the same type — the same rule `match` arms follow
(`09-match.md`).

```rust
let a = if cond { 1 } else { 2 };
```

Without an `else` the untaken path yields `()`, so the branches agree only when
the taken one does too — an `if` with no `else` is written as a statement.

## Return

`return` leaves the function immediately, with a value:

```rust
return expr;   // the value is expr
return;        // the value is ()
```

The value must match the function's return type. It leaves the **function**,
not the loop — `break` is what leaves a loop:

```rust
fn find(s: []u32, target: u32) -> ?usize {
  for (i, x) in enumerate(s) {
    if *x == target {
      return Some(i);
    }
  }
  None
}
```

`return` joins the existing rule rather than replacing it: a function's value is
still its last expression, and `return` is the way out before the end.

```rust
fn clamp(x: u32, lo: u32, hi: u32) -> u32 {
  if x < lo { return lo; }
  if x > hi { return hi; }
  x
}
```

A path that reaches the end of the body without a value is a type error — the
body's value is `()`, which does not match the declared return type. No separate
"every path returns" rule is needed.

### Divergence

There is no `never` type (`01-types.md`), so an arm ending in `return` does not
take part in the type agreement between arms (`09-match.md`): control never
reaches its end, and the compiler judges that from the syntax — nothing in the
type system is involved.

```rust
match o {
  Some(v) => { return v; },
  None => 0,
}
```

One call can diverge too: a function marked `#[noreturn]` (`01-types.md`)
never produces a value, and a path that ends in a call to one — `panic`, say —
leaves nothing to type check. This is why

```rust
fn open_or_die(path: []u8) -> File {
  match open(path) {
    Ok(f)  => f,
    Err(_) => panic("cannot open"),
  }
}
```

compiles: the `Err` arm's value is nothing, so it agrees with any other arm.
The judgement stays syntactic — the compiler does not trace control flow; it
looks for a `return`, a `break`, a `continue`, or a call to a `#[noreturn]`
function, and nothing deeper.

## Adapters

An adapter wraps an iterator in an ordinary struct — nothing about it needs
language support beyond `Iterator` itself. A library is expected to provide at
least:

| adapter | yields |
| ------- | ------ |
| `map(f)` | `f` applied to each item |
| `filter(f)` | the items `f` accepts |
| `zip(other)` | a pair, one from each iterator |
| `enumerate()` | a pair — the running count and the item |
| `fold(init, f)` | one value, accumulated |

They compose: `zip` over `enumerate`, and so on. Each is a struct holding the
inner iterator plus whatever state it needs, whose `next` calls the inner one:

```rust
struct Enumerate<I: Iterator> {
  mut iter: I,
  mut count: usize,
}

impl<I: Iterator> Iterator for Enumerate<I> {
  type Item = (usize, I::Item);

  fn next(self: *mut Self) -> ?(usize, I::Item) {
    match self.iter.next() {
      Some(x) => {
        let i = self.count;
        self.count = self.count + 1;
        Some((i, x))
      }
      None => None,
    }
  }
}
```

`enumerate` yields a **count**, not an index — a borrowed iteration already
yields a pointer to the element, but which item this is carries information the
pointer does not.

## Serialization

With `match`, `@field`, and `const for`, a deserializer fills a struct field by
field:

```rust
fn deserialize<T>(s: []u8) -> T {
  let doc = parse(s);          // the bytes become a tree of values — no T yet
  match @typeinfo<T>() {
    Struct { fields, .. } => {
      let mut v = T{};                 // zero-init
      const for f in fields {          // f: *Field — a borrow
        *@field(v, f.name) = doc.get<$$f.type>(f.name);
      }
      v
    }
    _ => @compileError("not a struct"),
  }
}
```

The input is consumed once, before the `match`; the `match` only routes on the
shape of `T`. `$$f.type` splices the field's type reference back into a type and
`f.name` is compile-time known because the loop is unrolled — the two things the
assignment needs (`08-reflection.md`).

A format that is not self-describing — a binary layout with no tags — cannot be
parsed before `T` is known, since how long a field is depends on its type. There
the parse has to move with the loop, carrying the rest of the input along:

```rust
let mut cur = s;
const for f in fields {
  let (x, rest) = parse_field<$$f.type>(cur);
  *@field(v, f.name) = x;
  cur = rest;
}
```

Writing a field is governed by its `mut` (`08-reflection.md`), so this fills in a
`T` whose fields are declared `mut`. A type with immutable fields cannot be built
this way — it has to be produced whole, by a constructor or a literal.
