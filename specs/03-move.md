# Move

This chapter continues `02-layout.md`.

Assignment moves by default. Once a value has been moved out of a binding, that
binding can no longer be used:

```rust
let a = X{ a: 1, b: 2, c: 3.0 };

let b = a;            // ✅ a is moved into b
let d = a;            // ❌ a has been moved
```

## Copy

A type that implements the `Copy` trait is duplicated instead of moved, leaving
the source usable. `Copy` is a marker trait — its impl is empty — and the
compiler accepts it only when every field (or element) is itself `Copy`:

```rust
struct Point {
  mut x: u32,
  y: u32,
}

impl Copy for Point { }

let p = Point{ x: 1, y: 2 };

let q = p;   // ✅ Point is Copy, so p is still usable

assert(p.x == 1);
```

Primitives, pointers, slices, unions and `()` are `Copy` out of the box — a
slice is a pointer and a length, so even `[]mut T` is `Copy`, and so is a `dyn A`
handle, which is a pointer and a vtable (`06-dispatch.md`). An enum carries
whatever its payload carries, so `?T` is `Copy` exactly when `T` is, while an
enum without a payload — `enum X(u32)` — is `Copy`. An aggregate — an array, a
tuple or a struct — is `Copy` exactly when every field is, so `[3]u32` is `Copy`
while `[3]X` is not, unless `X` implements it too:

```rust
let a = [3]u32{1, 2, 3};

let b = a;   // ✅ the elements are Copy

assert(a[0] == 1);
```

## Moving Out of a Place

A move can only start from a **binding**. Reading a whole non-Copy value out of
a place — behind `*p`, a field, or an index — is a compile error: a place does
not own the value, a binding does, and whoever owns it still expects a legal
value to be there. `@take` (below) is the one way around this:

```rust
struct Big { a: u32, b: u32 }

let mut b = Big{ a: 1, b: 2 };
let p: *mut Big = &mut b;

let m = *p;        // ❌ Big is not Copy, cannot move out of a place
let n = p.a;      // ✅ u32 is Copy, reading a field is fine

*p = Big{ a: 3, b: 4 };   // ✅ moving *into* a place is fine
```

Fields of a binding are places too. Moving a non-Copy field out would leave the
binding partially uninitialized, so it is rejected the same way; a Copy field is
an ordinary read:

```rust
struct Wrap { n: u32, big: Big }

let w = Wrap{ n: 1, big: Big{ a: 1, b: 2 } };

let x = w.n;     // ✅ u32 is Copy
let y = w.big;   // ❌ Big is not Copy, cannot move out of a field
let z = w;       // ✅ the move above was rejected, so w is still whole
```

Because a partial move never happens, a binding is either whole or dead — never
half-moved. That is what makes the drop set statically known (see Static
Insertion below).

Copying or taking over a non-Copy value from a place is a job for the standard
library, not for the language — there are no `@copy`/`@read` builtins; `@take`
below is the one sanctioned way.

## Arguments

Passing an argument follows the same rule as assignment: a `Copy` value is
duplicated, anything else is moved and the source binding becomes unusable.

```rust
let s = Big{ a: 1, b: 2 };

f(s);   // ✅ moved into the parameter

let t = s;   // ❌ s has been moved
```

A non-Copy place cannot be passed either — `f(*p)` or `f(w.big)` is the same
move-out error as above. To hand a big value to a function without moving it,
pass a pointer or a slice explicitly:

```rust
f(&s);        // ✅ pass a pointer
g(s);         // ✅ s is not Copy, so this moves — see above
```

The implicit borrowing that other languages insert here does not exist: what
moves is what you wrote.

## Drop

A type that owns a resource implements `Drop`: its destructor runs when the
binding that owns the value reaches the end of its scope. The running example is
a file handle:

```rust
struct File {
  mut fd: i32,
}
```

`Drop` and `Copy` are mutually exclusive. A `Copy` assignment duplicates the
bits, so both bindings would carry the same handle and the destructor would run
twice — a double free:

```rust
struct Bad { f: File }
impl Copy for Bad { }   // ❌ File has a destructor
```

No extra rule is needed beyond the impl check: destructors are structural — a
type whose field implements `Drop` inherits a destructor for that field — and
`impl Copy` is rejected when any field is not `Copy` or a destructor exists.

Moving transfers the destructor along with the value: the moved-from binding is
already dead, so nothing runs for it at the end of the scope.

### Assignment

Where a `Drop` value is written also matters, because the old value must be
destructed exactly once. Writing through a binding, or through a `*mut T`, is
fine — a `*mut T` is exclusive (`01-types.md`), so the old value has exactly one
owner and can be destructed in place:

```rust
let mut x = File{ fd: 3 };

x = File{ fd: 4 };   // ✅ the old value is destructed first
x.fd = 5;            // ✅ File::fd is mut — writing a field does not destruct

let p: *mut File = &mut x;

*p = File{ fd: 6 };  // ✅ p is exclusive, so the old value is destructed
```

### Unions Forget

A union has no destructor, ever. Nothing stored in one is destructed — not when
the binding leaves its scope, not when a field is overwritten. This is not an
oversight: it is the built-in `forget`. Dropping destructor tracking is always
sound — it leaks resources, but never memory safety.

```rust
union Sink {
  f: File,
  n: usize,
}

let u = Sink{ f: File{ fd: 3 } };

// end of the scope: nothing runs — fd 3 stays open, on purpose
```

The `Copy`/`Drop` conflict does not reach into a union: a union is `Copy` even
when a field implements `Drop`, sound for the very reason the conflict exists —
no destructor can run twice when none runs at all. This is the only place a
dropping type can sit inside a `Copy` one, and it is also the exception to the
assignment rule above: overwriting `u.f` leaks the old `File` instead of
destructing it.

A struct containing such a union inherits no destructor from it, so the forget
composes. And what goes into a union cannot come back out — a non-`Copy` value
cannot be moved out of a place — so this is forgetting, not storage; retrieval
is the job of `@take`.

## Take

`@take(p)` moves the value out of the place `p` points at — `p` is a `*mut T` —
and immediately writes the zero value back. It is the one sanctioned way around
the move-out restriction. The zero is what makes it safe: `p` does not own the
place, and whoever does will still look at it — and destruct it. A zero value
survives that; a moved-out one would not.

```rust
let mut a = File{ fd: 3 };
let p: *mut File = &mut a;

let v = @take(p);   // v owns fd 3; *p — and so a — now holds the zero value

// end of the scope: destructing the zero File is a no-op,
// v is destructed exactly once and closes fd 3
```

Everything the standard library needs is built from it:

```text
take(x)      = @take(&mut x)   // x must be a mut slot
drop_at(p)   = { let v = @take(p); }   // destructs the old value, leaves zero
```

Retrieving a value stored in a union is `@take(&mut u.f)`, and it is the caller's
job to know the active field — reading an inactive one yields garbage, exactly as
with a plain union read. A union field cannot be marked `mut` (`01-types.md`);
`u` itself has to be `mut`, which is what makes the whole union writable.

Destructors must treat the zero value as a no-op. This is what makes `@take`
sound — and it forbids types whose zero value is itself a live resource (a
"zero" that means fd 0 would close stdin when destructed).

## Timing

A binding that still owns a value is destructed when its scope ends, in reverse
declaration order. A moved-from binding owns nothing and runs nothing — it died
at the move, and the compile-time move check is what guarantees the value has
exactly one owner:

```rust
{
  let a = File{ fd: 3 };
  let b = a;   // b owns fd 3; a is dead
}              // destruct b only — nothing runs for a
```

Reverse order is not negotiable: a later binding may hold a pointer to an
earlier one, so the earlier one must outlive it.

Early exit — `return`, `break` — counts as reaching the end of the scope. A
value with no owner, such as a bare `File{ fd: 3 };`, is destructed at the end
of its statement. Reassignment destructs the old value at the assignment, before
the new value is moved in.

There is no last-use destruction: the drop point is the closing brace, not the
last reference.

### Static Insertion

Which destructors run is decided entirely at compile time. The compiler tracks
the move state at every program point and inserts the calls statically — the
generated code carries no "was this moved?" runtime flag. This works because
the move check rejects any program whose drop set is not statically known (see
Branches below); the rejection is precisely what buys purely static insertion.

Drops are inserted the same way in `debug` and in `release`. When a `debug`
runtime check panics (`01-types.md`), the process aborts without unwinding —
destructors do not run, resources are left to the operating system.

### Branches

A move is legal when it executes at most once on every path:

```rust
if cond {
  let a = b;      // ✅ this path moves b exactly once
}

if cond {
  let a = b;      // ✅ each path moves b once —
} else {
  let c = b;      //    never both
}

for cond {
  let a = b;      // ❌ the second iteration would move a dead b
}
```

After a move inside a branch, the source is unusable even on the path where the
branch did not run — the compiler cannot prove which path was taken, so it
rejects what it cannot prove:

```rust
if cond {
  let a = b;
}
let d = b;   // ❌ on the cond path, b is dead
```

A condition is evaluated before either branch, so a move in the condition is
visible to both.

## Borrow

Slicing an array borrows it and does not move it:

```rust
let a = [3]u32{1, 2, 3};
let s: []u32 = a[..];   // ✅ a still owns the elements

assert(a[0] == 1);
```

## No defer

Some languages add a scoped exit hook — `defer` — for cleanup a destructor
does not cover. xyz does not carry one, on three counts:

- **The error path cannot use it.** A panic aborts without unwinding (Panic,
  `01-types.md`), so a `defer` would not run there — the one place a hook
  sounds most valuable is exactly where it is a no-op, and `errdefer` with it.
- **The normal path already has it.** Destructors are inserted statically on
  the paths the compiler can see (`Timing` above); early `return` included.
- **What is left is FFI resources**, and those are the wrapper's business:
  a `File` whose `fd` came from C closes in its `Drop`, written by hand in
  xyz, and the compiler never sees a C resource it could hook anyway.

The word `defer` stays reserved (`15-grammar.md`) — the decision is v0's,
not the language's forever.
