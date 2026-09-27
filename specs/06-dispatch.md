# Dispatch

This chapter continues `05-traits.md`. It is the one place in the language where
a call is not resolved until the program runs.

## Dyn

A trait is not a type: it names a set of signatures, and `Self` has a meaning
only inside an `impl` (`05-traits.md`). To hold a value whose type is not known
until run time, the trait has to be turned into a type first. `dyn` does that —
for a trait `A`, `dyn A` is the type "a handle to some value that implements
`A`":

```rust
let a: dyn A = &dyn b;   // b is any value whose type implements A
```

Which trait is meant comes from the type the handle is being made for, so a
value implementing several traits is not ambiguous.

`dyn A` is a fat pointer — the address of the value plus the address of its
vtable — and is therefore two machine words, like a slice (`02-layout.md`). It is
an ordinary value type: it can be a field, an element of an array, and it is
`Copy` like any pointer (`03-move.md`).

What has been erased cannot be reached back. Through a `dyn A` only the methods
of `A` are visible — never a field of the concrete type:

```rust
a.show();   // ✅ a method of A
a.x;        // ❌ the concrete type is gone
```

### Mutability

`dyn mut A` is the handle through which the `self: *mut Self` methods of `A` are
available. `&mut dyn b` makes one, and requires `b` to be a `mut` slot
(`01-types.md`):

```rust
let m: dyn mut A = &mut dyn b;

m.reset();
```

This is the same shape as `[]T` and `[]mut T`: `mut` sits on what the handle
points at (`01-types.md`).

## Which impl runs

The impl is chosen where the handle is made, not where the method is called.
`&dyn b` resolves `A` for the type of `b` by the ordinary rules of
`04-generics.md` — the most specific matching impl wins — and puts a pointer to
it into the vtable. Calling `a.show()` reads that entry.

So the choice is still made at compile time, and still by the specialization
order; `dyn` only carries the result to a place that no longer knows the type.
Nothing in `04-generics.md` changes because of it.

A generic function is unaffected: `fn f<T: A>(x: T)` still monomorphizes, and
every call inside it is resolved statically. `dyn` is the opt-in dynamic path,
and it is visible in the type — which is the point of spelling it.

## Object safety

A vtable can only be built for a trait whose methods can all be dispatched
without knowing `Self`:

- a method taking `self: Self` by value is not allowed — its size is not known
- a generic method is not allowed — there is no single address to put in the
  table
- an associated type has to be given, so that a type such as `Self::Item` is
  not left open

```rust
let it: dyn Iterator<Item = u32> = &mut dyn iter;
```

## Layout

| type | `@sizeof` | `@alignof` |
| --- | --- | --- |
| `dyn A`, `dyn mut A` | two machine words | machine word alignment |

There is no borrow checker, so a handle that outlives what it points at is
undefined behaviour — exactly as a slice is (`01-types.md`). A `debug` build may
catch some of these; the language does not say how.

## Open items

- `dyn A + B` — a handle to a value implementing two traits.
- Whether `@typeinfo<dyn A>()` should get a variant of its own; the handle is a
  pointer today, and `Pointer { child: type, .. }` has no way to name a trait
  (`08-reflection.md`).
