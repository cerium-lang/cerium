# Layout

This chapter continues `01-types.md`. All sizes and offsets are in **bytes**.

## Alignment and Size

For every type `T`, `@sizeof(T)` is a multiple of `@alignof(T)`, and a field of
type `T` is placed at an offset that is a multiple of `@alignof(T)`.

| type                       | `@sizeof`         | `@alignof`             |
| -------------------------- | ----------------- | ---------------------- |
| `i8`..`i128`, `u8`..`u128` | 1, 2, 4, 8, 16    | same as `@sizeof`      |
| `isize`, `usize`           | one machine word  | machine word alignment |
| `f32`, `f64`               | 4, 8              | same as `@sizeof`      |
| `bool`                     | 1                 | 1                      |
| `*T`, `?*T`, `voidptr`     | one machine word  | machine word alignment |
| `[]T`                      | two machine words | machine word alignment |
| `dyn A`, `dyn mut A`       | two machine words | machine word alignment |
| `[N]T`                     | `N * @sizeof(T)`  | `@alignof(T)`          |
| `enum X(U)`                | `@sizeof(U)`      | `@alignof(U)`          |

`?T` is `Option<T>`: when `T` has a niche, `?T` reuses it and keeps the size of
`T`; otherwise it grows by a tag and rounds up to `@alignof(T)`. The same holds
for `E?T`, which is `Result<T, E>`: whichever of the two has an unused bit
pattern is where the other case is recorded (`01-types.md`).

An enum carrying a payload is laid out as its tag followed by a union of the
payloads: `@sizeof` is the tag plus the largest payload, rounded up, and
`@alignof` is the largest alignment among the tag and the payloads. An enum with
no payload stays exactly `@sizeof(U)` (`01-types.md`).

`bool` only ever holds the bit patterns `0` and `1`. Any other pattern can only
appear through a union or a pointer, and reading it is unspecified.

## Struct

Fields are laid out in **declaration order** — the compiler never reorders them.
Each field is placed at the next offset that satisfies its alignment; the gaps in
between are padding. `@alignof` of a struct is the maximum alignment of its
fields, and `@sizeof` is the total size rounded up to that alignment. The
trailing padding is what keeps the elements of `[N]T` aligned without padding
between them.

```rust
struct S {
  a: u8,
  b: u32,
  c: u16,
}

assert(@offset<S>("a") == 0);
assert(@offset<S>("b") == 4);   // bytes 1..3 are padding
assert(@offset<S>("c") == 8);
assert(@sizeof(S) == 12);       // bytes 10..11 are trailing padding
assert(@alignof(S) == 4);
```

```text
| 0 | 1..3    | 4..7    | 8..9 | 10..11  |
| a | padding |    b    |  c   | padding |
```

## Union

Every field starts at offset 0. `@alignof` is the maximum alignment of the
fields, and `@sizeof` is the largest field size rounded up to that alignment.

```rust
union X {
  a: u16,
  b: u32,
}

assert(@offset<X>("a") == 0);
assert(@offset<X>("b") == 0);
assert(@sizeof(X) == 4);
assert(@alignof(X) == 4);
```

## Zero-Sized Types

An empty struct or union has `@sizeof == 0` and `@alignof == 1`.

```rust
struct E {}

assert(@sizeof(E) == 0);
assert(@alignof(E) == 1);
assert(@sizeof([3]E) == 0);
assert(@sizeof(()) == 0);
```

A zero-sized field takes no space; its offset is the running offset, so several
zero-sized fields may share one offset. Distinct addresses are not guaranteed.

## Layout attributes

`#[packed]` and `#[align(N)]` are attributes (`01-types.md`) that change the
layout of the declaration they mark.

`#[packed]` drops all padding and sets the alignment to 1. A field of a packed
struct may therefore be unaligned, and taking its address is a compile error —
an unaligned `*T` cannot be represented:

```rust
#[packed]
struct P {
  a: u8,
  b: u32,
}

assert(@offset<P>("b") == 1);
assert(@sizeof(P) == 5);
assert(@alignof(P) == 1);

let p = P{ a: 1, b: 2 };

let q: *u32 = &p.b;   // ❌ cannot take the address of a packed field
let x = p.b;          // ✅ reading a packed field is fine
```

`align(N)` raises the alignment of the whole type, and `@sizeof` rounds up to
it:

```rust
#[align(16)]
struct C {
  a: u8,
}

assert(@alignof(C) == 16);
assert(@sizeof(C) == 16);
```

`#[packed]` and `#[align(N)]` contradict each other, so a declaration carrying
both — one bracket or two — is a compile error.

## Endianness

Offsets and sizes do not depend on endianness. Only the byte order inside a
multi-byte field follows the target.
