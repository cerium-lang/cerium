# Match

This chapter continues `01-types.md`, which defines `enum`. Pattern matching
is the language's one control-flow construct that destructures a value. Loops
and `return` come in `10-iteration.md`.

`match` tests a value against a list of patterns and evaluates the arm of the
first pattern that fits. It is an expression: the arm's value is the match's
value, and every arm must have the same type — except an arm ending in `return`,
which produces no value at all (`10-iteration.md`).

```rust
match o {
  None => 0,
  Some(v) => v,
}
```

## Patterns

A pattern names a variant by its short name — the scrutinee's type picks out
the enum — and binds the variant's payload. An enum is also a namespace: its
variants live inside it (`11-namespaces.md`).

### No payload

```rust
match o {
  None => 0,
}
```

### Positional payload

A variant declared as `Some(T)` binds positionally:

```rust
match o {
  Some(v) => v,   // v: the payload
}
```

### Named payload

A variant declared with a field list — `Int { bits: u16, signed: bool }` — is
destructured by field name, mirroring the declaration:

```rust
match @typeinfo<T>() {
  Int { bits, signed } => bits,   // bits: u16, signed: bool
}
```

The field name is the binding name — there is no renaming, so `Int { bits: b }`
is not defined, and a pattern reads exactly like the declaration it mirrors.
Fields left out are ignored; `{ .. }`
ignores every field, and naming some plus `..` ignores the rest:

```rust
match @typeinfo<T>() {
  Int    { .. }         => "int",    // ignore both
  Struct { fields, .. } => fields,   // bind fields only
}
```

### Wildcard

`_` matches anything and binds nothing:

```rust
match @typeinfo<T>() {
  Int { .. } => "int",
  _ => "other",
}
```

### Or-patterns

`|` joins alternatives that share one arm, and the arm covers whichever of them
fits:

```rust
match @typeinfo<T>() {
  Int { .. } | Float { .. } => "number",
  Struct { fields, .. }     => "struct",
  _                         => "other",
}
```

Every alternative has to bind the same names with the same types — `Some(x) |
Other(x)` is fine, `Some(x) | Other(y)` is not, because the body has one spelling
to refer to them by.

### Tuple

A tuple is destructured element by element:

```rust
match p {
  (x, y) => x + y,     // x: the first element, y: the second
}
```

`_` skips an element — `(x, _, y)`. The scrutinee's type fixes the arity, so a
tuple pattern always fits and needs no wildcard arm of its own.

A pattern also appears wherever a value is bound — `let`, `for x in c`, and
`for let` (`10-iteration.md`) — not only in `match`.

### No guards, no ranges, no literal patterns

A pattern destructures; it does not test. There are no guards
(`Some(v) if v > 0`), no ranges (`1..=5`), and no literal patterns — `0`, `1`,
`true` — so an integer or a `bool` is not a scrutinee. Testing is what `if` is
for, and `if` is an expression (`10-iteration.md`):

```rust
let sign = if n > 0 { 1 } else if n < 0 { -1 } else { 0 };

match o {
  Some(v) => if v > 0 { "positive" } else { "not" },
  None    => "none",
}
```

A value that is really a set of cases — an opcode, a state, an error code — is
modelled as an enum with a tag type and explicit discriminants (`01-types.md`),
which `match` covers exhaustively. That is strictly better than matching the
integer: an integer can never be covered, so it would always need `_`.

Keeping testing out of patterns is what keeps exhaustiveness simple: an arm
covers its pattern unconditionally, so covering every variant — or using `_` —
is the whole rule.

## Exhaustiveness

A `match` over an enum must cover every variant, by name or by `_`. A match
that can fall through is a compile error; adding a variant to an enum therefore
breaks every existing match that does not cover it, and the compiler lists
each one.

```rust
match o {
  None => 0,
  // ❌ Some not covered — compile error
}
```

## Bindings

A binding takes a pattern on its left, not only a name:

```rust
let x = v;           // a name is a pattern that matches anything
let (a, b) = t;      // t is a tuple — a and b are its elements
let { a, b } = s;    // s is a struct — a and b are its fields
let _ = v;           // matches, binds nothing
```

An identifier is the simplest pattern: it matches anything and binds the whole
value, so `let x = v;` needs no rule of its own. `_` discards — `let (a, _) = t;`
keeps only the first element. Since a pattern binds under the field's own name, a
binding that wants another name does not destructure — `let x = s.a;`.

A binding pattern must be **irrefutable** — it always fits. The type fixes a
tuple's arity and a struct's field names, so both are checked at compile time:
a wrong arity or an unknown field name is an error. `for let`
(`10-iteration.md`) is the refutable counterpart, where the pattern may fail and
failure ends the loop.

## Blocks

An arm with several statements is a block; its last expression is its value:

```rust
let first = match @typeinfo<T>() {
  Struct { fields, .. } => {
    let f = fields[0];
    f.name
  }
  _ => "",
};
```

## Compile-time execution

`match` is an ordinary expression, so it is compile-time callable exactly when the
scrutinee is compile-time known — and `@typeinfo<T>()` is the prime scrutinee.
