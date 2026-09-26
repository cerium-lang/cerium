# Reflection

This chapter continues `07-operators.md`.

This chapter defines the model, `std::meta::TypeInfo`, and the query builtins.

## Compile-time execution

Compile-time execution is the ordinary language evaluated by the compiler.

Any expression whose inputs are compile-time known is evaluated at compile
time, with ordinary semantics. Two restrictions apply: inputs must be
compile-time known, and evaluation must have no effects — it only produces
a value.

Two criteria decide, and nothing else: every input is compile-time known, and
the evaluation has no effects — it produces a value and does nothing else.
There is no separate compile-time language: no construct is compile-time-only,
and none is banned from compile time by name. What the criteria rule out:

| cannot run at compile time | blocked by |
| --- | --- |
| a call whose argument is a runtime value | inputs not known |
| `alloc`, and anything that allocates | an allocation is an effect, and a block's address is not a value the compiler can keep |
| a call through `#[extern(C)]` | the linker is not part of evaluation |
| `panic` | `@compileError` is how compile-time code reports; panic belongs to the running program |

```rust
let a = [3]u32{1, 2, 3};
let sum = a[0] + a[1] + a[2];

assert(sum == 6);
```

User functions are callable at compile time when their arguments are
compile-time known. The function itself needs no annotation:

```rust
fn twice(x: u32) -> u32 {
  x + x
}

let n = io::read_u32();          // runtime
let four = twice(twice(n));      // runtime call — the argument is not known

assert(twice(21) == 42);        // the argument is known, so it evaluates at compile time
```

Functions with control flow become compile-time callable once that control flow
is defined — `match` (`09-match.md`) and the loops (`10-iteration.md`).

### What the spec promises

The evaluator is not specified beyond four promises:

1. *Determinism.* Same inputs, same value — evaluation observes nothing, so it
   cannot vary.
2. *No `Drop`.* Destructors are inserted into the running program's control
   flow (`03-move.md`); compile-time evaluation has no such flow, and runs
   none. A move at compile time produces the value; nothing is destructed.
3. *Overflow is an error.* Not a wrap: the compile-time checks of
   `01-types.md` apply to every step of evaluation, not only to literals.
4. *An end.* Evaluation must terminate: the implementation sets a step limit,
   and exceeding it is a compile error — never a hang. How many steps is up
   to the implementation.

### Evaluation budget

Compile time is not an infinite resource. Two counters guard it: the 256
unfoldings of generic recursion (`04-generics.md`), and the evaluator's step
limit above. They are different mechanisms — one counts template expansion,
one counts executed steps — and both end in a compile error, not a hang.

## Const parameters

Implicit evaluation covers the case where a value happens to be known; a
`const` parameter states the requirement — that an argument *must* be
compile-time known. It is the language's one explicit const marker, and it
appears in three positions — on a parameter, on an `if`, and on a `for`:

| where | requires | effect |
| --- | --- | --- |
| `const x: T` in a parameter list | the argument is compile-time known | usable wherever a compile-time value is required |
| `const if cond` | the condition is compile-time known | the untaken block is discarded before type checking (`04-generics.md`) |
| `const for x in xs` | the iterated value is compile-time known | the loop is unrolled; `x` is compile-time known (`10-iteration.md`) |

A parameter list means any of them — a function, an `impl`, a `struct`, or a
trait. On a type parameter list it marks a *value* parameter: one that has to be
a compile-time value rather than a type, which is what an array length is:

```rust
impl<T, const N: usize> IntoIterator for [N]T { ... }
```

An ordinary `if` or `for` whose input happens to be compile-time known is
evaluated at compile time too; the `const` form is what turns that into a
requirement.

`const` is part of the signature: `fn f(x: u32)` and `fn f(const x: u32)`
are distinct overloads. Any type may be marked `const`; a type whose value
cannot exist at compile time (a `File`, say) is rejected by the no-effects
rule, not by a special const-type check.

```rust
fn field_offset<T>(const name: []u8) -> usize {
  @offset<T>(name)   // name is compile-time known here
}
```

A `const` parameter can be used anywhere a compile-time value is required:
`@field`, `@offset`, `const if`, an array length, or another `const`
argument.
At the call site the argument must be compile-time known; a runtime value is
rejected there, not inside the body:

```rust
let n = read_string();          // runtime
field_offset<Point>(n);        // ❌ n is not compile-time known
```

A `const` parameter also carries a value that is part of a type — an array
length, say:

```rust
impl<T, const N: usize> IntoIterator for [N]T { ... }   // 10-iteration.md
```

A function with a `const` parameter is a compile-time tool and cannot be used
as a value: assigning it to a function pointer, passing it as an argument, or
spelling it as a type argument are all compile errors. The `const` arguments
are baked into each instantiation, and a pointer call has nowhere to hand them
over — it would silently skip the compile-time-known check
(`let g: fn([]u8) -> usize = field_offset<Point>; g(runtime_string)`). If a
function must be callable both ways, it is two functions: the `const` one
delegates to the plain one.

```rust
fn field_offset_plain<T>(name: []u8) -> usize { @offset<T>(name) }
fn field_offset<T>(const name: []u8) -> usize { field_offset_plain<T>(name) }
```

## TypeInfo

`std::meta::TypeInfo` is a tagged union — an enum whose variants carry a
payload — that describes a type's structure. `@typeinfo` produces it. Unlike
an opaque handle it is ordinary data: it can be stored, compared, and matched
apart to learn how a type is built.

```rust
// in std::meta
enum TypeInfo {
  Bool,
  Int     { bits: u16, signed: bool },
  Float   { bits: u16 },
  Pointer { child: type, mutable: bool },                  // *T / *mut T
  Slice   { child: type, mutable: bool },                  // []T / []mut T
  Array   { len: usize, child: type, mutable: bool },       // [N]T / [N]mut T
  Struct  { fields: []Field, attrs: []Attr },
  Union   { fields: []Field, attrs: []Attr },
  Enum    { tag: type, variants: []EnumField },
  Tuple   { fields: []Field },                             // (A, B, ...) — () has none; names are empty
  Fn      { args: []FnArg, ret: type },                    // fn(A) -> R
  Optional { child: type },                                // ?T — the sugar, whatever T is
  Result   { ok: type, err: type },                        // E?T — the sugar, whatever T is
  Voidptr,                                                 // voidptr
}

struct Field {
  name: []u8,
  type: type,        // a type reference
  offset: usize,
  mutable: bool,
  attrs: []Attr,     // the attributes marked on this field
}

struct EnumField {
  name: []u8,
  value: i64,
  type: ?type,       // None when the variant carries no payload
  attrs: []Attr,     // the attributes marked on this variant
}

struct FnArg {
  name: []u8,        // always empty — an argument's name lives in the declaration, not the type
  type: type,
}

struct Attr {
  name: []u8,
  args: []AttrArg,
}

enum AttrArg {          // an argument is an identifier, a number, or a string
  Ident([]u8),          // #[build(debug)] — debug
  Int(i64),             // #[align(16)] — 16
  Str([]u8),            // a string literal
}
```

`attrs` records the `#[...]` attributes on the declaration (`01-types.md`); a
`Field` and an `EnumField` carry the ones marked on that field or variant. The
variants that describe types with no declaration of their own — `Bool`, `Int`,
`Float`, and the rest — carry no `attrs`: there is nothing to mark. Layout is
not decoded here either: `@sizeof`, `@alignof` and `@offset` answer those
questions directly, so `#[packed]` and `#[align(N)]` are recorded as names,
not as layout facts.

`fn(A, B) -> R` is a pointer at the language level (`01-types.md`) — one word,
nullable — but its reflection is its own variant, not a `Pointer`: `Fn` carries
the arguments and the return type directly. Staying out of `Pointer` is what
keeps `*fn(A, B) -> R` a plain `Pointer` whose child is an `Fn`, rather than
two nested `Pointer` layers. An argument's name and its attributes live in the
declaration, not the type — `fn twice(x: u32)` and `fn double(y: u32)` are the
same type — so an `FnArg`'s `name` is always empty: the same bargain `Tuple`
makes with `Field` above.

`?T` is sugar for `Option<T>` (`01-types.md`), and reflection says so:
`@typeinfo<?T>()` is always `Optional { child: ... }` — for `?*T` no less than
for `?u32`. How the value is laid out is a different question: a pointer,
slice or fn has a null niche to hold the empty case in, so `@sizeof(?*T)` is
one word (`01-types.md`), while `?u32` grows a tag. Reflection does not
re-tell that story — it would be the same fact twice — any more than it decodes
layout. `E?T` follows the same rule on its side: `@typeinfo<E?T>()` is always
`Result { ok: ..., err: ... }`, and whether `E?*T` reuses the niche is
`@sizeof`'s to answer. Generic code that asks "can this be empty?" matches one
variant instead of guessing at `Enum` shapes.

`@typeinfo` runs in two slots. In the type slot it describes a type; in the
value slot it describes the type of a value:

```rust
let a: TypeInfo = @typeinfo<u32>();   // Int { bits: 32, signed: false }
let b: TypeInfo = @typeinfo(42);      // Int { bits: 32, signed: true }
```

`?T` is `Option<T>`, so `None`, `Some` and `match` work on it whatever `T` is —
and so does narrowing (`01-types.md`), whether the empty case sits in a niche
or in a tag.

`()` is the unit type — there is no `void` (`01-types.md`) — so `@typeinfo<()>()`
is a `Tuple` with no fields, not a distinct `Void` variant. `voidptr` keeps its
own variant: it is a primitive, opaque and not a pointer to anything.

## Lifting and splicing

A `type` field holds a *type reference* — a value whose type is `type` and
whose value is a type. Crossing between the two domains is written explicitly,
with a pair of prefix operators:

| operator | direction | meaning                                   |
| -------- | --------- | ----------------------------------------- |
| `^^T`    | lifting   | a type becomes a type reference — a value |
| `$$x`    | splicing  | a type reference becomes a type           |

Lifting goes up, into the meta level: a type becomes data about a type.
Splicing comes back down: that data becomes a type again.

```rust
let t: type = ^^u32;                          // lifting — up into the meta level
assert(is_same<$$t, u32>::value);             // splicing — back down to a type

match @typeinfo<*u32>() {
  Pointer { child, .. } => {
    assert(is_same<$$child, u32>::value);     // child is already a reference
    let c: TypeInfo = @typeinfo<$$child>();   // splice, then expand
  }
}
```

`^^` is one token, not two `^`: `^` is xor, and `^^` appears only in prefix
position, so `a ^^ b` is a syntax error. `$$` is likewise one token.

`@typeof(a)` yields the type of a value as a reference — the one way to reach
a type from a value, since `^^` starts from a type that is already named:

```rust
let a = 42;
assert(is_same<$$@typeof(a), i32>::value);
```

The two slots of `@typeinfo` differ here, and the difference matters. In the
type slot the reference is spliced — `@typeinfo<$$child>()` describes the type
`child` names. In the value slot it is not — `@typeinfo(child)` describes
`child`'s own type, which is `type`.

A reference is lazy — it names a type without inlining that type's `TypeInfo`.
That is what keeps recursion sound: for `struct Node { next: *Node }`,
`@typeinfo<Node>()` records `next`'s type as a reference to `Node`, not as
`Node`'s `TypeInfo` inlined, which would never terminate. `type` and `TypeInfo`
are the pointer and the pointee; `@typeinfo` is the dereference.

A `type` value *is* a type, so inside reflection types are first-class at
compile time: one can be stored in a field, passed as an argument, and
expanded. The rest of the language keeps them apart — a name for a type is
declared with `type Name = ...` in type position (`01-types.md`), not bound
like an ordinary value. Reflection is the one place a type reaches into the
value domain, and it does so only at compile time — with `^^` and `$$` marking
each crossing.

### Type parameter or `type` value

A function that works on values of any type takes a type parameter; one that
inspects or computes a type takes a `type` value:

```rust
fn first<T>(xs: []T) -> T { xs[0] }      // works on values of T

fn is_pointer(t: type) -> bool {         // inspects the type itself
  match @typeinfo<$$t>() {
    Pointer { .. } => true,
    _ => false,
  }
}
```

The two overlap where a function reports a property of a type — `size_of` can
be written either way. Prefer a type parameter there: it needs no `^^` or `$$`.
A `type` value earns its keep where a type parameter cannot follow — being
stored, passed along, and returned, since a generic function cannot return a
type:

```rust
fn remove_pointer<T>() -> type {
  match @typeinfo<T>() {
    Pointer { child, .. } => child,
    _ => @compileError("T is not a pointer"),
  }
}

assert(is_same<$$remove_pointer<*u32>(), u32>::value);
```

## Field access

`@field` reads or writes a field by name. It takes a value and the field's
name as a compile-time string, and yields the field's address — `@field(v, "x")`
is `&v.x`. Writing is governed by the field's `mut`, exactly as `v.x` is:

```rust
let mut p = Point{ x: 0, y: 0 };

*@field(p, "x") = 42;                 // p.x = 42
assert(*@field(p, "x") == 42);
```

The name must be compile-time known, so the field's type and offset are
resolved during compilation. Serialization walks `@typeinfo` for the field
list and uses `@field` per field; the walk is a `const for`
(`10-iteration.md`), which is what makes each name compile-time known.
`@offset<T>("f")` stays the layout query (`02-layout.md`).

## Type equality

Type equality is a predicate struct, `is_same`, built from struct
specialization (`04-generics.md`) plus an associated constant supplied by an
inherent impl (`05-traits.md`):

```rust
// in std::meta
struct is_same<A, B> {}
struct is_same<T, T> {}

impl<A, B> is_same<A, B> { const value: bool = false; }
impl<T>    is_same<T, T> { const value: bool = true;  }

assert(is_same<i32, i32>::value);
assert(!is_same<i32, u32>::value);
```

`TypeInfo` equality is structural — two distinct types that happen to be built
the same way compare equal. `is_same` is identity: whether two names are the
same type, which structure alone cannot answer.

Predicate structs are `snake_case`, ordinary structs are `PascalCase`:
`is_same` reads as a question, `TypeInfo` as a noun.

## Builtins

`@xxx` is a builtin — a function the language provides, not one written in it.
There are ten, and each is defined where it belongs:

| builtin | what it does | defined in |
| --- | --- | --- |
| `@sizeof<T>()`, `@sizeof(a)` | size in bytes | `02-layout.md` |
| `@alignof<T>()`, `@alignof(a)` | alignment in bytes | `02-layout.md` |
| `@offset<T>("field")` | field offset in bytes | `02-layout.md` |
| `@cast<T>(a)` | a well-defined conversion | `01-types.md` |
| `@typeinfo<T>()`, `@typeinfo(a)` | the `TypeInfo` of a type | this chapter |
| `@typeof(a)` | the type of a value, as a reference | this chapter |
| `@field(v, "name")` | the address of field `name` | this chapter |
| `@count(...Ts)` | pack length | `04-generics.md` |
| `@take(p)` | move a value out of a place, leave the zero value | `03-move.md` |
| `@compileError(msg)` | report a compile error | this chapter |

Grouped by what they are for: layout — `@sizeof`, `@alignof`, `@offset`;
conversion — `@cast`; reflection — `@typeinfo`, `@typeof`, `@field`; packs —
`@count`; ownership — `@take`; const — `@compileError`. A conditional is
ordinary `if` (`10-iteration.md`), not a builtin.

What the standard library provides is not a builtin: `print` and `close` are
ordinary functions reached through an ordinary path (`11-namespaces.md`).

### Queries

All queries share one shape — a type in the type-parameter slot, data in the
value slots:

| builtin                          | returns                                  |
| -------------------------------- | ---------------------------------------- |
| `@typeinfo<T>()`, `@typeinfo(a)` | the `TypeInfo` of `T` / of `a`'s type    |
| `@typeof(a)`                     | the type of `a`, as a reference (`type`) |
| `@field(v, "name")`              | the address of field `name`, `&v.name`   |
| `@sizeof<T>()`, `@sizeof(a)`     | size in bytes                            |
| `@alignof<T>()`, `@alignof(a)`   | alignment in bytes                       |
| `@offset<T>("field")`            | field offset in bytes                    |
| `@count(...Ts)`                  | pack length                              |

## Error Reporting

`@compileError(msg)` reports a compile error from compile-time code. It is the
base for library-defined assertions and contracts:

```rust
const if @count(...Ts) > 16 {
  @compileError("too many arguments");
}
```
