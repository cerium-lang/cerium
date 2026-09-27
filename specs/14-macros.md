# Macros

This chapter continues `08-reflection.md`. It records why xyz has, **so far**, no
user-defined macro system, and why no built-in `#` construct remains. The
conclusion is not settled — see If this changes below.

## No macro system

A macro earns its place in most languages by generating code per type. Generics
and reflection make that unnecessary here, so xyz has none.

### One impl instead of one per type

Where another language writes a derive macro, xyz writes a single generic impl
and asks the type what it is:

```rust
impl<T> Serialize for T {
  fn write(self: *Self, out: *mut Writer) -> () {
    match @typeinfo<T>() {
      Struct { fields, .. } => {
        const for f in fields {
          write_field(out, @field(*self, f.name));
        }
      }
      Enum { .. }   => { /* tag, then payload */ }
      Int  { .. }   => { /* the bytes */ }
      _ => @compileError("cannot serialize"),
    }
  }
}
```

One impl covers every type, and its branches give each type its own code —
which is what per-type generation was for. Nothing is generated, and nothing is
instantiated once per type.

### Specialization instead of generation

A type needing a different implementation gets its own impl; specificity
(`04-generics.md`) chooses it:

```rust
impl<T> Serialize for T { /* general, through reflection */ }
impl Serialize for []u8 { /* fast path */ }
```

### Named access instead of generated methods

Generating `get_x` / `get_y` per field would need a macro. Reflection reaches a
field by name instead:

```rust
*@field(p, "x") = 42;
```

The shape differs from `p.get_x()`, but it costs no machinery and works for
types written long after the accessor would have been.

## What remains built in

Nothing. The one `#` construct this chapter once carried — `#assert`, a
compile-time assertion — is deferred: v0 ships the runtime `assert` as a
`std::debug` function (`01-types.md`) and no `#` syntax at all. Should a
compile-time `assert` ever be introduced, it returns here.

Layout changed from construct to attribute: `#repr` became `#[packed]` and
`#[align(N)]` (`02-layout.md`). Attributes are not macros — they are fixed
markers on declarations, read by the compiler or by reflection
(`01-types.md`). Like builtins, they are fixed in number and defined by the
language; as it stands there is no way to add one, and no `derive` — a
marker trait such as `Copy` is implemented by hand (`05-traits.md`).

## If this changes

A macro system would only earn its place for something reflection cannot
express — new syntax, which is what a DSL needs. That is out of scope here.
Should it ever be wanted, the first question is whether the language wants
new syntax at all.
