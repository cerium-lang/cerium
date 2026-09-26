# Preliminaries

How to read this specification, and the terms every chapter uses.

## Notation

- `@xxx` is a builtin — a function the language provides, not one written in it.
  There are ten, listed in `08-reflection.md`.
- There is no `#xxx` syntax. The one built-in construct drafts carried — the
  compile-time `#assert` — is deferred (`14-macros.md`).
- `#[...]` is an attribute — a marker on a declaration, read by the compiler
  or by reflection. The list is in `01-types.md`.
- In an example, ✅ means it compiles and ❌ means it does not.
- Examples write `assert` bare; it is the `std::debug` function
  (`01-types.md`).

## Terms

- A **slot** is somewhere a value can sit: a variable, a field, an element of an
  array or tuple, or a parameter. `mut` marks the slot right after it
  (`01-types.md`).
- A **place** is an expression that names a slot — `x`, `x.f`, `a[0]`, `*p`
  (`01-types.md`).
- A value is **compile-time known** when the compiler can evaluate it before the
  program runs (`08-reflection.md`).
