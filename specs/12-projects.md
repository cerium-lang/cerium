# Projects

This chapter continues `11-namespaces.md`. It defines what the compiler takes
a project to be: one compilation unit, one artifact, and the name that tells
an executable from a library.

## Entry and artifacts

The compilation unit is the whole project. The compiler reads the tree from the
root and sees every declaration at once, which is what two rules elsewhere
already assume: an instantiation is emitted once globally no matter how many
namespaces call it, and an impl is checked at declaration against every existing
impl (`04-generics.md`). Separate compilation would break both, so v0 does not
have it; incremental builds are a matter of caching, not of the language, and
are left to the future.

A project builds one artifact. If the root namespace declares `fn main`, the
artifact is an executable; if it does not, it is a library — something another
project depends on, and nothing more. A project is one or the other, never
both, and there is no manifest and no target list.

`main` is a name, not a keyword and not an attribute: the compiler looks for
`fn main` in the root namespace, in any file of it. It may return `()`, or it
may return an integer — `i32` the usual one — and the program exits with that
code, or it may return `E?()` for an error type of the program's choosing —
the sugar that makes `?` usable in `main` itself:

```rust
// src/main.ce — the root namespace
fn main() -> io::Error?() {
  let f = open(config()?)?;    // ? hands errors back, main is the last stop
  ...
}
```

The compiler arranges the platform's entry — an unmangled C `main` that calls
this one — so the name never meets the mangler. How the program ends follows
from how `main` ends:

| `main` ends | the program |
| --- | --- |
| returns `()`, or `Ok` | exits with code 0 |
| returns an integer, `i32` the usual one | exits with that code |
| returns `Err(e)` | prints `e`, exits with code 1 — a clean exit, not an abort: the error path is a normal one, and the state is trusted |
| panics | aborts (`01-types.md`, Panic) |

These three are the whole contract: a `main` returning anything else is
rejected where it is declared.

Printing an `Err` needs no trait: the runtime prints through reflection — the
variant name for an enum, field by field for a struct — so any error type works
without a derive.

## std, the project's first files

The standard library is a directory the compiler reads like the project's
first files — the same walk, the same declares, the same resolves, the same
body checks, ahead of the project's own. Where the directory is, three
answers, first match wins: the `CERIUM_SYSROOT` environment variable names it;
`std` beside the compiler's own executable is the usual install shape, a
checkout's too; `std` in the working directory is the last resort. Nothing
is embedded: std is source on disk, every compile reads it, and a project
cannot turn it off. What it holds today is small — the sugar's four
(`std::option.ce`, `std::result.ce`, `std::copy.ce`, `std::drop.ce`:
`Option`, `Result`, `Copy`, `Drop`, what `?T` and the exclusion checks read
by pointer, `01-types.md` and `03-move.md`), `std::meta`, the reflection
model the checker itself reads against (`08-reflection.md`), and
`std::panic`, the one fn every runtime check fails into (`01-types.md`,
Panic) — each file in the namespace its path names, every item `pub`,
reached by path or by use like any namespace's (`11-namespaces.md`).

The sugar's four live in std as source, and the names the language once
held for them are free: a project may declare its own `Option`, its own
`Copy` — the sugar does not follow the name. `?T` is std's `Option<T>`
wherever it is spelled (`01-types.md`), and the exclusion checks read
std's `Copy` and `Drop` (`03-move.md`) — the pointer, not the name.

## The prelude

std's flat face — every `pub` item directly in `std`, the sugar's four and
`panic` among them — is bound into every user file without a `use` written:
the prelude, an injected glob, one per file. The injected names yield to
everything: a declaration of the file's namespace, a declaration of the
root, a name an explicit `use` brought in — each wins by being read first,
the injected name never an error and never a shadow. `std::meta` is not in
the face — a glob is not recursive (`11-namespaces.md`), and the reflection
model is opted into by its own `use` — and the sugar's variants
(`Some`, `None`, `Ok`, `Err`) ride the sugar's owner lookup, not a binding
(`01-types.md`).

`std` is the library's own name, reserved: a project whose tree carries a
`std/` directory is rejected — the namespace is the library's, and no
project may write into it (`11-namespaces.md`).

The compiler and its sysroot move together — the checker reads
`std::meta` to check, so the two cannot drift apart — and a sysroot
without `meta::TypeInfo` is a broken one, said at the first compile,
not at the first `@typeinfo`.

## What v0 does not carry

The language has no threads, no atomics, no `volatile`, no inline assembly,
and no memory model to run any of them under. A systems language owes that
statement, not just the silence. Concurrency opens aliasing questions the
design has deliberately left outside — `README.md`'s "Not addressed" tier —
and v0 does not reach for them: the whole axis is v1 work or later, a
language-wide decision, not a chapter's.

What the boundary looks like in practice: everything on that list is a
platform facility, and platform facilities enter through `#[extern(C)]`
(`01-types.md`) — the same door `Heap`'s implementation uses. A thread, a
lock, an atomic load, a `volatile` read, or an `asm` block is an `extern`
call into code the linker resolves; from the language's side it is a call
it cannot check, which is exactly the standing bargain of every `extern`
call. A library of such bindings can be written in Cerium itself — the
signatures are ordinary declarations — and one day will be.

## Open items

- std's walk cost. Source read from the sysroot, walked as the project's
  first files, is right while std is small. The switches that end it: std
  past a few thousand lines, or front-end time the walk makes felt. The
  answer then is a symbol cache — the parse and the checks paid once, not
  per compile.
- External libraries, and how their paths enter the root. They will distribute
  as source — the compiler has to read a library to check against it — but how
  a dependency is declared, and what happens when two want different versions
  of the same library, waits for a manifest to exist.
