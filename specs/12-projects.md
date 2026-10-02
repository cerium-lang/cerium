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
// src/main.xyz — the root namespace
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

The compiler carries the standard library's source itself, embedded, and
walks it as the project's first files — the same declares, the same
resolves, the same body checks, ahead of the project's own. std is not a
manifest and not a search path: it is simply there, every compile, and a
project cannot turn it off. What it holds today is small — `std::meta`, the
reflection model the checker itself reads against (`08-reflection.md`), and
`std::panic`, the one fn every runtime check fails into (`01-types.md`,
Panic) — each file in the namespace its path names, every item `pub`,
reached by path or by use like any namespace's (`11-namespaces.md`).

The embedding is a v0 shape, not a principle. `std::meta` will always ride
with the compiler — the checker reads it to check, so the two have to move
together — but the runtime half may outgrow the walk; the switches that say
when are the open item's below.

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
call. A library of such bindings can be written in xyz itself — the
signatures are ordinary declarations — and one day will be.

## Open items

- std's distribution. Embedded source, walked as the project's first files,
  is right while std is small. The switches that end it: std past a few
  thousand lines, front-end time the walk makes felt, or std needing to
  iterate outside the compiler's own releases. The answer then is a sysroot
  — a directory the compiler reads like a project — with a symbol cache, so
  the parse and the checks are paid once, not per compile.
- External libraries, and how their paths enter the root. They will distribute
  as source — the compiler has to read a library to check against it — but how
  a dependency is declared, and what happens when two want different versions
  of the same library, waits for a manifest to exist.
