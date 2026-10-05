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

The compiler arranges the platform's entry — an injected `#[extern(C)] fn main`
that calls this one — and `main` itself mangles like any other name, a root fn
the project's own first segment spells. How the program ends follows from how
`main` ends:

| `main` ends | the program |
| --- | --- |
| returns `()`, or `Ok` | exits with code 0 |
| returns an integer, `i32` the usual one | exits with that code |
| returns `Err(e)` | prints `e` and a newline to stderr, exits with code 1 — a clean exit, not an abort: the error path is a normal one, and the state is trusted |
| panics | aborts (`01-types.md`, Panic) |

These three are the whole contract: a `main` returning anything else is
rejected where it is declared.

Printing an `Err` wants the error type's own words: `E` must implement `Fmt`
(`std::fmt`), checked where `main` declares its ending. The built-ins' own
rows give an integer its digits, a str its text, a float the arithmetic's own
words in `%.15g`'s shape, and `std::io::Error` its errno — so the plain
shapes work unadorned, and a program's own say themselves.

## std, the project's first files

The standard library is a directory the compiler reads like the project's
first files — the same walk, the same declares, the same resolves, the same
body checks, ahead of the project's own. Where the directory is, three
answers, first match wins: the `CERIUM_SYSROOT` environment variable names it;
`std` beside the compiler's own executable is the usual install shape, a
checkout's too; `std` in the working directory is the last resort. Nothing
is embedded: std is source on disk, every compile reads it, and a project
cannot turn it off. What it holds today is small — the language's citizens
(`std::option.ce` and `std::result.ce`: `Option` and `Result`, what `?T`
and `E?T` read by pointer, `01-types.md` and `03-move.md`; `std::panic.ce`:
the one fn every runtime check fails into, `01-types.md`, Panic),
`std::meta`, the reflection model the checker itself reads against
(`08-reflection.md`), and `std::ops` — the operator family, the sugar's
own traits (`07-operators.md`) with `Copy` and `Drop` beside them, the two
the compiler calls on its own, at a move and at a scope's end
(`03-move.md`) — each file in the namespace its path names, every item
`pub`, reached by path or by use like any namespace's (`11-namespaces.md`).

The citizens live in std as source, and the names the language once
held for them are free: a project may declare its own `Option`, its own
`Copy` — the sugar does not follow the name. `?T` is std's `Option<T>`
wherever it is spelled (`01-types.md`), and the exclusion checks read
`std::ops`'s `Copy` and `Drop` (`03-move.md`) — the pointer, not the
name.

## The prelude

std's flat face — every `pub` item directly in `std`, the citizens
`Option` and `Result` and `panic` among them — is bound into every file
without a `use` written: the prelude, an injected glob, one per file.
std's own files read it like anyone's — the declares-all pass has made
the face whole before any read — and the injected names yield to
everything: a declaration of the file's namespace, a declaration of the
root, a name an explicit `use` brought in — each wins by being read first,
the injected name never an error and never a shadow. `std::ops` is not in
the face: a glob is not recursive (`11-namespaces.md`), and `Copy` and
`Drop` ride no prelude — a file that impls one names it by `use`, a copy
a privilege a type opts into, the use the file's own word that it does.
`std::meta` is not in the face either — the reflection model is opted
into by its own `use` — and the sugar's variants
(`Some`, `None`, `Ok`, `Err`) ride the sugar's owner lookup, not a binding
(`01-types.md`).

`std` is the library's own name, reserved: a project whose tree carries a
`std/` directory is rejected — the namespace is the library's, and no
project may write into it (`11-namespaces.md`) — and a project whose own
directory is named `std` is refused with it, the first segment of every
symbol being the library's.

The compiler and its sysroot move together — the checker reads
`std::meta` to check, so the two cannot drift apart — and a sysroot
without `meta::TypeInfo` is a broken one, said at the first compile,
not at the first `@typeinfo`.

## Symbols

Every declaration the emitter writes carries a mangled name: the `ceri`
prefix, then each name a count of its bytes, then the bytes — the
project's own name first, the namespace path, the declaration's. A
segment's bytes never begin with a digit — the identifier's own law —
so the count's digits end exactly where the name begins and the split
is the string's own: a namespace named `my_app` and a `my` holding an
`app` never fold the same, and no `_` the alphabet holds is asked to
say a boundary the bytes themselves do not.

The project's name is its directory's — a single file's parent too,
for a file is a project of one and the directory it stands in the
project it grows into, the symbols steady across the growth. However
the path spells that directory — a bare file and a `.` the shell's
own, a `..` or a link standing somewhere else — it is resolved to the
directory itself: one directory names the project one way. A user
project's files stand in the anonymous root, so the name says what the
root cannot; std's stand in the `std` namespace, which is its project's
root, and the path walks from there: `std::fmt::print_one` spells
`ceri3std3fmt9print_one`, a root `fn fail` in a project named `web` spells
`ceri3web4fail`, and `my::app::parse` and `my_app::parse` — the pair
the underscore's two meanings once folded together — spell
`ceri3web2my3app5parse` and `ceri3web6my_app5parse`.

Types ride the same law in a closed code: the primitives their own
words (`i32` stays `i32`; no word begins another), the composites a tag
letter and their parts — `p`/`P` a pointer and its writable slot,
`s`/`S` a slice, `a`/`A` an array and its count, `t` a tuple and its
rows, `n` a named type's whole path from the root and its bindings,
`d`/`D` a dyn, `f` a fn's own signature, `u` a generic parameter
keeping its name where a declared shape is spelled. An overload says
its signature after its name — the argument count, each argument, the
return; an instantiation says its binding after the fn's own mangle,
and the numbers a const parameter bakes are fixed-width hex, for a
length never names a value: decimal lengths beside decimal digits read
two ways. A method says its target type's code, the trait's beside it
for a trait impl, then its own name — so `impl Tag for i32` and
`impl Tag for i64` are symbols apart by the words themselves, and a
generic type's methods carry the type, not the count of their arrival.

The whole is a code no two different spellings fold onto: no twin a
declaration order numbered off, no instance named by when it was first
seen. `#[extern(C)]` keeps the C name and nothing else does; `main` meets the
mangler like any name, the platform's door the wrapper the compiler arranges
(`01-types.md`, and above). Two compilations
of the same project say the same symbols — a library the linker can
meet; two projects say them apart, the project's name each one's first
segment.

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
  of the same library, waits for a manifest to exist. The symbols are ready
  for the day: a project's name is every one of its first segment, and what
  a named type's code still lacks — the project's own segment beside the
  path, for a type of one project named inside another's — is one segment,
  added then, the law already written.
