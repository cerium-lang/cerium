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
may return an `i32` — the exit code is one, the platform's own word for it,
and no wider integer answers the parent's read — or it may return `E?()` for
an error type of the program's choosing — the sugar that makes `?` usable in
`main` itself:

```rust
// src/main.ce — the root namespace
fn main() -> fmt::Error?() {
  let f = open(config()?)?;    // ? hands errors back, main is the last stop
  ...
}
```

The compiler arranges the platform's door — an injected `#[extern(C)] fn
main` that hands this one to std, by address with the platform's own two,
the count and the table of words — and `main` itself mangles like any other
name, a root fn the project's own first segment spells. One entry fn per
ending: `run_unit` calls the main and answers a clean zero, `run_i32`
answers the code itself, `run_err` hands the ending to `fmt`'s `exit`, the
`Err` half printed through the error type's own words first. Each saves the
platform's two for `sys::args` (`std::sys`), the program's own words one
iterator the asking, a `[]u8` per word into the platform's own table. The
three are private to std, the wrapper alone their caller — a use or a
qualified path from a project's file is turned away (`11-namespaces.md`);
the compiler takes their Syms at its own face-taking, the private face
among them. How the program ends follows from how `main` ends:

| `main` ends | the program |
| --- | --- |
| returns `()`, or `Ok` | exits with code 0 |
| returns an `i32` | exits with that code |
| returns `Err(e)` | prints `e` and a newline to stderr, exits with code 1 — a clean exit, not an abort: the error path is a normal one, and the state is trusted |
| panics | aborts (`01-types.md`, Panic) |

These three are the whole contract: a `main` returning anything else is
rejected where it is declared.

Printing an `Err` wants the error type's own words: `E` must implement `Fmt`
(`std::fmt`), checked where `main` declares its ending. The built-ins' own
rows give an integer its digits, a str its text, a float the arithmetic's own
words in `%.15g`'s shape, and `fmt::Error` its errno — so the plain
shapes work unadorned, and a program's own say themselves.

The prints take a string of words and many values:
`print<...T: Fmt>(const fmt: []u8, args: ...T)` writes each value where the
string's `{}` stands, the pack's order the string's own, `{{` and `}}` the
braces themselves, a `}` alone the byte it is. A hole may carry its own
words between its braces, `{:...}`: a fill, an alignment, a width, a
precision, a base, in that order — `<` `^` `>` the three alignments, the fill
the byte the alignment follows (`{:*^6}`, the `*`), the width a decimal
run and a floor: a longer value left whole, never cut; the precision a
dot and the decimal run behind it (`{:.3}`): a float held to the
fraction digits it asks — the dot always, the window zero-filled, the
rounding's carry the next integer — and a str cut to the bytes it
asks; the other values take no notice, and `.0` spells no asking at
all, the width's own zero the same way; the base one of `x` `o` `b`,
the spec's last word — the integers' asking alone: their digits in
it, lowercase (`{:x}` of 255, `ff`), a negative its magnitude and
its sign (`-ff`), a two's complement never what an asking means,
the floats and the strs taking no notice; the `x` an alignment
follows is a fill, the `x` a `}` follows the base. Where the spec
names no alignment, the type's own answer stands — the integers,
the floats and the pointers right, everything else left. The
string is const, and the compiler reads it whole: the holes
counted against the pack's rows, a call that disagrees refused
(`@count(...T)`, `04-generics.md`); a `{` that opens neither a
hole, a brace, nor a well-formed spec refused with it — a format
error cannot reach a running program, the grammar itself a const
fn the instance's re-check runs (`08-reflection.md`). The numbered
holes, `{0}` and its kin, are later milestones': the order is the
only binder today. `eprint` is the same
walk to the stderr door. The walk is the pack's own peel, one unfolding a
value, the pack's depth cap its own (`04-generics.md`), and the count handed
back is the whole print's, however many writes it took.

## Conditions

A declaration may name the platforms it lives on — `#[cfg(linux)] fn
reboot() { ... }` exists only where the word holds, and a compile
anywhere else never sees it: not its name, not its uses, not its
impls, not its bodies. The cull is the first thing the checker does,
before a single name is declared, so a culled item is not hidden but
absent — a use that names it is the unknown name any absent thing is.

Three dimensions hold the words: the system — `linux` or `darwin`,
the libc the platform carries — and the machine — `amd64` or `arm64`,
the qbe backend that answers it. `arm64` is the one word for both
Linux's aarch64 and Apple's arm64_apple: the IL above them has no
stake in the calling conventions that part them.

The third is the build's own word: the mode — `debug` or `release`,
`-r`'s say (`01-types.md`, Build Modes). On every item but a fn a mode
word is the platform words' own cull: the declaration is absent in
the modes it does not name, and a use that names it is the unknown
name any absent thing is — a debug shape and a release shape are two
declarations of one name, each compile reading exactly one. On a fn
it is the other shape: the fn is kept, and in the modes it does not
name every call to it is removed whole — the statement gone, the
arguments with it, not evaluated and not checked — the door the
body's pass owns (`01-types.md`, Mode-gated functions). A fn carries
at most one `#[cfg]` that holds a mode word: the words of two would
meet with `and` — a fn neither mode holds — while the call-site
removal reads the two doors' words as one fn every mode holds, and
the shapes do not compose.

Several words in one pair of
parentheses must all hold — `#[cfg(darwin, arm64)]` is Apple silicon,
the system and the machine each named — and several `#[cfg]`s on one
declaration meet the same way. There is no negation: a library lists
the platforms it supports, not the ones it does not — "not this one"
is every other platform written out — and two words from one dimension
in one pair of parentheses is an error, the hand that meant
`#[cfg(linux, amd64)]` worth stopping rather than meeting to a quiet
false. The same dimension across several `#[cfg]`s is the same
error, and for the same hand: the attributes meet with `and` too,
and the words of one dimension never hold together. A word the dimensions do not know is an error too, the same
guard a typo wants; an uname the tables cannot name at all keeps every
word false — the honest answer for a platform the compiler was never
told about.

The platform the words read is the one the compiler itself runs on:
host and target are the same machine, a cross compile its own
milestone. std is the first customer — `errno`, the word a failed call
sets, lives behind `__errno_location` where glibc and musl put it and
behind `__error` where Darwin's libSystem does, two `#[cfg]`d
declarations of the one `pub fn`, each compile reading exactly one.

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
(`08-reflection.md`), `std::ops` — the operator family, the sugar's
own traits (`07-operators.md`) with `Copy` and `Drop` beside them, the two
the compiler calls on its own, at a move and at a scope's end
(`03-move.md`) — `std::fmt`, the words and the doors one house
now (`print` the string of words, the two doors and the sink's
contract beside), `std::sys`, the
platform's own calls, the arguments among them (`args` the iterator the
door fills), and `std::iter`, the `Iter` family itself
(`10-iteration.md`) — each file in the namespace its path names, every
item `pub`, reached by path or by use like any namespace's
(`11-namespaces.md`). In the root beside the citizens live two more
kinds: `exit`, the ending an `E?()` main has answered as the platform
takes it — pub like any citizen, run_err's own arm, a program free to
call it itself — and the one exception: the entry fns, `run_unit` and
`run_i32` and `run_err`, private to std — the wrapper the compiler
arranges is their one caller, said above.

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

A const's or a static's value rides a data symbol of its own: the
kind, the namespace's path in dots, then the name — a root's
`$static.N`, std's `$static.std.sys.argc`. An identifier holds
no dot, so the split is the string's own; and the path is there for
the fn's own reason — two namespaces' same-named slots are two
values, and no two spellings may fold onto one symbol.

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
