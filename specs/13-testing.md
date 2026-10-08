# Testing

This chapter continues `12-projects.md`. It defines the test artifact, the
functions that go into it, how the runner reports their outcome, and the
one word that builds and runs it.

`#[test]` (`01-types.md`) marks a test function. A test may return `()`, or
`E?()` for an error type of its choosing — the same two shapes `main` has, for
the same reason: `?` in a test hands the error straight to the runner.

The test artifact is a second product a project can build, beside the library
or the executable: the compiler collects every `#[test]` function — from every
namespace, `pub` or not — into a table the generated runner walks:

```rust
// the compiler generates this table and a runner over it
struct TestCase {
  name: []u8,          // the function's name
  desc: []u8,          // #[test("...")] — empty when absent
  run:  fn() -> (),
}
```

Beside is the whole word: the library and the executable carry none
of a test's code. A `#[test]` fn is the artifact's own, and a name
that reaches for one outside a test build is refused — the call and
the value both, at the place the name is read: nothing is left for a
linker to say. The tree holds the fn whole in every build — its
shape checked, its body read — for the difference is the product's,
not the checker's.

The runner is the entry point of the artifact, the way the shim is for `main`:
a project's own `fn main` is not involved and need not exist. A test that
panics counts as failed — the report says so and the runner moves on — and a
test returning `Err` is failed the same way: the runner prints the error
(through the same reflection printing `main`'s `Err` uses) and continues. The
artifact exits 0 when every test passed, 1 otherwise.

`#[cfg]`'s mode words apply as usual — a `#[cfg(debug)]` helper compiled
out in `release` is absent from a test build too, which is built in `debug`
shape. There is no `test` build mode: the artifact is its own product, and
the debug/release axis is not what distinguishes it. A `#[test]` fn itself
is never mode-gated: a mode word on one is a compile error, for the
artifact is the debug shape, the one mode a runner knows.

## The word a user says

`cerium test [dir]` builds the artifact and runs it: the report on stdout,
the exit code through — the compiler's own hand on the product it built,
the first word that runs one. `-c` and `-x` keep the older shape, the
artifact left for the shell; `test` is the one word that closes the
distance, and the only one.

A dir is a project's, read as `-x` reads it. None is the empty project:
the sysroot alone, no file of a project's own, the library's rows the
whole artifact — the door an install owns without a project to point at
(`12-projects.md`, std). The artifact the word builds is a passing file:
made aside, run, and gone — nothing of it touches the project's tree.
