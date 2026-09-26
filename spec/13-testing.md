# Testing

This chapter continues `12-projects.md`. It defines the test artifact, the
functions that go into it, and how the runner reports their outcome.

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

The runner is the entry point of the artifact, the way the shim is for `main`:
a project's own `fn main` is not involved and need not exist. A test that
panics counts as failed — the report says so and the runner moves on — and a
test returning `Err` is failed the same way: the runner prints the error
(through the same reflection printing `main`'s `Err` uses) and continues. The
artifact exits 0 when every test passed, 1 otherwise.

`#[build]` applies as usual — a `#[build(debug)]` helper compiled out in
`release` is absent from a test build too, which is built in `debug` shape.
There is no `test` build mode: the artifact is its own product, and the
debug/release axis is not what distinguishes it.
