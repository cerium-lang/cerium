# Cerium

Syntax highlighting for [Cerium](https://github.com/cerium-lang/cerium), a
small systems language with Rust's shape. Every `.ce` file — the compiler's
own `std`, its tests, yours — gets colored.

## What it colors

- **Comments** — `//` to the end of the line, and `/* */` blocks that nest,
  the way the language's lexer reads them.
- **Strings** — `"…"`, plus the three prefixed forms: `c"…"` for a
  NUL-terminated C string, `r"…"` for a raw one with no escapes, `cr"…"` for
  both. Multiline `"""…"""` included, with its prefixes. Escapes
  (`\n \t \r \0 \' \" \\ \xNN`) are picked out; an escape the language does
  not have is marked.
- **Byte literals** — `'x'`, `'\n'`.
- **Numbers** — decimal, `0x` / `0o` / `0b`, underscores between digits,
  floats with an exponent. A tuple index (`t.1.0`) reads as two integers,
  not as a fraction.
- **Attributes** — `#[extern(C)]`, `#[align(16)]`, `#[packed]`, `#[noreturn]`…
- **Built-ins** — `@sizeof`, `@alignof`, `@offset`, `@cast`, `@typeinfo`,
  `@typeof`, `@field`, `@count`, `@take`, `@compileError`, `@slice`, and the
  `$$` splice.
- **Types** — the primitives (`i8`…`i128`, `u8`…`u128`, `isize`, `usize`,
  `f32`, `f64`, `bool`, `voidptr`) in their own color, and a capital
  identifier as a type or a variant.

Also: bracket matching, `//` and `/* */` toggling, and indentation that
follows the braces.

## Installing from source

```sh
code --install-extension cerium-0.0.1.vsix
```

or, to run it out of the checkout:

```sh
code --extensionDevelopmentPath=editors/vscode <your project>
```

## Known limits

Highlighting only. No language server yet — no completion, no diagnostics,
no go-to-definition. The grammar is a TextMate one, so it reads a line at a
time and trusts the shapes above rather than the parser.

## License

MIT.
