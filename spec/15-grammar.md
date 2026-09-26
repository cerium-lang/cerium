# Grammar

This chapter states the language's grammar in EBNF, in three passes that have
all landed: the lexical grammar — the tokens; expressions and types — the
precedence levels, the three jobs of `?`, the brackets; declarations and
statements — the items, the blocks, the patterns, the shapes of `for`. The
grammar is closed: every construct the chapters define has a production here,
and every production traces back to a chapter.

## Notation

| form | meaning |
| --- | --- |
| `x = y` | `x` is defined as `y` |
| `x \| y` | one of the alternatives |
| `[ x ]` | optional — `x` or nothing |
| `{ x }` | repetition — `x` zero or more times |
| `( x \| y )` | grouping |
| `x … y` | the range from `x` to `y` |
| `"..."` | the characters themselves — a terminal |
| `'...'` | the same, when the terminal itself contains a `"` |

A production may be stated twice with `|` omitted; the alternatives accumulate.
Terminals are always quoted.

## Source text

A source file is UTF-8. A carriage return before a newline is read as part of
the newline, and a byte order mark at the very start of a file is skipped;
otherwise the compiler sees the bytes as they are. Identifiers are ASCII,
deliberately: opening up to a Unicode identifier set later is pure addition,
closing down after opening is a breaking change, and a system language's
identifiers skew English anyway — comments and string literals carry any
bytes they like.

Comments and blanks are equivalent — a comment is whitespace:

```ebnf
whitespace = blank | comment ;
blank      = " " | "\t" | newline ;
newline    = "\n" ;
comment    = "//" { any character except newline } [ newline ]
           | "/*" { comment | any character } "*/" ;
```

A `/*` comment runs to the matching `*/`, and a `/*` inside it opens another
one, which must also close — the production above is the intent; the two
characters of `*/` may not be split by anything, and `*` alone is content, so
`/* a * b */` is a whole comment. Nesting is deliberate: commenting out a
block that already contains a comment works. An unterminated comment is a
lexical error, not a run to end of file.

## Identifiers and keywords

```ebnf
identifier = letter { letter | digit | "_" } ;
letter     = "A" … "Z" | "a" … "z" ;
digit      = "0" … "9" ;
```

The keywords are:

| declarations | values | control flow |
| --- | --- | --- |
| `fn` `struct` `enum` `union` `trait` `impl` `type` `use` `pub` | `let` `const` `static` `mut` `dyn` `true` `false` | `if` `match` `for` `in` `break` `continue` `return` |

Two more are reserved, unused: `macro` and `defer`. Each names a feature the
design has considered and not carried — a user-defined macro system
(`14-macros.md`) and scoped cleanup — so the words wait for the decision
instead of being spent.

`true` and `false` are keywords because they are literals: `42` is lexed, not
resolved, and a boolean is the same kind of thing. The words that look like
keywords but are not:

- `self` — an ordinary parameter, by name convention (`05-traits.md`)
- `Self`, `main`, `Some`, `None` — names the language or the standard library
  defines, reached through the namespace rules like any other (`11-namespaces.md`,
  `12-projects.md`)
- attribute names — `packed`, `align`, `build`, `test`, and every user
  attribute are ordinary identifiers inside `#[...]` (`01-types.md`)

## Integer literals

```ebnf
integer   = decimal | hexadecimal | octal | binary ;

decimal     = digit { [ "_" ] digit } ;
hexadecimal = "0x" hex_digit { [ "_" ] hex_digit } ;
octal       = "0o" octal_digit { [ "_" ] octal_digit } ;
binary      = "0b" binary_digit { [ "_" ] binary_digit } ;

hex_digit    = digit | "a" … "f" | "A" … "F" ;
octal_digit  = "0" … "7" ;
binary_digit = "0" | "1" ;
```

An underscore separates digits and may not do anything else: it needs a digit
on each side, so `1_000_000` is one million, `1__0` and `1000_` are errors,
and `_1000` is an identifier. There is no suffix — `42u8` is not a form; the
type comes from context, an unsuffixed literal is `i32` (`01-types.md`), and
an explicit conversion is `@cast`. A sign is not part of the literal: `-1` is
the operator applied to `1`.

## Float literals

```ebnf
float     = digits "." digits [ exponent ]
          | digits exponent ;

digits    = digit { [ "_" ] digit } ;
exponent  = ( "e" | "E" ) [ "+" | "-" ] digits ;
```

A float needs digits on both sides of the dot — `1.` and `.5` are not forms;
write `1.0` and `0.5`. Only decimals carry floats; there is no hexadecimal
float. An unsuffixed float literal is `f32` (`01-types.md`).

## Byte and string literals

There is no `char` type (`01-types.md`): a single-quoted literal is a `u8`,
and it holds one byte.

```ebnf
byte_literal = "'" ( byte_character | escape ) "'" ;

plain_string = '"' { string_character | escape } '"' ;
c_string     = "c" '"' { string_character | escape } '"' ;
raw_string   = "r" '"' { raw_character } '"' ;
raw_c_string = "cr" '"' { raw_character } '"' ;

string_literal = plain_string | c_string | raw_string | raw_c_string
               | multiline_string ;

multiline_string = [ "cr" | "c" | "r" ] '"""' newline
                   { ml_line newline } ml_indent '"""' ;

ml_line     = { ml_character | escape } ;
ml_character = any character except newline ;
ml_indent   = { " " | "\t" } ;

escape          = "\" ( "n" | "t" | "r" | "0" | "'" | '"' | "\" | "x" hex_digit hex_digit ) ;

byte_character   = any character except "'" | "\" | newline ;
string_character = any character except '"' | "\" | newline ;
raw_character    = any character except '"' | newline ;
```

The escapes are the whole set: `\n` `\t` `\r` `\0` `\'` `\"` `\\` and
`\xNN`, which takes any value through `0xFF` — the elements are bytes
(`01-types.md`). There is no `\u{...}`: a string literal keeps the bytes of
the source, and source is UTF-8, so a character beyond ASCII is written as
itself.

The four string forms differ only in prefix and character set. `r` turns
every `\` into a plain byte and admits no escapes; `c` appends a `'\0'`
terminator — the meanings are `01-types.md`'s, and the prefixes combine as
`cr`. A prefix is part of the literal only when it touches it: an identifier
`c`, `r`, or `cr` immediately followed by `"` is the prefixed literal, and
that is the whole rule — an identifier by any other spelling, or with a
space before the quote, lexes as itself. A single-line literal does not
cross a newline; a string with a newline in it is written with `\n` — or as
a multiline literal:

### Multiline strings

A multiline string opens with `"""` — and a newline must follow immediately;
that newline is not content. The content runs to a closing `"""`, which
stands at the head of its own line; the newline before it is not content
either. Each content line contributes itself and one newline, except the
last one, whose newline is the one before the closing quotes — so the
literal ends with a newline exactly when the last content line is empty:

```rust
let s = """
  {
    "lang": true
  }
  """;
```

The bytes are `{\n  "lang": true\n}` — the closing quotes' own indentation
is stripped from the start of every content line. That is the whole
indentation rule, and it is two sentences: a line with less indentation than
the closing `"""` is a lexical error, and an empty line is exempt from the
check — as a content line it contributes just its newline. Stripping matches
the closing line's whitespace characters one for one, so a tab-indented
content line under a space-indented closer is an error, not a quiet
misalignment.

The prefixes and escapes carry over unchanged: `c` appends the `'\0'`,
`r` makes every `\` a plain byte — so the plain form takes the eight
escapes, the raw form takes none. In the plain form `\"` hides a quote from
the delimiter; in the raw form `"""` cannot appear at all — it closes the
literal — so three quotes in a row are written in the plain form or built
by concatenation. A `"` or `""` inside a line is content in either form.
The carriage returns and BOM of the source are normalized before this rule
runs, as everywhere.

## Punctuation

`keyword` and `identifier` below are the sets defined above; every other
token is fixed here:

```ebnf
token = identifier | keyword | integer | float
      | byte_literal | string_literal
      | "(" | ")" | "[" | "]" | "{" | "}"
      | "," | ";" | ":" | "::" | "." | ".." | "..."
      | "->" | "?" | "@" | "$$" | "^^" | "#["
      | "+" | "-" | "*" | "/" | "%" | "~"
      | "^" | "&" | "|" | "!" | "<<" | ">>"
      | "<" | ">" | "<=" | ">=" | "==" | "!=" | "&&" | "||"
      | "=" | "+=" | "-=" | "*=" | "/=" | "<<=" | ">>=" ;
```

Lexing takes the longest match: `::` is one token, never two `:`, and
`a..b` reads `a`, `..`, `b`. This is what makes `^^` and `$$` single tokens
(`08-reflection.md`) and `#[` the only place `#` appears — a `#` not followed
by `[` is a lexical error. What the tokens mean in which position — the seven
uses of `[`, the three of `?` — is the expression pass.

`>>` is one token and two, and position decides: the lexer always emits a
single `>>`, and the parser splits it — but only at a generic-arguments
closing. `is_same<i32, i32>>()` needs two closers, so the `>>` there counts
as `>` `>` (`05-traits.md`); everywhere else it is the shift operator. The
split lives in one production — `generic_args`, shared by the expression and
type grammars — and the existing lookahead still governs: after a closing
`>`, a `(`, `::`, or `.` must follow, or what came before is a comparison.

## Expressions

The precedence levels, tightest first:

| level | operators | associativity |
| --- | --- | --- |
| range | `..` | **one per expression, loosest** |
| postfix | `.f` `.0` `[...]` `(...)` `?` | chains left to right |
| unary | `*` `&` `&mut` `!` `-` `~` `^^` `$$` `@name` | prefix |
| multiplicative | `*` `/` `%` | left |
| additive | `+` `-` | left |
| shift | `<<` `>>` | left |
| bit and | `&` | left |
| bit xor | `^` | left |
| bit or | `\|` | left |
| relational | `<` `>` `<=` `>=` `==` `!=` | **no chaining** |
| logical and | `&&` | left, short-circuit |
| logical or | `\|\|` | left, short-circuit |

Assignment is not on the table: it is a statement, not an expression
(`a = b = c` is not a form). The relational level does not chain — `a < b < c`
is a syntax error, and comparing two booleans is written with parentheses,
`(a < b) == (c < d)`; `&&` and `||` chain freely because a chain of them is
the common shape. `^^` and `$$` are prefix-only: `a ^^ b` is a syntax error
(`08-reflection.md`).

A range is loosest of all: `a..b` is one `..`, two full expressions, and
no more than one per expression — `a..b..c` is a syntax error. Both ends
are required: the open forms `..n` and `n..` stay where they were, inside
index brackets (`01-types.md`), because an open end is an index into a
slice's length, not a value.

```ebnf
expression  = if_expr | match_expr | range_expr ;

range_expr  = or_expr [ ".." or_expr ] ;

if_expr     = [ "const" ] "if" expression block
              [ "else" ( if_expr | block ) ] ;

match_expr  = "match" expression "{" { arm "," } "}" ;
arm         = pattern "=>" ( block | expression ) ;

block       = "{" { statement } [ expression ] "}" ;
statement   = let_statement | assignment | expression ";" ;

or_expr     = and_expr { "||" and_expr } ;
and_expr    = cmp_expr { "&&" cmp_expr } ;
cmp_expr    = bitor_expr [ cmp_operator bitor_expr ] ;
cmp_operator = "<" | ">" | "<=" | ">=" | "==" | "!=" ;
bitor_expr  = bitxor_expr { "|" bitxor_expr } ;
bitxor_expr = bitand_expr { "^" bitand_expr } ;
bitand_expr = shift_expr { "&" shift_expr } ;
shift_expr  = add_expr { ( "<<" | ">>" ) add_expr } ;
add_expr    = mul_expr { ( "+" | "-" ) mul_expr } ;
mul_expr    = unary_expr { ( "*" | "/" | "%" ) unary_expr } ;

unary_expr  = ( "*" | "&" [ "mut" ] | "!" | "-" | "~" | "^^" | "$$" ) unary_expr
            | postfix_expr ;

postfix_expr = primary_expr { postfix } ;
postfix      = "." ( identifier | integer )
             | "[" expression "]"
             | "[" [ expression ] ".." [ expression ] "]"
             | "(" [ arguments ] ")"
             | "?" ;

primary_expr = literal
            | path
            | "(" ")"
            | "(" expression { "," expression } [ "," ] ")"
            | array_literal
            | struct_literal
            | bare_struct_literal
            | closure
            | builtin_call ;

path         = [ "::" ] segment { "::" segment } ;
segment      = identifier [ generic_args ] ;
generic_args = "<" generic_arg { "," generic_arg } [ "," ] ">" ;
generic_arg  = ( "$$" | "^^" ) postfix_expr | type ;

arguments    = argument { "," argument } [ "," ] ;
argument     = [ "..." ] expression ;

array_literal = "[" [ integer ] "]" [ "mut" ] type
                "{" [ expression { "," expression } [ "," ] ] "}" ;
struct_literal = path "{" [ field_init { "," field_init } [ "," ] ] "}" ;
bare_struct_literal = "{" [ field_init { "," field_init } [ "," ] ] "}" ;
field_init    = identifier ":" expression ;

closure     = "fn" "[" [ captures ] "]"
              "(" [ parameters ] ")" [ "->" type ] block ;
captures    = capture { "," capture } [ "," ] ;

builtin_call = "@" identifier ( "(" [ arguments ] ")"
               | generic_args "(" [ arguments ] ")" ) ;
```

An `if` is an expression: both branches have one type, and an `if` without an
`else` yields `()` on the untaken path, so it is written as a statement
(`10-iteration.md`). A `match` is an expression the same way, and an arm with
several statements is a block whose last expression is its value
(`09-match.md`). A block appears where a value is expected only as an `if`
branch, a `match` arm, or a function body — a bare `{ ... }` is not an
expression, so no binding takes one as its value. That absence is what makes
the bare struct literal unambiguous: `{ a: 1 }` in expression position is
`X{ a: 1 }` with the name left out, valid only where the type is already
known (`01-types.md`) — and needing parentheses where a block is about to
open. The condition of an `if`
and the scrutinee of a `match` are such places —
`if Point{ x: 1, y: 2 } == q` is not a form;
`if (Point{ x: 1, y: 2 }) == q` is.

A `<` after an identifier in primary position starts generic arguments only
when a matching `>` and what generic arguments lead to — a `(`, a `::`, a
`.` — follow: `f<T>(x)` is a path with arguments called, `a < b` is a
comparison, and the parser decides by looking ahead to the closing `>`.
`is_same<i32, i32>::value` is the same rule one segment deeper — a path
segment may carry arguments anywhere along the path, and so may splice:
`is_same<$$t, u32>::value` passes a `type` value as an argument
(`08-reflection.md`). The `.` of a tuple index takes an integer — `a.0`,
never `a[0]` on a tuple (`01-types.md`).

The `...` of an argument is spread: `sum(...ts)` passes the elements of a
tuple one argument each (`04-generics.md`).

`let_statement`, `assignment`, `pattern`, and `parameters` appear above as
references into the third pass — declarations and statements, which closes
the grammar. A block may hold statements followed by one expression; what a
statement may be is settled there, and nothing here depends on the details.

## Types

Types have a grammar of their own, beside the expressions:

```ebnf
type          = result_type ;

result_type   = prefix_type [ "?" prefix_type ] ;
prefix_type   = "?" prefix_type
              | "*" [ "mut" ] prefix_type
              | "[" [ integer ] "]" [ "mut" ] prefix_type
              | primary_type ;

primary_type  = path
              | "(" ")"
              | "(" type { "," type } [ "," ] ")"
              | "fn" "(" [ parameters ] ")" [ "->" type ]
              | "dyn" path
              | "type" ;
```

`?T` is `Option<T>` and `E?T` is `Result<T, E>` — the `?` is a prefix when
the error side is empty and an infix when it is named (`05-traits.md`). The
grammar reads it in one step: a type is `E ? T` where `E` may be omitted,
and both sides nest — `E??T` is `Result<Option<T>, E>`, and an error type
that is itself optional parses, whether or not it makes sense.

`*T`, `*mut T`, `[N]T`, `[N]mut T`, `[]T`, `[]mut T` are all prefixes of the
type they wrap. Generic arguments are a suffix of a path — `Vec<u32>` — and
close with `>` tokens, one at a time, which is where the missing `>>` token
earns its keep.

### The `?` three ways

The one character with three jobs never shares a position: in a type it is
Option or Result, in an expression it is postfix propagation — `f()?` — and
nowhere else. A `?` at the head of an expression is a syntax error, and a
type never appears inside an expression without a marker (`@cast<u32>(x)` is
a builtin call; the type lives in its angle brackets). No lexer hint is
needed: the two grammars are disjoint, and position decides.

### The brackets

`[` has one use per position, and every one has its own production: a type
prefix (`[N]T`, `[]T`, `[]mut T`), an array literal at the head of an
expression (`[3]u32{1, 2, 3}` — the only `[` an expression may start with),
a postfix index or range (`a[i]`, `a[1..2]`, `a[..]`), the capture list
after `fn` (`fn[mut n]` — `fn` is a keyword, so the `[` is not ambiguous),
and `#[`, which the lexer has already taken. The capture list keeps its
brackets: no `fn|x|` — the `fn` prefix disambiguates on its own.

## Declarations

A file is a sequence of items, and an item is an attribute sequence on one of
the declaration forms:

```ebnf
file  = { item } ;

item = attributes
      ( fn_item | struct_item | union_item | enum_item | trait_item
      | impl_item | type_item | use_item | const_item | static_item ) ;

attributes    = { "#[" attribute "]" } ;
attribute     = identifier [ "(" [ attribute_args ] ")" ] ;
attribute_args = attribute_arg { "," attribute_arg } [ "," ] ;
attribute_arg = identifier | integer | float | string_literal ;

fn_item = "fn" identifier [ generic_params ]
          "(" [ [ attributes ] parameters ] ")"
          [ "->" type ] ( block | ";" ) ;

generic_params = "<" generic_param { "," generic_param } [ "," ] ">" ;
generic_param  = identifier [ ":" bound ] [ "=" type ]
              | "..." identifier ;
bound = path { "+" path } ;

parameters = parameter { "," parameter } [ "," ] ;
parameter  = [ attributes ] [ "const" ] [ "mut" ] identifier ":" type ;

struct_item = "struct" identifier [ generic_params ]
              "{" [ field { "," field } [ "," ] ] "}" ;
field = attributes [ "mut" ] identifier ":" type ;

union_item = "union" identifier [ generic_params ]
             "{" [ field { "," field } [ "," ] ] "}" ;

enum_item = "enum" identifier [ generic_params ]
            "{" [ variant { "," variant } [ "," ] ] "}" ;
variant = attributes identifier
          [ "=" integer | payload ] ;
payload = "(" [ type { "," type } [ "," ] ] ")"
        | "{" [ field { "," field } [ "," ] ] "}" ;

trait_item = "trait" identifier [ generic_params ] "{" { trait_member } "}" ;
trait_member = "type" identifier ";"
             | "const" identifier ":" type ";"
             | fn_item ;

impl_item = "impl" [ generic_params ] path [ "for" type ] "{" { impl_member } "}" ;
impl_member = fn_item
            | "const" identifier ":" type "=" expression ";"
            | "type" identifier "=" type ";" ;

type_item  = "type" identifier [ generic_params ] "=" type ";" ;

use_item   = "use" use_tree ";" ;
use_tree   = path [ "::" ( "{" use_tree { "," use_tree } [ "," ] "}" | "*" ) ] ;

const_item  = "const" identifier ":" type "=" expression ";" ;
static_item = "static" [ "mut" ] identifier ":" type "=" expression ";" ;
```

The attribute arguments are identifiers or literals, never expressions, and
they do not nest (`01-types.md`); `#[a] #[b]` and `#[a, b]` are the same
pair. An attribute may mark an item, a variant, a field, or a parameter —
the four `attributes` slots above — and never a statement or an expression.

A function takes a body or a semicolon: the semicolon form is a declaration
without a definition — a trait member, or an `#[extern(C)]` import
(`01-types.md`). `generic_param` carries a bound, a default, or the `...`
of a pack (`04-generics.md`); `bound` is a `+`-list of paths. The `mut` of a
parameter marks the slot the way a field's does (`01-types.md`), and `const`
states that the argument must be compile-time known (`08-reflection.md`).
A positional payload lists bare types — `Circle(f32)` — and a named payload
lists fields the way a struct does — `Rect { w: f32, h: f32 }`
(`01-types.md`). An `impl` names a path, then the type it is for when the
impl is for a trait — inherent impls omit the `for` (`05-traits.md`).

## Statements

A block holds statements, then perhaps one expression — its value. What a
statement may be:

```ebnf
statement = let_statement
          | assignment
          | jump_statement
          | for_statement
          | const_item
          | expression ";" ;

let_statement = "let" [ "mut" ] pattern [ ":" type ] "=" expression ";" ;

assignment = expression ( "=" | "+=" | "-=" | "*=" | "/=" | "<<=" | ">>=" ) expression ";" ;

jump_statement = "return" [ expression ] ";"
               | "break" ";"
               | "continue" ";" ;

for_statement = [ "const" ] "for" for_head block ;
for_head      = expression
              | "let" pattern "=" expression
              | pattern "in" expression ;
```

The `const_item` among the statements is the one declaration allowed inside
a block — a compile-time value with no address has no reason to wait for a
namespace (`01-types.md`). Every other item lives at the top of a file; a
function is not declared inside a function. There are no labels, so `break`
and `continue` take no argument and leave the innermost loop.

The left side of an assignment is parsed as an expression and checked as a
place — the grammar does not separate places out, because a place is an
expression shape (`00-preliminaries.md`), and the check is semantic.

### The for shapes

Three heads, one keyword. `for cond` runs while the condition holds — the
`while` of other languages. `for let PAT = e` matches the pattern and ends
the loop when it stops fitting. `for PAT in c` iterates a container through
its iterator. A head starting with `let` is the second shape; otherwise the
parser reads an expression, and an `in` behind it re-reads what came before
as a pattern — the two are written the same way, which is why the re-read is
free (`09-match.md`). A `const` before the `for` asks for compile-time
evaluation of the whole loop (`10-iteration.md`).

There is no C-style three-part `for (init; cond; step)`: `for cond` with a
`let mut` before it and a step at the tail of the body says the same thing
with parts the language already has, and an iterator says it better when the
step is a walk. Zig makes the same cut — `while` and `for`, no third head.
A range closes the common case: `for i in 0..10` is the third shape over a
`Range` iterator, the range being the one expression form `..` has outside
index brackets (`10-iteration.md`).

### Patterns

The patterns of `let`, `for`, and `match` arms:

```ebnf
pattern      = or_pattern ;
or_pattern   = unit_pattern { "|" unit_pattern } ;
unit_pattern = "_"
            | path [ payload ]
            | "(" [ pattern { "," pattern } [ "," ] ] ")"
            | "{" [ field_pattern { "," field_pattern } [ "," ] ] [ ".." ] "}" ;

payload      = "(" [ pattern { "," pattern } [ "," ] ] ")"
            | "{" [ field_pattern { "," field_pattern } [ "," ] ] [ ".." ] "}" ;

field_pattern = identifier [ ":" pattern ] ;
```

A pattern payload takes patterns where a declaration payload took types —
`Some(x)` matches what `Some(T)` declares. A bare `{ a, b }` matches a
struct field by field, the way a bare `{ a: 1 }` builds one
(`01-types.md`); `Rect { w, h }` names the struct. A `..` ignores the rest
of a named payload. An or-pattern lists alternatives; there are no guards,
no ranges, and no literal patterns — a value to compare against is a
condition, and it goes in the expression, not the pattern (`09-match.md`).

## Open items

None. The grammar is closed and the corners are decided; anything added
from here — a new operator, a new literal form — opens a new item rather
than quietly extending a table.
