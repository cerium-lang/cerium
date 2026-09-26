# Namespaces

This chapter continues `01-types.md`. It defines how declarations are named
across files: a directory is a namespace, and `use` brings a name into scope.

A **directory** is a namespace. A **file** is not: every file in a directory
contributes to that directory's namespace, so a large namespace can be split
across files without becoming any deeper.

```text
src/
  main.xyz            → the root namespace
  net/
    socket.xyz        → net
    tls.xyz           → net — another file, the same namespace
    pool.xyz          → net::pool
    pool/
      conn.xyz        → net::pool::conn
```

A declaration lands in the namespace of the directory its file sits in. A
subdirectory is a sub-namespace: `net/pool/conn.xyz` holds `net::pool::conn`,
while `net/pool.xyz` — the file beside the directory — holds items of
`net::pool` itself.

Paths are absolute — they start at the root of the project. `std` is the
standard library: it lives outside the project tree and appears as a name at the
root. Its name is reserved, so no namespace may have a `std` of its own. How a
name is found is in Name resolution below.

## Visibility

An item is private to its namespace unless it is marked `pub`:

```rust
pub struct TypeInfo { ... }   // visible outside std::meta
struct Helper { ... }          // visible only inside std::meta
```

Private means visible to every file of the same directory. A file is not a
namespace, so there is no file-level privacy: to hide a helper from the rest of
a namespace, give it a directory of its own.

## Use

`use` brings a name into scope:

```rust
use std::meta::TypeInfo;              // one item
use std::meta::{TypeInfo, Field};     // several — instead of the line above
use std::meta;                        // the namespace itself — meta::TypeInfo
use std::meta::*;                     // every pub item — instead of the above
```

A `use` puts a name into scope — an item, a namespace, or with `*` every `pub`
item. That is what makes `meta::TypeInfo` resolve after `use std::meta;`. How a
name is found, and what happens when two collide, is in Name resolution below.

There is no renaming: `use X as Y` is not defined. A name that collides is
reached by its full path, or left out of a brace list — see below.

An enum is a namespace of its variants (`09-match.md`), so a variant is
imported the same way:

```rust
use Color::Red;

let c = Red;
```

`Some` and `None` need no `use` — writing them bare is part of the `?T` sugar
(`05-traits.md`), not of this mechanism.

## Name resolution

A name is looked up in the current namespace first — the directory the file sits
in, which holds both the declarations its files make and its sub-namespaces:

```rust
// src/net/tls.xyz — the namespace `net`
pub struct Tls { ... }

fn handshake(s: *Socket, t: *Tls) -> () { ... }
//                ^^^^^^ Socket is declared in net, and so is this file
```

If it is not there, the names `use` brought in are tried, and then the root's
sub-namespaces. `std` is one of those, so a standard-library path reads the same
from anywhere:

```rust
// src/net/pool/conn.xyz — the namespace `net::pool`
let t = std::meta::TypeInfo{ ... };
```

`::name` skips the first two and takes `name` from the root. That is how a
sub-namespace of the current one is told apart from a sub-namespace of the root:

```rust
// a file of std::meta; the project's root also has a `repr`
let a = repr::Foo;    // std::meta::repr::Foo
let b = ::repr::Foo;  // repr::Foo — the root's
```

A name brought in by `use` that the current namespace already declares is a
compile error, as are two `use` of the same name. Neither has a renaming to fall
back on; both are resolved by writing the full path instead:

```rust
// src/net/tls.xyz — the namespace `net`, which already declares a `Socket`
use quic::Socket;     // ❌ net already declares a Socket

fn g(s: *quic::Socket) -> () { }   // ✅ reach it by path
```

A glob is all or nothing — no renaming, and no way to leave one name out. If any
name it brings in collides, the whole `use` is an error, and the way out is the
brace form or the full path:

```rust
use quic::*;                  // ❌ quic has a Socket, and so does net
use quic::{Client, Server};   // ✅ the ones that do not collide
```

There are no relative paths — `self::` and `super::` are not defined, and nothing
needs them: a sibling is in the current namespace and is named with no path at
all, and everything else is named absolutely. What a path means therefore does
not depend on which file it is written in.
