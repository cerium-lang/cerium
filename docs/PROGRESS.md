# Progress

The long-form build log: one stretch a milestone, in the order they
landed, each saying what arrived and how it works. The README holds the
compiler's cross-section -- what exists, how it is used; this file holds
how it got there. The specs hold what the language says; the merge log
holds the patches. The repository grew up under the working codename
`xyz`; the name landed as Cerium, its files `.ce`, with #63.

The lexer and the parser are in: `cerium -t file.ce` dumps the token
stream — position, kind, value — and `cerium -a file.ce` dumps the parse
tree as S-expressions, one node a line, children indented. The type
checker is in through the bodies: `cerium -T file.ce` declares every
name in the file, resolves every type a declaration carries — the
same flags read a directory as a project, every `.ce` under it a
file of it, each in the namespace its path spells
(`12-projects.md`) — aliases
expand to their targets, `?T`/`E?T` build on the prelude's
`Option`/`Result`, `dyn A` names its trait and gives its
associated types — `dyn Iterator<Item = u32>`, the slots aligned to
the trait's declaration order — then resolves trait and
impl members against their `Self`, checks a trait impl supplies
exactly what the trait declares, and orders every pair of impls by
shape-pattern specificity, bounds inclusion, or the `Copy`/`Drop`
exclusion (`04-generics.md`), rejecting the rest on the spot. It then walks
every function body: expressions get their types — the method sugar
adapts its receiver and names any receiver an impl's pattern can
match, scalars included, a trait method found by matching impl
patterns against the receiver's type with every bound answered
there — a bound the receiver cannot answer keeps its impl out —
and the most specific match winning, an exact target above
patterns and bounds ordering equal shapes (`Trait::member(&p)` and
a handle's vtable pick the same one), a generic fn's bounds
checked where the call binds its parameters — `Copy` read
structurally, a `Drop` impl anywhere in the type excluding it
(`03-move.md`) — an associated type read outside an impl —
`Counter::Item` — taking what the impl supplies, and every borrow a
call writes — an argument's
as much as the receiver's — ends with that call, `None` takes its
`?T` from the other side, a `match` is exhaustive variant by
variant — and the flow rules hold:
a move kills its binding downstream, a borrow freezes what it
touched, and `mut` stays two orthogonal levels, the slot and the
field, with a pointer gating everything it lends (`01-types.md`,
`03-move.md`, `09-match.md`, `10-iteration.md`).

A declaration may close its own tail: a type parameter's default
fills the arguments left out — at a path, in a literal's inference,
on an impl's head — naming only the parameters before it, and a
trait's may be `Self`, so `impl Add for V3` is `Add<V3>` for `V3`
and the member's `Rhs` is that argument (`04-generics.md`,
`07-operators.md`). A fn's parameters come from its arguments and
an impl's from the trait it implements: neither carries one.

Compile-time evaluation has begun, the leaf layer: a `const` and a
`static` initializer run in the compiler — literals, arithmetic and
the operators over them, the signedness read from the type, the
short circuit kept, `@sizeof`/`@alignof` over the layout tables,
`@cast` across the matrix, a float truncating as `01-types.md` says
— and the const references that chain them, lazy: each value is
kept once it lands, so a chain resolves once however many read it.
`08-reflection.md`'s four promises hold — every step checked
against the type it lands in, so overflow and a division by zero
are compile errors; the in-flight stack is the cycle check, a
const that depends on itself an error, not a hang; a step budget
ends what would not end; nothing is observed, so nothing varies.
The sign rides a literal: `-2147483648` is i32's least, spelled
the only way it can be — the const's evaluator folds it, and the
body's inference does too, at every place a want arrives: a
binding, a call's argument, a field, an element. The domain check
reads the signed whole, so the least passes and one past it does
not, and a negative never fits an unsigned.
An array's length and a variant's discriminant are const
expressions now, not only literals — `[N * 2 + 1]u8` and
`enum E(u8) { A = D, B }` both evaluate, `B` counting from `D` —
and a body reads a const as an immediate, a float riding the data
segment, the literal's ride. An fn call in an initializer waits
for the passes that know the bodies.

The wait is over for the plain fns: a call whose arguments are
compile-time known runs the callee in the compiler, no annotation
asked — `08-reflection.md`'s own words. Its body walks a statement
subset: a `let` that binds one name, an assignment to a local —
the compound six included — an `if` in either shape with the
else-if chain, an early `return`, and the tail. Each call gets a
frame — a floor on the binding stack, so an inner call never
writes a live outer slot — and the depth has a budget (128) of
its own, before the shared step budget has to speak. A forward
reference resolves lazily, the signature read on demand; an
overload chain picks by the arguments, and a name that cannot run
says why — extern, generic, no body — instead of a shrug. A call
lands anywhere a const does, a length included. `@compileError`
is the report: reached, it is the compile error itself; a branch
a finished run did not take stands as a branch — the body check
leaves it, and emit gives it no runtime behavior, for panic
belongs to the running program — while a fn the evaluator never
ran still owns every `@compileError` it holds, a misuse. Loops,
`match`, and aggregate values are the next layer.

The arrays are values now: a literal's elements evaluate — the
zero fill behind them, a const chain inside them, a call as the
length, the nesting an element at a time — and an index reads one,
checked against the length the initializer spelled. A `mut` in the
type is the slot's permission, not the element's own, so the reads
are the same either way. A slice literal is a borrow, and
evaluation allocates nothing; no operator spans aggregates yet —
the words an array's value sits in are an address here, and
comparing those compares nothing. An array crosses a frame as a
parameter and comes back as an answer; a `const`'s value keeps its
elements memoized beside its scalar half, and a body reading one
walks the data segment it rides.

The signed steps work through the negatives now too: the domain
check asks the type's question, not the u64's — a borrow was an
overflow, a negative product wrapped the wrong domain — and the
division rounds the way the running program's does, toward zero,
not the floor the host's C89 might have chosen. An i64's own ends
are the signs' to catch: operands agreeing, an answer that flipped
is past them, and the least times minus one is the one quotient no
division holds.

The aggregates are values in full now: a struct's rows evaluate by
name with the ones left out zero, a tuple's by position, and the
reading matches — a field by name, a row by number. A union keeps
one row active — the last name a literal wrote, or the zeroed whole
reading zero every row — and a read of any other is the unspecified
thing the spec says not to rely on, which the promise of determinism
turns into a report. An enum constructs in every shape it parses:
positional, named, payloadless by path, and the prelude's bare
`Some`/`None`; the discriminant rides the value, `@cast<u32>` reads
it out — at compile time and in a body now, the tag loaded at its
own width — and the payload waits for `match`. The nesting recurses,
a struct in a struct, an array of them; the frames carry them as
parameters and answers; a const's Sym keeps the elements beside the
scalar half. The operators stay the trait table's — `==` is `Eq`'s,
and that table arrives with dispatch — and a generic's rows wait
for the binding a call's own words spell.

The data segment is here, and a body reads a const aggregate off
it: the evaluator's memoized value spelled as qbe data items, the
layout tables' offsets walked and the padding zero — a struct's
fields in order, a tuple's rows, an array's elements, a union's
active row with the rest zero, a tagged enum's discriminant at its
own width with the payloads packed after it, and a niche enum the
one word it is. A const has no address to lend (`01-types.md`), so
a read that takes the value copies — `let p = A` blits into
storage of its own, and a call's argument with it — and a read
that walks it — a field, an index, a view, a loop's iterable, a
match's scrutinee, a destructuring let — walks the segment where
it sits, no copy at all. A static's first value rides the same
way, but its symbol is its own slot: whole-program, and writable
where `mut` marked it — the checker now reads the place's
leftmost global, a const never writable however mut its fields,
a static by its own `mut`; a `static mut`'s aggregate field and
its scalar both write, and the write stays. A union's
non-active row reads zero in a body — the evaluator would report
it, the read cannot, and the report waits for reflection
(`08-reflection.md`).

The match runs, and the loops with it. A pattern destructures; it
does not test — so an arm's turn is a discriminant's compare, the
patterns that miss naming another variant, and the bindings an arm
spelled land in the frame and end with it. A `let` takes a pattern
now, the tuple by position, the struct by field, a variant's
payload under it — irrefutable, and a value that says otherwise is
the abort it would be where it runs, reported here. The three `for`
heads unwind: a condition while it holds, a `let` while its
pattern fits the value re-read every round, an `in` over an owned
array's elements or the `?T` niche — one round at most, so a
`continue` ends it like a `break` would — and a refutable element
pattern ends the loop the way a `for let`'s misfit does. `break`
and `continue` unwind one level, a `return` everything; a round
costs a step, so a body that empties still meets the budget, and
what would not end ends. Iterating a borrow stays out — the
pointer a slice lends is a runtime thing — and so does an iterator
the method table would pick. A call's arguments take their wants
from the one plain overload that takes them, so `Some(3)`
constructs in an argument's place, and a match lands at a const's
own initializer, as the `if` already did.

The const `for` unrolls, and no loop survives it. The `in` shape
only — the condition and the `let` forms re-read their terms, so
they are runtime shapes — and the iterable is compile-time known or
it stops where it stands: the marker is a promise, not a hope. An
owned array gives an element a round; a `?T` gives its payload or
nothing; a slice lends each element out, and no loop is emitted to
lend one; anything else arrives with a later milestone. The
iteration runs in the compiler, and each round lands as the words it
stands for: a `let` a binding — the value the evaluator holds
spelled back as a literal, the type spelled with it, so the passes
walk the round the way they walk the program's own words — and the
body after it, shared between the rounds, for the passes read it
without writing it. The pattern's fit was proven in the evaluator,
so it flattens into one binding a `let` — a `let`'s pattern is
irrefutable where it is checked — and a misfit ends the loop the way
a `for let`'s does. A nested const `for` at the body's top level
expands in the round it stands in, its iterable the outer round's
own name; below the top level — a branch, an arm — it unrolls where
it stands now too, the walks writing the copy they walk
(`10-iteration.md`). A
`break`, a `continue` have no for to reach; a `return` returns from
the fn as any return would. In a compile-time fn the loop runs as
the runtime one does, a round a step against the budget.

The types are values now, and a value's type reads back out of it:
`^^i32` lifts a type into a value — the compounds too, an array, a
tuple, an option — and `@typeof(v)` is the way back, the type a
value holds. A type value splices where a type is spelled: `$$R`
in a const's or a static's own declaration, the operand evaluated
in the compiler — a const's chain, a call's answer spelled right
there, `$$pick(true)` the branch it took — and the slot the splice
names is the type the value holds, the initializer asked of it as
any slot's initializer is: the domain check asks the first step
too now, a literal checked against the type it lands in — the sign
still rides a literal, `-2147483648` folding before the literal's
own step asks the magnitude alone — and a splice that names i32
asks its initializer the same question there. A type is a
zero-sized word: the layout says zero and one, the ABI carries it
in a word like a unit's, a branch joins it as the constant zero
it is, and an array of types rides the data segment, an index of
it a type anywhere one is wanted. The splice's value must stand
without a frame, and a body's pass has none: `$$param`, a local,
a const `for`'s round are refused where they stand, the per-call
resolution arriving with `@typeinfo` (`08-reflection.md`).

A variant's named payload constructs by name now, both walks a
value takes: `Enum::V{x: 1}` in a const's initializer and in a
body's let — the names in any order, the payload's own order the
value's — and the node is rewritten a call's shape right where it
stands, so the emitter and the evaluator walk it as the
construction a positional call always was. The checks are the
struct literal's own: a field left out, a name the payload does
not carry, a name given twice. The bind the rewriting surfaced
was older than it: emit's binding stack kept its popped frames in
the vector and pulled the count back up on every bind, so a
binding pushed after a scope popped — a match arm's name let'd
again below it — read the arm's slot, the let never reached. The
count is the stack now; the vector only its high-water mark
(`01-types.md`, `09-match.md`).

The language's own source ships inside the compiler now, and
std::meta is the first of it: `std/meta.ce` — the `TypeInfo` enum
and its five companions, exactly as `08-reflection.md` spells them —
is embedded as a table of string literals (`tools/embed.sh`), joined
in the arena, and parsed before the user's file, the lexer reading
the memory text through a source of its own; its items declare and
resolve under the clean symbol table, and its names are taken before
the first line is read. A field may be named `type` — a keyword and
a field's name both, `Field{type: ^^u32}` — in a declaration, a
literal, a pattern, and the access. A zero-sized payload holds no
value and emits nothing now: the variant's and the literal's stores
skip it (the join slots already did, `08-reflection.md`), and a type
reference is Copy — no bits, no destructor, nothing to move — so a
stored reference reads back out and passes along. What a const's
own hand cannot build yet stays honestly refused: a string, an
empty slice — evaluation allocates nothing, and the values that
carry them arrive with `@typeinfo`, built inside the compiler rather
than spelt (`08-reflection.md`).

`@typeinfo` answers now, every row the model spells: the fourteen
variants built from the type's own halves — an `Int` its width and
sign, a `Pointer`/`Slice`/`Array` its child and mutability, a
`Struct`/`Union` its `Field` rows (the name, the bound type, the
offset the layout settled on, the mutability, the attrs the
declaration was marked with), an `Enum` its tag and its `EnumField`
rows (the `?type` a `Some` only where the variant carries exactly
one positional type — the shapes the spec leaves unspoken hold no
single type for the slot to name), a `Tuple` the canonical offsets
replayed, a `Fn` its arg rows and its return — the sugar answering
first (`?T` an `Optional`, `E?T` a `Result`), and the value slot
describing a value's static type. The checker builds the value
right where the call stands, the body pass's rewrite replacing the
node in place with the tree a literal parses to — a runtime match
walks it like any data. The deferred splice answers where a frame
is: a fn called at compile time, the evaluator's own frame
resolving `$$t` to the argument's type; the body pass holds no
frame, and a splice that names a parameter or a local is refused
with the way out named. A const's match pulls the slices out —
`const FS: []Field` — and the const `for` unrolls over them, the
runtime read riding the data segment: a slice's two words point at
a child symbol of its own, the rows spelled beside the parent's
line and replayed with it every pass, the attrs along (an
`AttrArg` an `Ident`, an `Int`, or a `Str`). What waits is the
generic walk — a parameter's `@typeinfo` arrives with
specialization (`08-reflection.md`).

The field-address walk is here: `@field(v, "name")` yields the
field's address, and `@offset<T>("f")` folds to the layout's own
constant — the two sides of `08-reflection.md`'s field access. The
name must be compile-time known, and two sources spell one: a
literal's bytes read straight off the lexer's token — evaluation
allocates nothing, still — and a const `for`'s round, the unroll's
own value riding the binding it spelled, so the body's walk reads
the name off the frame the evaluator's listing no longer holds. The
address rewrites into the hand's own borrow — `@field(p, "z")`
becomes `&p.z`, `&mut` where the field is `mut`, so writing is
governed by the field's own `mut` exactly as `p.z`'s is, the packed
rule and the place checks the access's own. The deref spends that
inline borrow whole — `*(&mut p.z) = 40` never freezes `p` past the
statement, a promise the language's own hand-written spelling broke
before: the borrow died with no binding to thaw it, and the next
statement read `'p' is borrowed`. And the walk the spec promises —
`const for f in fields { *@field(p, f.name) }` — each round takes
its own copy of the body's statements, for the passes write what
they walk: a round's rewrite is its own, and what one round wrote
the next must not read.

The generic walk is here: the serialization walk `08-reflection.md`
promises a parameter runs inside the instance — `match
@typeinfo<T>() { Struct { fields, .. } => { const for f in fields {
r += *@field(v, f.name) } } }`, end to end. The declaration's own
walk holds `T` as a black box and defers: `@typeinfo<T>` and
`@offset<T>` answer with the shape alone, the node left standing
(the `$$` splice's deferral again), a `const for` whose iterable
names the box unrolls nothing, and a match on `@typeinfo` — the
prime scrutinee (`09-match.md`) — holds the shape the world around
it wants. The instantiation re-checks the body on a copy of its own
— the emitter clones the shared declaration per binding, for the
passes write what they walk and what one instance wrote the next
must not read — and there the box is a type: the answers rewrite
in, the match routes at compile time, the taken arm's bindings
spelled as the lets they are and the match itself rewritten a block
whose tail is the arm's own body, the arms that miss discarded
before checking, the rounds unrolling over the fields the answer
carried and `@field` borrowing each one out. A `@field` spelled
against the bare parameter still refuses — the walk is the
instance's own (`04-generics.md`) — and an untyped `let` under a
black-box match meets the placeholder's shape at its use, the
honest edge.

The const parameter is here: `fn field_offset<T>(const name: []u8)
-> usize { @offset<T>(name) }`, the spec's own spelling — the
argument baked into each instantiation, a const parameter the
value's generic. The caller's argument must be compile-time known,
and the pick is static: a literal, a const's name, arithmetic over
either, or another const parameter — a runtime read names the
plain half of an overload pair. The fn itself walks the instance
pipeline the generic walk built, the instantiation's key widened
from the types to the values — `Inst` carrying `cvals` beside
`tys`, each spelling its own clone — and inside the declaration's
walk the name is a black box again: a read of a const parameter
the frame holds no value for defers, and the re-check under the
binding — the clone whose frame carries `cv` — folds `@offset` and
`@field` where the value has landed, the generic's own deferral,
twice over when a const passes as the const. A compile-time call
runs the body in the evaluator instead, the frame holding the
baked argument, so a const spelled from the call folds too. The
const spelling is the more specific overload — `f(1)` takes the
const half, a runtime read the plain — and a fn with a const
parameter is no value, a fn pointer having nowhere to hand one
over; a method's table slot has nowhere either, and its const
parameters arrive with the milestone that routes them. What waits:
the const in the type position — `[N]T`, `const N: usize` in the
angle brackets — and the const `if` (`08-reflection.md`).

The const generic parameter is here: `fn first<T: Copy, const
N: usize>(v: [N]T) -> T { v[0] }`, the length a value parameter of
the angle brackets — what an array length is (`08-reflection.md`).
There is no way to spell its argument: generic arguments cannot be
spelled in expression position (`04-generics.md`), so the call's
expected type is the only spelling — `first(a)` with `a` a
`[3]u32`, and the unifier meeting `[N]T` with `[3]u32` binds N to
3, the instance's key widened again, `_g3` beside the types. The
binding rides the environment's own entry — `Bind` carrying `cv`
beside its type — and the re-check reads a length spelled off the
name there; inside the declaration's walk the name is the black box
again, the frame materializing a const slot the evaluator reads as
zero, the generic's deferral a third time over. A read of N in the
body folds to its number — a const generic parameter has no runtime
slot to read — so `v[N-1]` and `@sizeof<[N]T>()` fold per instance,
and a nested call under a still-open N defers its own binding, the
outer instance's re-check landing both. The unifier can leave a slot
empty when nothing ever met the length, and an empty slot cannot
bake an instance: `cannot infer 'M' from the call`. A const generic
parameter names a length: usize is the type it takes — the spec's
only spelling — and on an impl or a method it arrives with the
milestone that routes the vtable, a table slot having nowhere to
hand a baked length either. What waits: the const `if`, and packs
with it (`08-reflection.md`).

The const `if` is here: `const if @count(...Ts) > 16 { @compileError("too
many arguments") }`, the condition compile-time known or refused, and
the branch picked at the walk — the taken block the conditional's
own, rewritten in place as the block it is, the untaken discarded
before checking, so it may hold code that only compiles for some
instantiations: `const if N > 2 { v[9] }` never checks against a
length of 1 (`04-generics.md`). The condition the declaration
cannot know defers the whole conditional — a generic's length, a
const parameter's number — the same routing a match on `@typeinfo`
takes, the instance's re-check answering; and the const parameter
rides a table the body pass marks around its walk, the evaluator
meeting the name no frame answers and asking there, the box again.
An else chains — `else const if`, or an ordinary `if` the untaken
side runs as itself — and a compile-time call picks its branch in
the evaluator, the marker changing nothing there. A typed `let`
names the shape a deferred branch answers to, the black box
carrying it as its placeholder; an untyped one meets the
placeholder's shape at its use, the honest edge the black-box
match already owns. What waits: packs — the tuple they stand on,
`@count` among them (`04-generics.md`).

The tuple, it turns out, was already here — the type, the rows, the
pattern, the element-wise `mut` — walked in with the milestones that
needed it and never got the tests that pin it. What was missing was
the spread: `f(...t)` spells every row of a tuple as an argument of
its own (`01-types.md`), the walk rewriting the spread to the row
reads in place, so every pass below walks the list as the one the
words spelled — the emitter included. The empty tuple is the unit
type, and it spreads nothing; a slice's length is a runtime thing,
its spread waiting for the pack it feeds (`04-generics.md`). A
compile-time call answers the same way, the rows riding the
operand's own value, the arity and the wants widened before the
overloads answer. What waits: the packs themselves — the angle
brackets' `...Ts`, `@count` among them (`04-generics.md`).

The packs are here, the angle brackets growing their own syntax:
`fn sum<...Ts>(ts: ...Ts)` declares one, last in both lists — the
generic parameters' and the parameters' — the `...Ts` of the
parameter saying its type is the tuple the binding stands for ("ts
is a tuple of type (...Ts)"), so an instance's `ts` is an ordinary
tuple parameter and every row operation the language already has
applies. Three ways to feed it: a tuple handed over whole
("unifies the pack with the tuple's elements"), loose arguments
rolled into the tail, and the spread — a pack's spread stays a
spread, the arity unknown at the declaration, the signature taking
the call on faith: the return type only, the instance's re-check
landing the truth. `@count(...Ts)` reads the binding, folded to a
literal the moment the binding is known; `ts[0]` and `ts[1..]` are
the tuple's own index and slice, rewritten in place. A bound
(`<...Ts: Show>`) holds every row, an empty pack failing none. The
peel — `<First, ...Rest>` beside an overload for the empty case —
needs no count at all; both spellings of the recursive sum run
(`04-generics.md`). The recursion is capped — 256 unfoldings, each
instance one, the chain the emitter walks — a cap the walk taught
us to want: a slice of the pack once re-indexed through the whole
of it, every argument rebuilding the sub-tuple, the emitted lines
growing with the cube of the pack until the rows learned to stand
as the arguments they are. What waits: pack impls, and a slice's
spread.

Both waited, and both are here. `impl<...Ts: Show> Show for
(...Ts)` holds a trait for tuples of every arity: the `(...Ts)` is
the pack's own grouping — resolve reading it as the Typaram
itself, the rows standing as the tuple's own — and a receiver's
tuple binds Ts whole, the same unification a handed-over argument
makes. The bound holds every row (`<...Ts: Show>` asks each one,
the empty pack asking none — the whole tuple asked as one would
find this impl answering itself, so the row-by-row question is the
only honest one), and the specialization order needs nothing new:
an exact tuple is a shape above a variable, so `impl Show for
(i32, i32)` wins wherever it matches — mid-recursion included, the
sub-tuple the peel lands on found by the exact table before the
pack pattern sees it. The method's recursion is the sum's own
writ small — the count's const if, `v[0]` and `v[1..]` — with one
twist the fn never had: the peel is the receiver itself,
`Show::show(v[1..])` handing the sub-tuple over whole rather than
spreading its rows as arguments the method's one parameter cannot
take. The spread grew its other half too: an array's length is
the type's own, so `f(...arr)` spells every element an argument
of its own; a slice's is a runtime thing, refused at the call —
but a value the evaluator knows (a const's) rides its own rows,
the compile-time call's spread feeding them as a tuple's
(`04-generics.md`). A pack among rows is no type — `(...Ts, i32)`
refused, the pack standing for the whole tuple or nothing.

The type predicate is here, std::meta's `is_same` the first: a struct
of no fields, two impls for it — `impl<A, B> is_same<A, B>` carrying
`const value: bool = false`, `impl<T> is_same<T, T>` carrying true —
and the read, `is_same<i32, i32>::value`, naming the instance its
angle brackets spell. The spec's second declaration — `struct
is_same<T, T> {}`, the specialized struct — turned out idle: the
repeated variable of `(T, T)` must land both arguments on one type,
a strictness the specialization order already reads, so the two
impls order themselves with no declaration above them
(`04-generics.md`). The read walks its arguments to the receiver
they name, the impls fitted against it, the most specific one's
member the answer — folded where it stands, the expression rewritten
as the literal it is, so the instance takes no space and the emitter
never sees it. Inside a generic fn the read defers —
`is_same<T, U>::value` a black box until the instance lands its
types, the re-check folding it there — and a splice names an
argument too: `is_same<$$I, i32>::value` with `I: type = ^^i32`
answering true (`08-reflection.md`). What the predicate needed of
the language was the std table: the embedded source's impls now
enter pass 3 with the user's — held from the prelude's walk to
every checkfile — so the read needs no declaration of its own, and
an impl the user writes over std's is the conflict the coherence
check names (`05-traits.md`).

The namespace tree is here, its first real branch the prelude's own
(`11-namespaces.md`): std::meta, the embedded source declaring into
a namespace of its name instead of the root's flat table. A path
reads it — `std::meta::TypeInfo` in type position, the const's
`std::meta::is_same<i32, i32>::value` through the same `::value`
walk, the rooted `::std::meta::...` beside — the leading segments
walking the root's sub-namespaces, what is left reading as it
stood: the type resolves in the namespace it lands in, the value's
member walk its own. The bare name still answers too — the root's
miss falling through to std::meta's table, the prelude-era
stand-in until `use` lands — and through that stand-in a root
declaration of one of std::meta's names stays the taken name it
always was: the fallthrough could not settle the ambiguity, so it
is refused, the fallthrough's own semantics held honest until the
step that retires it. What waits: `use` — the four shapes of it,
the fallthrough retired, and the namespaced `meta::TypeInfo` a use
brings in (`11-namespaces.md`).

The `use` shapes are here, and the stand-in is retired
(`11-namespaces.md`). A file's `use` binds ahead of pass 2 — a
const's own type may read one — and the four shapes each land:
the single item (`use std::meta::is_same`), the brace tree
(`use std::meta::{TypeInfo, Field}`), the namespace itself (`use
std::meta`, `meta::TypeInfo` reading through it), and the glob
(`use std::meta::*`, every pub item, all or nothing: a name it
would bring in that is held — by a declaration or another use —
takes the whole use down, the way out the path). The bare name's
lookup reads the namespace being checked first — the embedded
source's items reach their own neighbours bare, the user's file
sitting in the root as it always has — then the root's table,
then the uses. With that ordering the root fallthrough to
std::meta is gone, and a same-name root declaration is free at
last: `@typeinfo`'s rewrites spell their symbols' namespaces out
(the walk to `std::meta::TypeInfo` its own), so the user's
`enum TypeInfo` and std's model answer each their own path, a
match on either finding its variant. A namespaced pattern
(`std::meta::TypeInfo::Bool` in a match arm) resolves by the same
walk, the arm rewritten to the variant's short name — the
scrutinee's own enum picks it out from there, the evaluator and
the emitter reading the shape they always read.

`11-namespaces.md` closed with `11c`, the project milestone's own
half. A directory passed to `-T`, `-s` or `-c` is a project: every
`.ce` under it a file of it, each in the namespace its path
spells — a subdirectory a sub-namespace, and `X.ce` beside an `X/`
directory the two halves of one, the pair's namespace `X`'s own
(the spec's own example table said otherwise for a file inside a
directory; the reading that survives is the directory chain's, and
the example's fifth row now agrees: `pool/conn.ce` sits in
`net::pool`, its own name spelling no segment). The walk sorts
every directory's entries, so a project compiles the same whatever
the file system hands over; a directory's files come ahead of its
subdirectories, a paired file's declarations the first into the
namespace it shares. `main` is the project's own fn — the root's,
and nowhere else, a diagnostic naming the file that tried. `-t`
and `-a` stay single-file: a project's tokens and AST are its
files' own.

The checker walks a project as the four passes it always had, a
file at a time where a file's own matters (`checkproject`): pass 1
declares every file's names into its own namespace — the whole
project first, so a type may read a name another file declared —
then each file's uses bind and its declarations resolve in its own
context, the impl table builds for all (std's impls ahead, the
walk's order after), and the bodies check back in their files. The
uses are a file's own all the way through — one use environment a
file, switched per pass (`11b`'s semantics made real: A's `use`
binds nothing for B), and a diagnostic names the file it belongs
to, not whichever parsed last. The namespace head of a path walks
the same chain a bare name does now — the file's own
sub-namespaces first, the root's, then what a `use` brought in —
so `repr::Foo` inside `net` reads `net`'s own `repr`, and `::`
stays absolute, the root's tree only.

The emitter spells a namespaced symbol out: `cerium_`, then the
namespace's path folded on — each `::` a `__`, so a namespace
named `net_pool` and a `net` holding a `pool` never fold the same
(`cerium_net_pool` against `cerium_net__pool`). The root's own spells
the single-file era's bare `cerium_foo`, so every name a golden or a
linker ever saw stays its own. An impl's members carry their
file's namespace (`ownns`, filled at declare for the impl and at
member resolution for its methods), and the emitter's walk
switches to each file's namespace and uses as it goes — a
non-root file's fns read from its own table, its glob's bindings
its own.

The project tests live the way the single-file ones do
(`tools/run-tests.sh`): `check/ok/06-project/` diffs its `-T`
against the `.golden` beside it — one `(file ...)` block per file,
the walk's order — `run/126-project/` compiles to one binary whose
exit the `.expect` names, and `check/err/100-102` reject: `main`
outside the root, a use not shared, a name declared twice in one
namespace from two files.

Codegen has begun: `cerium -s file.ce` prints the `.ssa` text — qbe's
input — and `cerium -c file.ce -o out` runs the pipeline, qbe as a
subprocess and the system `cc` linking. Checking writes each node's
type back into the tree, so the emitter never re-derives one. What
it emits covers the expression language: arithmetic and comparisons
with the signedness picked from the operands, `@cast` across the
int/float/bool matrix (`u16`→`i64` widens by the source, a float
narrows by the target), `@sizeof`/`@alignof` folding over the
layout tables of `02-layout.md` in full — struct padding in
declaration order, unions, enums as a tag and a payload union, the
`?T` niche, `#[packed]` and `#[align(N)]` — calls through a name or
a fn value, an impl's methods emitted as the fns they are, inherent
or trait, exact or a pattern the receiver instantiates (the sugar's
receiver adapted at the call, a `Type::member` call, a
`Trait::member(&p)` one resolving its impl by the receiver, and a
method held as a value included) — and a method may take parameters
of its own, the trait declaring the family: the impl's parameters
and the method's bind together at the call, the arguments picking
the member, one instance per binding, a generic caller's re-check
carrying both (04-generics.md) — `dyn A` handles — `&dyn b` builds
the fat where the impl is known, one vtable per trait and concrete
type in the data segment — the most specific impl's, as every
call-site pick — the call reading its slot through the
table (`dyn mut A` writing too), and a handle's spelling giving the
associated types — a projected `?Self::Item` return takes its type
from the spelling, not from the impl the vtable erased — `#[extern(C)]` imports and
exports keeping their symbols, and the aggregate half: a struct literal (a nested one
`blit`s into its field), a string landing in the data segment with
its slice on the stack, `.ptr`/`.len` reads, and field/deref/slot
places to read and write through, `mut` permitting. A literal's
left-out half reads as zero — a struct's missing fields, an
array's shorter tail, a bare `{}` — the storage starts zeroed
(`01-types.md`). An overload
chain resolves at the checker — the call site writes its pick back,
and one instantiation is re-checked per binding (M3e); a fn value
carries the same resolution, a generic one instantiated from the
type expected of it, and a generic struct's literal instantiates
the same way: the expected type first, the field values binding
what it leaves open, the declaration's defaults covering the rest.

Control flow is M3c's, and in: `if` is an expression — value form,
else-if chains, nesting — `&&`/`||` skip the right side, and the
three `for` shapes of `10-iteration.md` run: a condition re-checked
every round, a `let` pattern re-fitted every round (a misfit stops
the loop), and `in` — an Option yields its one payload, niche or
tagged, one round at most, a `continue` ending it like a `break`
would, and a slice lends each element out as a pointer, so the
binding is a `*T`. `match` destructures per `09-match.md`:
positional and named payloads, nested struct patterns with `..`,
wildcards, or-patterns whose shared bindings are pre-bound before
the alternatives — the join must read what every path defined —
and the short variant name, which the scrutinee's enum settles
against the bare binding. Arm order decides. Compound assignment
covers the operators' full set with their signedness, and
assigning an aggregate blits.

A binding's stack is the fn's frame, asked for at the entry: every
slot the body wants — a let, a pattern's, a value's, an
iteration's — goes out at `@start`, once, the rounds of a loop
reusing what they were given. An alloc left inside a loop is bytes
taken again every round, and a long enough loop walks its frame
off the guard page; the entry ask is Clang's alloca discipline,
and the emitter holds the fn's text back so the asks can go ahead
of it.

Aggregates cross calls on the platform's C convention — that is
what `01-types.md` made Cerium's own convention, and qbe lowers it:
register eightbytes, stack order, an sret, all of it. Every
aggregate that crosses a call is named in a `:type` registry —
structs, tuples, unions, enums as tag plus payload union, slices
as two words, arrays — registered recursively, the declarations
printed innermost-first ahead of the functions, the order qbe
reads. A niche `?ptr`/`E?ptr` stays the one scalar it is at the
boundary, loaded out and stored back on either side. A `#[packed]`
shape, or one whose natural layout C would pad differently, rides
an opaque `align N { size }` — memory carries it, correctness
over speed. Parameters arrive as the copies qbe makes (C
semantics); a shared borrow may stack on a live shared one, the
checker now reading `01-types.md`'s "a `*T` is not exclusive"
as written. Array literals and indexing, tuple expressions and
row writes — `(T, mut U)`, one row its own slot — and
monomorphization are in (M3d–M3f): one copy per binding, the
same instance emitted once, all of it static.

The layout and the behavior are tested by running them:
`tests/run` holds one `.ce` per binary with an `.expect` naming
its exit code, an optional `.stdout` holding the bytes it must
print — `#[extern(C)] fn write` is how the language prints for now
(`tools/run-tests.sh`); the section runs only when `qbe/qbe`
is built.
`tests/lex`, `tests/parse` and `tests/check` hold the golden tests,
split by pass: `ok/` has one `.golden` per `.ce` that the dumps must
reproduce exactly, `err/` has inputs that must be rejected
(`tools/run-tests.sh`).

The diverge is real now, and panic with it — the first fn std's
runtime half carries (`01-types.md`, `10-iteration.md`).
`#[noreturn]` marks a fn that never comes back, and the compiler
judges a path's leaving from the syntax alone: a `return`, a `break`,
a `continue`, or a call to one of those fns, nothing deeper. A branch
that leaves does not join the type agreement — the spec's own `match`
with a `return` arm beside a value arm compiles now, `open_or_die`
with it — and the want a checker flows into the leaving position is
cut: a `let` whose init panics binds the shape it spelled, the dead
code after it checking nothing. The emitter walks the same judgement:
the paths that leave carry no join and no trailing return, and a body
whose last text is the call itself ends there — a dead `ret` written
for qbe's terminator rule alone, never run. panic itself is a plain
fn in std, embedded the way std::meta is (`std/panic.ce`, the
registry now a table of files): `write` under `#[extern(C)]` for the
message, `abort` for the end — no unwind, no destructors, the process
taken while the state is still trusted enough for the message it just
wrote (`12-projects.md`). And `main` may return an integer now —
`i32` the usual one, the program exiting with that code; anything
else it returns is rejected where it is declared.

The embedding is retired: std is a directory on disk, read as the
project's first files every compile — the sysroot
(`12-projects.md`). `CERIUM_SYSROOT` names it; `std` beside the
compiler's own executable is the usual install shape, a checkout's
too; `std` in the working directory is the last resort. Nothing of
the library ships inside the binary — `tools/embed.sh` and the
string-literal table are gone — and the walk that reads `std/` is
the one that reads a project's own tree, each file in the namespace
its path spells: `std/meta/meta.ce` sits in `std::meta`,
`std/panic.ce` in `std` itself. The checker reads `std::meta` to
check, so the sysroot and the compiler move together — a sysroot
without `meta::TypeInfo` is named at the first compile — and `std`
is a reserved name: a project whose tree carries a `std/` directory
is rejected, the namespace the library's own. std's files walk the
same four passes the project's do, their impls into pass 3's table
with the user's, and `-T`'s dump starts past them — the goldens hold
the user's files only.

`pub use` arrives with it, the re-export: a use that binds the
namespace instead of the file (`11-namespaces.md`). The name becomes
part of the namespace's public face — a use from another file, a
glob, a path in type position, the bare name of a sibling file, every
way a name is found sees it. Three rules hold it simple: the target
is a pub item, and a namespace is not one to give; never transitive —
a re-export points at a real item, not another re-export; the name
taken whole — a declaration under it rejects the pub use, an
overloaded fn not chaining onto one. A glob form is not defined: the
names a namespace gives away are named.

And the prelude: std's flat pub face — the sugar's four and `panic`
— bound into every user file without a use written, an injected glob
per file (`12-projects.md`). The injected names yield to everything:
a declaration, an explicit use — each wins by being read first, the
injected name never an error. The sugar's four are std's source now
(`std/option.ce`, `std/result.ce`, `std/copy.ce`,
`std/drop.ce`), declared and resolved like any file's items, their
Syms taken back after pass 1 — and the names they held are free: a
project may declare its own `Option`, its own `Copy`, while `?T`
still means std's `Option<T>` and the exclusion checks still read
std's `Copy` and `Drop` (`01`, `03`). `std::meta` is not in the
face — a glob is not recursive, the reflection model opted into by
its own use — and `Some`, `None`, `Ok`, `Err` ride the sugar's owner
lookup, not a binding.

The runtime checks are here, the four of them: index, arithmetic
overflow, shift, and cast (`01-types.md`). An index reads its bound
— an array's own length, a slice's loaded second word — and one
unsigned compare holds both doors, a negative riding the
sign-extended word into the far past the bound; a range checks its
own low against its high before either meets the length; a constant
index on an array folds at the checker, the compile error it always
was, while the same constant on a slice still runs — a slice's
length is a runtime thing. Arithmetic recomputes in the wide domain
and asks the type's own ends: the types narrower than a register
widen first, for the register does not wrap where the type would,
and the register-width pairs carry the xor shape instead — an add
flipping where its addends agreed, a sub borrowing past the least —
an l-domain mul taking the divide back, `div` signed and `udiv`
unsigned, with its two guards walked first, a zero divisor and a
minus one the only pair the round trip cannot answer on its own. A
negation asks one question — the least is the only value its own
negation misses — and an unsigned negation wraps, not checked, the
type's own arithmetic. A shift asks the operand's own width, at or
above it out of range (`07-operators.md`); a negative amount
extends the way an index does, the far past again. A cast narrows
against the target's ends, int to int in the wide domain, an enum's
tag checked at its own width; a float to an int meets two bounds —
the target's least, and one past its most, a power of two the
format holds exactly — and a NaN fails them both, the one check
that catches a value which is not a number by asking a question a
comparison can answer. Every failure is one call — std's panic,
the abort behind it — and the message rides the data segment once
a compile, deduplicated beside the floats. `-r` rides `-s` and
`-c`: release, the checks out, the wraps a release owns
(`01-types.md`). The runner knows both modes — a `.release` beside
a test compiles it with `-r`, an optional `.stderr` holds the
bytes a panic must write there, and abort's own exit, 134, is what
the `.expect` names.

The operators are trait calls now, `07-operators.md`'s sugar made
real: `a + b` is `Add::add(&a, &b)`, the body pass rewriting the
operator in place where the built-in table's domain ends — both
sides places of types the table does not hold, a scalar pair
(numbers, `bool`, pointers) staying the language's own answer,
the mixed pair its own error. The four arithmetic ones become
their `Add`/`Sub`/`Mul`/`Div`, `==` the `Eq::eq` itself and `!=`
the negated one, and the ordering operators `Ord::cmp` read
against `Ordering::Less` and `::Greater`, each way round its own.
`%`, the bitwise ones and the shifts are language, not traits —
the rewrite has no row for them, and the operator's own error
answers. The compound arithmetic forms ride the plain assignment:
`a += b` becomes `a = a + b` under the same rewrite, the
operator's call built first and the assignment wrapped around
it, so every check the assignment owns is still its own. The
operands walk as places: the move the entry read made of a
non-Copy operand unwinds before the rewrite takes its address —
the operator's own words only borrow, and what a borrow touches
was never moved — so `v + n` leaves `v` readable after, a `Drop`
impl on it notwithstanding. std::ops is here with it: `Ordering`
and the six traits, `Rhs` defaulting to `Self`, riding the
sysroot — the rewrite spells the whole path, so the operator
needs no use, the call spelled out still needing one
(`11-namespaces.md`).

Two bugs the milestone's tests flushed out, both older than it.
A trait method call froze its receiver and never thawed it: the
dispatch took its snapshot of the arguments' borrows after the
receiver had walked, so the picture held the freeze itself and
the restore put it back — the receiver stayed borrowed past the
call, the second operator over the same operands the one to name
it, the hand-written `Trait::method(&recv, ..)` no safer. The
picture is taken before anything walks now, the receiver's
borrow unwinding with the call like any argument's
(`01-types.md`). And the checker had accepted an enum's `==`
from the start — `Ordering`'s own compare rides it — but emit
never carried it: the comparison loads each head's tag at its
own width and compares now, a payloadless enum exact, and one
with payloads refuses with its trait named (`07-operators.md`).
Two sides stay open, both older than the milestone too: a
literal or a temporary operand — `v + 3`, `make() + make()` —
cannot be borrowed, the language's `&` temporary not spelled
yet, and the words refuse honestly; and a non-Copy place read on
an assignment's left is the move chapter's own bug, the
hand-written `a = Add::add(&a, &b)` its own evidence
(`03-move.md`).

The built-in impls are here, the spec's promise kept as source:
std::ops carries the compiler's own table — the arithmetic four
for every number, `Eq` and `Ord` for the integers and the `bool`
— fifty-eight rows, each the plain words the operator's sugar
names. A spelled call runs the row (`Add::add(&n, &m)` is `n +
m` said the long way), and the pairs the language holds no row
of its own for find their trait's: `bool` against `bool` orders
through `Ord` now, the language never comparing bools. The
floats stay out of `Eq` and `Ord` — a NaN equals nothing, itself
included, so the laws do not hold, the calls refused, the
operators answering as the language always did
(`07-operators.md`). A mixed scalar pair is the language's own
error still: the scalar rows are all `Rhs = Self`, so nothing in
the table answers, and the rewrite's words would only misdirect
the report. A pointer's `Add<usize>`, the spec's own mixed
`Rhs`, is the one that will ask the question again — its casts
not carried by emit yet (`01-types.md`) — and a project's own
impl over a built-in is the conflict the coherence check names:
the compiler's table is source, and two sources cannot spell
one row.

Both sides left open at the milestone's edge are closed now,
each its own change of shape. The temporary's `&` gains its
address: a value with no place of its own — a literal, a call's
answer, a sum — is materialised, a nameless slot the statement's
own block holds, the binding dying with the block so the pointer
it lent has nothing left to conflict with, the nesting `&(&3)`
as ordinary as the flat form, a loop's round reusing its slot as
any let does (`01-types.md`). The operator's rewrite and the
hand-written call both reach it — `v + V3{..}` and
`Eq::eq(&x, &(y - 1))` one grammar now — and a `&mut` temporary
stays refused, honestly: the materialised slot is a frozen
share, and a mutable borrow of it is not the answer — the mut
wants a place, and only a place.

The assignment's left is a place the store writes, not a value
the read moves — the walk's own value read had marked it dead
before the store could revive it, so the plain `a = W{..}` lost
its `a.n`, the hand-written `a = Add::add(&a, &b)` poisoned its
own right, and `a = a` tripped the very mark it had just set
(`03-move.md`). The marking unwinds at the read and the store
restores the binding to the alive its value gives it — one line
each side of the store, bare locals only, for a field's chain
may carry a partial move the store must not erase. The chapter's
destructor question stays where it was: the assign path inserts
no drop of its own, so the fix adds no inconsistency — the full
answer is the move chapter's to finish.

And the temporary's test flushed a third out, older than the
milestone too: the checker's value read moved unconditionally,
so a Copy's read under a shared borrow was refused — `let p =
&a; let y = a * 1;` the words against the law, a copy not a
move-out and a share tolerating it. The read asks the type
first now (`01-types.md`), and none of the four hundred
twenty-six had leaned on the old conservativeness — nothing
regressed, the check simply said less.

And then the operators turned round: the milestone's own review
asked what a right value would do, and the answer the language
took is the one Rust took -- the operands enter by value, the
left one moving where its type is not Copy, the right one a
value the parameter's own slot takes whole (07-operators.md).
The table turned out to be standing on ground already laid:
associated types were 05's own feature whole -- a trait declares
`type Output;`, an impl binds it, `Self::Output` resolves
(`05-traits.md`) -- a by-value `self` had its precedent in
`Drop`'s own signature, and the pointer was already Copy. What
the turn asked for was the signatures themselves: the arithmetic
four carrying an `Output`, the comparisons deciding outright
(`cmp` and `eq` answer `Ordering` and `bool`, no Output of their
own), and a compound of its own -- `AddAssign` and its three
siblings, the left borrowed for the write, the right a value,
the trait the in-place open item had been waiting for.

The rewrite turned with them: `a + b` is `Add::add(a, b)`, and
`a += b` is `AddAssign::add_assign(&mut a, b)`. The operand
unmarking kept its place for a new reason -- the walk's first
read had marked the left moved, and the re-entered call reads it
again: the mark unwinds so the second read is the one move that
stays. The materialisation left the operator's path entirely --
a right value enters the parameter's slot directly, and the
hand-written `&` of a temporary keeps its slot for itself
(`01-types.md`). A non-Copy left operand now moves, and the
words are honest about it: `v + v` is the moved report, and `a =
Add::add(a, b)` moves the left out and stores the answer back
-- the assignment's own revive (03-move.md) serving the new
shape as it served the old. Two old walls showed themselves on
the way, both older than the turn: a `*mut` parameter's field
is not a writable place yet, and a non-Copy store to a borrowed
place wants the take the move chapter has not written -- so a
non-Copy `AddAssign` cannot spell its body today, the open item
that carries (`03-move.md`, `07-operators.md`). The spelled
borrowing call -- `Add::add(&n, &m)` -- refuses the way Rust's
does: the pointer's rows are an impl of the operator's own
trait, and they wait on the pointer as a Self (07-operators.md).

Then the rows arrived, and the last sentence turned past tense.
A generic row -- `impl<T: Add + Copy> Add for *T` -- is one
impl where another language writes a macro over a matrix
(`14-macros.md`), and Cerium's lack of lifetimes is what makes
it writable at all: Rust's core cannot blanket-impl `Add for &T`
because a lifetime would ride the row, and here nothing does.
The `Copy` bound is the row's own law, not a convenience:
reading `*self` out of a shared borrow is a copy or it is a
move out of one, and the latter has no spelling (`03-move.md`)
-- so a type carrying a Drop borrows no row, and the call says
`no 'Add' for *W` without ever reaching the body nobody could
have written.

Landing the row asked three things of the compiler, none of
them large and each of them a hole a different pass was keeping.
The trait's own `Rhs` stayed a bare parameter through the bound
walks -- `selfsubst` replaced `Self` and nothing else -- so the
declared signature met the argument as a stray `Rhs` the call
could not place; the defaults machinery (`dflttail`) already
knew the answer, and the bound walk now fills the tail the way
an impl head always had. The projection the row answers with --
`type Output = T::Output`, `T::Output` the parameter's own
bounds resolving to a `typroj` -- walked through `gsubst`
unchanged, so an instantiation carried `Add::V::Output` into
the emitter as a type nobody could lower; the walk now opens a
projection over a concrete Self the way a spelled name always
could (`projopen`, the impl table's own answer), at the call,
at the re-check, and at the emit's instantiation close. And the
coherence walks read a bound's trait name with whatever
namespace the checker had served last -- std's own row naming
`Add` from `std::ops` while the cursor sat on the last file
read -- so the walks now restore each impl's own file, its
namespace and uses beside the path its diagnostics print; std's
files take a `use std::Copy` for the same reason, a library
not reading its own face (`12-projects.md`).

What the rows bought is the whole story of the turn's last
open item closing except its pointer half: `&a + &b` works over
every Copy element, the std's and a user's, the comparisons
borrowing the same way (`Eq` deciding, `Ord` ordering), and
`impl<T> Add<usize> for *T` -- pointer arithmetic -- is the one
row still waiting, its body having no expression to spell until
the emitter takes a pointer beside an integer
(`07-operators.md`).

Then the rest of the table landed, and the open item's first
word closed whole: `Rem`, the bitwise three, the shift pair, and
unary `-` -- `Neg` -- seven traits over six files in
`std::ops`, each the borrowed row's shape the turn already knew.
The compound family closed the same way the lexer spells it:
`<<=` and `>>=` exist as tokens, so `ShlAssign` and `ShrAssign`
ride beside `AddAssign`, while `%=` `&=` `|=` `^=` are nobody's
tokens and no Assign waits for them -- a trait without its
operator is a trait nobody calls. `Rem` writes integers only,
the remainder an integer's own idea, a float pair keeping the
language's error; `Neg` takes the floats too, negating one its
own arithmetic. The rewrite grew its unary half -- a helper
beside the binary one, `-v` becoming `Neg::neg(v)` when the
operand is no scalar the checker answers itself, so `-&a` keeps
the language's error and a library spells the call.

`bool` asked its own question on the way, and the crash answered
before the spec did: std's bool row for `&` spells `self &
other`, and with no built-in answering a bool pair first, the
walk rewrote the row's own body into `BitAnd::bitand(self,
other)` -- the row calling itself, the stack overflowing, the
process dying at 139. The integers never showed it because the
built-in table takes them first, the same shield the arithmetic
four ride; the fix is the shield itself extended, the table
taking a bool pair for `&`, `|`, and `^` -- the one-bit integer
taken whole, the rows beside them for the spelled call -- while
`%` and the shifts keep their integer sense, no bool row ever
written.

And the re-check's own context hole surfaced with the rows, the
one 07d's test could not see: a generic row's body re-checks per
instantiation in the emitter, and the walk read whatever
namespace the caller's file had served last -- std's row naming
`Rem::rem` from its own body while the cursor sat on a user's
file that bound nothing of it, the report a mixed position, the
caller's path over the row's own line. The 167 test had used
every trait, so the caller always bound them; a use-less file
was the reveal. The fix rides the Sym itself now: every
declaration carries its own file, pass 1 and pass 3 filling it,
and the instantiation's walk switches to it -- namespace, uses,
the path its diagnostics print -- the same law the coherence
walks took in 07d, the emitter's half arriving with it
(`04-generics.md`). The sysroot's completeness face grew the
nine new names beside the arithmetic seven's, so a broken
sysroot says so at its own face rather than an unknown name's.

Then the last operator row landed, and the milestone's oldest
waiting word closed: pointer arithmetic. The checker's binop had
taken a pointer beside an integer since the spec's own first
draft of it; the emitter had not, and `p + 2` died at qbe's face
-- an add with a pointer on one side and a w on the other, no
width to share. The emit takes it now, both spellings: the
amount widened whole to l, scaled by the element's own size,
the pointer stepped by the product; the compound the same walk
in place. Any integer steps -- the table's own answer, the
`i8` amount no stranger than the `usize` one -- and a voidptr
is no Typtr and never arrives: no element size to scale by,
the operator's own trait refusing where the language holds no
row either.

The rows spell it in source, four of them: `Add<usize>` and
`Sub<usize>` for `*T`, the compounds beside. The body is the
language's own pair -- `self + other`, a pointer beside an
integer, the built-in table answering it -- so the row never
answers itself, the shield the integer rows ride; and no Copy
bound rides it, the body dereferences nothing. What the body
of the open item's own words had feared -- no expression to
spell, `*self + other` the call the row would answer -- was
the deref nobody asked for: the row steps the pointer, it
does not read through it.

The specificity walks surfaced their own context hole with
the rows: the emitter's pick -- `implfind`, ordered by
`implspecific` -- reads the bounds' trait names, and read them
in whatever namespace the caller's file had served last, std's
`T: Sub` resolving against a user's file that bound nothing of
it. The coherence walks had taken the file-restoring law in
07d and the instantiation's re-check in 07e; this was the same
function's third consumer, and the fix is not another restore
but a cache: pass 3 reads each impl's bounds once, in its own
file, and the table rides the Sym -- every specificity walk
after, coherence's or the emitter's, reads what pass 3 wrote.

And the pick itself showed what the milestone leaves open:
`implfind` reads the receiver alone, the trait's own
parameters never narrowing the candidates -- so the spelled
call `Add::add(p, n)` takes the borrowed row's signature over
the mixed one's, the mixed row waiting on a fix that reads the
trait's parameters in, at the call from the arguments, in a
bound from what the bound names. The operator's own path never
notices -- the binop answers the pointer pair before any row
is asked -- and the pair's `-` keeps the borrowed row's own
answer, the elements' difference, the spec's "no subtraction
of two pointers" now saying what it always meant: no distance
(`01-types.md`).

The bound's own arguments came next, the open item's other half.
The parser had kept them all along -- `T: Add<usize>`'s `<usize>`
sat whole in the bound's own segment, for no one to read: every
bound walk answered the trait's name alone, so a bound that
spelled `Add<usize>` was satisfied by any `Add` row at all, and
one that spelled a trait the file never bound passed silently
too. The spelling is the constraint now, and the spec says so
(`04-generics.md`, the section written with this): the row that
answers must take the arguments the bound named, whatever its
receiver fits.

The reading happens where the bound was written. Every consumer
of a bound runs under whoever called it -- the call's binding
check, the generic branch's signature, a projection's hunt, the
impl table's own bounds -- and a name reads that caller's
context: the fourth consumer of the family 07d's file-restoring
law grew for, and this one takes the cache 07f took, on the
bound's own node this time -- the trait's Sym and the arguments
it spelled, read once at the declaration, in the declaring file,
before any signature that might read them resolves. Free fns and
plain types ride the same walk, so an unknown trait, a trait's
parameters overspelled, a tail no default can fill -- all
diagnosed where the bound is written, no longer silently
ignored.

The spelled call reads the spelling into the signature: the
bound's arguments the prefix, the trait's defaults the tail, the
Self of the call the parameter under the bound -- and the method
sugar takes the same filling, which closes a hole the sugar had
all along: `a.add(b)` under `T: Add` died on a naked `Rhs`,
selfsubst never reaching the trait's own parameters. The binding
check reads them into the row selection: the row's own head must
take them, the same pattern walk its receiver takes, the row's
variables bound against the arguments -- so the borrowed row's
`*T` no longer answers a bound that asked for `usize`. The tail
a bound left unspelled tightened with it: the trait's own
defaults, this type the Self they read -- `T: Add` asks
`Add<T>`, the row's own Self, what `Rhs` always meant. The
diagnostic names the ask now, `'u8' does not implement
'Marks<i64>'` saying which Marks, and the copy walk reads the
cache too -- a `Copy` of the file's own making grants nothing,
the Sym's identity the promise, not the name's spelling.

What remains is the call's own half: implfind still reads the
receiver alone, and a spelled call's other arguments never
narrow the candidates -- `Add::add(p, n)` with `n: usize` still
takes the borrowed row's signature over the mixed one's. The
bound's half is closed; the call's waits on walking the
arguments ahead of the pick, without the freezes the walk takes
(`07-operators.md`).

The call's own half closes the same way the bound's did, with
the row's claim read where it lives. implfind answered a spelled
call with one row, the receiver's pick ordered by specificity
and never asked about the arguments -- so `Add::add(p, n)` with
`n: usize` died on the borrowed row's `*T`, the pointer's own
row waiting under it unheard. The fix is the overload chain's
own discipline, moved one table over: the rows the receiver
fits, collected in specificity order (`implcands`, the sugar's
own `traitcands` beside it), and walked one by one -- the first
row whose signature takes the call's arguments is the call's, a
row that does not take them steps aside. The sugar walks the
same rows (`p.add(n)` now reads the arguments into its pick
too), and a generic's re-check lands on the same walk, which is
where a `step<T>` over pointers really answers.

A row that steps aside must leave nothing behind. Its argument
walks freeze borrows -- unwound, as any call's are -- and move
what they read, and a move left standing would poison the next
row's walk: the second W row of the test reads the `D` the
first row moved, and without the unwind it reports a move the
program never made. The moves ride a snapshot now, one dead bit
per binding (`movsnap`/`movrestore`), taken where the freezes'
own pictures are -- and nothing else in the walk needs one:
narrowing is the branch forms' own, a let is a statement, and
the rewrites an expression walk makes are idempotent, the next
row's want writing over them. The overload chain itself was
leaving moves behind the same way, `f(d, x)` failing one
signature after walking `d` and reporting `d` moved to the next
-- the hole the snapshot was built for, closed in passing, the
one mechanism serving both tables. And when no row takes the
arguments, the report is the first row's own: the walk runs
once more over the most specific row in earnest, and the
diagnostic says what the receiver's pick alone would have said,
`'add' wants *i32 here, this is usize` -- the same words the
single-row call always had.

body.c had grown past its shape: 4882 lines, the walk and three
sublanguages beside it -- the @ builtins, the operator table,
the patterns a match arms. The split follows the seams the
section markers had already drawn, the same discipline as
flow.c before it: the shapes the language spells itself --
@cast, -a, a + b, each read by hand and rewritten into the
call it answers -- are operators.c now (07-operators.md,
08-reflection.md), and the patterns a pattern fits and the
match that arms them are patterns.c (09-match.md). body.c is
the walk itself again, 3761 lines and one concern, and body.h
is the face the four files share.

The face is wider than the section markers promised. The walk
calls nearly every helper the operator table had built for
itself -- the comparison chain reads binop and opscalars, the
in-place rewrites assemble their calls with opnode and
opborrow and opvpush -- so the whole table is shared,
fourteen functions, and nothing private is left in the new
file. That is the honest shape: the spelled surface is the
walk's own vocabulary, not a module behind a door. No behavior
moved -- the 450 stand as they were.

The row's identity is its trait's arguments beside the type it
is for. The overlap check had been reading half the row -- two
impls of one trait were compared by their for-type and their
bounds alone, so `Add for S` and `Add<usize> for S` were
rejected as one row written twice, and the pointer rows of std
were coexisting on their bounds difference alone, the argument
that really told them apart (usize against *T) never read. The
declaration's order and disjointness grew the arguments: one
specification walk over the whole row (`rowspec`, the variables
binding across the argument positions and the for-type
together), one disjointness read where any position that
already disagrees settles it. Concrete arguments that differ
are disjoint rows; an argument a variable can bind orders
instead, `impl<T> Add<T> for S` standing under `Add<usize> for
S`; the same shape with a variable either can bind is still the
conflict it always was. The call-site specificity order
(`implspecific`) runs the same walk now, so the rows the
receiver collects keep the order their declaration was checked
under. One shape of row stays out of reach on purpose: a row
whose variables live only in its arguments (`impl<T> Add<T> for
S`) is declared legal but no call reaches it -- the candidate
walk binds a row's variables from the receiver alone -- and
that binding from the call's own arguments is the open item
beside the destructor half.

The assignment half of the destructor story, the first cut of
07's remaining half. The left of a store is a place now: Nassign
walks it with rplace, the same place read a field access takes,
so the store never moves what it overwrites -- and a non-Copy
AddAssign can spell its body at last, `*self = D { ... }` the
borrowed-place store the move chapter legislated but the walk
could not say. The old value's destructor rides the same
statement: the checker resolves the place's own Drop row and
hangs a pre-made call off the assignment, and the emitter runs
it between the new value and the store -- the new value fixed to
a temporary first, for `x = x` hands the destructor the very
place it reads, and what the store writes back has to survive
it. @take is emitted on the same page: the value out to a slot,
the zero block back over the place, and the checker spends the
inline borrow the argument made, as a deref always has -- the
std sugar `take(x)` is `@take(&mut x)`, and a sugar that froze
its operand past the take would be no sugar at all.

The new caller exposed two holes rplace had carried since it was
only a base-chain reader. Its Tstar case read the pointer
without the spend wrapper, so `*@field(p, "z") = 40` froze p
inside the very walk that would hand the store its address --
the second walk the store's own checks make found the freeze and
reported it; the wrapper the value read and the writability ask
both carry closes it. And its index case never walked the index
expression, because a base chain hands the outer node its own
walk -- an assignment's left has no outer node, so `a[three()] =
9` reached the emitter with an index that was never checked, and
the emitter reads an index's type before it reads the element.
The walk the value read takes moved in, and with it a latent
crash on main became a fix: `m[i][0]` with a runtime i was
checked nowhere -- the base-chain read left the inner index to
an outer walk that did not exist -- and now both walks read the
whole chain. The A row of the place walk answers a range index
with the slice view itself now too, the same type the value read
gives, so the store's own report on one names the slice it
cannot write.

One discovery on the way, worth its words: the compound
assignment's rewrite spells `std::ops::AddAssign` whole, so the
row a user's `a +=` reaches is std's trait -- an AddAssign
declared beside the use site never enters, and the impl has to
name std's own (`use std::ops::AddAssign; impl AddAssign<D> for
D`). That is the design -- the operator needs no use
(11-namespaces.md) -- but it is the first place the distinction
is user-visible, and the probe that declared its own trait
chased "no 'AddAssign' for D" through two Syms of one name
before the three-segment path in the rewrite said what it was
doing.

The scope half of the destructor story, and 07's operators
milestone closes. Spec 03's Timing section is written whole: a
binding's destructor runs where its scope ends, reverse
declaration order (a later binding may point into an earlier
one's storage, so the earlier outlives); a moved-from binding
runs nothing -- the move check's rejection is what buys the
insertion's purity, no "was this moved?" flag at runtime; the
early exits -- return, break, continue -- reach a scope's end
as surely as the closing brace does; a bare temporary on its
own statement drops at that statement's end; a panic aborts
and runs none of it.

The discovery that shaped the implementation: hasdrop already
existed, whole. The iscopy rejection had carried the full
judgment since the move milestone -- a struct's own row or any
field's, recursed through generic substitution, enums, tuples,
arrays; unions, scalars, pointers, slices never -- so the
scope half needed no new judgment, only the expansion of the
answer into the calls themselves. dropcalls does that: a type
with its own row answers one pre-made call; a struct without
one walks its fields in reverse; an array of N drops N indexed
places; a tuple drops the rows it has; and an enum -- the
interesting one -- hands back a match the checker built, one
arm per variant, the payload bound to an invisible `.v` name
whose own drops ride the arm's block, so Option<Option<File>>
recurses for free and the emitter runs the whole thing through
the machinery matches already have: the niche test, the tag
read, the payload addressing -- no new emit at all.

Three fears the exploration dissolved, each by a mechanism
already there. A match arm's fail path skips its drops --
emapat jumps before any binding is established, so a
scrutinee that matches nothing leaks nothing. The enum's own
storage cannot double-drop -- the arm's binding takes the
scrutinee over with the dead flag patterns have always set,
and the block's drops filter on it. And a generic drop
instantiates -- the pre-made call carries its type arguments,
so instensure queues the instance the way any spelled call
does.

The calls hang at every ending a value has: a block's close,
the three early exits, a statement's bare temporary, a `for`'s
pattern bindings once a round, the function's own parameters
before the ret. The `for` splits deliberately -- the body's
block owns the bindings inside it, the loop owns the
pattern's, and break's early exit carries both, the paths
never overlapping because the block's exit is the path break
never takes. The assignment's pre-drop grew the same
expansion: 07a dropped only a type's own row, so a struct
with fields that had rows and no row of its own dropped
nothing on reassignment -- now the store walks the fields a
scope's end would.

Five run tests hold it up -- scope ordering and moved-from,
the early exits, temporaries and parameter slots, the
structural walks (fields, arrays, tuples, the assignment's own
upgrade), and the enum's conditional rows -- and 175's stdout
grew a fifth character: main's close now drops the binding the
assignment test had left alive, which is the new semantics
arriving, not a regression.

The last open item of 07, and the milestone's close: a row whose
variables live only in the trait's arguments -- `impl<T> Add<T>
for S` -- was declared legal and ordered under its specific rows
since the specificity work, but no call reached it. implfit
matched its pattern with every slot demanded landed, and a `T`
the receiver cannot land failed the row before the candidate
table ever saw it. The fix is a split: implfitp matches the
pattern alone and returns with the slots open, implfit keeps the
old whole judgment for the walks that follow no arguments --
inherent lookup, impl satisfaction, the bound side -- where a
half-landed row would be a lie. The trial owns the landing now:
each row joins the walk with its binding copied to a local
buffer (the candidate's own slots must not hear a failed row's
pollution), the arguments unify against both families at once --
the member's and the row's, gunify already blind to parameters
not its own -- and the row's words stand until every slot lands,
because gsubst through a NULL slot builds a NULL type. What the
receiver lands and what the arguments land meet in one check of
the bounds, once, after.

Two bugs of the change's own making, each caught by a test
already there. The first reversed argfit's rule: a signature
with no family to bind -- the specific row of 173's disjoint
probe -- let a mismatch through, because "both families unified"
was written as "no family failed". No family is the old rule's
rejection: nothing rides the arguments, nothing can save it.
The second mutated the signature the row shares with every other
call -- `t->t = gsubst(...)` in the trial -- and the first
instance's return type fixed itself into the shared node, the
second call's recheck reading the pollution. The row's
substitution is a local walk now; the shared signature never
hears it.

And one gap the change opened, closed in the same breath: a row
whose trait argument is a shape (`impl<T> Add<Wrap<T>> for S`)
hands the literal a want carrying the row's own parameter, and
the literal's instantiation took that borrowed parameter for a
binding -- the field value then failing against a slot it should
have owned. The want lends; it does not bind: where a lent
parameter stands in a value's way, the value's word outranks it,
the slot returns to the open and the fields bind it. The phantom
case -- a want naming a parameter no field mentions -- keeps its
lending, as ever: the fields never ask.

And a confession the probe earned the hard way. The change's
first expect said 1437, the binary exited 130, and the
arithmetic that explained the 130 -- 1410, the sum under the
body the rewrite had just changed, mod 256 -- cracked a colder
case open: the "emit bug" an earlier chase had convicted, a fn
returning an aggregate whose field read back as 400-turned-144,
ten experiments and a clean-main reproduction deep, was no bug
at all. An exit code carries eight bits; 400's low byte is 144,
and every "garbage value" in that probe's table was a true
sum's mod 256. No issue to open, the aggregate return was right
all along -- the lesson being to read an exit code as an exit
code before reading it as a value.

Four tests: 182 walks the sugar, the spelled call and the method
sugar over one pair of rows -- the bare `Add<T>` beside the
shaped `Add<Wrap<T>>`, the specificity order handing each call
its own; 183 races a generic row against a specific one through
the method sugar; 209 names the variable no argument rides; 210
catches two arguments binding one variable to different types.
464 green, and 07 is closed.

The 12-projects list named ?'s propagation, and the change that
landed it owns no emitter line at all. `f()?` *is* the match the
spec spells it as (01-types.md) -- the value through, the error
handed back the way a return hands anything -- so once the E?T
checks pass, the checker says so in place: the Ntry node becomes
an Nmatch, the operand its scrutinee, the arms synthesized with
bindings no lexer can spell (`.ok` and `.err`, the dot keeping
anything a program declared from meeting them), the Err arm's
body a block whose return the frame's drops ride out on
(03-move.md). Everything downstream was already there -- a
non-Copy scrutinee's take is the match's own move-out, the early
exit's drops are the return's, the Err arm's construction takes
its want from the fn's return -- the enum constructor's rewrite
and mkdropmatch the two precedents for a node changing kind
under the checker. The emitter never learned a thing.

The first cut earned a bug worth its paragraph: a double walk.
The old case read its operand once for the checks, value-wise;
the rewritten match reads its scrutinee again, place first --
and a value read of a non-Copy binding marks it moved, so the
second read reported `'r' has been moved` in every generic fn:
a black-box T is no Copy, `Error?T` moves on the read, while
the concrete `Error?i32` copies and hid the whole thing from
the non-generic smoke. The fix divides by shape. A place operand
lends its type as a place (rplace reads, nothing moves -- the
match's own walk is the one move, its scrutinee take the ?'s);
a computed operand -- `pass(r)?` -- is left to the match, whose
rplace-then-rexpr fallback is the only value walk it gets, the
checks running behind on the type that walk left on the node.

And a gap the probing tripped on the way, older than the change
and left for its own issue: a fn's tail expression is never
reconciled with the return it declares. The explicit return
reconciles ("the fn returns %s, this is %s"); the tail does not
-- `fn f() -> i32 { true }` compiles, the want only a hint to
inference, and a mismatched shape rides through to emit where
the assembler rejects what it finds. The nested `?` type sugar
(`Error?Error?i32`) does not parse either, one more for the
list.

Four tests: 184 walks the value through, the short circuit, the
computed operand, a generic chain and two tries sharing one fn;
185 pins the drops, the short circuit's own against the closing
brace's, one digit on stdout either way; 211 catches the error
the fn does not hand back; 212 the second take of a moved black
box. 468 green.

The overlap read met its first mixed pair and fell over. An
inherent row's arguments are its type's own count -- a trait's
rows all take the trait's (the defaults filling the tail), an
inherent row's its type's, and a bare `impl Pair` carries none
at all, its args a NULL. The read walked one row's count against
the other's half, indexing that NULL: any file writing an
inherent impl of its own died in rowdisjoint against
std::meta's is_same, the library's two generic rows riding in
every project. No test had ever written the shape -- the
library's own is_same pairs both carry two, equal, and equal
counts pair safely.

The fix takes the count from the pairing itself: the arguments
compare only where both rows carry the same count of them, the
way disjoint's own struct case already reads ("resolve already
shaped them; stay unproven"), and rows that disagree on the
count can only meet through their types -- the read's last
line, which a differing type name answers outright. 186 writes
the mix into a golden: a bare row, a generic one unbounded (a
bounded one the bounds' own ordering parts, before the read),
the predicate's read whole through it all; 213 pins the
conflict the read must still name, two rows for one type
neither disjoint nor ordered. 470 green.

And the std's own writing asked the question the table could not
answer: a bound about a type the fn itself holds only as a
parameter. `print<T: Fmt>` handing its T to `fmt_to<T: Fmt>`
asked the impl table for T's Fmt, and no row carries a parameter
-- the ask died as `'T' does not implement 'Fmt'`. The spec's
own words were the fix's: everything the body does to T must be
justified by a bound, checked once at the declaration -- and
passing T on is done by that promise. implsatisfies now reads
the parameter's own declared bounds when the type is one, the
spelled arguments matched one for one, the tail falling to the
defaults the way any ask's does; a bound the declaration never
made still fails it. 187 chains the asks three deep and hands a
spelled `Marks<i32>` through the same door, the concrete
instance still walking the table; 214 the ask the declaration
never made. 472 green.

And the linker named the last of them: a call however many
namespaces the name walked fell through the direct-call branch,
which read one bare segment only, into the value-call path -- the
address of a symbol, taken for a call through it. A plain fn's
symbol is its own and the call linked by accident; a generic one
has no symbol at all, only the instances the checker's picks
spell, and the linker asked for a fn that never was. The branch
now walks the namespaces off the name the way every other path
read does (11-namespaces.md), the prefixed name no local shadows
-- the walk chose it -- and the checker's pick names its
instance. 188 calls a generic through the path and bare, a
second instance two bindings wide, one project's own box.

And the leaks the uses named. std's own files had never written
one -- panic a prefixed call, the traits nothing -- and fmt and
io arrived with theirs, and every project that took a name the
root's way fell over files it never wrote. Three reads held the
root's face open to anyone: symfind's table unconditionally,
nssubfind's sub-namespaces the same, and the use's own collision
check, which asked the root's table whether a glob could bind --
a user's Option reached std::fmt's `use std::*` and the project
died in the library. The spec's model was always the other one:
a name is looked up in the current namespace, then what a use
brought in, and the root's own items and sub-namespaces are the
root's own files' bare names -- a sibling is named with no path
at all, everything else absolutely, `::name` the one spelling
(11-namespaces.md) -- and a glob's collision is with the use's
own namespace, the shape the pub use's check already had. All
three reads close the way the spec reads: the root's face is the
root's files' own, whichever of the three tables it lives in.
191 takes every name the root's way and reads them from a
namespace below -- the glob binds, the bare Err falls to the
sugar a declare a file away never shadowed, and a parameter b
stands unclaimed by the root's sub-namespace of the name; 215
the collision the check still owes, a namespace's own declare
taking its glob down, the check moved, not removed. 475 green.

And with the name space whole, the library itself: std::fmt and
std::io, the language's first print. The Write trait takes the
bytes -- the process's two doors behind it, the C write the
unmangled door both halves and panic reach for -- and the Fmt a
type implements to say itself, composed from the Writer's typed
writes: str, bool, u8, and the u64 the digits live in once, the
signs and the narrow widths widened on the way in. Every write
hands its Result back -- ? the composition, the trait's own
shape the propagation's -- and the count is the Writer's own
bookkeeping: a write answers its own segment's bytes, the running
total kept in n, and written() reads it out, an impl's tail and
the prints' own answer, the whole print in one number however
many writes it took. fmt_to joins any Fmt to any Write, print
and eprint the two doors.

Writing it was the point: the library is the features' first
consumer, and the compiler broke where nothing had walked.
An inherent impl alongside a generic one took the process down
(#85); a bound's ask about a parameter the table could not
answer (#86); a call however many namespaces the name walked
fell to the value path (#87); and the root's face stood open to
every file's read (#88) -- a user's names reaching into std's
own, the project dying in a file it never wrote. Three gaps
stayed open, each named and left for its issue: a mutable
slice does not yet hand itself to an immutable parameter
(01-types.md), so the digits go out a byte at a time; the byte
literal lexes and parses but no body reads it, so 48 casts its
way to '0'; and a &mut dyn borrow never thaws, so the tests
count from the prints' own answers and read the bytes from the
door, not the sink. 189 prints pairs and flags and a u64 at its
twenty digits, the counts asserted bitwise; 190 a Fmt of the
test's own into a sink of the test's own, stderr golden both
ways. 477 green.

The review reshaped it, each ask a corner of the same library.
The glob is gone from std -- a std file names what it takes, the
brace form where two come from one place -- and what it takes is
spelled: Result from the root, Error and Write from io. The
platform's own call moved home, std::sys its own namespace with
the C write declared the ABI's way, the door every output and
panic reach through; io keeps the doors the process was born
with and the prints, the Write they all take a file of its own
beside them. The Error carries the errno now, the C write's
negative answer negated back -- Linux answers -errno, and the
reading is the Linux one. And the Fmt returns nothing: its work
is done when its writes are, the count the Writer kept all
along, read out by the print that owns it -- the segments'
numbers stay with the writes, the whole print's with the print.
The literals go straight to the writes that take them; a single
character is a write_u8 away, the byte literal it wants still
a later milestone (#91). 477 green, the golden bytes the same.

The second pass took the same library one home further in. The
Writer is a file of its own beside the Fmt it serves (writer.ce),
the trait's file the trait alone. And the errno is the real one
now: the C library answers a failed call -1 and sets a word it
keeps per thread -- __errno_location is where every C library
agrees the word lives, the errno macro in C the same call spelled
-- and sys::errno reads it out for whoever asks. The Error
carries it as Sys(i32), the system call's own failure, whichever
call it was; the write's own answer is only the -1 that said it
failed. 477 green, the golden bytes the same.

The mangle got words of its own. The old one folded names
together -- a namespace's `_`, a fn's `__` -- and the fold held
ambiguity the identifiers themselves never had: `my::app::parse`
and `my_app::parse` spelled one symbol, the collision told apart
by a twin numbered off the declaration order; an instance named
itself by its arrival -- `Pair<T>` and `Cell<T>`, one method name
between them, instantiated to `ceriwarm__g_i32_1` and `_2`, the
type nowhere in the name, a line moved flipping it. The new one
says every name as a count of its bytes and the bytes themselves
(`12-projects.md`, Symbols): a payload never opens with a digit,
so the count's digits end exactly where the name begins, and the
split is the string's own.

The project's own name is the first segment -- a single file's
project the directory it stands in, however the path spells it: a
bare file and a `.` the shell's own directory, a `..` or a link
what they stand in, resolved to the directory itself, one
directory one name. (The bare filename first said `_` -- the
spelling carried no directory to name -- and sampling the fresh
symbols caught it; realpath is the resolution, a POSIX call glibc
still hides behind an XSI guard, `_XOPEN_SOURCE 700` the door.)
std's root is the std segment itself, the paths below it spelled
from there.

Types ride the same law in a closed code: the primitives their
own bare words -- no one of them another's prefix -- the
composites a tag letter and their parts, a named type its whole
path from the root and its bindings, so `Pair<i32>` and
`Cell<i32>` can never spell the same. And a law the writing
itself found: a length never names a value. Decimal lengths
beside decimal digits read two ways -- `1` `0` and `10` the same
bytes -- so a count (an arity, an array's length) takes a segment
and a value (the const generics) fixed-width hex, the bits as the
machine holds them, a float memcpy'd to its u64, the enum's tag
beside it.

A method's symbol is its target's code, the trait's or the empty
slot, then its own name: `impl Tag for i32` and `impl Tag for
i64` apart by the words themselves, an inherent and a trait impl
of one name apart the same way. The overload is its signature
after the name -- the argument count, each argument, the return --
no twin, no declaration order in a symbol; the instance is its
bindings spelled out, no try-counter, no first-seen. The vtable
data carries the code too, the fourth producer -- the build
itself found it, the first three walked by hand.

The road had its lessons. The encoder never wrote its NUL -- the
arena strings carried a byte of garbage, qbe reading characters
of its own. printvts sized its line by the old names' length --
the new ones half again longer, the heap corrupted, the segfault
at 84-dyn-dispatch's door; the entries are collected and the
length summed before the allocation now. And the primitives first
came out segments of themselves, `3i32` -- the word says itself
better alone.

The tests hold the shape: 192 the namespace pair that once
folded, `my::app::parse` and `my_app::parse` both called, 83 the
answer; 193 the method identity pair, `Pair` and `Cell` sharing
one `half`; 194 a trait answered for i32 and for i64, each call
finding its own; 195 the fold family -- a `*mut i32` beside a
type named `mut_i32`, an array, a slice, a tuple, a pointer, two
arguments -- seven overloads of one `pick`; and a project named
std refused at the door, the name the library's own. 482 green.

The built-ins' own Fmt came next, m9's second step. The rows live
in std/fmt/builtin.ce, apart from the trait's own file -- ops'
rows sit beside their traits (an operator's row is the operator's
meaning, too close to move), but a default format is a
convention the language picks, and the file's own head says so.
Nine rows: the signed and unsigned integers their digits, usize
its own, bool its words, a str its text, a u8 the number it is
-- not the byte -- and the two floats handing themselves to the
Writer's own writes, an f32 widened first, printed as the double
it becomes: 0.1 as f32 says 0.100000001490116, its window honest
about the bits it was.

The floats were the step's own weight. The language has no bit
reinterpretation -- @cast converts, it does not transmute -- so
the specials read off the arithmetic's own face: NaN is n != n,
the infinities n - n != 0. Below 2^63 the fixed form, the
integer part exact and the fraction a window of digits; above,
or below 1e-15, the exponent form, the mantissa carried into
[1, 10) by decade ladders -- each rung a comparison, the
composed power never past the value it measured, nothing
overflowing -- and past 1e-308 the subnormals, where no composed
power fits, the decades counted a multiplication at a time.

The first window took fifteen fraction digits and truncated
them -- and the probe's first run caught it. The ladders compose
their power from four roundings, the division a fifth, and a
mantissa that means to land on one lands a few parts in 1e15
under it: 1e300 printed as 9.999999999999998e299, wrong to the
eye where %.15g says 1e+300. The fix took %.15g's semantics
whole: the window fifteen significant digits, the integer part's
own count eating into it (three integer digits leave twelve, an
exponent's mantissa always fourteen), the digits rounded with
the carry moving the integer (999.9995 the next integer, not a
string of nines), and the mantissa pulled to the decade it meant
to land on -- one within 1e-14 of an edge belongs to the edge,
the pull wider than the ladders' noise, wider than the window's
own rounding, so a carry it would make never reaches a mantissa
of ten. DBL_MAX prints 1.79769313486232e308, glibc's %.15g word
for word; the least subnormal 4.94065645841247e-324, the same.

The writing learned the language's own edges on the way. An if
is an expression and nothing else -- in statement position it
takes its semicolon (97's own shape), and a for, a statement,
takes none: the ladders first went without the ones they wanted,
the rewrite added them where they did not belong. A float
literal is an f32 until something tells it otherwise: p * 1.0e256
is fine, the literal widening to p, but 10.0 - 1.0e-14, two
literals alone, meet as f32s first and the f64 comparison
refuses them; and an if's arms do not adapt to each other, 1.0
one arm and m the other, f32 against f64. @cast<f64>(1.0)
spells the arm that means the double, and the pull's reach
lives in a binding of its own.

The tests hold the rows: 196 prints all nine through the
generic door, then walks every path the floats own -- the
rounding (0.1+0.2 a clean 0.3, the repeating third), the
specials, the ladders both ways, the decade's pull (1e300 on
one, 1e-16 the same), the subnormal dust -- the .stdout the
words, the exit four counts of bytes, a bit each. 483 green.

The prints began in std::io, and the map of the namespaces turned
a circle: io reached across for Fmt and the Writer to print with,
fmt reached back for io's Error and Write everywhere a shape was
spelled, and the two held each other. Nothing broke -- std walks
the whole tree in one parse, no unit waiting on another -- but
the map was wrong, and the wrong was worth fixing before anything
grew on it. The three prints moved across: fmt_to, print_one and
eprint_one now live in std::fmt (print.ce), io a door-keeper's
namespace again -- Write, Error, Stdout, Stderr, each its own
file now, io.ce gone -- and the arrow between the two points one
way, fmt to io, the way Rust draws it. The names changed on the
road: print and eprint are held for the print to come, a string
of a value's own words for many values at once, and printing one
value gets its own honest name -- print_one, eprint_one; fmt_to,
the general join to any sink, keeps the name it already answered
to. The tests read the new names through the same doors: 189,
190 and 196 print as before, 483 green.

The door main stood at was a special case: the emitter spelled
the name plain, the linker took it for the platform's own, and
the arrangement left no room for the wrapper an E?() ending had
been promised -- the check took the shape, the print was "a later
milestone's," and a main that handed errors back answered the
platform with the Result's own bits. The special case is gone
now: main mangles like any other name, the project's own first
segment ahead of it, and the platform's door is a wrapper the
compiler arranges -- an injected #[extern(C)] fn main, a few
lines of the emitter's own text, that calls the project's main
and answers per its shape: nothing the platform's zero, an
integer itself (a long's low half the copy a narrowing cast
keeps), an E?() handed to std::fmt's exit, the Err half eprinted
through the error type's own words and the newline after them,
the code one. The exit lives in std because the match does -- the
Ok/Err walk is the language's own, not a shape the emitter should
copy -- and the instance the wrapper calls the emitter queues by
hand, the same queue a call site's own walk fills. E: Fmt is
checked where the ending is declared, a plain diagnostic at the
main itself; #[extern(C)] on a main is refused, one program one
door. io::Error grew its own Fmt -- the errno's number beside the
namespace's name -- the spec's own example had spelled a main
that could not compile until it did. The pointer-shaped E, the
niche its one pointer is, waits on a method-resolution fix (the
receiver a generic hands a pointer impl); the tests hold the
rest: the Ok half clean, the Err half printed, the errno said,
the wide integer cut to the platform's word. 489 green.

And the prelude, re-cut. The declares-all pass had always made std's
face whole before any read -- the reason a user file's bare Result
resolves -- so "a library does not read its own face" was a stance,
not a necessity, and the stance is gone: the prelude is bound into
every file, std's own among them, the eight `use std::Result;` lines
that decorated io and fmt deleted with it. The face itself is
smaller and truer: the language's citizens -- Option, Result, panic,
what the sugar and the runtime checks spell -- and nothing else.
Copy and Drop moved home to std::ops, the operator family, where the
traits the compiler calls on its own belong: Add at a `+`, Copy at a
move, Drop at a scope's end. They ride no prelude -- a copy is a
privilege a type opts into, the use that names it the file's own
word that it does -- so the thirteen ops files meet Copy as a bare
neighbour and the tests that impl or bound it say `use
std::ops::Copy` like anyone. The checker's own reads moved with
them: the face it takes back names Copy and Drop in std::ops now,
and the sysroot check that guards the family counts them among its
must-be-there. 489 green, the same count with twenty-three tests
newly spelling their uses and one golden grown a line.

The door thinned to one call. main's ending had been the wrapper's
own work -- three shapes of call spelled in the emitter, the integer
one even reading a u64's low half -- and the exit code is an i32,
the platform's own word for it, nothing's low half: the contract
tightened to (), i32, E?(), the wide door a cast's own law now,
spelled where it happens. The work itself moved home to std::entry
-- three runs, one per ending, and the wrapper one call whole: the
main by address, the platform's own two beside it, the entry saving
the two for sys::args, calling the main, answering the ending
(fmt's exit for the E?() one, the Err half printed through its own
words there). fn values as arguments the ABI already had -- a
wrapper handing a fn by address is a call like any other. The words
themselves arrived with the door: sys::args(), one iterator the
asking, a []u8 per word into the platform's own table -- the table
lives as long as the process does, the slice borrows what cannot
die -- next a hand-rolled match, for-let the loop over it, the
Iterator trait's own desugar a later milestone's (10-iteration.md).
The forty-two tests that answered a wider word than i32 now say the
code itself, their expects the same numbers. 491 green.

The review's two asks. The entry fns had a namespace of their own,
and no namespace was owed them: they are three fns and a story, and
the story lives in std's root now (std/entry.ce), beside panic -- no
std::entry to spell. And they are no one else's to call: private to
std, which the language already had a word for -- the use was turned
away all along, the glob never brought them in, but a qualified path
read the table and never asked pub, an openness the spec never
promised (11-namespaces.md: private is the directory's own). The
path asks now, a call and a value alike -- 134's two fns, written
against the openness by accident, say pub and mean it -- and the
wrapper is the three's one caller: the compiler takes the Syms at
its own face-taking, the private face among them.

The Iterator family found its home too: std::iter, the trait itself,
and sys::args' cursor its first library impl -- impl Iterator for
Args, a []u8 a word, next a hand-rolled match until the desugar
lands (10-iteration.md). 493 green, two of them the privacy's own.

The review's second round. The three entry assignments sat in a
block of their own at the face-taking, and a block no story owed:
they are three more of the same kind as the four around them, and
they sit in the row now, the comment beside each its own.

And the hand-rolled slice next built -- two slots written a step
apart, a view pointing one way and counting another between them --
was the last writer the language had: the slots read for every ABI
that hands a pair to C (std's own write the busiest reader), and
from here they write never. @slice is the way a view is born, the
eleventh @: the pointer's reach and the length together, one step,
a *mut T answering a []mut T whole (01-types.md). args' next asks
it now, and the spec's own Iterators -- whose ptr-and-len literals
were a shape the language never grew -- spell it too. 495 green,
two of them @slice's own.

The groundwork under a print. A format string is a const []u8 the
compiler itself must read, a byte at a time, and the evaluator
knew only an array's elements -- a slice's were "a borrow of its
storage", its len the same refusal. Both are the walk's own now:
an index reads a slice's bytes the way a literal's are read, the
bounds the len the view carries, and the len itself folds where a
field read finds it -- the ptr alone stays a borrow, no address
names it at compile time (08-reflection.md). The bytes were in the
Val all along; only the asking had been refused.

Reading them inside a generic asked one thing more. A const slice
argument reached the instance's re-walk as a runtime slot -- the
integer's own fold to an Nint an earlier round spelled, but a
string sat in memory and every read of it was a black box's -- so
the counting body, holes(fmt), never ran. A const []u8 parameter
now folds to the Nstr it spelled, on the value walk and the place
walk alike (fmt.len's fmt is a place), and a runtime position
takes the fold for the literal it is, the data segment its home.

And the counting asked for a fn the two shapes had never met: a
const word with a pack behind it, tally(const fmt: []u8, ...T).
The const-mode test demanded the argument count equal the declared
count, and a pack absorbs any tail -- three arguments, two
parameters, tally never const at all. The test reads a pack's tail
now. The deeper cut sat under it: cvals was sized by the argument
count while the instantiator reads the bake by the declaration's
own -- an empty pack, tally("") with nothing behind it, read past
the array's end and handed the instance a garbage pointer, a
segfault whose cut-short qbe IL named an iterator's instance, the
corruption's nearest neighbour, not its home. The array holds the
chain's widest declaration now, and the empty pack is a row of
none like any other.

The spec owes none of this a word: 08-reflection.md promised a
compile time that is not a separate language -- a value the
compiler knows and a body without side effects is the run itself
-- and this is the implementation arriving. The tests hold the
three: 203 a const string read a byte at a time, 204 a const word
before a pack, and 220 the print's own contract -- the format's
holes a compile-time count, the pack's rows another, a call that
disagrees refused before any program runs, the @compileError the
instance's re-check reports, an .expect of ! in the run suite
where the pack's truths already live (115, 116). 498 green, three
of them this round's.

And the print itself, arrived. `print<...T: Fmt>(const fmt: []u8,
args: ...T)` and `eprint` beside it: a value where each `{}`
stands, `{{` and `}}` the braces themselves, a `}` alone the byte
it is, the count handed back the whole print's. Four private
pieces under the two doors: `shaped` and `holes` the compiler's
reads of the const string, `lit` the runtime walk of a literal
run -- the cursor a value the peel threads, not a pointer it
holds: `lit` answers the index it stopped at, the next unfolding
starting past the hole -- and `runs` the pack's own peel, one
unfolding a value, the depth cap the pack's (04-generics.md). A
method on a pack row (`args[0].fmt(w)`, the receiver a pointer
impl) and the whole pack forwarded through a second generic both
proved out on the first probe -- the method-resolution fix the
door round thought owed did not bite here.

The round's find is a rule the spec had already told. The first
shape held the grammar's words inside the counting fn itself --
`@compileError` behind a plain if -- and every compile of std
fell at it: an `@compileError` is a report at a body's own walk
unless the evaluator has run that body (92's guard lives because
a const initializer runs it before the walk; std holds no call
that runs the counter ahead of its walk). The words moved into
the doors themselves -- const ifs in the prints, the shape a
bool, the count a number, the two reads pure -- and the nesting
proves out: the shape taken, the count taken, neither taken,
each branch its own report, the untaken never walked. 503 green,
five of them the print's own: the bytes through the stdout door,
the same walk to stderr, and the three refusals -- a count that
disagrees, a `{` that opens neither a hole nor a brace, a `{`
that stands alone at the end.


And the holes learned their own words. A `{:...}` between the
braces -- a fill, an alignment, a width, in that order: the fill
the byte the alignment follows (`{:<<6}`, the first `<` a fill,
the second behind it the alignment), the width a decimal run and
a floor, a longer value left whole, never cut. Where the spec
names no alignment the type's own answer stands, asked at the
run: `@typeinfo<V>()` routing -- the integers, the floats and
the pointers right, everything else left, the three languages'
own consensus. The pad carries no heap -- std has none, and a
buffer would cap a str's own length: a counting walk first
(`Count`, a sink that only counts), the fill written around the
value's second walk as the alignment asks, the Fmt protocol
itself never touched, the Rust road taken -- the pad held above
the shape, not {fmt}'s parse/format split below it. The spec
read where the walk stops, a handful of ASCII bytes the grammar
door already asked -- lifting them to the compile time would
thread a const depth through the peel, an order of complexity
for nanoseconds. The round's finds: the first `holes` ate a bare
hole's `}` and the hole behind it (a skip to the close, run
after a two-byte hop already past it -- both cases one rule now:
onto the byte behind the `{`, then to the `}`, the bare hole's
at once); a `for` body may not wear the `;` an `if` body may;
and a struct field answers assignment only where the field
itself says `mut` -- the mutability the field's own, not the
binding's alone. 506 green, three of them the round's: the
twelve lines through the doors -- the width, the three
alignments, a fill, the floor a longer value walks over, the
types' own answers -- and the two refusals: a fill the alignment
does not follow, a width that runs into what it should not hold.

Two rearrangements while the ground is quiet. The Iter family:
`Iterator` the trait's own name shortened to `Iter`, `IntoIter`
the same road -- and the trait's associated type with it, for the
trait renamed would have worn its own name (`type IntoIter:
Iter<...>`); the cursor a container yields is named for what it
is, `type Iter: Iter<...>`, the path `I::Iter`. std::iter's own
trait and sys::args' impl the only code it touched; the specs and
the README walked along, and the tests' own local traits kept
theirs -- a name a project may take, like Option's. And the exit
found its home: out of fmt, where it lived only for the eprint it
made, into entry.ce beside the run it serves -- pub in std's root,
beside panic, the two program-level verbs, for a program may call
it itself. The compiler's asking moved with it (`nsitem(std,
"exit")`, the error message's own words), and the two eprint_one
writes became one eprint -- the string of words the better door,
now that it is. 229 the direct call: the Ok half a zero, the Err
half one with its words on the err door, the code itself the
answer's arithmetic. 507 green, one of them the round's.

And the print's own line closes. Five PRs the arc -- #100 the
groundwork (const strings read at compile time, the const word
and the pack met), #101 the print itself, #102 and #103 the
holes' own words, #104 the exit home and the Iter family -- and
the close is two tests. 230 the boundaries no single line had
asked: two spec'd holes the one format, the peel's cursor
threaded through a spec's end; the empty spec `{:}` the bare hole
it spells; the width two digits wide; bare and spec'd holes
interleaved, the one walk serving both. And 231 the numbered hole
refused -- `{0}` a later milestone's, the spec's own words, and
the boundary now a test that says so. 509 green, two of them the
close's. What remains is written where later milestones live --
`{N}` and its kin, a precision, a base -- the order still the
only binder, the doors still the compiler's own two.

And two of the later milestones give themselves up. The precision
first, a spec's fourth word: a float held to the fraction digits it
asks -- write_fixed_prec, the dot always, the window zero-filled,
the carry the next integer -- and a str cut to the bytes it asks,
the cut the value's own (builtin.ce, not write_str: the specials
would lose their names to a truncating write, "NaN" half gone at
two). The asking rides the Writer's state, both of the pad's
walks, the counting no less, so the width measures the cut value;
`.0` spells no asking, the width's own zero the same way; the
exponent roads unchanged, a wider asking cut to the fifteen a
stack block holds. Then the base, the spec's last word: x o b the
letters, the digits lowercase, the integers' asking alone --
write_int and write_uint read it where the integers are spelled,
the floats' windows decimal's own whatever a hole asks, a negative
its magnitude and its sign. The x an alignment follows is a fill,
the x a `}` follows the base, the grammar's own order the answer;
228's refusing shape moved with the letters, `{:>z}` the letter
that is none. 232 the precision's behaviors, 233 the dot that
names nothing, 234 the base's. 512 green, three of them the
round's. And the later list is down to one: `{0}` and its kin, the
order still the only binder -- that one the language's own doors
to open, a runtime fn that cannot carry a const parameter
(08-reflection.md), a pack whose rows are their own types, no heap
to hold a reorder.

And a compiler bug gives itself up. The static's data symbol
carried its name alone -- `$static.N`, no namespace in it -- where
a fn's mangle walks the whole path; two namespaces' same-named
slots met at the assembler, one symbol for two values, the
linker's own refusal the only report. The fix the fn's law
brought across: the kind, the namespace's path in dots, then the
name -- `$static.std.sys.arg_count` now, a root's `$static.N` the
old shape kept, the dots safe by the identifier's own law. The
const rides the same door and the same fix. 235 the directory
project that pins it: two namespaces, the same names, the statics
and the array consts (a scalar const folds to nothing, its data
line never asked for) each their own symbol, 53 the answer. The
spec's Symbols section says the data symbols' law now too.

And a hole the review saw shallow, the floor beneath it gone.
The record said the tail coerced where a return would not -- the
truth no check at all: runbody and rclosure dropped the block's
own answer on the floor, a fn's tail against its declared return
a comparison nowhere made, the spec's own empty-body error
unimplemented. The check stands now, both doors the same rule --
the tail must be the declared type, a literal that coerces still
may, the message the return's own shape, "the fn returns i32,
this is u32". A closure answers the same door, its annotated
return the fn's declared one. And the dive grew a second arm: a
tail that never lands -- a panic -- is a must-exit the block's
walk now honors, not only a statement's (130's own finding). The
tightening swept the old suite: forty-two tests that leaned on
the unchecked tail, a @sizeof's usize out an i32 door, each its
@cast now; a value where nothing was declared is the same
refusal, the spec's model that a fn's value is its tail holding
both ways. 220 through 224 the refusing pins -- the typed tail,
the empty body, the statements with nothing after, the value a
unit fn never asked for, the closure's own -- 236 the shapes
that pass, the literal, the variable, the if, the match, the
spec's return section carrying the rule's own sentence.

And the private door, half its hinges on. The value's two
walks asked the pub -- the qualified read, the qualified call --
but a type never did, and neither did the family the value's
own branches spell: a variant named as a value, a construction
called by its path, a literal the same, a match's pattern, each
its own read of the namespace's table, each finding the private
as freely as the pub. Five doors the one hinge now -- the
qualified name across namespaces reaches only what the namespace
gives away, whichever position it stands in, and the re-export's
target need not the question again (the use asked it at its own
door). The same-directory face no tighter for it: a neighbor
file's bare names read the private the way they always did, 226
the ok project that pins it, 225 through 227 the refusing
shapes -- a type, a pattern, a literal. Five hundred twenty-
three green, the spec's Visibility section carrying the sentence
that names the five positions one door.

And the borrow's own arithmetic, a layer counted twice. A
[N]mut T's rows are mut slots -- & lends them *mut T by their own
nature -- but &mut wrapped the pointer's mut around the place's
mut again, *mut mut u32 a shape no type ever spelled on purpose
(the pointer's mut is the pointee's layer, one, by type.h's own
law). The wrap is idempotent now: the slot's own writability is
the pointer's, &mut no richer than & for a slot already writable,
the exclusiveness the freeze's own book, never the spelling. The
sweep of tymut's other makers found no second stacker -- the
type-position's &mut reads a spelled type, bare by grammar; the
arrays' and tuples' wraps are the slot's own permission.

And the thaw that never came. The probe that walks the borrow to
its end -- the binding that holds it gone, the slot to answer
after -- found the block's exit keeping the freeze anyway: the
pop asked the dying binding's own frz, but the freeze's book
names the holder on the frozen side, and a borrower nobody
borrowed carries no mark at all -- the if's else arm the only
shape the old condition ever caught. The pop reads the frozen
side now: a binding outside the block, frozen by one inside it,
thaws as the borrower goes -- 03-move's own sentence, a borrow
lives as long as the binding that holds it, true at the exits
too. 237 the walk that pins it end to end -- the borrow, the
write through it, the thaw, the answer after, a shared read
beside a shared borrow -- 228 the refusal while the borrow
lives. Five hundred twenty-five green, the spec's &mut section
naming the mut slot's element one layer's own.

And the question that answered itself: never there. The const
aggregate's address, read through, was to be garbage somewhere --
the probe went at every door it could think of. The direct read,
the index; the address taken, the deref the other side; a field's
own address, an element's, either handed to a fn; two
namespaces' worth of aggregates at once, statics among them; the
floats, the longs, the shorts, the bytes; a struct in a struct;
the slice view over a const row; the release road; the const
evaluator reading one const's rows to make another. Every answer
the right one, on the fixed compiler and the one before the fix
alike -- the one shape the old compiler did own was X1's own, two
namespaces' same-named rows meeting at the assembler, a refusal
loud and hard, no quiet garbage in it. The honest close: the
observation itself most likely the error, the arithmetic of an
expected value the easiest thing a reader fumbles (the probe
writer's own hand four times this round -- an exit code's eight
bits, a modulo done in the head -- each one a garbage value that
was only ever the reader's). 238 the pin anyway: the same-named
rows' addresses walked end to end, twenty-one the answer, the
ground held for whoever reads the code next.

Then the tests got their own door. `#[test]` had waited in the
specs with its whole chapter -- the second product a project
builds, `-x` beside `-c`, every marked fn from every namespace pub
or not walked by a runner the compiler writes; now the writing
itself. The shape the chapter names came home in the existing
grammar's own pieces: the attribute was already parsed, the
checker's main checks already knew the two endings a fn may
answer, and std::entry's runs already wrapped both -- a () handed
to `run_unit`, an E?() to the `run_err` instance over its error,
the same doors the main wrapper arranges, no trampoline of its
own to generate. The new part was the isolation: a panic is an
abort, no unwinding, no destructors, so the runner forks per test
-- the child calls the entry and returns what it answered, the
parent waits, reads the status word's two halves (a signal the
abort, an exit code the Err's own clean one), and walks on. The
report is writes and nothing buffered: a line a test, `ok` or
`FAIL`, the name with its namespace, the description the attribute
wrote; the sum line last, a count in decimal a tiny hand-written
itoa spells; exit 0 all green, 1 any red. The checker's side
takes the main's own rules and answers them for a test's shape --
no arguments, no generics, a body, one of the two endings, the
description one string, a method or an #[extern(C)] refused --
eight refusals pinned. The first walk taught the table's own
shape the hard way: a namespace's table is a hash, not a row, and
the entries the collector found through it came out in the hash's
own order -- the report pins that order the way a golden pins its
dump, and four projects hold the whole door open: the all-green
walk with an E?()'s Ok in it, the failing one with a deliberate
panic and a deliberate Err, the project whose main stands aside
uncalled, and the empty table's honest zero. Five hundred
thirty-eight green, and S1 and S2 -- the reason the door was
built first -- have somewhere to live.

And the platform got its word. The Mac's suite had fallen two
hundred and eleven times over, every fall the same missing symbol
-- __errno_location, the glibc spelling of the door the errno waits
behind, and Darwin keeps the same word behind __error, nothing but
the spelling parting them. The fix is the language's own: a #[cfg]
on a declaration, two dimensions of names -- the system, linux or
darwin, the libc it carries; the machine, amd64 or arm64, the qbe
backend that answers it, the one word arm64 covering both the
aarch64 Linux and the Apple silicon beneath it -- the arguments
and-ed, several attributes and-ed the same way, no negation
anywhere (a library names the platforms it stands on, not the ones
it does not), and the cull the checker's very first walk: before a
single name is declared, a false word's declaration is not hidden
but absent, its uses and impls and bodies gone with it. std is the
first customer -- errno twice over, one spelling per system, each
compile reading exactly one. And the four noreturns beside it: die
and cerrat and berr and voom, their declarations now saying what
their bodies always did, and Apple clang's
possibly-uninitialized -- the lie it told about every caller that
treated the exit as reachable -- gone quiet. 237 through 241 the
refusals: the unknown name, the two systems in one pair of
parentheses, the literal where a name goes, the attribute with no
word at all, and the culled fn's own caller, the unknown name any
absent thing is. 239 the run beside them, the words that hold here
holding, and-ed, the answer they make. Five hundred thirty-two
green.
