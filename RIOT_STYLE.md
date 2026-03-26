# RiotStyle

## The Essence Of Style

> “People don't die from bullets or disease. They die when they're forgotten.”
> — Dr. Hiriluk

Riot's coding style is still being forged. Part actor runtime, part systems programming, part
toolsmithing, part web stack, part build system. Engineering and ergonomics. Boundaries and flow.
Reason and taste. OCaml and ideas stolen shamelessly from elsewhere when they make the system
better.

Riot is not trying to preserve every inherited convention forever. It optimizes for programmer
happiness, shipping, conventions over configuration, and a stack that is comfortable owning more of
itself. Style is how those values become visible in code.

The goal is not to make every file sound like the same person. The goal is to make the whole
repository feel like one coherent system.

## Why Have Style?

Another word for style is design.

> “The design is not just what it looks like and feels like. The design is how it works.” 
> — Steve Jobs

In Riot, good style serves a few recurring goals:

- make ownership obvious
- keep behavior explicit
- make code easier to change without leaking internals
- keep the common path pleasant and unsurprising

Riot's own principles say that safety, performance, simplicity, and developer experience all
matter, and that type safety is a spectrum. Style is how we balance that spectrum deliberately
instead of accidentally.

Put this way, style is more than readability, and readability is table stakes, a means to an end
rather than an end in itself.

> “...in programming, style is not something to pursue directly. Style is necessary only where
> understanding is missing.” ─ [Let Over
> Lambda](https://letoverlambda.com/index.cl/guest/chap1.html)

In Riot, style matters most where understanding would otherwise go missing:

- at package boundaries
- at API boundaries
- at failure boundaries
- at suspension and scheduling boundaries

## On Simplicity And Elegance

Simplicity is not a free pass. It is not "the fewest tokens", and it is not permission to collapse
important distinctions.

Rather, simplicity in Riot usually means the smallest shape that keeps ownership obvious:

- `kernel` owns raw platform edges and mechanical primitives
- `miniriot` owns the runtime model, scheduling, mailboxes, timers, and process lifecycle
- `std` owns the default ergonomic surface

The elegant solution is often the one that improves the boundary, the data shape, and the call site
all at once.

> “Logic is the beginning of wisdom, not the end.”
> — Spock

A public mutable field is not simpler because it saves one accessor. A helper in the wrong package
is not simpler because it saves one import. A custom operator is not simpler because it saves three
characters. A tuple is not simpler if the caller now has to memorize the field order forever.

Riot prefers strong conventions over local preference because shared defaults scale better than
personal dialects.

> “the simple and elegant systems tend to be easier and faster to design and get right, more
> efficient in execution, and much more reliable” — Edsger Dijkstra

## Technical Debt

What could go wrong? What's wrong? Which question would we rather ask? The former.

In Riot, debt piles up fastest when boundaries blur:

- when lower layers start absorbing framework or application policy
- when interfaces expose representation that should stay private
- when APIs make failure ambiguous
- when convenience wrappers normalize unclear naming, booleans, tuples, or exceptions

> “You shall not pass!” — Gandalf

We do not treat "we can clean it up later" as an API design strategy. Riot values progress over
stability, but that does not mean shipping accidental contracts and hoping to fix them in the next
pass. It means being willing to improve the system when a better boundary or a clearer convention
appears, then carrying that change through code, docs, and tooling.

## Safety And Boundaries

> “Without facts, the decision cannot be made logically.”
> — Spock

Riot is not a safety-critical firmware project, but it does have strong boundary discipline.

- Respect package ownership. If behavior is low-level and mechanical, it probably belongs in
  `kernel`. If it changes scheduler or process semantics, it belongs in `miniriot`. If it improves
  everyday ergonomics, it probably belongs in `std`. If it is wire behavior, keep it in `http`.
  If it is framework behavior, keep it in `suri`.
- Outside `kernel` and `miniriot`, `open Std`. Inside `std`, prefer `open Global`. Do not
  reintroduce direct `Stdlib`, `Unix`, `Sys`, or `Obj` usage outside the packages that already own
  those edges.
- Keep low-level APIs narrow and mechanical. Higher-level policy belongs above them.
- Prefer explicit error variants and `Result` or `Option` for expected failure. Do not use
  exceptions as ordinary control flow, and do not normalize exception-throwing APIs behind `_exn`
  names.
- Treat `.mli` files as real design surfaces. Library source modules should usually have a matching
  interface. Internal helpers should stay internal by default.
- If an interface already exposes accessors, the type usually wants to stay opaque. Do not publish
  representation just because you can.
- Do not expose public mutable fields in interfaces. If callers need mutation, expose operations
  that own the invariant.
- Use `Cell.t` for standalone mutable values and record `mutable` fields for mutable record state.
  Mutation should have an obvious owner.
- Use `panic`, assertions, `Result.expect`, and `Option.unwrap` for broken invariants, impossible
  states, trusted test setup, and paths that would indicate our bug if reached. Do not use them to
  dodge API design.
- In actor code, keep protocols explicit. `type Message.t += ...` with named payload fields is
  better than anonymous positional encodings.
- Runtime behavior should stay deterministic where practical. Prefer explicit actor loops,
  selector-based receives, and visible suspension points over hidden global state or implicit
  blocking.

For example, this is the kind of message shape Riot code should gravitate toward:

```ocaml
type Message.t +=
  | Fetch of { reply_to : Pid.t; key : string }
  | Fetched of { key : string; value : string option }
```

## Performance

> “Risk is our business. That's what this starship is all about.”
> — James T. Kirk

Performance in Riot starts at design time.

- The wrong package boundary is often the slowest abstraction.
- The wrong concurrency shape is often the most expensive bug.
- The wrong data shape is often the most persistent tax on every call site.

Prefer designs that keep the cost model obvious:

- in actor code, do not hide blocking; yield, receive, or suspend explicitly
- batch work through queues, mailboxes, timers, and clear protocol steps where possible
- keep hot paths boring and mechanical rather than clever
- avoid large tuples, positional booleans, and deeply nested matches that obscure both meaning and
  work
- when a function keeps accreting parameters or branching depth, introduce a record, a helper, or a
  named intermediate value
- use pipelines when nested calls have become hard to read inside out

The runtime should not have to guess what your code means. The reader should not have to guess
either.

## Developer Experience

> “The first duty of every Starfleet officer is to the truth.”
> — Jean-Luc Picard

### Naming Things

- Use `snake_case` for files, source paths, functions, variables, record fields, type names, and
  polyvariant tags.
- Use `ClassCase` for modules, module types, and constructors.
- If a module has one obvious main type, call it `t`. `User.t` reads better than `User.user`.
- Prefer descriptive type variables in reusable and public types. `'value`, `'error`, `'msg`, and
  `'state` age better than `'a` and `'b`.
- Avoid single-letter names for real APIs and long-lived locals. If the name does not survive grep,
  stack traces, or signatures, it is not done yet.
- Avoid prime variables and placeholder suffixes that force the reader to remember invisible
  chronology.
- Avoid custom operators unless they are already conventional OCaml punctuation.
- Avoid `_exn` suffixed functions. Expected failure should be explicit in the type instead.
- Prefer names that still make sense outside the immediate file, because they will eventually appear
  in docs, reviews, diagnostics, and `git blame`.

### Function And API Shape

- Keep parameter counts small. When a signature keeps growing, it usually wants a named record.
- Order function parameters as labeled, then optional, then positional.
- When named arguments are present, keep `t` as the first positional argument so call sites read
  consistently.
- Prefer named parameters or small enums over positional `bool`s.
- Put function type annotations on the binding, not inline on each parameter.
- Prefer explicit parameters for named functions over `function` shorthand. `let decode value = ...`
  scales better than `let decode = function ...`.
- Prefer records over large tuples, especially when several tuple slots share the same type.
- If a helper immediately destructures a record, destructure it at the parameter boundary rather
  than hiding the real shape in the first line of the body.
- Prefer `if` over matching on `true` and `false`.
- Prefer `;` sequences over `let () = ... in ...` when all you mean is sequencing.
- Flatten deep towers of nested `match`. Three levels is already a warning sign.
- Prefer pipelines for call chains that have become hard to read from the inside out.

These shapes are typical Riot APIs:

```ocaml
let render ~width ~height t = ...

let output =
  input
  |> decode
  |> normalize
  |> render
```

### Modules, Files, And Opens

- Keep file-wide `open`s scarce. Two well-chosen opens are usually enough.
- Do not use `open!`. Either the open is safe enough to keep the compiler's shadowing warning, or
  the code wants explicit qualification.
- Prefer local opens for genuinely small scopes.
- Prefer scoped qualification that preserves the natural shape of the expression, for example
  `Module.(value.field)` or `Module.{ field = value }`.
- Keep important exported types and functions near the top of the file or interface. The first
  screen matters.
- Prefer abstract `.mli` surfaces when exposing package APIs. An implementation should be free to
  rearrange helpers without turning every internal detail into public contract.

### Comments And Docs

- Always motivate the code. Explain why the code is shaped this way, not just what the syntax does.
- Put comments before the item they document, not after it.
- Treat `.mli` doc comments as part of the API, not as decoration. Riot already uses `(** ... *)`
  heavily in public surfaces; follow that habit.
- Comments are prose. Write them as sentences when they explain behavior or rationale.
- Keep architecture snapshots and package docs descriptive and present-tense unless the document is
  explicitly a proposal.
- Keep package names, file names, and module names accurate in docs. Narrative drift is still
  drift.
- When a test or benchmark has non-obvious methodology, explain the goal and method so the next
  reader can decide quickly whether to dive deeper.

Don't forget to say why. Code alone is not documentation. Use comments to explain why you wrote the
code the way you did. Show your workings.

Don't forget to say how. Tests, fixtures, and protocol examples get much easier to read when the
reader knows the intended shape up front.

## Testing

All errors must be handled.

An [analysis of production failures in distributed data-intensive
systems](https://www.usenix.org/system/files/conference/osdi14/osdi14-paper-yuan.pdf) found that
the majority of catastrophic failures could have been prevented by simple testing of error handling
code.

> “Specifically, we found that almost all (92%) of the catastrophic system failures are the result
> of incorrect handling of non-fatal errors explicitly signaled in software.”

Riot testing style follows from that:

- write descriptive `Test.case` and `Test.property` names that describe behavior, not implementation
  mechanics
- test negative space, not only happy paths
- when changing protocol parsing or encoding, prefer focused fixtures and protocol-level tests
- when a package already has a fixture runner or focused verification command, use it
- in fixture-heavy tests, prefer multiline string literals (`{| ... |}`) over hand-escaped `\n`
- keep fixture corpora curated: add narrow regression fixtures when real code exposes a regression,
  not because a directory full of edge cases feels impressive

Tests are part of the design surface. Make them readable enough that they can teach the code back to
the next person.

## Style By The Numbers

- Use `tusk fmt`. The formatter backend may evolve, but the repository should have one formatting
  entrypoint and one shared style.
- Use `tusk fix` to surface style drift that the formatter cannot settle by itself.
- Optimize for readability. Newlines are cheap.
- Around 100 columns is a good target. Do not make horizontal scanning the cost of understanding the
  code.
- Break type definitions and `match` arms across lines whenever that makes the shape clearer.
- Prefer multiline strings in tests and fixtures.
- Format large numeric literals with `_`.
- If you keep fighting the formatter, change the code shape, not the whitespace.

Riot wants one formatter, one lint surface, and one obvious path through the codebase. Style should
remove choices where choices are not buying anything.

## Dependencies

Riot is vertically integrated by design. It is comfortable owning more of the stack when doing so
reduces accidental complexity.

That does not mean "never depend on anything". It means:

- reach for repo-owned packages first
- add dependencies deliberately, not casually
- prefer coherent package boundaries over a pile of one-off helpers
- do not add a new abstraction layer unless it removes more complexity than it creates

A dependency is also a boundary, a toolchain surface, and a maintenance contract. Treat it that
way.

## Tooling

Similarly, tools have costs. A small standardized toolbox is simpler to operate than an array of
specialized instruments each with a dedicated manual.

> “If you don't take risks, you can't create a future.”
> — Monkey D. Luffy

For Riot development:

- run `tusk` from the workspace root
- use `tusk completions --packages`, `--binaries`, and `--tests` for discovery
- use `tusk fmt` for formatting
- use `tusk fix` for style and rewrite guidance
- wrap long-running commands with `timeout`
- prefer Riot's tooling entrypoints over ad hoc shell glue when a first-class command already exists

The exact implementation behind the command may change. The shared workflow should not.

## The Last Stage

At the end of the day, keep trying things out, have fun, and remember: Riot is supposed to make
shipping feel better, not heavier.

Small sharp modules. Explicit actor protocols. Clear boundaries. One stack that sounds like one
stack.

> You don’t really suppose, do you, that all your adventures and escapes were managed by mere luck,
> just for your sole benefit? You are a very fine person, Mr. Baggins, and I am very fond of you;
> but you are only quite a little fellow in a wide world after all!”
>
> “Thank goodness!” said Bilbo laughing, and handed him the tobacco-jar.
