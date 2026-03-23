# RFD0020 - Krasny Pretty Printer

- Feature Name: `krasny_pretty_printer`
- Start Date: `2026-03-23`
- Status: `presented`
- RFD PR: [leostera/riot#0000](https://github.com/leostera/riot/pull/0000)
- Riot Issue: [leostera/riot#0000](https://github.com/leostera/riot/issues/0000)

## Summary
[summary]: #summary

This RFD proposes a new standalone package, `krasny`, as Riot's owned OCaml
formatter.

`krasny` should:

- consume `Syn.parse_result`
- try to format primarily from `Syn.Cst`
- lower syntax into a formatting document IR
- render formatted output through an `Std.IO`-friendly writer API
- expose one formatting style for everyone, with no user-facing knobs

The formatter should be the only rendering system Riot builds for formatted
OCaml output. We should not build a separate fix-only fragment printer and then
later build a real formatter beside it.

## Motivation
[motivation]: #motivation

Riot is heading toward two adjacent capabilities:

- syntax-directed lint rewrites
- a canonical `tusk fmt`

If synthetic rewrites eventually need fresh syntax materialization, Riot will
need some way to turn structured syntax back into OCaml source. Building one
"small renderer for fixes" and later a second full formatter would be a bad
split. Both systems would grow to cover similar syntax and layout concerns, and
the repo would end up paying for two rendering pipelines.

The cleaner path is to start the formatter now and let all future rendering
needs grow from that single subsystem.

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

Contributors should think of `krasny` as a syntax-to-document pipeline:

1. `Syn.parse_result`
2. typed CST when available
3. formatting document IR
4. layout engine
5. `Std.IO` writer

At a high level:

- `syn` remains responsible for parsing and faithful syntax structure
- `krasny` is responsible for layout and rendering policy
- `tusk fmt` becomes the user-facing entrypoint to `krasny`

### One formatter, one style

`krasny` should be explicitly opinionated:

- no per-project knobs
- no style variants
- no local escape hatches beyond what OCaml syntax itself requires

If the style changes, Riot changes it globally and deliberately.

### Primary path: CST-driven formatting

Formatting should primarily traverse `Syn.Cst`, not raw Ceibo nodes. CST gives
the formatter the structure it actually wants:

- expressions
- patterns
- types
- declarations
- module items
- function parameters
- records and variants

That makes the formatter easier to reason about than a raw token/node printer.

### Fallback path: formatting should not fail

`krasny` should accept `Syn.parse_result`, not only successful CST lifts.

The normal path is:

- parse
- build CST
- format from CST

If CST building fails because the current lift does not yet cover some parsed
syntax, `krasny` should still be able to render from the underlying Ceibo tree
instead of crashing or refusing to format.

This fallback does not need to be pretty immediately. It needs to be complete
and valid enough that the formatter remains usable while the CST grows.

## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

## 1. Package boundary

Introduce a new package:

```text
packages/krasny
```

Responsibilities:

- document IR
- CST-to-doc lowering
- Ceibo fallback lowering
- layout engine
- rendering to `Std.IO`

Non-responsibilities:

- parsing
- linting
- rewrite planning

## 2. Public API shape

The formatter should expose an `Std.IO`-friendly writer surface.

A first-pass API could look like:

```ocaml
module Krasny : sig
  type config = {
    width : int;
  }

  val default_config : config

  val format :
    ?config:config ->
    Syn.Parser.parse_result ->
    (string, string) result

  val write :
    ?config:config ->
    writer:Std.IO.writer ->
    Syn.Parser.parse_result ->
    (unit, string) result
end
```

Even if the first implementation renders into a string internally, the API
should be shaped around writing because that is how `tusk fmt` and future
callers will naturally use it.

## 3. Internal pipeline

The formatter pipeline should be:

```text
Syn.parse_result
  -> try Syn.build_cst
  -> cst_to_doc
  -> layout
  -> writer
```

with fallback:

```text
Syn.parse_result
  -> ceibo_to_doc
  -> layout
  -> writer
```

The formatter should never require downstream callers to choose that path
manually.

## 4. Document IR

`krasny` should not render directly by string concatenation. It should lower
syntax into a document IR.

A first-pass document model should be in the Wadler/Oppen family, for example:

```ocaml
module Doc : sig
  type t =
    | Empty
    | Text of string
    | Space
    | Line
    | Hard_line
    | Concat of t list
    | Indent of int * t
    | Group of t
    | Flat_alt of {
        when_flat : t;
        when_broken : t;
      }
end
```

The exact constructor list is less important than the architectural point:

- CST decides structure
- `Doc` decides layout
- rendering happens after layout choices are made

## 5. Layout engine

The layout engine should follow the broad shape used by classical
pretty-printers:

- scan/measuring pass for grouped layouts
- printing pass that chooses flat or broken forms under width constraints

The implementation does not need to be academically pure. It does need to be:

- deterministic
- simple to reason about
- able to grow without rewriting the architecture later

## 6. Comments and trivia

Comments and trivia must be handled intentionally.

Because the formatter is CST-first, but comments live naturally in Ceibo
trivia, `krasny` will need a comment attachment strategy when lowering syntax
to documents.

This RFD does not prescribe the full comment algorithm yet, but it does make
one call explicit:

- comments are part of formatting design, not a post-processing hack

## 7. Why this is not a fragment printer

This RFD explicitly rejects building a fix-only "render one node to text"
printer as a separate subsystem.

If Riot later needs to materialize synthetic rewrites, that rendering should
come from `krasny`'s document pipeline, not from a second rendering codepath
inside `tusk-fix`.

That means:

- fragment rendering may eventually exist
- but it should be a mode of the formatter, not a separate tool

## Drawbacks
[drawbacks]: #drawbacks

This is a substantial new subsystem.

Even a simple formatter will require:

- broad syntax coverage
- comment handling
- document/layout infrastructure
- long-term stability discipline

It also introduces pressure to define formatting behavior for syntax that Riot
does not yet lint or transform, because a formatter must cover the whole
language surface.

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

### Alternative: build a small fix-only renderer first

Rejected.

That would create a second rendering system that overlaps heavily with the
formatter we already know Riot will want.

### Alternative: print directly from Ceibo without CST

Rejected as the primary design.

Ceibo is the right source of truth for lossless syntax and fallback behavior,
but formatting wants structured syntax more than raw red-tree traversal. CST is
the better primary traversal surface.

### Alternative: direct AST/CST printer with no document IR

Rejected.

OCaml layout is rich enough that direct string printing would quickly collapse
into handwritten spacing and line-breaking heuristics scattered across many
node printers. A document IR gives the formatter a cleaner long-term shape.

### Alternative: wait until synthetic rewrites force the issue

Rejected.

Riot already wants `tusk fmt`, and waiting would only encourage ad hoc
rendering logic to leak into unrelated systems first.

## Prior art
[prior-art]: #prior-art

Three formatter families are especially relevant.

### Swift `swift-format`

Swift's official formatter is the closest structural model for `krasny`:

- syntax tree traversal
- lowering into a formatting token/document stream
- explicit grouping and break opportunities
- Oppen-style scan/print layout engine

That is the clearest precedent for a CST-driven formatter with a real layout
engine instead of direct string printing.

Source:
- https://github.com/swiftlang/swift-format/blob/main/Documentation/PrettyPrinter.md

### Go `gofmt`

`gofmt` is the clearest precedent for formatter philosophy:

- one canonical style
- no user knobs
- formatter as the standard tool, not an optional preference layer

Its implementation is more direct AST printing than Riot likely wants, but its
social model is exactly the one Riot should copy.

Sources:
- https://pkg.go.dev/go/printer
- https://pkg.go.dev/cmd/gofmt

### Rust `rustfmt`

`rustfmt` is useful as a warning and as a source of practical lessons:

- it is AST-driven
- it uses many handwritten, width-aware rewrite routines
- it is pragmatic and large-scale, but quite heuristic

Riot should expect to grow some construct-specific heuristics over time, but
should not start there. A document-based architecture gives a better first
shape than jumping straight into a large family of handwritten rewrite
functions.

Source:
- https://github.com/rust-lang/rustfmt/blob/main/Design.md

### Document algebras

The underlying pretty-printing lineage from Wadler, Oppen, and later work is
still the right conceptual base:

- build docs, not strings
- separate structure from layout
- make width-sensitive decisions late

That is the family of ideas `krasny` should build on.

## Unresolved questions
[unresolved-questions]: #unresolved-questions

1. What is the smallest good first `Doc` algebra for OCaml in Riot?
2. How should comments be attached from Ceibo trivia to CST-driven formatting
   nodes?
3. Should the first version guarantee formatting of parser-recovered code, or
   only diagnostics-free parse results plus a best-effort fallback?
4. How should future fragment rendering for synthetic fixes reuse the same
   pipeline without exposing formatting internals everywhere?
