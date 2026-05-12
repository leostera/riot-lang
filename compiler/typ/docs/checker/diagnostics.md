# Typ Diagnostics

This document specifies the diagnostics contract for `typ`.

The point here is simple: structured diagnostics are one of the main reasons
this checker exists at all.

That means diagnostics cannot be treated as a nice rendering layer bolted on at
the end. They are part of the semantic API.

If we get this wrong, we end up back where we started:

- flattening checker state into strings
- reparsing those strings in the CLI or LSP
- losing important structure because the first renderer did not know what later
  consumers would need

That is exactly what this project is trying to avoid.

## 1. Scope

This document covers:

- parse diagnostics crossing in from `syn`
- lowering diagnostics produced by `typ`
- typing diagnostics produced by `typ`
- severity
- machine-readable codes
- origin and span attachment
- query-level diagnostic aggregation

This document does not cover:

- exact CLI rendering
- exact LSP rendering
- exact human message wording as a stable compatibility contract

Those are downstream concerns.

## 2. Diagnostics Are Data First

Diagnostics in `typ` should be data first.

That means every diagnostic should be a structured value that carries:

- what happened
- where it happened
- the structured facts around it

Human-readable messages are derived views over that data.

JSON output is also a derived view.

Pretty terminal rendering is also a derived view.

This is the same general philosophy `syn` already uses, and `typ` should keep
that contract rather than collapsing back to strings.

## 3. Diagnostic Layers

The full diagnostic story spans three layers.

### Parse Diagnostics

These come from `syn`.

They describe malformed or incomplete source syntax and are already structured.

`typ` should preserve them as parse diagnostics instead of re-rendering and
re-parsing them.

### Lowering Diagnostics

These come from the lowering layer.

They describe things like:

- unsupported syntax lowered into recovery
- ignored syntax details during lowering
- CST builder failures
- interface forms the current lowering lane does not support yet

These should still be tied to source spans and syntax kinds.

### Typing Diagnostics

These come from inference and the solver.

They describe things like:

- unbound names
- type mismatches
- unsupported semantic expressions that made it through recovery
- escaping or invalid recursive shapes

These should still be structured around actual typing facts, not collapsed to
one formatted paragraph.

## 4. Query-Level Aggregation

The query layer should expose diagnostics as a sum over those layers.

Conceptually:

```text
diagnostic =
  | Parse(parse_diagnostic)
  | Lowering(lowering_diagnostic)
  | Typing(typing_diagnostic)
```

That is already the broad shape of `Typ.Query.diagnostic`.

The important point is:

the query layer should preserve where each diagnostic came from instead of
flattening everything into one generic record too early.

Consumers can always normalize later if they need to.

## 5. Codes And Identity

Every `typ` diagnostic should have a stable machine-readable code.

Examples in the current prototype are things like:

- `TYP1001`
- `TYP2001`
- `TYP2002`

Those codes are not just for humans.

They are for:

- `riot check --explain`
- editor integrations
- suppression or classification systems later on
- snapshot stability

The exact message text may evolve. The code should remain the stable identity.

## 6. Severity

Severity should be a regular variant, not a string and not a polymorphic
variant.

At a minimum:

```text
severity ::= Error | Warning
```

Later extensions can add things like informational hints if they are genuinely
useful, but the semantic contract should stay explicit.

Severity is part of the data, not just something inferred from the message
prefix.

## 7. Primary Location

Every diagnostic must have a primary source location.

For `typ`, that means at least one source span.

This span should point at:

- the syntax that caused the problem
- or the best recovery anchor if the syntax itself is incomplete

The current `typ` prototype uses per-constructor span fields and a
`primary_span` helper. That is good as an implementation detail, but the spec
rule is simpler:

every diagnostic must have a primary location that downstream tools can render
without guessing.

## 8. Structured Payload

A diagnostic should carry structured payload that matches the semantic
situation, not one generic pile of “notes”.

That means different diagnostic families should carry different fields with
meaningful names.

Examples:

- `UnsupportedSyntax`
  should carry syntax kind, context, recovery form, and optional reason
- `TypeMismatch`
  should carry mismatch structure, not just one sentence
- `CstBuilderError`
  should carry the original structured builder error, not a stringified copy
- `UnboundName`
  should carry the unresolved name and its reference span

This is the big design rule:

each constructor should be self-contained and semantically meaningful on its
own.

That is much better than a big shared wrapper with vague fields like
`notes : string list`.

## 9. Diagnostics Should Preserve Context, Not Guess Consumers

The layer that emits a diagnostic should preserve the structured facts it knows
right then.

It should not try to predict the one final human rendering every later consumer
will want.

That means:

- lowering should preserve syntax-kind context
- typing should preserve type-level context
- solver failures should preserve mismatch structure

Later consumers can render:

- rich CLI messages
- terse JSON events
- editor hovers
- code actions

from the same diagnostic value.

## 10. Recovery And Diagnostics

Recovery forms and diagnostics go together.

If lowering or typing chooses to recover instead of fail hard, it should make
that explicit in the diagnostic payload.

For example:

- unsupported syntax lowered to `EHole`
- unsupported pattern lowered to `PUnsupported`
- unsupported top-level form lowered to `Unsupported`

That way the host can tell the difference between:

- “this was fully supported and typed”
- “this was recovered so the file could keep moving”

That distinction matters for both trust/export decisions and for editor UX.

## 11. Diagnostic Trust And Export Decisions

Diagnostics are not just for display. They also influence export trust.

That means the engine may need to classify a module's exports as:

- trusted
- errored but still computed
- not exportable

based in part on which diagnostics were produced and at what phase.

The exact export-trust model belongs in the engine and summary docs, but the
important diagnostics rule is:

the diagnostic layer must expose enough structure that the engine can make that
decision intentionally, not by grep-ing message text.

## 12. JSON Is A View, Not The Semantic Source

`to_json` is useful and necessary.

But JSON is just one rendering of the diagnostic value.

The semantic source of truth is the structured diagnostic constructor itself.

That means:

- the in-memory representation should stay rich
- JSON should preserve as much of that structure as reasonably possible
- callers should not be forced to parse JSON just to understand diagnostics in
  process

This matters a lot because `typ` is meant to be a library.

## 13. Diagnostics And Queries

Queries should return diagnostics incrementally and in source order when
possible.

That means:

- one file's diagnostics should be available without forcing unrelated files
- a clean file should still be representable as “checked ok” in machine-facing
  query or CLI layers
- JSON mode should be able to stream per-file or per-event results instead of
  one giant terminal blob

The exact event protocol belongs to callers like `riot check`, but the engine
and query layers should make that shape possible.

## 14. Diagnostics And Origins

Diagnostics should be origin-backed where possible.

That means:

- lowering diagnostics should point at source spans tied to the lowered syntax
- typing diagnostics should point at the semantic node's origin span
- exported summary data should retain enough origin information that later
  cross-module diagnostics and jumps can still land on the right place

The important point is:

diagnostics should not need to rediscover source positions by re-walking syntax
from scratch every time.

## 15. Diagnostic Families We Already Know We Need

Even without freezing every future constructor today, the contract already
suggests a few stable families:

- parse diagnostics from `syn`
- unsupported-syntax and recovery diagnostics
- name-resolution diagnostics
- type-mismatch diagnostics
- solver escape or rigidity diagnostics
- export/trust diagnostics when relevant

Those are the broad categories the rest of the system should expect.

## 16. Mapping To `typ`

This document implies a few architectural constraints for `typ`.

1. `Diagnostic.t` should stay a big structured variant.

2. Each constructor should carry its own meaningful fields instead of relying on
a generic wrapper with vague shared slots.

3. `Typ.Query.diagnostic` should preserve parse/lowering/typing provenance.

4. Rendering helpers like `message`, `to_string`, and `to_json` should remain
derived views over the structured data.

5. Snapshot tests for diagnostics should prefer structured JSON, not fragile
human-formatted strings.

## 17. Relationship To `syn`

`syn` already sets the right tone here.

Its diagnostics are structured, span-backed, and machine-usable.

`typ` should follow that model and extend it for lowering and typing rather
than inventing a second diagnostic philosophy.

That keeps parse, lowering, and typing diagnostics composable instead of making
every consumer translate between incompatible diagnostic worlds.

## 18. Relationship To Upstream OCaml

This is one of the clearest places where `typ` should diverge from the upstream
compiler experience.

Today a lot of OCaml-based tooling still ends up consuming rendered messages and
post-processing them.

That is exactly the thing we do not want.

The contract here is:

- diagnostics are structured values first
- strings are for rendering
- JSON is for transport
- the semantic shape should stay rich enough for multiple later consumers

That is one of the main reasons this checker is worth building in the first
place.
