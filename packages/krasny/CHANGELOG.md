# Changelog

All notable changes to `krasny` are documented here.

## 0.0.31 - 2026-05-03

### Changed

- Inline snapshot assertions now compile cleanly under warning-as-error release builds. Snapshot-heavy formatter tests no longer trigger warning 10 from a non-unit expression in generated test code.

## 0.0.30 - 2026-05-02

### Changed

- Signature comments now stay attached to the value or type item that follows them. Formatting no longer inserts an unwanted blank line between a docstring/comment block and the signature item it documents.

## 0.0.27 - 2026-05-01

### Changed

- Formatter policy is more stable for field access, including qualified record fields such as `Module.record.field`, dereference field access, and constructor-like field expressions.
- Match-case comments are preserved before their cases, preventing formatter runs from dropping meaningful comments inside pattern matches.
- Pattern layout was tightened for multiline lists, constructor records, inline records, and complex syntax. Closing delimiters now align with their opening syntax, constructor record payloads align under the constructor, and small readable record/list patterns stay inline when they fit.
- Let right-hand-side layout now retries after width overflow and keeps value delimiters such as `{`, `[`, and `[|` attached to value bindings while function bodies still break after `->`.

## 0.0.26 - 2026-04-28

### Changed

- Krasny is now centered on the streaming formatter path. The old document solver pipeline, old stream-doc intermediary, and old lower2-style naming were removed or renamed so the formatter has one primary architecture.
- Formatter internals were split and renamed around the actual streaming formatter responsibilities, with text helpers, formatter entrypoints, and layout policy surfaces separated more clearly.
- Layout decisions now route through a central policy layer for let right-hand sides, function bodies, applications, infix chains, records, lists, tuples, parenthesized expressions, if conditions, and type separators.
- Application formatting now uses explicit layout roles rather than ambient force flags, making nested applications and function bodies more predictable.
- Layout policy tracing was added and covered by tests, so future formatter policy changes can explain why a node chose inline, hanging, vertical, or block layout.
- The formatter preserves typed-expression parentheses correctly and avoids adding redundant parentheses around ascriptions in match scrutinees, constructor payloads, and parenthesized typed expressions.
- Formatter policy now keeps fitting constructor or-patterns inline, breaks nested constructor patterns when needed, breaks match bodies after multiline constructor patterns, and keeps paired `if` branch parentheses on the right line.
- Local let bindings now respect the configured width, including overflowing single-line bindings and multiline bodies that require `in` placement to stay stable.

## 0.0.25 - 2026-04-27

### Changed

- `krasny` now routes formatting through the typed syntax views and streaming lowerer, improving formatter stability across modules, signatures, local opens, type declarations, attributes, and comments.
- Formatter policy was tightened for pipelines, tuples, function parameters, binding operators, branch layouts, docstrings, phrase separators, and parenthesized expressions.
- Snapshot and fixture coverage was expanded across real files and focused parser/formatter regressions, giving future formatter work a broader safety net.
