# Changelog

All notable changes to `typ` are documented here.

## 0.0.27 - 2026-05-01

### Changed

- Typ gained a source-backed diagnostic fixture runner, JSON diagnostics, and source-rendered diagnostic output so editor and CLI integrations can consume type-analysis errors more directly.
- The experimental inference engine now handles functions, lets, tuples, arrays, lists, records, record updates, field access, constructors, constructor payloads, pattern matching, modules, module aliases, includes, functors, polymorphic variants, GADTs, and inline record constructors across a broader fixture corpus.
- Typ now tracks expression types and query context for LSP features such as hover, completion, and stable inlay hints.
- Type rendering and inference environment internals were refactored around explicit scopes, constructor descriptions, record field inference, and source-backed Ast views, making future checker work less dependent on ad hoc syntax lowering.
