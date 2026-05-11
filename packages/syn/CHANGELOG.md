# Changelog

All notable changes to `syn` are documented here.

## 0.0.27 - 2026-05-01

### Changed

- Syn now uses its own `Span.t` instead of depending on Ceibo for source spans, reducing parser dependencies and keeping source locations in the parser package.
- The semantic Ast views were tightened around explicit identifiers, required fields, local spans, and typed view modules. Downstream tools can rely on stronger `Syn.Ast` handles instead of token-list identifiers or generic nodes.
- The Ast implementation was split into a library directory so identifiers, tokens, nodes, type expressions, and related view helpers can be maintained in smaller focused modules.
- Dotted field access is parsed as field access rather than as a plain identifier, including qualified forms such as `Hello.record.field`.

## 0.0.26 - 2026-04-28

### Changed

- `Syn.Ast` continued the semantic-view cleanup: source files are now concrete implementation or interface views, and the old empty source-file state was removed from the public shape.
- Ast view handles are opaque at the public boundary and expose typed helpers such as `span`, `width`, `view`, `fold_*`, and count accessors instead of requiring downstream callers to unwrap arbitrary syntax nodes.
- Ast casts now use structured cast results, so callers can distinguish successful typed views, unknown recovery nodes, and true cast errors explicitly.
- Identifier handling was normalized around opaque `Ident` views instead of loose path vectors, making downstream dependency and lint logic less likely to accidentally traverse arbitrary token sequences.
- Module expression and module type views now expose structured bodies, including module declarations, module type declarations, module type constraints, and body items, instead of leaking parser-specific placeholder states.
- Parameter views were normalized into a more semantic shape, covering labeled and optional parameters without splitting optional-default syntax into unrelated variants.
- Pattern views were tightened to remove non-pattern constructs and expose constructor, record, alias, first-class module, and constraint structure more directly.
- Expression and type views were simplified around semantic constructs; parenthesized and syntactic-only wrappers were collapsed where possible so consumers see the expression or type they actually need to analyze.
- Destructuring `let` binding patterns and function parameter spines are parsed more precisely, including the distinction between multiple function parameters and parenthesized constructor patterns.
- Class syntax support was removed from the supported Syn subset, matching the language surface Riot wants to keep formatting and analyzing.
- `Syn.Deps` and other Ast consumers now use the new views and controlled folds, with less list churn in hot dependency-analysis paths.

## 0.0.25 - 2026-04-27

### Changed

- `syn` completed the streaming parser migration, including the replacement CST builder, typed syntax views, broader diagnostic recovery, and parser-backed dependency analysis.
- Snapshot and fixture coverage was expanded across real files and focused parser/formatter regressions, giving future formatter work a broader safety net.
