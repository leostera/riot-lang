# Changelog

All notable changes to `pubgrub` are documented here.

## 0.0.29 - 2026-05-01

### Changed

- Restored the published `Pubgrub.mli` interface as valid source text, so dependency analysis and downstream builds can parse the package interface normally.

## 0.0.26 - 2026-04-28

### Changed

- Pubgrub range operations, term algebra, incompatibility explanations, partial-solution caching, backtracking, and deterministic solution ordering were tightened.
- Solver diagnostics now preserve dependency ranges and no-version explanations more clearly, which helps users understand why a package resolution failed.
- Solver code now avoids removed `List.reverse_append` usage, keeping the package compatible with the cleaned-up standard collection surface.
