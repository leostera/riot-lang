# typ AGENTS

`typ` is the experimental type-analysis package for Riot.

## Specs

The docs under `packages/typ/docs` are the current normative spec stack for
`typ`.

Start with:

- `packages/typ/docs/README.md`
- `packages/typ/docs/checker.md`
- `packages/typ/docs/solver.md`
- `packages/typ/docs/lowering.md`
- `packages/typ/docs/engine.md`

Then use the feature slices that match the work you are doing.

## Rules

1. Keep semantic work centered on the semantic tree, not raw `Syn.Cst`.
2. Treat source spans and origins as explicit data; do not smuggle CST nodes into long-lived semantic state.
3. Keep prototype diagnostics structured and span-backed, even when the checker falls back to holes or recovery nodes.
4. Prefer snapshot-heavy examples that dump semantic structure, environments, and diagnostics together.
5. Keep prototype scope narrow and explicit; unsupported syntax should surface as recovery plus diagnostics, not silent drops.
6. Keep cross-query state explicit. Query-local mutation is fine, but it must not escape the query boundary.
7. Keep structured diagnostic-shape regressions covered under `packages/typ/tests/diagnostics`, not only in human-readable report snapshots.

## Validate

`timeout 180 riot test -p typ`
