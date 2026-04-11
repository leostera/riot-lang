# typ AGENTS

`typ` is Riot's type-checker engine package.

## Specs

The docs under `packages/typ/docs` are the current normative spec stack for
`typ`.

Start with:

- `packages/typ/docs/index.md`
- `packages/typ/docs/checker/index.md`
- `packages/typ/docs/checker/checker.md`
- `packages/typ/docs/checker/solver.md`
- `packages/typ/docs/checker/lowering.md`
- `packages/typ/docs/checker/engine.md`

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

Run the current validation stack in this order:

```sh
riot fix ./packages/typ
riot fix ./packages/riot-check
riot fmt ./packages/typ
riot fmt ./packages/riot-check
riot build typ riot-check
riot test -p typ
riot bench -p typ
riot run riot -- check -p kernel-new
```

Interpret the results carefully:

- `riot fix` currently reports an existing lint backlog in both `typ` and
  `riot-check`; do not treat that as a slice-specific regression unless your
  batch made it worse.
- `riot bench -p typ` currently reports `No benchmark suites found in package
  'typ'`.
- `riot run riot -- check -p kernel-new` is only a meaningful `typ` signal when
  planner/source preparation succeeds and actually hands `typ` prepared sources.
