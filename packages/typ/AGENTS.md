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
- `packages/typ/docs/checker/engine.md`

Then use the feature slices that match the work you are doing.

The active implementation direction is deliberately small while the checker is
being rebuilt: parse with `Syn`, build a single `Typ.Ast`, run
`Typ.Infer.check` over that tree, and return an inferred module interface plus
diagnostics through the same path.

The package currently keeps only the minimum runtime surface needed for that
path:

- `Typ.Ast`
- `Typ.Infer`
- `Typ.Diagnostics`
- `Typ.Model` source/path/id helpers

## Rules

1. Keep semantic work centered on `Typ.Ast`; raw `Syn.Ast.Node.t` and parser events are source-stage details.
2. Treat source spans and origins as explicit data in long-lived semantic state.
3. Keep prototype diagnostics structured and span-backed, even when the checker falls back to holes or recovery nodes.
4. Prefer snapshot-heavy examples that dump semantic structure, environments, and diagnostics together.
5. Keep prototype scope narrow and explicit; unsupported syntax should surface as recovery plus diagnostics, not silent drops.
6. Keep cross-query state explicit. Query-local mutation should stay inside the query boundary.
7. Keep structured diagnostic-shape regressions covered by focused snapshots,
   not only human-readable reports.
8. Add checker subsystems back only when a slice needs them, and make their
   ownership clear from the module name and public interface.

## Current Caveat

- `riot test -p typ` may fail snapshots while the expected checker output is
  being rebuilt around `Typ.Ast`; distinguish compile failures from intentional
  expected-output drift.
