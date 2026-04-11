# raml-cli AGENTS

`compiler/raml-cli` owns the executable entrypoint for the Raml compiler.

It should stay thin.

## Ownership

This package owns:

- CLI argument parsing
- target-string parsing at the command boundary
- emitted artifact writing
- temporary runtime companion copying needed to execute JS output locally

This package does **not** own:

- frontend lowering or `Core_ir`
- JS/native/wasm backend implementation details
- compiler facade orchestration beyond calling the public `Raml` API

## Rules

1. Depend on the public `Raml` facade, not backend packages directly.
2. Keep compile orchestration in `compiler/raml`; keep this package as a thin
   shell around it.
3. If the CLI needs new compiler data, prefer adding a narrow compiler accessor
   over parsing internal pipeline JSON in the binary.
4. JS runtime asset copying is a temporary repo-local packaging step. Do not
   move backend logic into this package.

## Verification

Prefer:

- `riot build raml-cli`
- `./_build/default/compiler/raml-cli/src/raml --help`
