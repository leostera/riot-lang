# raml AGENTS

`compiler/raml` is now the thin facade and integration package for the Raml
compiler family.

It should stay small.

## Ownership

`compiler/raml` owns:

- the public `Raml` API in `src/raml.ml`
- top-level compile orchestration in `src/raml_driver.ml`
- backend dispatch in `src/backend_compile.ml`
- integration helpers such as `src/example_pipeline.ml`
- fixture-only helpers that have not been moved out yet
- cross-backend integration tests that still live under `compiler/raml/tests/`

Shared frontend/core compiler logic and backend implementations live in the
owning `raml-*` packages.

## Route First

Before changing code, pick the right package:

- `compiler/raml-core/AGENTS.md`
  Shared `Config` / `Event` / `Target`, `Source_unit`, `Core_ir`,
  `Typ_lowering`, and frontend pipeline work.
- `compiler/raml-native/AGENTS.md`
  `NIR` / `MIR` / `LIR`, native emitter/linker, and native pass work.
- `compiler/raml-wasm/AGENTS.md`
  Wasm backend work.
- `compiler/raml-js/AGENTS.md`
  `JIR` / `JST`, JS runtime/import lowering, and JS pass work.
- `compiler/raml-cli/AGENTS.md`
  CLI argument parsing, emitted artifact writing, and repo-local JS runtime
  asset copying.
- `compiler/raml/AGENTS.md`
  Only for facade wiring, backend dispatch, public API, integration helpers,
  and cross-backend integration tests.

## Rules

1. Keep the facade thin, with backend logic in backend packages.
2. Shared compiler types and frontend stages belong in `raml-core`.
3. Native lowering/codegen belongs in `raml-native`.
4. Wasm lowering/codegen belongs in `raml-wasm`.
5. JS lowering/codegen belongs in `raml-js`.
6. If the public `Raml` contract changes, update the facade plus the owning
   package docs/AGENTS together.
7. If an integration helper only exists for tests, keep it out of the public
   API unless there is a deliberate reason to expose it.
