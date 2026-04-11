# raml-wasm AGENTS

`compiler/raml-wasm` owns the wasm backend.

## Ownership

This package owns wasm-only lowering and codegen work.

Right now the first owned seams are:

- `WIR`: wasm-specific lowering, runtime import discovery, and backend passes
- `Codegen`: the first runnable wasm slice, currently a narrow direct binary
  emitter plus a Node-compatible runner string
- `Artifact_store`: wasm-owned semantics for objects, linked programs, and
  runnable module artifacts on top of a caller-provided `Contentstore`

Grow the wasm backend here instead of adding speculative wasm code back into
`compiler/raml` or `raml-core`.

## Rules

1. Keep wasm separate from native until a real shared post-`Core_ir` layer
   proves itself.
2. Do not force native ABI or object-format assumptions into wasm.
3. Keep host/target routing early: wasm targets should route here directly.
4. If wasm needs shared semantics, add them to `raml-core`, not to the facade.

## Read First

- `compiler/raml/docs/wasm/index.md`
- `compiler/raml/docs/wasm/sketch.md`
- `compiler/raml/docs/wasm/pipeline.md`
- `compiler/raml/docs/wasm/ir.md`

## Verification

Prefer:

- `riot build raml-wasm`
