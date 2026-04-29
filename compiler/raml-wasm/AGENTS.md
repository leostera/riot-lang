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

Grow the wasm backend here; `compiler/raml` and `raml-core` stay facade/core
packages.

## Rules

1. Keep wasm separate from native until a real shared post-`Core_ir` layer
   proves itself.
2. Keep native ABI and object-format assumptions in native backend packages.
3. Keep host/target routing early: wasm targets should route here directly.
4. If wasm needs shared semantics, add them to `raml-core`, not to the facade.

## Read First

- `compiler/raml/docs/wasm/index.md`
- `compiler/raml/docs/wasm/sketch.md`
- `compiler/raml/docs/wasm/pipeline.md`
- `compiler/raml/docs/wasm/ir.md`
