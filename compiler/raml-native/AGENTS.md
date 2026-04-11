# raml-native AGENTS

`compiler/raml-native` owns the native backend.

## Read First

- [NATIVE_LOOP.md](/Users/leostera/Developer/github.com/leostera/riot/compiler/raml-native/NATIVE_LOOP.md)
- `compiler/raml/docs/native/index.md`
- `compiler/raml/docs/native/strategy.md`
- `compiler/raml/docs/native/pipeline.md`
- `compiler/asm/AGENTS.md`

## Ownership

This package owns:

- `backend.ml`
- `native.ml`
- `nir/`
- `mir/`
- `lir/`
- `emitter/`
- `linker/`

## Rules

1. Keep pass threading explicit. Do not reintroduce a generic native pass
   framework.
2. Keep `aarch64-apple-darwin` as the primary target until it is good.
3. Do not widen to new native targets just to prove abstraction purity.
4. Keep backend-neutral semantics in `raml-core`; native-only runtime/layout
   choices belong here.
5. Use `compiler/asm` for typed assembly DSL work instead of open-coded target
   text machinery where that package can own the concern.
6. Snapshot every named native pass that materially changes the program.

## Current Shape

Today the intended native stack is:

`Core_ir -> NIR -> MIR -> LIR -> Emitter -> Linker`

`NIR` is the first native-only layer.

## Verification

Prefer:

- `riot build raml-native`

The native fixture harness and snapshots may still live under
`compiler/raml/tests/` while the package split settles. Treat that as
temporary, not as ownership.
