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
- `artifact_store.ml`
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
7. Document native passes in their `.ml` and `.mli` modules. Do not maintain a
   separate markdown pass catalogue.

## Current Shape

Today the intended native stack is:

`Core_ir -> NIR -> MIR -> LIR -> Emitter -> Linker`

`NIR` is the first native-only layer.

Within `LIR`, the current pass shape is:

`layout_frames -> allocate_homes -> simplify -> schedule -> assign_homes`

`layout_frames` computes the frame skeleton and call facts. `allocate_homes`
does the first real location assignment pass: it uses `LIR` liveness to keep
short-lived values in a small caller-saved register pool, puts call-live
values in a small callee-saved pool, and spills the rest to stack homes while
reusing stack slots for non-overlapping spill intervals. The Darwin emitter is
responsible for saving and restoring the callee-saved homes that allocation
marks as used.

## Verification

Prefer:

- `riot build raml-native`

The native fixture harness and snapshots may still live under
`compiler/raml/tests/` while the package split settles. Treat that as
temporary, not as ownership.
