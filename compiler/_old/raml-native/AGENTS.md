# raml-native AGENTS

`compiler/raml-native` owns the native backend.

## Read First

- [NATIVE_LOOP.md](NATIVE_LOOP.md)
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

1. Keep pass threading explicit.
2. Keep `aarch64-apple-darwin` as the primary target until it is good.
3. Add native targets when there is a real target need and implementation path.
4. Keep backend-neutral semantics in `raml-core`; native-only runtime/layout
   choices belong here.
5. Use `compiler/asm` for typed assembly DSL work where that package can own
   the concern.
6. Snapshot every named native pass that materially changes the program.
7. Document native passes in their `.ml` and `.mli` modules.

## Current Shape

Today the intended native stack is:

`Core_ir -> NIR -> MIR -> LIR -> Emitter -> Linker`

`NIR` is the first native-only layer.

Within `LIR`, the current pass shape is:

`simplify -> dead_code -> schedule -> layout_frames -> allocate_homes -> assign_homes -> legalize -> calling_convention`

The cheap virtual cleanups happen first. `simplify` and `dead_code` trim the
linear stream while values are still virtual, and `schedule` then removes the
label/jump clutter that cleanup leaves behind. `layout_frames` runs after that
so frame analysis only sees the body that will actually survive. `allocate_homes`
does the first real location assignment pass: it uses `LIR` liveness to keep
short-lived values in a small caller-saved register pool, puts call-live
values in a small callee-saved pool, and spills the rest to stack homes while
reusing stack slots for non-overlapping spill intervals. The Darwin emitter is
responsible for saving and restoring the callee-saved homes that allocation
marks as used. `assign_homes` then rewrites virtual names to those concrete
homes. `legalize` performs the target-owned reload step that makes scratch
register traffic explicit, and `calling_convention` lowers entry parameters,
call arguments, and call results into ordinary `LIR` moves using the shared
compilation context. That keeps ABI shuffling out of the emitter.

Target-specific register and toolchain policy should live in
`target_profile.ml`, not be duplicated across `allocate_homes`, `legalize`,
`calling_convention`, `Emitter`, and `Linker`.

The native fixture harness and snapshots may still live under
`compiler/raml/tests/` while the package split settles. Treat that as
temporary, not as ownership.
