# raml AGENTS

`compiler/raml` is Riot's multi-backend compiler package.

It sits between `typ`'s semantic tree and backend-specific lowerings for JS,
native, and wasm.

## Read First

Start here before changing code:

- `compiler/raml/docs/index.md`
- `compiler/raml/docs/architecture.md`
- `compiler/raml/JS_LOOP.md`
- `compiler/raml/NATIVE_LOOP.md`
- `compiler/raml/TODO.md`

Then read the owning backend manual:

- `compiler/raml/docs/js/index.md`
- `compiler/raml/docs/native/index.md`
- `compiler/raml/docs/wasm/index.md`

For native-backend work, also read:

- `compiler/raml/docs/native/strategy.md`
- `compiler/raml/NATIVE_LOOP.md`

If the change touches the `typ -> raml` handoff, also read:

- `packages/typ/AGENTS.md`

## Current Compiler Shape

The shared compiler center is `Raml Core IR`.

Today that means:

- `Core_ir.Compilation_unit` is the shared unit-level wrapper
- `Core_ir.Binding_group` owns ordered init groups plus exports
- `Core_ir.Init_item` distinguishes named `Binding` items from effectful `Eval`
  items
- `Core_ir.Expr` is Lambda-shaped: `Constant`, `Var`, `Apply`, `Lambda`,
  `Let`, `Sequence`, `If_then_else`, and `Primitive`

Backend ownership is:

- JS backend code lives under `compiler/raml/src/js/`
- `JIR` is JS-late and may become JS-shaped; grow
  `compiler/raml/src/js/jir/`
- `JST` is the final JS syntax/emission tree and should live under
  `compiler/raml/src/js/jst/`
- native backend code should grow under `compiler/raml/src/native/`
- the native scaffold now lives under:
  `compiler/raml/src/native/nir/`,
  `compiler/raml/src/native/mir/`,
  `compiler/raml/src/native/lir/`,
  and `compiler/raml/src/native/emitter/`
- the first native late IR is `NIR`, followed by `MIR` and `LIR`
- wasm backend code should grow under `compiler/raml/src/wasm/`
- wasm should get its own post-`Core_ir` runtime/host IR family
- only extract a shared post-`Core_ir` native/wasm layer later if the
  implemented backends prove they actually share one
- backend selection should be driven by explicit `host` and `target` triples;
  the target triple chooses the backend family, and the host triple informs
  toolchain/runtime decisions around that backend
- `Core_ir` must stay backend-neutral

Top-level compiler entrypoints are exposed through:

- `compiler/raml/src/raml.mli`
- `Raml.Config`
- `Raml.Event`
- `Raml.compile`
- `Raml.compile_source`

Use `Raml.Config.make ~host ~target ()` when backend selection matters.
Do not infer the backend family from the machine running the compiler.
The target triple decides the backend; the host triple only describes where
the compiler is executing.

Keep diagnostics/event emission structured. Do not replace it with ad hoc text
logging.

## Rules

1. Work example-first, not pass-first. Grow the compiler by making one source
   example move coherently through `Core_ir`, the implemented backend IRs, and
   codegen.
2. Do not move to the next example until the current one is supported across
   every backend layer that already exists.
3. Keep `Core_ir` backend-neutral. Do not leak raw JS, JS-specific optional
   encoding, ESM/CJS choices, wasm imports, or native ABI details into the
   shared IR.
4. Put JS-specific runtime and module-system choices in `JIR`, not in
   `Core_ir`.
5. Do not invent a shared native/wasm post-`Core_ir` layer in advance. Grow
   native and wasm separately until a real shared seam proves itself.
6. Preserve structured compiler events. If a top-level compiler stage changes,
   update `Raml.Event` payloads and their callers deliberately.
7. Prefer explicit unsupported cases with structured errors over silent
   fallback or implicit dropping of semantics.
8. When IR contracts move, update docs, snapshots, `TODO.md`, and this
   `AGENTS.md` in the same change.

## Testing

`compiler/raml` is snapshot-driven.

Use the existing fixture families under `compiler/raml/tests/fixtures/`:

- `corpus/`
- `core_ir/`
- `typ_lowering/`
- `jir/`
- `jir_lowering/`
- `js/`
- `native/`
- `wasm/`

The shared `*.ml` source corpus lives under `compiler/raml/tests/fixtures/corpus/`.
Backend-specific suites should read from that corpus when they are source-driven
and keep their approved snapshots under backend directories such as `js/`,
`native/`, or `wasm/`.
Shared IR input fixtures such as `core_ir/`, `jir/`, and `jir_lowering/` may
keep their source `.json` inputs where they are, but their backend-owned
`.expected` files should still live under the backend snapshot directories.
Ordered corpus filenames like `0001_hello_world.ml` are fixture names, not
compiler-facing module identities. When feeding corpus files into the compiler,
strip the numeric ordering prefix from the logical relpath first.

The native scaffold currently reuses `core_ir/` fixtures and snapshots
`*.nir.expected`, `*.mir.expected`, `*.lir.expected`, and `*.native.expected`
next to the input `*.json`.
Native work should grow toward corpus-driven fixture coverage and explicit
snapshot surfaces for every named native pass.

For new compiler behavior:

1. Add or update the source example first.
2. Snapshot the shared IR and backend projections separately.
3. Prefer small, readable snapshots over one giant end-to-end dump.
4. Keep example fixtures cross-target so feature drift is obvious.

The example-driven suite is the cross-backend regression layer.
Keep it centered on `Raml.Example_pipeline`, so every example snapshots the
shared `Core_ir` view plus backend projections in one place.

Use the public `Raml.compile_source` / `Raml.compile` API to snapshot the
selected-backend contract separately through `Raml.Compilation`.

## Validate

Run this stack in order:

```sh
riot fix ./compiler/raml
riot fmt ./compiler/raml
riot build raml
riot test -p raml
git diff --check -- compiler/raml
```

Interpret results carefully:

- snapshot drift is not automatically a regression; inspect whether the old
  behavior or the new behavior is wrong
- if the IR contract changed intentionally, update the snapshots in the same
  change
- if `riot test -p raml` stops running the fixture bins, the harness regressed

## Common Pitfalls

- Do not fix a JS example by hardcoding runtime names directly in the emitter.
  Decide the JS boundary in `JIR` or in typing/runtime setup first.
- Do not add backend-specific fields to `Core_ir` just to get one example
  green.
- Do not hide top-level side effects behind fake named bindings. Use explicit
  init/eval items.
