# Compiler-Emitted OCaml E2E Fixtures

These fixtures are the first compiler-facing bridge in `zort/e2e`.

They are intentionally small `.ml` programs compiled with:

- `~/.riot/toolchains/5.5.0-riot.2/aarch64-apple-darwin/bin/ocamlopt.opt`
- native `-output-obj` mode

Why `-output-obj`:

- it emits native OCaml code plus startup glue into an object file,
- it is the right mode for embedding or linking against a non-default runtime,
- it is the closest current baseline to "same compiler-emitted object, later
  linked against zort instead of vendor `libasmrun`".

Current workflow:

1. compile each `.ml` fixture with `ocamlopt.opt -output-obj`
2. link it with a tiny C host stub
3. link today against vendor `libasmrun` as the baseline runtime
4. keep the same `.ml` fixtures for the future `zort` compiler-compatibility
   shim

Current status:

- `zig build e2e-ml` runs the baseline fixtures against vendor `libasmrun`
- `zig build e2e-ml-zort` currently runs one intentionally narrow fixture
  against `zort`'s compiler-compatibility shim on `aarch64-apple-darwin`

Current cases:

- `noalloc_callback.ml`: no-allocation-ish callback smoke through
  `Callback.register`
- `alloc_pair_callback.ml`: tuple allocation and field inspection from C
- `external_identity_callback.ml`: external primitive call through a native C
  symbol plus callback registration
- `min_external_startup.ml`: strict `-nostdlib -nopervasives` top-level external
  call intended as the first compiler-emitted program that can run against the
  `zort` compiler-compatibility shim

Run them with:

- `zig build e2e-ml`
- `zig build e2e-ml-zort`

or directly:

- `./e2e/compile_ml_examples.sh`
- `./e2e/compile_zort_ml_examples.sh <path-to-libzort-compiler-compat.dylib>`

Generated artifacts land in:

- `zort/zig-out/e2e-ml/`

Most of these fixtures are not yet linked against `zort`.
They are the baseline compiler-emitted programs we will later use to prove:

- startup compatibility
- raw-value ABI compatibility
- allocation/GC compatibility
- external primitive compatibility
- callback/effect compatibility

The current `zort`-linked exception is `min_external_startup.ml`, which is
intentionally narrower:

- it avoids the standard library entirely,
- it uses only a top-level external primitive,
- and it exists to prove the first "compiler-emitted OCaml code ran against a
  zort shim" milestone before broader stdlib/runtime compatibility exists.

Successful `zort` milestone today:

- target: `aarch64-apple-darwin`
- compiler path:
  `~/.riot/toolchains/5.5.0-riot.2/aarch64-apple-darwin/bin/ocamlopt.opt`
- runtime path: `libzort-compiler-compat.dylib`
- observable result: `output=42`
