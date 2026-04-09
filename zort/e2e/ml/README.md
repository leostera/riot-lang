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

Current cases:

- `noalloc_callback.ml`: no-allocation-ish callback smoke through
  `Callback.register`
- `alloc_pair_callback.ml`: tuple allocation and field inspection from C
- `external_identity_callback.ml`: external primitive call through a native C
  symbol plus callback registration

Run them with:

- `zig build e2e-ml`

or directly:

- `./e2e/compile_ml_examples.sh`

Generated artifacts land in:

- `zort/zig-out/e2e-ml/`

These fixtures are not yet linked against `zort`.
They are the baseline compiler-emitted programs we will later use to prove:

- startup compatibility
- raw-value ABI compatibility
- allocation/GC compatibility
- external primitive compatibility
- callback/effect compatibility
