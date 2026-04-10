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
- `zig build e2e-ml-zort` now runs four intentionally narrow fixtures against
  `zort`'s compiler-compatibility shim on `aarch64-apple-darwin`
- the compiler-compat startup path now registers linked frametable,
  `gc_roots`, and code/data segment tables in compatibility-owned state and
  verifies that `caml_program` lands in a registered code fragment before
  entering `caml_start_program`
- startup trace observability now distinguishes raw `gc_roots` table entries
  from scannable global blocks and their exposed field slots in the locked
  `aarch64-apple-darwin` compatibility layer
- the compiler-compat startup path is now reference-counted on the locked
  `aarch64-apple-darwin` path:
  - the first `caml_startup` performs metadata registration and enters
    `caml_start_program`
  - nested `caml_startup` calls are ignored apart from the ownership count
  - `caml_shutdown` only tears metadata down on the final matching call
  - calling `caml_startup` after a matched `caml_shutdown` now emits a
    deterministic OCaml-style fatal stderr line and aborts
  - calling `caml_shutdown` without a matching `caml_startup` now emits a
    deterministic OCaml-style fatal stderr line and aborts
- each zort-linked fixture now carries:
  - expected stdout
  - expected startup-metadata trace output
  - a recorded `bench_ns_per_run.txt` signal from the harness
  - fatal fixtures additionally carry expected stderr and exit code

Current cases:

- `noalloc_callback.ml`: no-allocation-ish callback smoke through
  `Callback.register`
- `alloc_pair_callback.ml`: tuple allocation and field inspection from C
- `external_identity_callback.ml`: external primitive call through a native C
  symbol plus callback registration
- `min_pure_startup.ml`: strict `-nostdlib -nopervasives` pure startup smoke
  intended to prove the smallest compiler-emitted object can run against the
  `zort` compiler-compatibility shim without depending on externals
- `min_pure_startup_reentrant`: the same strict pure-startup object under a C
  host that calls `caml_startup`/`caml_shutdown` twice to prove nested startup
  ownership and final teardown behavior in the compatibility layer
- `min_pure_startup_after_shutdown_fatal`: the same pure-startup object under a
  C host that calls `caml_startup`, `caml_shutdown`, and then `caml_startup`
  again to lock the forbidden restart-after-shutdown fatal path
- `min_pure_shutdown_without_startup_fatal`: the same pure-startup object under
  a C host that calls `caml_shutdown` before any `caml_startup` to lock the
  unmatched-shutdown fatal path
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

The current `zort`-linked fixtures are intentionally narrow:

- `min_pure_startup.ml` avoids both the standard library and externals so the
  harness can prove bare startup reaches compiler-emitted code
- `min_pure_startup_reentrant` keeps the same pure-startup object but proves
  nested startup ownership and final metadata teardown under an embedding host
- `min_pure_startup_after_shutdown_fatal` keeps the same pure-startup object
  but proves the shutdown latch is permanent for embedders that attempt a
  forbidden restart
- `min_pure_shutdown_without_startup_fatal` keeps the same pure-startup object
  but proves embedders get a deterministic fatal before any startup-owned
  metadata or depth changes when they over-release the lifecycle
- `min_external_startup.ml` adds one top-level external primitive on the same
  startup path

Successful `zort` milestones today:

- target: `aarch64-apple-darwin`
- compiler path:
  `~/.riot/toolchains/5.5.0-riot.2/aarch64-apple-darwin/bin/ocamlopt.opt`
- runtime path: `libzort-compiler-compat.dylib`
- observable pure-startup result: `output=unit`
- observable external-startup result: `output=42`
