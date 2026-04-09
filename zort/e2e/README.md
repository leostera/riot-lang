# zort end-to-end smoke programs

This directory holds standalone Zig executables that link against `zort` and
run real runtime behavior.

The point is to validate more than subsystem unit tests:

- expected program behavior and output,
- expected trace/counter shapes from the event sink,
- lightweight benchmark signals for common paths.

Current cases:

- `alloc_gc_smoke.zig`: allocation, rooting, collection, reclamation, and basic
  trace counters.
- `effects_roundtrip_smoke.zig`: effect capture, suspended-stack inspection,
  continuation resume, and control-flow trace counters.
- `ml/*.ml`: compiler-emitted native smoke fixtures built with `ocamlopt.opt`
  in `-output-obj` mode and linked today against vendor `libasmrun` as the
  baseline runtime, with one narrow `zort`-linked exception for compiler-compat
  bring-up.

Run them with:

- `zig build e2e`
- `zig build e2e-ml`
- `zig build e2e-ml-zort`

They also run as part of:

- `zig build test`

These smoke programs are intentionally small and runtime-native first.
They are the foundation for a broader end-to-end pipeline where a RiotML
program is compiled to a binary that links with `zort` and executes under the
same harness shape.
