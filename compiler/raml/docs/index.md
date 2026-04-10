# Raml Compiler Manual

This directory is the top-level manual for `compiler/raml`.

The backend manuals under `native/`, `js/`, and `wasm/` are intentionally
source-driven snapshots of existing systems:

- `native/`
  current OCaml native backend structure
- `js/`
  current Melange JavaScript backend structure
- `wasm/`
  current Melange and `wasm_of_ocaml` wasm-relevant structures

The point of this top-level manual is different.

The point here is to state the shared `raml` architecture we are actually
trying to build on top of those findings.

## How To Read This Manual

Start here:

- [architecture.md](./architecture.md)
  the shared `raml` pipeline, IR stack, and backend split

Then read the backend manuals:

- [native/index.md](./native/index.md)
  the native backend snapshot and `zort` compatibility pressure
- [js/index.md](./js/index.md)
  the JavaScript backend snapshot, runtime shape, and IR lessons
- [wasm/index.md](./wasm/index.md)
  the wasm pipeline/runtime comparison and `zort` compatibility pressure

## Scope

This top-level manual covers:

- the intended shared architecture for `compiler/raml`
- the ownership boundary between `typ`, `raml`, and backend-specific lowering
- the shared-versus-backend-specific IR split
- the relation between the JS backend and the native/wasm-on-`zort` backends
- the role of artifacts and separate-compilation metadata in the compiler

This top-level manual does not deeply cover:

- backend-specific runtime representation details
- target-specific native code generation details
- JS module-system details
- wasm host and loader details

Those all belong in the backend manuals.

## What This Manual Owns

These docs are meant to keep ownership boundaries explicit.

- `index.md`
  owns the top-level routing and reading order
- `architecture.md`
  owns the shared `raml` compiler stack and backend split
- backend manuals
  own the source-driven details and compatibility implications for each backend

If the top-level docs start re-explaining backend internals in detail, they are
too wide.

## Current Big Picture

The architecture this manual describes is:

- `typ` owns semantic analysis
- `raml` owns compiler-facing executable IRs and backend orchestration
- one shared `Raml Core IR` sits above all backends
- JavaScript lowers from that shared IR into a JS-specific `JIR`
- native lowers directly from that shared IR into native-only late IRs such as
  `NIR`, `MIR`, and `LIR`
- wasm lowers directly from that shared IR into wasm-only runtime/host IR
  layers
- a shared post-`Core` IR should only be reintroduced later if the implemented
  native and wasm paths prove they genuinely need one

That is the main architectural conclusion produced by the native, JS, and wasm
manuals together.
