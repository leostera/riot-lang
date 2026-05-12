# Raml Wasm Backend Manual

This directory is the working manual for `raml-wasm`.

It is a source-driven snapshot of two related but different things:

- the current Melange backend shape vendored in this repo under
  `3rdparty/melange/jscomp`
- the current upstream `wasm_of_ocaml` compilation and runtime model

The point is not to treat either project as the design for `raml-wasm`.

The point is to make the seams, invariants, and runtime assumptions explicit
before `raml` grows a Riot package that can target native, JS, and wasm
together.

## Important Local Fact

This checkout does not currently vendor a local
`3rdparty/melange/wasm-of-ocaml` tree.

The closest local compiler sources are:

- `3rdparty/melange/jscomp/core`
- `3rdparty/melange/jscomp/runtime`
- `3rdparty/melange/docs/Manual.html`

So this manual is intentionally split:

- local facts come from the vendored Melange JS backend
- wasm-specific facts come from upstream `wasm_of_ocaml` primary docs and repo
  layout

## How To Read This Manual

Start here:

- [sketch.md](./sketch.md)
  the concrete `raml-wasm` package shape and first owned seams
- [pipeline.md](./pipeline.md)
  the two upstream pipeline shapes and what they imply for `raml`
- [runtime.md](./runtime.md)
  the runtime and host boundary lessons from Melange and `wasm_of_ocaml`
- [ir.md](./ir.md)
  what a shared `raml` IR stack has to preserve for native, JS, and wasm
- [grain-notes.md](./grain-notes.md)
  what Grain's wasm-first backend teaches us, and where wasm really overlaps
  with native
- [zort-compatibility.md](./zort-compatibility.md)
  what this means for a wasm target that wants to sit on `zort`

## Scope

This manual covers:

- the current Melange JS backend path from OCaml Lambda into target-specific
  IRs and artifacts
- the current `wasm_of_ocaml` path from OCaml bytecode into Wasm plus loader
  artifacts
- runtime representation and primitive-boundary facts visible from those
  systems
- the architectural pressure those facts put on `compiler/raml`
- a practical interpretation of those facts for a `zort`-targeted wasm path

This manual does not deeply cover:

- the OCaml native backend
- the full internal implementation of upstream `wasm_of_ocaml`
- Binaryen internals
- the full JS host runtime used by `js_of_ocaml`

Those are separate concerns.
The native half already has its own manual under `../native`.

## What This Manual Owns

These docs are meant to keep ownership boundaries explicit.

- `sketch.md`
  owns the concrete `raml-wasm` package shape and the first `WIR` boundary
- `pipeline.md`
  owns the stage graph and the main handoff choices
- `runtime.md`
  owns runtime representation, primitive boundaries, host integration, and
  feature gaps
- `ir.md`
  owns the shared `raml` IR requirements for a real multi-backend compiler
- `grain-notes.md`
  owns the Grain-specific backend lessons and the native/wasm overlap sketch
- `zort-compatibility.md`
  owns the compatibility implications for a `zort`-hosted wasm target

If two docs start trying to own the same seam, one of them is too wide.

## Current Big Picture

The current ecosystem shows two very different wasm-relevant strategies.

- Melange retargets the OCaml compiler early, at the Lambda layer, and then
  builds target-specific IRs, passes, runtime modules, and artifacts.
- `wasm_of_ocaml` keeps the stable bytecode entrypoint and compiles bytecode to
  Wasm, with a JavaScript loader and asset directory around it.
- Both strategies work, but they solve different problems.
- Neither is a good whole-architecture answer for `raml`, because `raml` is
  supposed to be a multi-backend compiler package, not only a JS fork or only
  a bytecode post-processor.

For `raml`, the important conclusion is:

- the shared compiler boundary should stay above JS-specific and wasm-specific
  lowering
- separate compilation and artifact metadata must be first-class
- runtime compatibility concerns must stay outside the semantic `zort` core

## Primary Source Anchors

Local Melange anchors:

- `3rdparty/melange/jscomp/core/js_implementation.cppo.ml`
- `3rdparty/melange/jscomp/core/lam_convert.cppo.ml`
- `3rdparty/melange/jscomp/core/lam.mli`
- `3rdparty/melange/jscomp/core/lam_compile_main.cppo.ml`
- `3rdparty/melange/jscomp/core/j.ml`
- `3rdparty/melange/jscomp/core/js_cmj_format.mli`
- `3rdparty/melange/jscomp/core/js_packages_info.mli`
- `3rdparty/melange/jscomp/core/js_runtime_modules.ml`
- `3rdparty/melange/jscomp/runtime/dune`
- `3rdparty/melange/docs/Manual.html`

Upstream wasm anchors:

- <https://ocsigen.org/js_of_ocaml/latest/manual/wasm_overview>
- <https://ocsigen.org/js_of_ocaml/latest/manual/wasm_runtime>
- <https://dune.readthedocs.io/en/latest/reference/dune/env.html>
- <https://github.com/ocaml-wasm/wasm_of_ocaml>

`zort` anchors:

- `zort/ARCHITECTURE.md`
- `zort/spec/compiler-runtime-integration.md`
