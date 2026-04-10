# Raml Wasm Pipeline Notes

This document turns the available Melange and `wasm_of_ocaml` sources into a
concrete view of the wasm pipeline choices `raml` has in front of it.

The main point is simple:

there is not one existing "OCaml wasm backend" shape to copy.

There are at least two materially different shapes:

- a high-level compiler-retargeting path, as seen in Melange's JS backend
- a bytecode-to-target path, as seen in `wasm_of_ocaml`

## 1. What The Local Melange Backend Actually Looks Like

The vendored Melange sources are not a generic "backend".

They are already a target-specific stack.

The clearest front door is `3rdparty/melange/jscomp/core/js_implementation.cppo.ml`.
For implementations, the path is:

1. parse and rewrite source
2. type it into a typedtree
3. call `Translmod.transl_implementation`
4. simplify the resulting Lambda with `Lambda_simplif.simplify_lambda`
5. hand the simplified Lambda to `Lam_compile_main.compile`

That means Melange's real backend input is not source and not typedtree.
It is already simplified OCaml Lambda.

## 2. Melange Does Not Go Straight From Lambda To Text

Inside `Lam_compile_main.compile`, Melange inserts several extra layers:

1. `Lam_convert.convert` rewrites OCaml Lambda into Melange's own `Lam.t`
2. a batch of `Lam_*` passes rewrites and optimizes that IR
3. `Lam_coercion.coerce_and_group_big_lambda` groups and export-shapes the IR
4. `Lam_compile.compile_lambda` lowers groups into `Js_output`
5. `Js_output` is assembled into `J.program`
6. `J.program` goes through JS-specific passes such as flattening, tailcall
   inlining, scope analysis, and shaking
7. the final result is emitted as JS plus `.cmj` metadata

So the real Melange stack is closer to:

- OCaml Lambda
- Melange `Lam`
- Melange JS IR `J`
- JS text plus package metadata

That is already a two-level target-specific IR family, not just a pretty
printer.

## 3. Melange's Intermediate Layers Already Carry Target Commitments

This matters for `raml`.

Melange's custom layers are not backend-neutral.

Examples:

- `lam_convert.cppo.ml` rewrites primitives according to JS runtime behavior
- exception packing is shaped around JS exception interop
- runtime helpers are named through `js_runtime_modules.ml`
- the final IR in `j.ml` is explicitly a JavaScript IR specialized for the
  OCaml Lambda backend
- `.cmj` artifacts store JS-oriented delayed programs, package specs, and
  closed lambdas for cross-module optimization

So even before `J.program`, the stack is already choosing:

- runtime module naming
- primitive lowering policy
- cross-module optimization artifact shape
- JS module/dependency packaging

This is a strong signal that a future wasm backend should not try to reuse
Melange's `Lam` or `J` as if they were neutral.

## 4. What `wasm_of_ocaml` Looks Like Upstream

The upstream docs describe a very different pipeline.

`wasm_of_ocaml` compiles OCaml bytecode programs to WebAssembly.

The documented path is:

1. compile with `ocamlc` to bytecode
2. run `wasm_of_ocaml` on the resulting `.byte`
3. get a JavaScript loading script and an `.assets` directory containing the
   Wasm code

So the visible stack is closer to:

- OCaml bytecode
- `wasm_of_ocaml`
- Wasm module plus JS loader/assets

That is a much lower entrypoint than Melange's Lambda path.

## 5. `wasm_of_ocaml` Treats Build Mode As A First-Class Concern

The Dune docs also matter here.

For `wasm_of_ocaml`, Dune exposes:

- `flags`
- `build_runtime`
- `link_flags`
- `compilation_mode` with `whole_program` or `separate`
- sourcemap control
- runtime alias selection

And the `wasm_of_ocaml` overview explicitly says Dune supports both standard
and separate compilation.

That means artifact shape is part of the backend contract.

It is not an afterthought that can be bolted on after code generation.

## 6. Wasm Primitives Are Also Part Of The Pipeline

Upstream `wasm_of_ocaml` does not stop at "emit Wasm".

It also exposes a primitive model:

- user primitives can be written as `.wat` or `.wasm` modules
- these runtime files are linked through Dune's `wasm_files`
- optional JavaScript files can still be linked for side-effect declarations or
  host functions

So the wasm pipeline already includes:

- code generation
- runtime imports
- sidecar runtime modules
- host-facing JavaScript glue

That is not far from the target-specific runtime packaging Melange does for JS.

## 7. The Important Comparison

The two upstream shapes differ at the most important seam.

### Melange

- enters at high-level simplified Lambda
- invents target-specific IRs early
- carries runtime and packaging assumptions through the middle of compilation
- emits per-module optimization artifacts

### `wasm_of_ocaml`

- enters at bytecode
- reuses the stable OCaml bytecode contract
- emits Wasm plus loader/assets
- treats build mode and runtime files as core backend concerns

Both are coherent.
They are coherent for different goals.

## 8. What This Means For `raml`

`raml` is supposed to be a multi-backend compiler with at least:

- native
- JS
- wasm

That makes one conclusion hard to avoid:

the shared `raml` boundary should not be Melange's JS-shaped IR stack and it
should not be OCaml bytecode either.

If `raml` chooses Melange-style `Lam` as the shared IR, wasm will inherit JS
runtime assumptions too early.

If `raml` chooses bytecode as the shared IR, native and JS will inherit an
OCaml-toolchain compatibility boundary that is too low-level and too specific.

## 9. A Better `raml` Decomposition

A more defensible stack is:

1. frontend semantic lowering
2. one shared backend-neutral `Raml Core IR`
3. target-specific lowering families:
   - JS lowering
   - Wasm lowering
   - native lowering
4. per-target artifact and packaging layers

That keeps the important seams honest:

- frontend lowering remains shared
- wasm gets its own runtime and import model
- JS keeps its own object-model and module packaging
- native keeps its own ABI and target codegen path

## 10. The Big Pipeline Conclusion

The wasm job for `raml` is not:

- "copy Melange and swap out the printer"

and it is not:

- "make bytecode the whole compiler contract"

It is:

- define a shared `Raml Core IR` that preserves the semantics all three
  backends need
- give wasm its own lowering, runtime, and artifact story
- keep separate compilation, runtime sidecars, and host integration explicit
  from day one
