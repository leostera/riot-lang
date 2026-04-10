# Raml Wasm Runtime Notes

This document records the runtime-facing facts that show up in the local
Melange sources and in the upstream `wasm_of_ocaml` docs.

The main point is simple:

for JS and wasm, the runtime is part of the backend architecture.

It is not only "whatever library gets linked later".

## 1. Melange's Runtime Is Part Of Code Generation

The local Melange backend makes this very obvious.

`js_runtime_modules.ml` hard-codes target runtime module names such as:

- `Caml_exceptions`
- `Caml_io`
- `Caml_array`
- `Caml_string`
- `Caml_float`
- `Caml_int64`
- `Curry`

Those names are not a packaging detail at the edge.
They are referenced during compilation and dependency analysis.

`lam_compile_main.cppo.ml` also computes hard dependencies, module effects, and
`.cmj` metadata around those runtime/module dependencies.

So the runtime surface is already threaded through:

- primitive lowering
- dependency collection
- purity/effect reasoning
- artifact generation

## 2. Melange Ships A Target-Specific Standard Runtime Library

`3rdparty/melange/jscomp/runtime/dune` shows that Melange builds a dedicated
runtime library in `melange` mode.

That runtime build is not neutral.

It carries flags such as:

- `-mel-no-check-div-by-zero`
- `-mel-cross-module-opt`
- `-unsafe`

and assembles modules like:

- `caml_io`
- `caml_parser`
- `caml_lexer`
- `js_*` interop modules
- generated curry helpers

This is a good warning for `raml`:

if wasm becomes a real backend, it will need its own runtime package surface,
not only a code emitter.

## 3. Melange Also Documents Semantic Runtime Drift

`3rdparty/melange/docs/Manual.html` makes the runtime drift explicit.

Examples called out there include:

- no C custom blocks because there is no C FFI
- physical equality is runtime-dependent and should not be treated as stable
- integer behavior differs from native OCaml in important places
- weak pointers do not behave like native OCaml's weak structures

So even when the source language looks like OCaml, the backend runtime can
change observable semantics.

That is another reason the runtime contract must stay explicit.

## 4. `wasm_of_ocaml` Exposes A Different Runtime Surface

The upstream `wasm_of_ocaml` manual describes a Wasm-GC-oriented runtime model.

At the value boundary:

- all OCaml values use `ref eq`
- immediates such as ints, chars, booleans, and constant constructors use
  `ref i31`

The documented heap shapes include distinct Wasm GC forms for:

- generic blocks
- bytes/strings
- boxed floats
- float arrays
- wrapped JavaScript values

That already tells us the wasm path is not just "JS runtime modules but in
binary form".

It uses a different value representation and a different primitive boundary.

## 5. `wasm_of_ocaml` Makes Primitive Imports Explicit

The primitive docs also show that wasm code imports helper functions for boxed
integer families such as:

- `Int32_val`
- `caml_copy_int32`
- `Nativeint_val`
- `caml_copy_nativeint`
- `Int64_val`
- `caml_copy_int64`

User-defined primitives are also explicit:

- write a `.wat` or `.wasm` module
- export functions over the OCaml value type
- optionally link JS files for side-effect metadata or host operations

That means a wasm backend needs a real import/export ABI, not just a list of
symbol names.

## 6. `wasm_of_ocaml` Still Depends On A Host Story

The overview page says `wasm_of_ocaml` is for running pure OCaml programs in
JavaScript environments such as browsers and Node.js.

It emits:

- a JavaScript loading script
- an asset directory containing the Wasm code

It also documents gaps and constraints:

- most of `Sys` is unsupported
- `Dynlink` is not supported
- building a toplevel is not supported
- the virtual filesystem is not implemented
- some library support is partial or host-shaped

So the wasm target is still not host-neutral.

The host boundary is just different from the JS backend's host boundary.

## 7. Effects Are A Runtime Strategy Choice

The wasm overview documents two effect-handler strategies:

- `--effects=cps`
- `--effects=jspi`

That is a very important architectural signal.

Effect support for wasm is not only a frontend typing question.
It is also a backend/runtime strategy question.

So `raml`'s wasm design needs one explicit owner for:

- effect lowering strategy
- host requirements
- performance tradeoffs
- fallback behavior

## 8. What `zort` Should Own

`zort/ARCHITECTURE.md` already draws the right boundary:

- semantic runtime core inside
- compatibility universe outside

For wasm, the semantic core should still own:

- heap storage
- allocation/mutation policy
- collection
- control/effects kernel
- primitive registry
- capability-gated host substrate

That keeps `zort` coherent even if the compiler compatibility layer changes.

## 9. What A Wasm Compatibility Layer Still Needs To Own

Even with a semantic `zort` core, a wasm-targeted compiler layer still needs a
dedicated compatibility/runtime surface for:

- value representation at the Wasm boundary
- imported primitive signatures
- JS interop objects and calls
- startup and module initialization
- asset and loader layout
- unsupported-library policy
- effect mode selection and host requirements

Those are backend-facing concerns.
They should not be smeared into the semantic core.

## 10. Runtime Design Pressure On `raml`

Taken together, the runtime analysis suggests a few hard rules.

### Keep runtime families per backend

JS and wasm should each get their own runtime family, even if large parts are
shared conceptually.

### Separate runtime primitives from foreign primitives

The compiler needs to know whether it is calling:

- a guaranteed runtime helper
- a host import
- a user primitive

Those are not the same thing.

### Keep host capabilities explicit

Browser, Node, and WASI are not one runtime.
The wasm backend should expose its host assumptions explicitly.

### Keep effect strategy explicit

A wasm backend needs one declared effect story.
It cannot treat effects as an invisible late detail.

## 11. The Big Runtime Conclusion

For `raml`, a wasm backend is not only:

- Wasm code generation

It is also:

- a value representation
- a primitive ABI
- a host integration model
- a capability policy
- an effects strategy

That is the runtime contract the compiler has to target.
