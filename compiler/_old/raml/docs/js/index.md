# Raml JavaScript Backend Manual

This directory is the step-1 manual for `compiler/raml`'s JavaScript backend
work.

It is a source-driven snapshot of what the current Melange backend does today
in:

- `3rdparty/melange/jscomp/common`
- `3rdparty/melange/jscomp/core`
- `3rdparty/melange/jscomp/runtime`

The point is not to treat Melange as the design for `raml`.

The point is to make the seams, invariants, runtime assumptions, and JS-
specific leaks explicit before we replace them with a Riot package that targets
JavaScript without poisoning the native and wasm paths.

## How To Read This Manual

Start here:

- [architecture.md](./architecture.md)
  the backend subsystem map, major modules, hooks, artifacts, and ownership
  boundaries
- [pipeline.md](./pipeline.md)
  the end-to-end compilation path from parsed source to `.cmj` and emitted JS
- [ir.md](./ir.md)
  the `Lam` and `J` IRs, what they encode, and what a shared `raml` IR must
  preserve
- [runtime-and-ffi.md](./runtime-and-ffi.md)
  runtime representation, package/import resolution, dynamic import, and FFI
  behavior
- [multi-backend-compatibility.md](./multi-backend-compatibility.md)
  what this backend shape implies for a multi-backend `raml` rewrite

## Scope

This manual covers:

- the high-level driver path in `js_implementation`
- Melange's backend-local `Lam` IR and optimization pipeline
- the JS-specific `J` IR and JS passes
- `.cmj` as the JS backend's cross-module artifact
- package/output-path resolution and import/export emission
- the visible runtime and FFI contracts this backend depends on

This manual does not deeply cover:

- the OCaml parser or typechecker in general
- `js_parser/`
- the test suite under `3rdparty/melange/jscomp/test`
- every runtime module in `runtime/`
- non-JS backends

That omission matters.

The current JS backend is not just "OCaml Lambda, but printed as JS".

The actual seam is:

- parse and rewrite
- typecheck
- OCaml Lambda
- Melange `Lam`
- Melange `Lam` normalization and grouping
- JS-specific `J`
- JS cleanup and shake passes
- `.cmj` summary emission
- CommonJS/ESM source printing

## What This Manual Owns

These docs are meant to keep ownership boundaries explicit.

- `architecture.md`
  owns the backend subsystem map and the major architectural seams
- `pipeline.md`
  owns the stage graph and handoff order
- `ir.md`
  owns the backend IR contracts and the leak points between shared and JS-only
  semantics
- `runtime-and-ffi.md`
  owns runtime representation, interop, package pathing, and module loading
- `multi-backend-compatibility.md`
  owns the implications for `compiler/raml` as a native/js/wasm compiler

If two docs start trying to own the same seam, one of them is too wide.

## Current Big Picture

The current Melange JS backend is organized around a few strong ideas:

- the frontend driver still owns parse, type, and Lambda extraction
- a backend-local Lambda derivative called `Lam` sits between OCaml Lambda and
  JS generation
- `Lam` already contains JS-specific primitives, FFI payloads, and dynamic-
  import state
- JS emission goes through a second IR, `J`, that is a JS subset specialized
  for OCaml runtime lowering
- `.cmj` is not only metadata; it also stores delayed JS programs and package
  info
- runtime data representation choices are visible from the compiler pipeline
  long before final printing

For `raml`, that means the job is not "rewrite one JS printer".

The job is to build a backend stack whose shared seams stay honest:

- backend-neutral enough for native and wasm
- explicit enough for JS runtime and FFI needs
- artifact-friendly enough for incremental and cross-module compilation

## Primary Source Anchors

The main source anchors used for this pass were:

- `3rdparty/melange/jscomp/core/js_implementation.cppo.ml`
- `3rdparty/melange/jscomp/core/initialization.cppo.ml`
- `3rdparty/melange/jscomp/core/ast_config.ml`
- `3rdparty/melange/jscomp/core/ast_io.cppo.ml`
- `3rdparty/melange/jscomp/core/lam.mli`
- `3rdparty/melange/jscomp/core/lam_primitive.mli`
- `3rdparty/melange/jscomp/core/lam_convert.cppo.ml`
- `3rdparty/melange/jscomp/core/lam_compile_main.cppo.ml`
- `3rdparty/melange/jscomp/core/lam_compile.ml`
- `3rdparty/melange/jscomp/core/lam_compile_env.mli`
- `3rdparty/melange/jscomp/core/lam_group.mli`
- `3rdparty/melange/jscomp/core/lam_coercion.mli`
- `3rdparty/melange/jscomp/core/lam_stats_export.ml`
- `3rdparty/melange/jscomp/core/j.ml`
- `3rdparty/melange/jscomp/core/js_output.mli`
- `3rdparty/melange/jscomp/core/js_dump_program.ml`
- `3rdparty/melange/jscomp/core/js_dump_import_export.ml`
- `3rdparty/melange/jscomp/core/js_packages_info.ml`
- `3rdparty/melange/jscomp/core/js_packages_state.ml`
- `3rdparty/melange/jscomp/core/js_name_of_module_id.ml`
- `3rdparty/melange/jscomp/core/js_pass_flatten_and_mark_dead.ml`
- `3rdparty/melange/jscomp/core/js_pass_scope.ml`
- `3rdparty/melange/jscomp/core/js_shake.ml`
- `3rdparty/melange/jscomp/core/lam_compile_external_call.ml`
- `3rdparty/melange/jscomp/core/lam_ffi.ml`
- `3rdparty/melange/jscomp/common/external_arg_spec.mli`
- `3rdparty/melange/jscomp/common/external_ffi_types.mli`
- `3rdparty/melange/jscomp/runtime/js.pre.ml`
- `3rdparty/melange/jscomp/runtime/caml_option.ml`
- `3rdparty/melange/jscomp/runtime/caml_module.ml`
- `3rdparty/melange/jscomp/runtime/caml_exceptions.ml`
- `3rdparty/melange/jscomp/runtime/caml_obj.ml`
- `3rdparty/melange/jscomp/runtime/caml_oo.ml`
