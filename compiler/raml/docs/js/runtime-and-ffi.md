# Raml JavaScript Runtime And FFI Notes

This document records the current runtime and interop contracts visible from
Melange's JS backend.

The main lesson is that the JS backend is tightly coupled to:

- a particular runtime library shape
- a particular OCaml-value encoding in JS
- a particular FFI attribute language
- a particular module-system and package-path model

## 1. The Runtime Is A Real ABI Surface

`core/js_runtime_modules.ml` names runtime modules directly:

- `Caml_array`
- `Caml_bytes`
- `Caml_exceptions`
- `Caml_float`
- `Caml_hash`
- `Caml_int32`
- `Caml_int64`
- `Caml_module`
- `Caml_obj`
- `Caml_option`
- `Caml_oo`
- `Curry`
- many others

The compiler refers to these names during lowering.

So the runtime is not a post-hoc library choice.
It is part of the backend ABI.

## 2. `Js` Is Mostly Interface, Not Runtime Logic

`runtime/js.pre.ml` says this explicitly:

- it should have no code
- its code should inline away
- there should never be `require("js")`

This module provides:

- types like `Js.null`, `Js.undefined`, `Js.nullable`
- externals such as `typeof`, `import`, and console logging
- aliases to `Js_*` support modules

That is a good design clue for `raml`.

The public user-facing JS API should be mostly:

- typed surface
- externals
- aliases

not one giant runtime module that must always be imported as a unit.

## 3. Option Encoding Is JS-Specific And Visible

`runtime/caml_option.ml` shows one of the clearest runtime contracts.

Important facts:

- `None` is represented as `undefined`
- `Some x` is often represented as `x`
- nested `Some` values need an extra wrapper marker
- nullable and optional conversions are runtime helpers

The nested-wrapper marker is the field:

- `MEL_PRIVATE_NESTED_SOME_NONE`

This is a concrete example of why JS representation must stay out of a shared
IR.

The semantic concept is:

- option values

The JS-specific representation is:

- `undefined`
- direct payload reuse
- nested-wrapper sentinel object

Those are different layers.

## 4. Block, Record, Module, And Variant Representation Is Also Visible

`js_of_lam_block` and friends make representation decisions based on
`Lam.Tag_info`.

The backend distinguishes shapes such as:

- arrays
- tuples
- records
- inline records
- modules
- variants
- exceptions and extension constructors
- polymorphic variants

The representation helpers then pick:

- array indexing
- object field access
- tagged block creation
- special extension accessors

So even before printing, the backend already knows whether a value becomes:

- array-like
- object-like
- special tagged helper structure

This is a JS-lowered concern, not a shared compiler-middle concern.

## 5. Recursive Modules Depend On Runtime Backpatching

`runtime/caml_module.ml` implements:

- `init_mod`
- `update_mod`

It builds dummy module shells and then backpatches them later.

That tells us two things.

### The compiler frontend semantics are not enough

Recursive modules need runtime support in the JS target.

### The exact runtime strategy is target-specific

The semantic requirement is:

- mutually recursive module initialization

The JS implementation strategy is:

- dummy object graph plus update

`raml` should preserve the semantic construct in shared lowering and choose the
runtime strategy later per backend.

## 6. Exceptions Are Also Runtime-Shaped

`runtime/caml_exceptions.ml` gives exceptions a JS-visible identifier field:

- `MEL_EXN_ID`

It also assigns unique suffix ids so functor-instantiated exceptions remain
distinct.

The compiler and runtime both rely on this representation.

Related lowering work includes:

- extension-slot handling
- `Pwrap_exn`
- exception wrapping when JS exceptions may flow into OCaml-style handling

Again, the semantic concept is backend-neutral.
The representation is JS-only.

## 7. Objects And OO Support Have Runtime Hooks Too

`runtime/caml_oo.ml` includes:

- method-cache tables
- OO id assignment
- method lookup helpers

That means JS object/class compilation is not just syntax lowering.
It also depends on runtime helper behavior.

This matters for `raml` even if objects are not part of the first JS slice:

- the backend must treat object support as a runtime-plus-compiler feature, not
  only as parser sugar

## 8. Package And Output Configuration Is Structured

`js_packages_info.ml` models package/output behavior with explicit data, not
just strings.

Important concepts:

- package name
- separate emission versus batch compilation
- module system
- output suffix
- package-relative path info
- module-name case rules

Supported output styles include at least:

- `CommonJS`
- `ESM`
- `ESM_global`

The current backend can emit one or several output styles depending on package
configuration.

For `raml`, that suggests a clean separation between:

- compilation result
- packaging/output policy

## 9. Import Path Resolution Is A Compiler Responsibility

`js_name_of_module_id.ml` resolves import paths for:

- runtime modules
- ordinary ML dependencies
- external packages
- script-mode dependencies

It uses:

- current package info
- dependency package info loaded from `.cmj`
- file-case info
- output module system
- output directory

This is more than pretty-printing.
It is backend dependency materialization.

The useful design fact for `raml` is:

- import resolution needs its own explicit layer

It should not be buried inside the final string printer.

## 10. Dynamic Import Is Supported, But Narrowly

Dynamic import is handled through:

- `Pimport`
- `dynamic_import` flags on module ids
- `lam_compile_dynamic_import`

The generated shape is:

- JS `import(module_path)`
- optional `.then(m => m.value)` wrapper if importing a specific module value

`lam_compile.ml` also enforces strong restrictions:

- the argument must be a module or module value tied to a file
- local values are rejected

So dynamic import is not a general-purpose effect in the IR.
It is a constrained module-loading feature.

That is a good design instinct for `raml` too.

## 11. The FFI Pipeline Has Several Layers

The current FFI path is:

1. source attributes are encoded into `[@mel.internal.ffi "..."]`
2. `Lam_convert` decodes that string into `External_ffi_types.t`
3. `Lam_ffi` lowers wrappers such as nullable returns and uncurried args
4. `Lam_compile_external_call` lowers the structured FFI spec to `J`

The structured FFI types distinguish several shapes:

- `Js_var`
- `Js_module_as_var`
- `Js_module_as_fn`
- `Js_module_as_class`
- `Js_call`
- `Js_send`
- `Js_new`
- property get/set
- index get/set

Argument specs model:

- labeled versus optional args
- constant inserted args
- uncurried function args
- unwrap behavior
- polymorphic-variant dispatch
- variadic spreading

Return wrappers model:

- identity
- replace with unit
- `null` to option
- `undefined` to option
- `null | undefined` to option

This is a lot of real structure.

The bad part is not the richness.
The bad part is that the shared IR boundary receives it through JS-specific
attribute decoding.

## 12. Variadics And Splicing Depend On Runtime Helpers

`lam_compile_external_call.ml` uses `Caml_splice_call` runtime helpers for
dynamic variadic cases such as:

- `spliceApply`
- `spliceNewApply`
- `spliceObjApply`

So even seemingly simple JS interop features may need runtime help when the
argument shape is not statically flat.

That is another reason to keep FFI lowering late and target-specific.

## 13. Raw JS Exists As A Dedicated Escape Hatch

The backend supports:

- `#raw_expr`
- `#raw_stmt`

These become `Praw_js_code` with classified raw-js payloads.

This is clearly JS-only.

If `raml` supports a similar escape hatch, it should stay in the JS backend
layer and not infect the shared compiler middle.

## 14. What `raml` Should Preserve

There are several good ideas here.

### Preserve explicit runtime modules

Keep the runtime ABI names and responsibilities documented and inspectable.

### Preserve typed FFI structure

Do not reduce FFI to arbitrary strings after parsing.
The structured spec types are useful.

### Preserve explicit package/path resolution

Import pathing needs its own phase and data model.

## 15. What `raml` Should Change

These are the main changes worth making.

### Move representation decisions later

Shared lowering should say:

- option
- variant
- record
- exception
- module

The JS backend should later decide:

- `undefined`
- tagged arrays
- objects
- wrapper sentinels

### Make FFI schema frontend-neutral

The shared compiler should probably carry a typed foreign declaration schema,
not a JS-only encoded attribute payload.

### Keep dynamic import as a JS backend feature

It belongs in JS-lowered IR, not shared middle IR.

That is the main compatibility extraction from this part of the codebase.
