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

The current `raml` slice already has a small explicit version of that rule.
Generated JS imports helpers from a sibling `./riot-runtime.js`, and the
currently owned surface is:

- `print_endline`
- `print_newline`
- `print_int`
- `print_string`
- `print_char`
- `makeCurried` for multi-parameter compiled lambdas whose source-level
  applications may be under-applied
- `callPrimitive` for `%addint`, `%subint`, `%mulint`, `%divint`, `%modint`,
  `%addfloat`, `%subfloat`, `%mulfloat`, `%divfloat`, `%concatstring`,
  `%string_of_int`, `%string_of_float`, `%int_of_string`,
  `%float_of_string`, `%sqrtfloat`, `%lt`, `%le`, `%gt`, `%ge`, `%eq`,
  `%neq`, `%trace`, `%tuple_make`, and `%tuple_get`

That surface is intentionally small, but it is already an ABI boundary the JS
backend chooses before final printing.
The current source-driven comparison coverage deliberately reaches direct
`<`, `<=`, `>`, and `>=` calls lowering through `%lt`, `%le`, `%gt`, and `%ge`;
keep structural ordered comparison separate until narrower fixtures prove it is
needed.
The current JS source-driven float coverage now also reaches direct `+.`,
`*.`, and `sqrt` calls lowering through `%addfloat`, `%mulfloat`, and
`%sqrtfloat` instead of emitting bare float operators or ambient globals.
The current JS source-driven string coverage now also reaches direct `^`
calls lowering through `%concatstring` instead of emitting a bare `^`
identifier.
The current JS source-driven char/stdout coverage now also reaches direct
`print_char` calls lowering through an explicit named import instead of
emitting a bare `print_char` identifier or relying on an ambient global; the
current JS backend keeps the representation choice separate by lowering shared
`Core_ir.Constant.Char` values to one-character JS strings in `JIR`.
The current JS source-driven conversion coverage now also reaches direct
`string_of_int` calls lowering through `%string_of_int` instead of emitting a
bare `string_of_int` identifier.
The current JS source-driven conversion coverage now also reaches direct
finite-input `string_of_float` calls lowering through `%string_of_float`
instead of emitting a bare `string_of_float` identifier; OCaml-exact
float-string formatting remains a narrower follow-up.
The current JS source-driven valid-input parsing coverage now also reaches
direct `int_of_string` calls lowering through `%int_of_string` instead of
emitting a bare `int_of_string` identifier; parse-failure and exception
semantics still stay separate until a later `try/with` slice owns them.
The current JS source-driven finite-input parsing coverage now also reaches
direct `float_of_string` calls lowering through `%float_of_string` instead of
emitting a bare `float_of_string` identifier; invalid-input and OCaml-exact
float parsing edge cases still stay separate until a later exception/runtime
slice owns them.
The current JS source-driven newline-I/O coverage now also reaches direct
`print_newline ()` calls lowering through an explicit named import instead of
emitting a bare `print_newline` identifier or relying on an ambient global.
The current JS source-driven integer-stdout coverage now also reaches direct
`print_int` calls lowering through an explicit named import instead of
emitting a bare `print_int` identifier or relying on an ambient global.
The current JS source-driven string-stdout coverage now also reaches direct
`print_string` calls lowering through an explicit named import instead of
emitting a bare `print_string` identifier or relying on an ambient global.
By contrast, the current source-driven boolean coverage keeps `not`, `&&`, and
`||` out of the runtime surface: the JS backend lowers them through nested
conditional expressions in `JIR`, so short-circuit behavior stays explicit
without inventing fake runtime primitives for boolean control flow.
The direct `ignore expr` slice stays out of the runtime surface too: shared
lowering turns it into `Sequence(expr, ())` before JS lowering, so emitted JS
reuses normal effect-position statements instead of importing or calling an
`ignore` helper.
The later import-materialization slice now also makes the owned runtime/import
boundary explicit in final `JIR`: after the last import-collection normalize
step, runtime-helper and source-module references become plain locals such as
`__callPrimitive`, `__print_endline`, `__print_newline`, `__print_int`,
`__print_string`, and `Printf`, while `program.imports` remains the only
import-declaration surface handed to `JST`.

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

The current `raml` slice has now taken one narrow step there:

- `JIR` still carries collected import requirements
- `JST` lowering now groups compatible named/default requirements from the same
  module into one emitted ESM import declaration
- namespace imports such as `import * as Printf from "./Printf.js"` still stay
  separate, because they model a distinct source-visible module boundary in the
  current backend

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

## 11. Current `raml` Runtime Slice

The current `raml` JS backend now uses one explicit runtime module surface for
the implemented slice:

- generated JS imports low-level runtime-owned helpers from
  `./riot-runtime.js`
- `print_endline` lowers to an explicit named import instead of an ambient
  global call
- `print_newline` lowers to an explicit named import from the same runtime
  module instead of an ambient global call
- `print_int` lowers to an explicit named import from the same runtime module
  instead of an ambient global call
- `print_string` lowers to an explicit named import from the same runtime
  module instead of an ambient global call
- multi-parameter compiled lambdas lower through `makeCurried` in the same
  runtime module so source curried semantics survive JS under-application
- `Core_ir.Primitive` lowering uses the same module through `callPrimitive`
- the first source-level integer arithmetic, float arithmetic, `sqrt`, and
  equality/comparison direct-call slices also lower through `callPrimitive`,
  and the current finite-input `string_of_float` plus valid-input
  `int_of_string` plus finite-input `float_of_string` slices do the same
  through `%string_of_float`, `%int_of_string`, and `%float_of_string`, so
  emitted JS does not fall back to bare operator identifiers such as `=`,
  `+.`, or `*.` or to a bare `string_of_float`, a bare `int_of_string`, a
  bare `float_of_string`, or an ambient `sqrt` when the shared `Core_ir`
  still carries a direct callee name
- the first source-visible standard-library namespace import now materializes
  as a sibling `./Printf.js` module that owns `printf` / `sprintf` formatting
  instead of smuggling formatted I/O through `callPrimitive`
- the first dead-binding slice now recomputes imports from the live `JIR`
  body after JS-only DCE, so helpers referenced only from eliminated dead
  bindings do not survive into emitted JS

This keeps the current runtime boundary JS-owned in `src/js/` without pushing
JS import or helper choices back into `Core_ir`.

The current split is deliberate:

- `./riot-runtime.js` owns low-level helpers the backend selects directly
- `./Printf.js` owns the first module-shaped JS stdlib surface used by
  source-visible dotted references such as `Printf.printf`

The current immutable-record slice is also deliberate:

- immutable record construction, field access, and functional update still
  reuse the shared tuple lowering path
- the emitted JS therefore keeps using `%tuple_make` and `%tuple_get` through
  `callPrimitive` instead of adding a record-specific JS runtime helper early

The first closed ordinary-variant slice is equally deliberate:

- ordinary constructor values currently lower through tagged tuples where slot
  `0` is the constructor tag and slot `1` is the optional payload
- when a constructor has more than one source argument, that slot-`1` payload
  stays shared by packing the arguments into a tuple first; the current
  prelude-list `::` slice is the first proof point for that rule
- exhaustive constructor-only matches therefore reuse `%eq`, `%tuple_make`,
  and `%tuple_get` through `callPrimitive` instead of adding a variant-specific
  JS runtime helper before the representation contract is settled

The current limitation is deliberate:

- direct-call lowering can recognize the current `print_endline`,
  `print_newline`, `print_int`, and `print_string` slices plus the current
  finite-input `string_of_float` plus valid-input `int_of_string` plus
  finite-input `float_of_string` primitive slices
- dotted module references can materialize the current `Printf.printf` slice as
  an explicit sibling JS module import
- the first explicit source `external print_endline : string -> unit = "print_endline"`
  slice now proves that a top-level declared value can stay compile-time-only
  at the shared `Typ -> Core_ir` boundary while the later JS direct call still
  reuses the owned `./riot-runtime.js` helper import
- invalid `int_of_string` behavior still does not claim OCaml exception
  semantics; the current runtime helper only owns the valid-input boundary
- invalid `float_of_string` behavior still does not claim OCaml exception
  semantics; the current runtime helper only owns the finite-input boundary
- general ambient/external provenance beyond that narrow declared-value case is
  still future work for a typed `external` or builtin-lowering path

## 12. The FFI Pipeline Has Several Layers

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

## 13. Variadics And Splicing Depend On Runtime Helpers

`lam_compile_external_call.ml` uses `Caml_splice_call` runtime helpers for
dynamic variadic cases such as:

- `spliceApply`
- `spliceNewApply`
- `spliceObjApply`

So even seemingly simple JS interop features may need runtime help when the
argument shape is not statically flat.

That is another reason to keep FFI lowering late and target-specific.

## 14. Raw JS Exists As A Dedicated Escape Hatch

The backend supports:

- `#raw_expr`
- `#raw_stmt`

These become `Praw_js_code` with classified raw-js payloads.

This is clearly JS-only.

If `raml` supports a similar escape hatch, it should stay in the JS backend
layer and not infect the shared compiler middle.

## 15. What `raml` Should Preserve

There are several good ideas here.

### Preserve explicit runtime modules

Keep the runtime ABI names and responsibilities documented and inspectable.

### Preserve typed FFI structure

Do not reduce FFI to arbitrary strings after parsing.
The structured spec types are useful.

### Preserve explicit package/path resolution

Import pathing needs its own phase and data model.

## 16. What `raml` Should Change

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
