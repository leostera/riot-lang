# Raml Lambda Notes

This document records what the existing backend front half does inside
`vendor/ocaml/lambda`.

This is the part of the native pipeline that turns typed OCaml into a runtime-
aware Lambda tree before the native middle end and `asmcomp` take over.

## 1. What Lambda Already Represents

The Lambda IR is not a tiny functional core.

It already contains:

- module-global access via `Pgetglobal` and `Psetglobal`
- heap allocation and mutation primitives
- array, string, bytes, boxed-integer, and bigarray primitives
- exception raising forms
- effect/runtime-control primitives:
  - `Prunstack`
  - `Pperform`
  - `Presume`
  - `Preperform`
  - `Ppoll`
  - `Pdls_get`
- structured constants
- switch and string-switch nodes
- explicit static exceptions and handlers
- loops, mutation, send, and event nodes

The IR also carries semantic metadata:

- `value_kind`
  - `Pgenval`
  - `Pfloatval`
  - boxed integer kinds
  - `Pintval`
- `let_kind`
  - `Strict`
  - `Alias`
  - `StrictOpt`
- function attributes
  - inline
  - specialise
  - local
  - poll
  - functor/stub/TMC/fuse-arity flags
- scoped locations via `Debuginfo.Scoped_location`

That means Lambda is already a backend IR with runtime commitments, not just a
frontend desugaring target.

## 2. The Main Entry Points

The important entry modules are:

- `translcore`
  core language translation from `Typedtree.expression` to `lambda`
- `translmod`
  module-language translation and `Lambda.program` construction
- `translprim`
  primitive translation and primitive-usage tracking
- `translattribute`
  `[@inline]`, `[@specialise]`, `[@local]`, `[@tailcall]`, and related
  attribute decoding
- `matching`
  pattern-match compilation
- `simplif`
  Lambda normalization and TMC invocation
- `value_rec_compiler`
  generic recursive-value lowering
- `translclass` and `translobj`
  class/object lowering helpers
- `printlambda`
  the printer used for raw Lambda and simplified Lambda dumps

## 3. Core Translation In `translcore`

`translcore` is the core typedtree-to-Lambda compiler.

It exposes:

- `transl_exp`
- `transl_apply`
- `transl_let`
- `transl_extension_constructor`
- `pure_module`

The implementation directly handles the major `Typedtree` expression forms,
including:

- functions
- applications
- matches
- try/with
- variants and records
- mutable field updates
- arrays
- conditionals, sequences, loops, and `for`
- method sends
- packs
- assertions
- lazy values
- objects
- `letop`
- `Texp_unreachable`

Several important backend facts are already decided here:

- syntactic arity matters
- method arity is fused with the hidden `self` parameter
- debug events are inserted here
- primitive applications can lower straight to runtime-facing primitives
- generic recursive bindings are handed to `Value_rec_compiler.compile_letrec`
- pattern matching is compiled during translation, not deferred to a later IR

## 4. Module Translation In `translmod`

`translmod` builds whole-module Lambda programs.

The most important entry points are:

- `transl_implementation_flambda`
- `transl_implementation`
- `transl_store_implementation`
- `transl_toplevel_definition`
- `transl_package`
- `transl_package_flambda`

Two design facts matter here.

### Flambda and closure paths differ already at Lambda-program construction

`transl_implementation_flambda` builds a program whose `code` returns the
module block value.

`transl_implementation` wraps that code in:

- `Lprim (Psetglobal module_ident, [implementation.code], Loc_unknown)`

So the closure path still thinks in terms of mutating the global module slot,
while the flambda path thinks in terms of producing an initialized block value.

### Required globals are a first-class output

`translmod` scans:

- `Pgetglobal`
- `Psetglobal`
- primitive usage recorded by `translprim`
- environment-required globals

and stores the result in `Lambda.program.required_globals`.

That computation is part of module initialization semantics, not a later linker
guess.

## 5. Pattern Matching Happens Here

`matching.mli` exposes separate entry points for different match contexts:

- `for_function`
- `for_trywith`
- `for_handler`
- `for_let`
- `for_multiple_match`
- `for_tupled_function`
- `for_optional_arg_default`

This is important for `raml`.

The current backend does not carry a generic high-level match node very far.
It lowers pattern matching while Lambda is still being built.

Other specific responsibilities inside `matching` include:

- flattening tuple patterns for tupled functions
- expanding string switches into test trees
- inlining the beginning of `Lazy.force` for lazy-pattern support
- tracking mutability along access paths during matching

So a rewrite has to answer whether it wants to keep:

- early match compilation, or
- a later pattern-matching IR stage

The existing system clearly chose the former.

## 6. Generic Recursive Values Are Lowered Before The Middle End

`value_rec_compiler.ml` is one of the most important files in this whole slice.

Its job is to translate source-level recursive bindings into Lambda forms that
Lambda itself can represent.

The source comment gives the three phases:

1. sizing
2. function lifting
3. compilation

### Classification

Bindings are classified as:

- `Dynamic`
- `Static`
- `Function`
- effectively constant/unreachable special cases

Static bindings are then further understood as fixed-size blocks:

- regular blocks
- float records
- lazy blocks

### Function lifting

Lambda `Lletrec` only accepts syntactic functions.

So the compiler lifts non-syntactic recursive functions by:

- finding a function in tail position
- eta-expanding when needed
- performing a local closure-conversion-like pass for local free variables
- creating an extra static context block when needed

### Compilation scheme

The generated evaluation order is:

1. evaluate dynamic bindings
2. pre-allocate static bindings
3. define functions
4. backpatch static bindings

It uses runtime-facing helpers such as:

- `caml_alloc_dummy`
- `caml_alloc_dummy_float`
- `caml_alloc_dummy_lazy`
- `caml_update_dummy`
- `caml_update_dummy_lazy`

This is a strong signal for `raml`: recursive-value lowering is not a backend
detail that can be postponed forever. It directly shapes the runtime ABI.

## 7. Simplification Is Structural, Not Cosmetic

`simplif.ml` is the normalization pass run before the middle end.

The entry-point order is:

1. `simplify_local_functions`
   in native mode or when debug is off
2. `simplify_exits`
3. `simplify_lets`
4. `Tmc.rewrite`
5. optional tailcall annotation emission

The comments and interfaces show concrete responsibilities:

- eliminate useless alias lets
- rewrite let-bound references into variables when possible
- simplify `staticraise` / `staticcatch`
- split optional-argument default wrappers into wrapper + inner function
- fuse nested function arities only when attributes allow it

So "simplified Lambda" is the actual backend input contract, not a pretty
optimization layer we can ignore.

## 8. TMC Is A Lambda-Level Rewrite

`tmc.mli` describes tail-modulo-cons as a Lambda-level transformation.

It:

- rewrites eligible recursive functions into destination-passing style
- creates a direct version and a DPS version
- mutates constructor placeholders with `Psetfield_computed`
- relies on Lambda constructors and mutable placeholder blocks

This matters for `zort` because TMC is not only a control-flow optimization.
It also bakes in assumptions about:

- constructor allocation
- placeholder mutation
- safe initialization ordering

## 9. Attributes Matter Semantically

`translattribute` attaches backend-relevant meaning to user attributes.

That includes:

- inlining
- specialisation
- locality
- inlined-module hints
- tailcall expectations
- poll attributes

These become `Lambda.function_attribute`, `tailcall_attribute`,
`inline_attribute`, and related metadata.

In other words, the frontend is already deciding backend policy, not only
frontend semantics.

## 10. Debug Scopes Are Explicit Data

`debuginfo.mli` exposes a structured scope stack:

- anonymous-function scopes
- value-definition scopes
- module-definition scopes
- class-definition scopes
- method-definition scopes

`Scoped_location.t` is then threaded into Lambda nodes and later debug info.

For `raml`, this is worth keeping explicit from day one.

If debug scope lineage is implicit, later stack maps, backtraces, and source
reporting become harder to rebuild.

## 11. Design Pressure On `raml`

The current Lambda layer suggests several concrete rules for a rewrite.

### Keep a rich frontend/backend IR boundary

The existing backend expects its pre-middle-end IR to know about:

- runtime primitives
- module globals
- effects
- polling
- value kinds
- function attributes

A replacement IR needs equally explicit information somewhere.

### Decide where match compilation lives

Today it lives before the middle end.
Moving it later is possible, but it would be a real architectural change, not
just a mechanical port.

### Keep recursive-value lowering explicit

The current implementation does not defer generic recursive-value handling to
late codegen.
It normalizes that problem early and explicitly.

### Keep module initialization shape explicit

`required_globals`, `main_module_block_size`, and the closure-vs-flambda
distinction are not incidental.
They are part of the backend contract.
