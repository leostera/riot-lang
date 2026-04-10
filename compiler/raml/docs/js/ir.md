# Raml JavaScript IR Notes

This document records what Melange's two backend IR layers currently represent
and what that implies for a shared `raml` IR.

The main lesson is simple:

Melange has useful seams, but its first backend IR is already too JS-specific
to reuse unchanged in a native/js/wasm compiler.

## 1. What `Lam` Already Represents

`core/lam.mli` defines `Lam.t`.

It preserves many Lambda-shaped forms:

- variables and mutable variables
- functions with arity and function attributes
- lets, mutable lets, and letrecs
- primitive applications
- int and string switches
- static exceptions
- try/with
- loops
- assignment
- method send
- explicit module globals

It also carries call metadata through:

- `apply_status`
- `ap_info`

and it keeps source locations on applications, primitives, and sends.

So far, that sounds like a plausible shared backend IR.

But the primitive set changes the story.

## 2. Why `Lam` Is Not Backend-Neutral

`core/lam_primitive.mli` includes many JS-colored primitives:

- `Pjs_call`
- `Pjs_object_create`
- `Pjs_apply`
- `Pjs_runtime_apply`
- `Pimport`
- `Pjs_typeof`
- `Pjs_function_length`
- `Pjs_fn_make`
- `Pjs_fn_method`
- `Praw_js_code`
- option/null/undefined wrappers used for JS interop

It also encodes backend-runtime behavior such as:

- `Pinit_mod`
- `Pupdate_mod`
- `Pcreate_extension`
- `Psome` and `Psome_not_nest`
- `Pwrap_exn`

That means `Lam` is not "OCaml semantics lowered once for all backends".

It is "OCaml semantics already partially lowered into the needs of the JS
backend and JS runtime".

## 3. Where JS Semantics Enter `Lam`

There are several entry points.

### FFI

`Lam_convert.convert_ccall` decodes Melange FFI metadata and emits:

- `Pjs_call`
- `Pjs_object_create`
- inline JS constants

That means JS interop is not postponed to a later backend layer.

### Raw JS

`#raw_expr` and `#raw_stmt` become `Praw_js_code`.

That is a direct JS-language escape hatch in the `Lam` layer.

### Dynamic import

`#import` becomes `Pimport`, and `dynamic_import` is threaded through module
references and FFI handling.

### JS call and option conventions

`Lam_ffi` and `Lam_convert` inject JS-facing apply and wrapper primitives
directly into `Lam`.

## 4. What `Lam` Is Still Good At

Even though `Lam` is too JS-specific to be shared unchanged, it still reveals
what the shared middle of `raml` needs to model explicitly.

The useful parts are:

- first-class module globals and module dependency tracking
- explicit arity and apply-mode information
- explicit control-flow nodes
- explicit recursive binding structure
- explicit runtime primitives instead of vague "builtin" tags
- source locations preserved through backend lowering

Those are all good properties for a shared backend-facing IR.

The mistake would be preserving the JS-specific primitive vocabulary in that
shared layer.

## 5. What `J` Represents

`core/j.ml` defines the last structured IR before printing.

Its own file says:

- it is a subset of JavaScript AST
- specialized for the OCaml Lambda backend
- `Block` is only a sequence, not a new scope

`J` includes:

- JS expressions and statements
- variable declarations
- loops, ifs, switches, try/catch
- function expressions
- module references
- program export lists
- delayed import metadata through `deps_program.modules`

But it also contains backend-runtime forms that are not ordinary JS surface
syntax:

- `Caml_block`
- `Optional_block`
- `Caml_block_tag`
- `Module`

So `J` is a JS codegen IR, not a general ESTree-style AST.

That is fine.

`raml` does want a JS-specific late IR.
`J` is close to the kind of layer a JS backend should own.

## 6. What The Shared `raml` IR Must Carry

A shared multi-backend IR should preserve the backend-relevant semantics that
Melange currently models in `Lam`, but without choosing JS-specific encoding
yet.

At minimum, that shared IR needs:

- module-global references and explicit module-init dependency info
- explicit exports
- function arity and application shape
- closures or enough information to build them later
- algebraic data construction and projection
- mutation, loops, and structured control flow
- exceptions and effectful control flow
- recursive values and recursive modules as explicit constructs
- source spans or origin ids for later diagnostics and tooling
- typed or structured external-call metadata

The key is that these are semantic backend needs, not JS needs.

## 7. What The Shared `raml` IR Must Not Decide Yet

These should stay out of the shared middle layer:

- CommonJS versus ESM import style
- exact JS file paths
- default-import versus namespace-import rules
- raw JS snippets
- JS-specific optional-argument encoding
- JS `null` and `undefined` wrappers
- JS object-literal FFI lowering rules
- JS-specific currying and splice-apply helpers
- dynamic `import()` lowering details
- concrete `Some`/`None` encoding in JavaScript

If these appear in the shared IR, the native and wasm paths will inherit JS
concerns they should not know about.

## 8. A Better Layer Split For `raml`

The current Melange stack suggests a better multi-backend split.

### Shared backend-neutral IR

This layer should own:

- closure and arity semantics
- data constructors abstractly
- explicit modules, exports, and init order
- control flow and mutation
- abstract foreign declarations

### JS-lowered IR

This layer should own:

- JS module loading style
- JS data representation
- JS runtime helper calls
- JS-specific FFI and raw JS
- JS tree-shaking and printing concerns

### Native-lowered IR

This layer can own:

- object layout and calling convention for the native runtime
- stack and register concerns
- target-specific emission

### Wasm-lowered IR

This layer can own:

- linear memory layout
- reference/value representation
- import/export ABI

## 9. A Concrete Way To Read Melange's IR Choices

There are two kinds of things in Melange `Lam`.

### Semantic wins worth preserving

- explicit arity
- explicit top-level grouping needs
- explicit dependency tracking
- explicit runtime primitive vocabulary

### JS leaks that should move later

- JS FFI encoded as primitives
- dynamic-import flags on module ids
- raw JS
- JS-specific option/null/undefined wrappers
- JS apply conventions

That is the main design extraction from this source pass.

## 10. The Right Question For `raml`

The question is not:

"Can `raml` reuse Melange `Lam`?"

The right question is:

"Which backend facts currently modeled in `Lam` actually belong in a shared
compiler middle layer, and which belong in a JS-only lowering layer?"

Most of the compiler work ahead sits inside that separation.

## 11. Current First Implemented `raml` `JIR` Slice

The current package now defines the first explicit `JIR` surface.

It is intentionally smaller than Melange `J`.

Today the implemented `JIR` only models:

- `Program` with `module_name`, ordered `body`, and explicit `exports`
- `Statement` as either a JS declaration or an expression statement
- `Declaration` with JS `const`, `let`, and `var`
- `Expr` as a literal, identifier, or call
- `Literal` as `undefined`, `null`, booleans, JS number literals, and strings

That boundary matters.

It keeps JS-only choices in the JS layer:

- declaration kind is chosen in `JIR`, not in shared `Raml Core IR`
- JS-only values like `undefined` first appear after the backend split
- exports are already JS-facing name-to-local mappings

It also keeps the first slice honest by not pretending more exists yet.

The current `JIR` does not yet carry:

- imports or module-system materialization
- property access or method-send structure
- function declarations or JS control flow
- runtime helper selection
- FFI or raw JS escape hatches
- cleanup/shaking metadata or final source-printing details

Those belong to later JS-backend slices.
