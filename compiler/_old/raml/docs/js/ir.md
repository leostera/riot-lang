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
- lexical name stabilization when JS scope and TDZ rules would otherwise change
  the meaning of source `let` bindings
- JS-specific FFI and raw JS
- JS tree-shaking and printing concerns

That last point is already visible in the current `raml` slice.
The source-driven `0006_let_shadowing` example lowers through `Core_ir.Expr.Let`
without backend names attached, but the JS backend has to freshen the nested
`x` binders in `JIR` before emission because `const x = x + 5` is not the same
as OCaml's nonrecursive `let x = x + 5 in ...`.
The current backend also treats the first direct I/O helpers `print_endline`,
`print_newline`, `print_int`, `print_string`, and `print_char` as JS-owned
runtime choices by lowering them to explicit named imports from
`./riot-runtime.js` instead of ambient globals. The first shared char slice
keeps that split explicit too: `Core_ir.Constant.Char` stays backend-neutral,
while `JIR` lowers it to a one-character JS string literal before emission.
It also treats the first built-in integer operators, float operators, string
concatenation through `^`, source-visible `string_of_int`, finite-input
`string_of_float`, valid-input `int_of_string`, finite-input
`float_of_string`, `sqrt`, and source-level `<`, `<=`, `>`, `>=`, `=`, and
`<>` direct calls as a JS-owned runtime choice by lowering them to
`callPrimitive("%addint" | "%subint" | "%mulint" | "%divint" | "%modint" |
"%addfloat" | "%subfloat" | "%mulfloat" | "%divfloat" | "%concatstring" |
"%string_of_int" | "%string_of_float" | "%int_of_string" |
"%float_of_string" | "%sqrtfloat" | "%lt" | "%le" | "%gt" | "%ge" | "%eq" |
"%neq")` instead of emitting bare operator identifiers such as `+`, `-`, `*`,
`/`, `mod`, `+.`, `-.`, `*.`, `/.`, `^`, a bare `string_of_int`, a bare
`string_of_float`, a bare `int_of_string`, a bare `float_of_string`, an
ambient `sqrt`, `<`, `<=`, `>`, `>=`, `=`, or `<>`. The current conversion
slices stay deliberately narrow: `string_of_float` only proves straightforward
finite direct calls, `int_of_string` only proves valid-input direct calls,
and `float_of_string` only proves finite-input direct calls, leaving
OCaml-exact float formatting plus parse-failure and exception semantics to
later runtime/`try/with`-owning slices.
The later `0004_boolean_logic` slice makes a different JS-only choice for
source-level `not`, `&&`, and `||`: they lower through nested `JIR`
conditional expressions instead of `callPrimitive`, so short-circuit behavior
stays explicit before emission without teaching the printer a new operator
surface.
The later `0026_sequence_and_ignore` slice makes a shared choice explicit
instead: direct source-level `ignore expr` lowers to backend-neutral
`Core_ir.Expr.Sequence` plus `Core_ir.Constant.Unit` before JS lowering, so
the JS backend does not need a dedicated `ignore` helper or runtime import for
that effect-only call shape.
The later `0023_partial_application` slice makes another JS-only choice
explicit: multi-parameter compiled lambdas lower through the runtime helper
`makeCurried` so under-applied calls stay source-correct without teaching the
printer about currying.
The later `0112_effect_position_local_let` slice makes one more JS-only
cleanup choice explicit: effect-position zero-arg IIFEs now flatten in `JIR`
before alpha stabilization when their body can be rewritten from tail returns
into plain statements, so top-level local-`let` eval slices stop emitting
statement-shaped wrapper calls.
The later `0113_initializer_shadowing` slice widens that rule in one narrower,
source-driven way: statement-shaped declaration-initializer zero-arg IIFEs now
lower through a temp binding plus lexical `Block` plus final declaration, so
initializer-local shadowing stays scoped while the outer binding keeps its
intended name without teaching the printer new tricks.
The later alias-cleanup slice adds one more JS-only cleanup rule after alpha
stabilization: immutable identifier-only temps such as tuple-destructure or
identifier-scrutinee aliases now disappear from `JIR` when their target name
is never assigned and the alias is not exported, so the printer no longer has
to carry `const __raml_tuple = value;` or `const __raml_match = value;`
wrappers that exist only to preserve a name the backend no longer needs.
The later `0117_dead_local_bindings` slice adds the first narrower dead-
binding elimination rule after that cleanup: unexported immutable `const`
bindings whose initializer is already effect-free now disappear from `JIR`
when the name is unused in scope, and a final `JIR` normalize step repopulates
imports from the live body so helpers referenced only from dead literal
bindings or dead closures do not leak into emitted JS.
The later `0118_printf_and_print_endline` slice settles one more late-JS
ownership rule: earlier `JIR` lowering and cleanup passes may still carry
`Imported` and `Runtime_helper` nodes while discovering dependencies, but once
the final normalize step has collected `program.imports`, a dedicated late
materialization pass rewrites those body references to plain local identifiers
such as `Printf`, `__callPrimitive`, `__print_endline`, `__print_newline`, or
`__print_int`, `__print_string`, or `__print_char` before `JST` lowering. That keeps
dependency discovery in `JIR` while keeping `JST` and the emitter out of the
business of import-reference rewriting.
The first closed ordinary-variant slice keeps one more representation choice
shared for now: `0009_variants_and_match` lowers constructors through tagged
tuples in `Core_ir` and lowers exhaustive constructor-only matches through
shared `%eq` tag tests plus tuple payload projection, rather than introducing
JS-only variant nodes before the invariant is proven across backends.
The later `0106_prelude_option_match` slice keeps that same contract honest for
stdlib `option`: `None` and `Some payload` reuse the shared tagged-tuple path
instead of introducing a JS-only `undefined`-style option encoding this early.
The later `0116_prelude_result_match` slice keeps that same contract honest
for stdlib `result`: `Ok payload` and `Error payload` reuse the shared tagged-
tuple path instead of introducing a JS-only result-object or exception-shaped
encoding this early.
The later `0012_list_recursion_sum` slice keeps that same contract honest for
stdlib `list`: `[]` reuses the tag-only tuple shape, while `::` keeps the
shared slot-`1` payload boundary by packing its head and tail arguments into a
tuple payload instead of introducing a JS-only cons cell shape this early.
The `0107_open_std_hello_world` slice keeps another scope-only rule shared:
once `typ` has resolved names through a top-level `open Std`, that open stays
compile-time-only and does not turn into a `Core_ir` node, JS import, or JS
runtime helper selection.

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

- `Program` with `module_name`, ordered `imports`, ordered `body`, and explicit
  `exports`
- `Statement` as a JS declaration, expression statement, `return`, or
  structured `if`/`else`
- `Declaration` with JS `const`, `let`, and `var`
- `Expr` as literals, identifiers, imports, runtime helpers, functions,
  property access, calls, conditional expressions, and assignments
- `Literal` as `undefined`, `null`, booleans, JS number literals, and strings

That boundary matters.

It keeps JS-only choices in the JS layer:

- declaration kind is chosen in `JIR`, not in shared `Raml Core IR`
- JS-only values like `undefined` first appear after the backend split
- exports are already JS-facing name-to-local mappings

It also keeps the first slice honest by not pretending more exists yet.

The current statement surface is deliberate.

When control flow is already in tail or effect position inside a statement-
producing body, prefer structured `Statement.If` nodes over encoding the same
branching as a JS conditional expression statement.

The current `JIR` does not yet carry:

- loops, switches, exceptions, or try/catch
- tree-shaking or scope-analysis metadata
- explicit import-materialization policy beyond collected requirements
- FFI or raw JS escape hatches
- cleanup/shaking metadata or final source-printing details

Those belong to later JS-backend slices.
