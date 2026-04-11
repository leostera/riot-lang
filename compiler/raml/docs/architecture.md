# Raml Compiler Architecture

This document states the intended top-level architecture for the Raml compiler
family.

It sits above the source-driven manuals in:

- `./native`
- `./js`
- `./wasm`

Those manuals explain what current systems do today.
This document explains the compiler shape the split `raml-core` /
`raml-native` / `raml-wasm` / `raml` packages are trying to own now.

## 1. Design Goal

Raml is now a split compiler family:

- `compiler/raml-core` owns the shared frontend and `Core_ir`
- `compiler/raml-native` owns the native backend
- `compiler/raml-wasm` owns the wasm backend
- `compiler/raml` is the thin public facade and integration package

The JS backend still lives under `compiler/raml` until that package split
lands.

Its job is to provide one compiler-owned middle layer above:

- JavaScript code generation
- native code generation
- wasm code generation

without freezing JS, native, and wasm representation choices too early.

That means two things have to stay true at once:

- the shared middle must be rich enough to drive real backend work
- backend-specific runtime commitments must happen in backend-owned layers

## 2. Top-Level Stack

The intended stack is:

```text
Typ Semantic Tree
  -> Raml Core IR
  -> shared semantic/backend-neutral passes
  -> backend split

    JS
      -> Raml JIR
      -> JS/runtime-oriented passes
      -> JS codegen
      -> JS artifacts

    Native
      -> Raml NIR
      -> native/runtime-oriented passes
      -> Raml MIR
      -> Raml LIR
      -> native codegen/linking
      -> native artifacts

    Wasm
      -> wasm/runtime-host IR
      -> wasm/runtime-host passes
      -> wasm codegen
      -> wasm loader/packaging
      -> wasm artifacts
```

This is the current architectural commitment.

`raml` no longer assumes there is one shared post-`Core` IR for native and
wasm.

Backend selection should be driven by explicit `host` and `target` triples:

- the target triple decides whether the backend family is JS, wasm, or native
- the host triple decides which local toolchain/runtime path the compiler can
  use while producing artifacts for that target
- cross-compilation is therefore a normal compiler mode, not a special case

If a real shared layer emerges later, it should be extracted from working
backend-specific IRs instead of guessed in advance.

## 3. Why The Boundary Sits After `typ`

`typ` owns semantic analysis.

That means `raml` should consume semantic truth from `typ`.

But `typ`'s semantic tree is not the long-term backend contract.

`typ` is checker-shaped.
`raml` needs a compiler-facing executable IR with explicit:

- compilation-unit identity
- stable compiler-owned symbol identity
- module init ordering and exports
- closures and arity
- direct versus indirect application
- recursive bindings
- structured control flow
- source-owned diagnostics and origins

So the handoff is:

- `typ` provides semantic truth
- `raml` lowers that truth into its own compiler IR

## 4. What `Raml Core IR` Owns

`Raml Core IR` is the shared compiler middle.

Today the implemented core is Lambda-shaped enough to grow real passes:

- `Compilation_unit`
- `Binding_group`
- `Init_item`
- `Surface_path`, `Binding_id`, and `Entity_id` re-exported from `Typ.Model`
- `Expr` with `Constant`, `Var`, `Apply`, `Lambda`, `Let`, `Sequence`,
  `Tuple`, `Tuple_get`, `If_then_else`, and `Primitive`

Those identity types are not local mirrors anymore.
For now, `raml` reuses the `typ` identity modules directly and keeps the local
`Core_ir.Surface_path`, `Core_ir.Binding_id`, and `Core_ir.Entity_id` module
names as the compiler-facing access points.

That means:

- `Surface_path` is the printable/current path view
- `Binding_id` is the binder identity
- `Entity_id` is the use-site/shared-reference identity carried by `Core_ir`
- unresolved global/module/prelude refs may remain unresolved in `Core_ir`
- local/current-unit refs should be reclassified to resolved `Entity_id`s
  during `typ -> raml` lowering whenever the lowering environment can do it

This layer should remain backend-neutral in representation, not in richness.

It should eventually preserve the semantics that all real backends need,
including:

- compilation-unit identity
- module imports, exports, and initialization order
- closure creation
- arity and application shape
- recursive values and recursive modules
- algebraic data construction and projection
- mutation categories
- structured control flow
- exceptions and effects
- typed foreign declarations

## 5. Current Implemented Slice

The current package split is intentionally uneven.

### Shared lowering that exists today

Today `raml` lowers a narrow implementation-only slice from `typ` into
`Core_ir`:

- top-level non-nested value groups
- variable and unit top-level binders
- constants, including backend-neutral char literals
- structured entity references instead of raw-string symbolic variables
- positional direct and indirect applies
- top-level lambdas
- source anonymous function expressions inside supported bindings and lambda
  bodies
- tuple construction
- top-level type declarations whose only effect is compile-time record or
  ordinary-variant layout information
- top-level open statements whose only effect is compile-time name resolution
  after the semantic tree has already resolved later references
- immutable record construction, field access, and functional update when the
  record labels resolve to one visible declaration, lowered through tuple
  construction and projection
- closed ordinary-variant constructor expressions when the constructor resolves
  to one visible declaration, including the current phantom-index-only
  GADT-style vector slice where the type indices erase to the same runtime
  constructor layout, or when `[]` / `::`, `None` / `Some`, and `Ok` /
  `Error` resolve to the builtin prelude `list` / `option` / `result`
  declarations, lowered through tagged tuple construction
- exhaustive constructor-only matches over one visible ordinary variant
  declaration, or over the builtin prelude `list` / `option` / `result`
  declarations, lowered through shared tag checks and tuple payload
  projection; constructors with more than one source argument currently keep
  the shared slot-`1` payload boundary by packing those arguments into one
  tuple payload before backend lowering
- source `if ... then ... else ...` expressions inside supported bindings and
  lambda bodies
- source sequence expressions inside supported bindings and lambda bodies
- direct source-level `ignore expr` calls lowered through that same shared
  sequence-plus-unit path instead of a backend helper
- source local `let` expressions with variable or tuple binders inside
  supported bindings and lambda bodies, including function-only recursive
  local groups with variable binders
- explicit init-time eval items for `let () = expr`

### JS path that exists today

The implemented JS path is:

- `Core_ir -> JIR`
- JS emission from `JIR`

That slice currently supports:

- constants representable by the current `JIR`
- symbolic variables as identifiers
- direct applies
- ordered eval items as expression statements
- explicit exports

### Native and wasm paths that do not exist yet

There is no active shared post-`Core` native/wasm IR anymore.

The previous experimental shared layer was removed because it was speculative:
it had not yet proven that native and wasm actually share one useful
post-`Core` runtime-oriented lowering.

The next real backend work should therefore define:

- a native-only `NIR`
- a wasm-only runtime/host IR

and only reintroduce a shared post-`Core` layer later if those two paths
converge enough to justify it.

## 6. What `Raml JIR` Owns

`Raml JIR` is the JS-specific late IR family.

It should own:

- JS module loading style
- JS import/export materialization
- JS runtime helper selection
- JS value representation
- JS-specific FFI lowering
- raw JS escape hatches, if they exist
- JS cleanup and final emission concerns

This is where the compiler is allowed to become JS-shaped.

## 7. What The Native Backend Owns

The native path should become native-shaped directly after `Core_ir`.

The first native-only layers should own:

- compiler/runtime compatibility ABI decisions
- raw value codecs still needed at the native boundary
- allocation, poll, barrier, and metadata entrypoints
- one locked target's calling convention and object-format constraints
- machine-independent native lowering
- machine-dependent low-level lowering
- a flat final low-level IR before emission

That is why the native manual now talks about:

- `NIR`
- `MIR`
- `LIR`

instead of a guessed shared late IR.

## 8. What The Wasm Backend Owns

The wasm path should also become wasm-shaped directly after `Core_ir`.

Its first wasm-only layers should own:

- Wasm value encoding strategy
- helper, primitive, and host imports
- startup and loader materialization
- host-profile differences
- effect strategy if it diverges by host/runtime mode

This is a different pressure profile from the native backend.

That is exactly why `raml` should not force both backends through one guessed
shared IR prematurely.

## 9. When To Reintroduce A Shared Post-`Core` IR

A shared post-`Core` layer should come back only if the working native and wasm
paths prove they still share a real runtime-oriented lowering boundary.

That means:

- shared calling/closure/runtime questions
- shared data-layout questions
- shared metadata and artifact questions

If those converge later, extract the shared layer from experience.

Do not invent it first and make both backends fit it afterward.

## 10. The Main Conclusion

The current architecture is:

- `typ` owns semantic analysis
- `raml` owns compiler-facing executable IRs and backend orchestration
- one shared `Raml Core IR` sits above all backends
- JavaScript lowers out of that IR into `JIR`
- native and wasm now lower directly out of `Core_ir` into their own
  backend-specific late IR families
- a new shared post-`Core` layer should only reappear if the implemented
  backends prove it is real

That is the cleanest current reading of the codebase and the native/js/wasm
analysis together.
