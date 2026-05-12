# raml-js

`raml-js` is the JavaScript backend package for the Raml compiler family.

The current backend stack is:

- `Raml_core.Core_ir`
- `RamlJs.Js.Jir`
- `RamlJs.Js.Jst`
- emitted `.js`

## Compared With Melange `jscomp` And ReScript

`raml-js` now sits between Melange and ReScript in design goals.

- From Melange we want:
  - explicit compiler subsystems
  - real pass structure
  - strong module/import ownership
  - identity preserved across lowering
- From ReScript we want:
  - natural JavaScript surface syntax
  - JS-native representations where semantics actually align
  - smaller reliance on a helper-heavy runtime layer

### `raml-js` vs Melange

`raml-js` is cleaner at the shared/backend boundary:

- `Raml_core.Core_ir` stays backend-neutral instead of absorbing JS runtime
  semantics
- `JIR -> JST -> emitter` is explicit instead of mixing late syntax and backend
  mechanics into one large IR
- pass composition stays local and readable in
  `src/js/jir/lowering.ml`

Melange is still ahead in backend machinery:

- deeper module/package path ownership
- richer typed FFI metadata
- stronger scope/shake/tailcall analysis
- artifact/dependency tracking across compilation units

### `raml-js` vs ReScript

ReScript is a better reference for emitted JS shape:

- prefer direct operators, globals, arrays, and objects
- shrink the runtime by lowering more constructs to ordinary JS syntax
- keep property access and object literal printing natural

`raml-js` is now moving in that direction:

- tuples lower to JS arrays
- records lower to JS objects
- arithmetic/comparisons/string conversion prefer native JS forms
- Riot-owned JS builtins are classified explicitly from `Std`/Riot surface paths
- shared primitives now arrive from `Raml_core` as a typed, backend-neutral
  contract instead of `%foo` strings
- direct-call lowering now lives in explicit `Jir.Calls` policy instead of
  mixing builtin dispatch into the main lowering walk
- primitive lowering now lives in an explicit `Jir.Primitives` subsystem instead
  of as a large inline match inside the main lowering walk
- JS-native globals, member access, calls, arrays, and ambient namespaces now
  live in shared `Jir.Intrinsics` helpers instead of being rebuilt ad hoc in
  each lowering subsystem
- JS object literals and named property access now live in explicit
  `Jir.Objects` helpers instead of being split between record lowering and
  reference lowering
- dotted entity references and namespace-import construction now live in
  explicit `Jir.References` lowering instead of being embedded directly in the
  main lowering walk
- object keys, property access, and emitted binder legality now share one
  syntax policy instead of backend-local heuristics
- backend lowering, late JIR passes, JST lowering, and emission now all accept
  one shared `Compilation_context`, so future target-sensitive JS decisions can
  key off host/target/source without re-plumbing the whole backend again
- module-surface selection now has an explicit `Jst.Module_format` seam, even
  though the only emitted format today is still ESM

Where `raml-js` still differs from ReScript:

- it does not yet have a broader representation-policy layer
- it still relies on a small builtin/runtime registry instead of a richer
  JS-facing interop surface
- it has not yet moved enough language constructs onto plain JS objects,
  arrays, and namespaces

## Current Backend Gaps

The main remaining architectural gaps are:

1. Module/import ownership is centralized in `Jir.Modules`, but path
   resolution is still heuristic: sibling unit or runtime module only.
   The module layer now owns import paths and namespace binders directly, so
   lowering no longer invents those ad hoc.
   `JST` now also retains a structured module ref until emission instead of
   collapsing imports down to bare path strings immediately.
   The current namespace/reference story is now also factored into
   `Jir.References`, and `Jir.Modules` now owns the actual namespace-vs-value
   path split plus namespace-import construction. The policy is still heuristic rather than package- or
   artifact-aware the way Melange is.
2. The builtin/runtime boundary is centralized, but still hand-written and
   small rather than typed and declarative.
   It is now Riot-owned end to end in the JS backend, and the remaining helper
   fallback goes through the typed shared primitive contract instead of raw
   OCaml-style primitive labels. The runtime helper surface is now down to the
   parse/validation cases that are not yet represented directly in `JIR`.
   The next step here is a richer typed JS interop/FFI registry, not more
   string matching in lowering.
3. `JST` has no post-lowering optimization layer yet.
4. There is no package-level dependency artifact analogous to Melange `.cmj`
   metadata.
5. Records now lower naturally to JS objects, but inner modules and more
   namespace-like constructs do not yet lower to JS objects.
   The object substrate is now explicit in `Jir.Objects`, which makes that next
   step much less invasive.
6. JS binder validity is now owned centrally, but module/path resolution is
   still much shallower than Melange or ReScript.
7. The backend now has shared compilation context plumbing, but it does not yet
   use that context for module-format decisions like ESM vs CommonJS.

## Current Cleanup Direction

The current cleanup direction in `raml-js` is:

1. keep semantic identity on `Binding_id` / `Entity_id`
2. centralize backend-local JS syntax rules instead of duplicating them across
   lowering and emission
3. strengthen shared JIR analysis/utilities before adding more passes
4. keep pushing semantically-aligned constructs toward native JS forms
5. add deeper ownership layers only when a concrete invariant requires them
