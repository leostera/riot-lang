# raml-js

`raml-js` is the JavaScript backend package for the Raml compiler family.

The current backend stack is:

- `Raml_core.Core_ir`
- `RamlJs.Js.Jir`
- `RamlJs.Js.Jst`
- emitted `.js`

## Compared With Melange `jscomp`

The current `raml-js` design is intentionally cleaner than Melange at the
shared/backend boundary, but still much smaller as a backend.

Where `raml-js` is cleaner:

- `Core_ir` stays backend-neutral instead of absorbing JS runtime semantics
- `JIR -> JST -> emitter` is explicit instead of collapsing syntax and backend
  concerns into one late IR
- pass composition is local and readable in `src/js/jir/lowering.ml`

Where Melange is still ahead:

- stronger module/path ownership
- richer typed FFI metadata
- deeper scope/shake/tailcall machinery
- artifact/dependency tracking across compilation units

## Current Backend Gaps

These are the main remaining architectural gaps relative to Melange:

1. Module/import ownership is still string-based.
2. Runtime/builtin knowledge is still spread across lowering and runtime
   helpers instead of one declarative registry.
3. `JST` has no post-lowering optimization layer yet.
4. There is no package-level dependency artifact analogous to Melange `.cmj`
   metadata.

## Current Cleanup Direction

The current cleanup direction in `raml-js` is:

1. keep semantic identity on `Binding_id` / `Entity_id`
2. strengthen shared JIR analysis utilities
3. improve local simplification and DCE before adding bigger subsystems
4. add deeper ownership layers only when a concrete invariant requires them
