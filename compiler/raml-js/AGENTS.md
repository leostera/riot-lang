# raml-js AGENTS

`compiler/raml-js` owns the JavaScript backend for the Raml compiler family.

It should own JS-specific lowering, passes, runtime/import materialization,
late syntax lowering, and JS emission.

## Ownership

`compiler/raml-js` owns:

- `src/backend.ml`
- `src/js/jir/*`
- `src/js/jst/*`
- `src/js/riot-runtime.js`
- `src/js/Printf.js`
- package-local JS backend docs

`compiler/raml-js` does **not** own:

- shared frontend or `Core_ir` contracts from `compiler/raml-core`
- facade/public `Raml` API wiring from `compiler/raml`
- native or wasm backends

## Rules

1. Keep `Core_ir` backend-neutral. JS-specific semantics belong in `JIR`,
   runtime metadata, or `JST`, not in `raml-core`.
2. Keep pass composition explicit in `src/js/jir/lowering.ml`. Do not add a
   pipeline abstraction layer just to sequence passes.
3. Every pass should document:
   - algorithm
   - effect
   - rationale
4. Prefer strengthening existing analyses and passes over adding new pass
   modules without a concrete invariant.
5. `JST` should only see resolved `JIR`. Do not reintroduce unresolved
   import/runtime expression forms into `JST`.
6. When comparing with Melange `jscomp`, copy invariants and subsystem ideas,
   not compiler-lib coupling or early JS leakage into the shared IR.
7. Treat `Raml_core.Primitive` as the shared primitive contract. Do not
   reintroduce `%foo` string matching into `raml-js`; any legacy string parsing
   belongs at compatibility boundaries, not in JS lowering.
8. In `raml-js`, `Object` means a plain JavaScript object literal/property
   shape. It does not mean the OCaml object system.
9. Use `src/js/syntax.*` for JS naming and property-syntax decisions. Do not
   duplicate ad hoc “is this a valid JS name?” heuristics in lowering,
   passes, or emission.

## Verification

Prefer:

- `riot build raml-js`
- `git diff --check -- compiler/raml-js`

If `raml-js` is blocked by unrelated upstream package failures, call that out
explicitly instead of papering over it.
