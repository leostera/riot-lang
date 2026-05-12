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

Other compiler packages own:

- shared frontend or `Core_ir` contracts from `compiler/raml-core`
- facade/public `Raml` API wiring from `compiler/raml`
- native or wasm backends

## Rules

1. Keep `Core_ir` backend-neutral. JS-specific semantics belong in `JIR`,
   runtime metadata, or `JST`, not in `raml-core`.
2. Keep pass composition explicit in `src/js/jir/lowering.ml`.
3. Every pass should document:
   - algorithm
   - effect
   - rationale
4. Prefer strengthening existing analyses and passes over adding new pass
   modules without a concrete invariant.
5. `JST` should only see resolved `JIR`; import/runtime expression forms are
   resolved before this layer.
6. When comparing with Melange `jscomp`, copy invariants and subsystem ideas,
   not compiler-lib coupling or early JS leakage into the shared IR.
7. Treat `Raml_core.Primitive` as the shared primitive contract. Legacy `%foo`
   string parsing belongs at compatibility boundaries.
8. In `raml-js`, `Object` means a plain JavaScript object literal/property
   shape. It does not mean the OCaml object system.
9. Use `src/js/syntax.*` for JS naming and property-syntax decisions across
   lowering, passes, and emission.
10. Thread `Raml_core.Compilation_context.t` through backend lowering, passes,
    `JST`, and emission. Future target-sensitive decisions like ESM vs CJS
    should read from that context.
11. Keep JS module-surface policy in `src/js/jst/module_format.*`, not inline
    in the emitter. New target-specific import/export shapes should hang off
    that module.
