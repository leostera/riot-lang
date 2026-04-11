# raml-core AGENTS

`compiler/raml-core` owns the shared frontend and backend-neutral compiler
surface.

## Ownership

This package owns:

- `Config`
- `Event`
- `Target`
- `Compilation`
- `Source_unit`
- `Core_ir`
- `Typ_lowering`
- `Pipeline_stage`
- `Backend_result`
- `Frontend_pipeline`

## Rules

1. Keep `raml-core` backend-neutral.
2. Do not introduce JS module-system details, native ABI details, or wasm host
   import policy here.
3. Prefer typed shared compiler contracts over stringly backend shims. Shared
   primitives should stay backend-neutral and Riot-owned; legacy `%foo` names
   are parser-compatibility only, not the live `Core_ir` contract.
4. If a transform is still shared across backends, it belongs here, not in a
   backend package.
5. If a change affects the public `Raml` facade contract, update
   `compiler/raml` in the same slice.
6. If the `typ -> raml` handoff changes, re-check `packages/typ/AGENTS.md`.

## Current Shape

The shared compiler path is:

`Syn/Typ -> Source_unit -> Typ_lowering -> Core_ir -> Frontend_pipeline`

`Core_ir` is the shared semantic center. Keep it Lambda-shaped and executable,
not backend-shaped.

## Verification

Prefer:

- `riot build raml-core`

If package-level tests are still living elsewhere, call that out explicitly
instead of pulling backend work into this package.
