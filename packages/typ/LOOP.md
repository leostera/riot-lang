# typ LOOP

Current rewrite target: make `typ` reflect the final boss architecture and
remove prototype-era compatibility surfaces.

## Primary Goal

Drive `kernel-new` toward a fast, correct, observable package-check path built
around:

- one authoritative incremental package engine
- one persistent semantic environment grown in dependency order
- one canonical `ModuleTypings` runtime artifact per finished group
- one hidden persistence codec for that artifact
- optional query/editor payload kept off the plain cold-check path

## Hard Rules

- No public one-shot compatibility lane.
- No string ids where a typed id or typed local-module name exists.
- No hot-path `to_string` / `of_string` round-trips for identity.
- No duplicated typed and string graph state unless profiling proves a
  boundary-only cache win.
- No rebuilding flat ambient lists when a persistent env can be extended
  instead.
- Imported module types must enter the build checker exactly once. Do not feed
  the same imported type declarations through both `TypConfig.ambient_*` and
  folded ambient surfaces.
- Do not share a concrete `Infer.Env.t` across analyses unless it is proven
  deeply immutable; cache prepared ambient surfaces or compiled module results,
  not mutable semantic env snapshots.
- Imported module metadata surfaces may be cached and reused. Concrete imported
  envs must still be rebuilt fresh per analysis until binding schemes and type
  declarations are proven safe to share.
- `ModuleTypings` is the single canonical module artifact. JSON is a temporary
  hidden codec detail, not a checker-facing runtime type.
- `CompiledScope` is a cached derived view carried by `ModuleTypings`, not a
  second authoritative module artifact.
- `SourceAnalysis` is a source result, not a package-engine scratchpad. Keep
  duplicated source payload and module-pairing ambient baggage out of it.
- No host-side reconstruction of package results that `Typ.Check` can return
  directly.
- No snapshot-era rooted orchestration on the cold build-check path.
- Keep `typ` phase events rich enough to explain where cold-check time goes.
- Do not introduce a second in-process module authority just to make env import
  easier. If the monotonic env cannot ingest `ModuleTypings` directly enough,
  fix the env/import path instead.

## Current Priorities

1. Feed loaded `ModuleTypings` directly into the monotonic package env without
   building parallel module authorities.
2. Replace the remaining analysis-time ambient rebuild path with a genuinely
   shareable compiled import path, not raw `Infer.Env.module_scope`.
3. Keep only one authoritative module artifact in memory and derive public
   views from it once.
4. Split build-check payload from query/editor payload more aggressively.
5. Delete transitional prototype surfaces as soon as the engine no longer needs
   them.
6. Measure `kernel-new` after every architectural cut.

## Validation

- `riot build typ`
- `riot run riot -- check -p kernel-new --json`
- targeted oracle runs when semantics move
