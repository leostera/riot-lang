# typ TODO

This file tracks the architecture rewrite for `packages/typ`.

The target shape is no longer "phase N of the old checker rewrite". The target
shape is a cleaner internal library split with explicit ownership:

- `Typ.Check`
  source-local type engine
- `Typ.Infer`
  logical-module analysis over one module group
- `Typ.Analyze`
  authoritative package-level orchestration
- `Typ.Query`
  read-only queries over immutable rooted worlds
- `Typ.Session`
  host/session machinery for editor and incremental workflows

The goal is not to make `typ` source-identical to `vendor/ocaml/typing`. The
goal is to get the same high-level authority story:

- one semantic core for typing
- one canonical module artifact
- one imported-world lookup story
- one authoritative build-time module producer
- query/snapshot APIs that consume the same semantics instead of inventing a
  second compiler

## Target Architecture

### `Typ.Check`

Given an `ImportedWorld` plus one lowered semantic tree, type one source and
emit source-local outputs:

- typing diagnostics
- item and expression traces
- exported values
- exported type declarations
- enough information for type-at, definition-at, and module-summary building

`Typ.Check` must not own:

- lowering from raw source text
- `.ml`/`.mli` pairing
- package ordering
- persistence
- rooted snapshot/session concerns

### `Typ.Infer`

Given one logical module group and one imported world, perform module-local
analysis:

- lower each source in the group
- call `Typ.Check` for each lowered source
- reconcile the group through `ModulePairing`
- produce one canonical internal `ModuleTypings`
- return per-source analyses adjusted with inclusion diagnostics

`Typ.Infer` must stay pure library code:

- no store writes
- no package-bundle writes
- no host-side package reconstruction

### `Typ.Analyze`

Given prepared package/module inputs, loaded modules, and store access,
authoritatively analyze a package:

- build or consume module ordering
- grow `PackageEnv` monotonically
- call `Typ.Infer` for each local module group
- persist authoritative canonical module artifacts
- emit package summary and loaded-module outputs

`Typ.Analyze` is the only build-time writer of authoritative module artifacts.

### `Typ.Query`

Given one immutable rooted world, answer read-only questions:

- diagnostics
- file summaries
- module typings
- type-at
- definition-at

`Typ.Query` may lazily force canonical computations, but it must not:

- persist artifacts
- define different imported-world semantics
- define different pairing semantics
- rebuild a second ambient world model

### `Typ.Session`

Own long-lived host/session concerns:

- source registry
- revisions
- rooted closure planning
- snapshot lifetime
- cache scopes

`Typ.Session` should prepare immutable rooted worlds for `Typ.Query`, not own a
second semantic engine.

## Non-Negotiables

- [x] no snapshot-time store writes on read/query paths
- [x] no second package-check writer defining authoritative persistence
- [x] no hot-path imported-module resolution through flat ambient replay lists
- [x] no snapshot dependency discovery by nested snapshot forcing
- [ ] no second semantic stack split across build-path and query-path module
      analysis
- [ ] no host-side reimplementation of canonical module visibility rules
- [ ] no second authoritative artifact alongside canonical `ModuleTypings`
- [ ] no package/query layer owning semantic rules that belong in `Typ.Check`

## Current Reality To Preserve

- [x] `Summary2` plus `Env.env_of_summary` remains the local lexical env
      reconstruction story
- [x] `PackageEnv` is the canonical imported-module artifact index
- [x] `ScopeView` is the visible-name resolution layer over `PackageEnv`
- [x] `ImportedWorld` is the right imported-module boundary for source analysis
- [x] the authoritative build-style module producer today is
      `Typ.Check.fold_package_sources`
- [x] rooted snapshots are read-only and no longer persist module typings
- [x] snapshot preparation discovers dependencies before query forcing begins
- [x] build and snapshot flows already share the imported-world lookup model
- [ ] the current public names reflect the target ownership boundaries

## Big Checklist

### Name The Layers Correctly

- [ ] decide the final public/internal names for the new split:
      `Typ.Check`, `Typ.Infer`, `Typ.Analyze`, `Typ.Query`, `Typ.Session`
- [ ] stop using `Typ.Check` to mean package orchestration once the new split is
      in place
- [ ] stop using `Typ.Infer` to mean the whole source-analysis stack once
      `Typ.Check` and module-level `Typ.Infer` are separated
- [ ] update `packages/typ/src/typ.mli` to describe the new ownership model
- [ ] make the module names in `packages/typ/src` line up with the architecture
      instead of the transition state

### Make `Typ.Check` An Explicit Source-Local Engine

- [x] keep imported-module input flowing through `ImportedWorld`
- [x] keep `Typ.Check` independent from package-level persistence
- [ ] define one explicit API for source-local typing over lowered trees
- [ ] make `SourceAnalysis` call that API through a narrow boundary instead of
      reaching into checker internals ad hoc
- [ ] make the `Typ.Check` result shape explicit:
      diagnostics, exports, type decls, traces, summary ingredients
- [ ] keep traces and query indexes optional host-controlled outputs, not
      mandatory checker work
- [ ] audit config fields and remove package/session concerns from the
      source-local checker API
- [ ] keep local env reconstruction (`Summary2`, `Env.env_of_summary`) under the
      `Typ.Check`/checker-core boundary
- [ ] ensure `Typ.Check` can be called from both `Typ.Infer` and tests without
      depending on package ordering code

### Turn `Typ.Infer` Into Module-Group Analysis

- [ ] define the canonical input type for one logical module group
      (`.ml`, `.mli`, or both)
- [ ] decide whether `Typ.Infer` returns `ModulePairing.t` directly or a new
      wrapper record containing the canonical `ModuleTypings` and per-source
      analyses
- [ ] move the repeated pattern
      "analyze each source in the group, then pair with `ModulePairing`"
      behind one reusable `Typ.Infer` API
- [ ] stop duplicating that module-group analysis logic in both
      `packages/typ/src/check.ml` and `packages/typ/src/session/Snapshot.ml`
- [ ] keep `Lower` plus source-summary packaging inside `Typ.Infer`, not in
      package orchestration
- [ ] keep `Typ.Infer` free of store access and package-bundle persistence
- [ ] make signature-inclusion diagnostics part of the module-group result, not
      a side effect of whichever caller happened to pair the sources
- [ ] make sure both build and query flows call the same `Typ.Infer` entrypoint
      for local module groups

### Make `Typ.Analyze` The Only Authoritative Build-Time Orchestrator

- [x] current cold package checks already route through the authoritative build
      engine
- [x] build-path module persistence happens on the authoritative path, not the
      query path
- [ ] rename or resurface the current package engine as `Typ.Analyze`
- [ ] narrow `Typ.Analyze` to orchestration concerns:
      graphing, ordering, imported-world growth, persistence, package summary
- [ ] make `Typ.Analyze` call only `Typ.Infer` for local module-group work
- [ ] keep monotonic `PackageEnv` growth in `Typ.Analyze`
- [ ] make it explicit which outputs are authoritative:
      canonical local module typings, loaded modules, package bundle
- [ ] keep public/module visibility shaping out of hosts and inside the
      canonical analysis layer
- [ ] ensure package summary emission happens only from authoritative completed
      package analysis
- [ ] keep event emission aligned to source-local check, module-group infer, and
      package analyze boundaries

### Make `Typ.Query` A True Read-Only Facade

- [x] `Typ.Query` is already a read API over snapshots
- [x] snapshot-time persistence is gone
- [ ] move all meaningful local module computation behind shared `Typ.Infer`
      boundaries so `Typ.Query` stays thin
- [ ] make `Typ.Query` responsible only for read APIs, not for assembling a
      second semantic model
- [ ] ensure query answers are derived from the same canonical module-group
      computations used by build analysis
- [ ] keep query caching keyed by revision, visible world, and loaded modules,
      not by alternate semantic assumptions
- [ ] ensure `type_at`, `definition_at`, diagnostics, and `module_typings_of`
      all observe the same `Typ.Infer` results
- [ ] remove any remaining snapshot-only module shaping or aliasing rules

### Keep `Typ.Session` Host-Oriented

- [x] rooted snapshots are revision-bound immutable worlds
- [x] dependency closure and local ordering logic are moving toward shared
      planning rather than opportunistic typing
- [ ] make `Typ.Session` own source registration, revisions, rooted closure,
      and cache scopes only
- [ ] move any remaining canonical module production logic out of
      `Session`/`Snapshot` and into `Typ.Infer` or `Typ.Analyze`
- [ ] keep snapshot caches as memoization of canonical computations, not as a
      second artifact layer
- [ ] decide whether `Snapshot` remains a first-class type or becomes an
      internal rooted-world representation behind `Typ.Query`

### Keep One Imported-World Story

- [x] `PackageEnv + ScopeView` is the canonical imported-world input for source
      analysis
- [x] build and rooted snapshot flows already use the same imported-world path
- [x] hot-path dependencies on `TypConfig.with_ambient*` were removed
- [ ] finish deleting compatibility-only ambient plumbing from APIs and data
      types once no caller needs it
- [ ] keep `hidden_export_names` scoped to export filtering only
- [ ] ensure local opens, includes, module aliases, record lookup, constructor
      lookup, and type lookup remain driven only through `ImportedWorld`
- [ ] keep imported-world creation outside `Typ.Check`; the checker should
      consume it, not build it

### Keep One Canonical Module Artifact

- [ ] keep `ModuleTypings` as the only canonical reusable module artifact
- [ ] keep `CompiledScope` as a derived cache over canonical module artifacts
- [ ] keep `FileSummary` as a source-facing summary, not a second module
      authority
- [ ] decide whether `Typ.Infer` exposes both per-source analyses and one
      canonical module artifact, or a richer result type containing both
- [ ] audit codepaths that still treat qualified/public variants of the same
      module as separate authoritative artifacts

### Stop Cloning Artifacts To Express Visibility

- [ ] replace hot-path use of `ModuleSurface.rebind_module_typings` with
      visible-name/view metadata
- [ ] stop synthesizing fresh source hashes just to represent alias/public names
- [ ] stop persisting cloned alias/public `ModuleTypings`
- [ ] decide whether package bundles store canonical modules only or canonical
      modules plus a separate visibility/public-name index
- [ ] make internal names, local aliases, and public package names resolve
      through one visibility/view layer
- [ ] keep qualified surface rendering as a derived helper for diagnostics and
      UI only
- [ ] add tests proving alias/public visibility does not require artifact
      cloning

### Strengthen Canonical Identity

- [ ] decide the canonical persistent module identity that crosses analysis,
      env, and persistence boundaries
- [ ] push canonical module ids through `PackageEnv`, bindings, entities, and
      store records
- [ ] stop treating `module_name : string` as the only imported authority
- [ ] stop treating `BindingId.Persistent of SurfacePath.t` as a sufficient
      imported identity
- [ ] preserve user-facing surface paths for diagnostics and UX, but keep them
      separate from canonical identity
- [ ] audit caches and comparisons that still key semantic identity by raw
      module name
- [ ] add tests for aliasing, public names, nested paths, and persistence round
      trips under the new identity model

### Make Module Inference And Interface Inclusion Explicit

- [ ] decide the long-term contract of `ModulePairing` under the new split
- [ ] decide whether interface inclusion should move closer to OCaml-style
      env-aware reasoning or stay intentionally narrower
- [ ] if it stays narrower, document the limitation explicitly
- [ ] if it broadens, replace simplistic surface comparison with imported-world
      aware signature reasoning
- [ ] keep mismatch diagnostics attached to both source analyses in a stable way
- [ ] add tests for imported aliases, included modules, hidden type identity,
      constructor ownership, record labels, and signature visibility
- [ ] ensure public/visibility views cannot change inclusion results

### Add Import Consistency To Persistence

- [ ] design the import-consistency payload stored with each authoritative
      module artifact
- [ ] record canonical imported module ids and hashes for every persisted module
- [ ] validate hydrated module artifacts against imported consistency metadata
- [ ] reject stale hydrated results explicitly instead of silently reusing them
- [ ] decide how package fingerprinting interacts with per-module import
      consistency
- [ ] add tests for renamed modules, changed dependencies, stale package
      bundles, and rejected hydration
- [ ] make events/diagnostics explicit when hydration is rejected

### Keep Dependency Discovery Separate From Typing

- [x] snapshot preparation no longer relies on nested snapshot forcing to learn
      missing modules
- [x] dependency discovery uses parse deps and cheap pre-typing data
- [ ] decide whether a separate persisted module header is still needed
- [ ] if needed, define exactly what the header contains:
      declared modules, exported nested module prefixes, requirements, cycle
      metadata
- [ ] make both `Typ.Analyze` and `Typ.Session` consume the same discovery
      boundary
- [ ] keep cycle discovery and diagnostics explicit while staying pre-typing
- [ ] add tests for nested modules, includes, implicit opens, and local cycles
      that only depend on the discovery boundary

### Simplify `riot-check` Host Integration

- [x] `riot-check` cold package checks already use the authoritative build path
- [ ] update `riot-check` to the new public surfaces once naming lands
- [ ] remove any remaining host-side reconstruction of package module results
- [ ] remove any remaining host-side visibility rebinding logic
- [ ] keep cold package checks on `Typ.Analyze`
- [ ] keep rooted editor/query flows on `Typ.Session` plus `Typ.Query`
- [ ] keep the minimal build payload separate from richer query payloads
- [ ] add integration tests that lock down the split between build and query
      workflows

### Rewrite The Docs Around The New Split

- [ ] update `packages/typ/docs/checker/index.md` for the new architecture
- [ ] update `packages/typ/docs/checker/engine.md` so it describes
      `Check -> Infer -> Analyze -> Query/Session`
- [ ] delete or rewrite docs that still describe the old package-check vs
      rooted-snapshot split as if both were architectural authorities
- [ ] update `packages/typ/AGENTS.md` to route work by the new ownership model
- [ ] keep the docs explicit about which data is authoritative, derived, or
      query-only
- [ ] add a short migration note mapping the old names to the new ones while the
      tree is still in transition

### Validation

- [ ] run `riot fix ./packages/typ`
- [ ] run `riot fix ./packages/riot-check`
- [ ] run `riot fmt ./packages/typ`
- [ ] run `riot fmt ./packages/riot-check`
- [ ] run `riot build typ riot-check`
- [ ] run `riot test -p typ`
- [ ] run `riot bench -p typ`
- [ ] run `riot run riot -- check -p kernel-new`
- [ ] keep before/after notes for cold package-check timing and memory behavior
- [ ] keep regression coverage for:
      source-local typing,
      module pairing,
      imported-world semantics,
      rooted query behavior,
      package analysis,
      persistence/hydration

## Open Design Decisions

- [ ] is `Typ.Infer` the right name, or should the module-group layer be called
      `Typ.SourceAnalysis`, `Typ.Module`, or something similar?
- [ ] should `Typ.Infer` expose `ModulePairing.t` directly, or hide it behind a
      more explicit result type?
- [ ] does `Snapshot` remain public, or should `Typ.Query` hide the rooted-world
      representation?
- [ ] do package bundles store canonical modules only, or canonical modules plus
      a public-name index?
- [ ] do we need a separate persisted discovery header, or can it be derived
      cheaply enough from canonical module artifacts?
- [ ] how much `Includemod` parity do we want in the first stable rewrite
      target?
