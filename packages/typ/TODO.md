# typ TODO

This file tracks the architecture rewrite for `packages/typ`.

It is based on:

- the current `typ` docs, especially `packages/typ/docs/checker/fast_package_check.md`
- the current implementation in `packages/typ/src`
- the audit against `vendor/ocaml/typing`

The goal is not to make `typ` source-identical to OCaml. The goal is to make
the high-level shape match the same architectural story:

- one authoritative module-producing path
- one canonical reusable module artifact
- one imported-module environment growth model
- rooted snapshots for query worlds, not as an alternate build checker

## End State

- [ ] `typ` has one authoritative build-style module-producing engine.
- [ ] `ModuleTypings` is the only canonical reusable module artifact.
- [ ] `PackageEnv + ScopeView` is the one imported-module lookup story.
- [ ] rooted snapshots consume authoritative module artifacts instead of
      rebuilding ambient exports and ambient type-decl lists.
- [ ] alias and public package names are views over canonical modules, not
      cloned `ModuleTypings`.
- [ ] persistence records enough imported-module consistency data to reject
      stale hydrated results.
- [ ] `riot-check` cold package checks use the authoritative package engine
      directly.

## Non-Negotiables

- [ ] no snapshot-time store writes on read/query paths
- [ ] no second package-check orchestrator with its own module semantics
- [ ] no host-side reimplementation of public-module shaping rules
- [ ] no hot-path rebuilding of flat ambient lists when a persistent imported
      env can answer the same question
- [ ] no second authoritative artifact alongside `ModuleTypings`

## Good Pieces To Preserve

- [ ] keep `Summary2` + `Env.env_of_summary` as the local lexical env
      reconstruction story
- [ ] keep `PackageEnv` as the canonical imported-module artifact index
- [ ] keep `ScopeView` as the visible-name resolution layer
- [ ] keep `CompiledScope` as a cached derived view on `ModuleTypings`, not as
      a second authority
- [ ] keep `SourceAnalysis` focused on source results rather than package-engine
      scratch state

## Phase 0. Baseline, Guardrails, and Invariants

Current touchpoints:

- `packages/typ/src/check.ml`
- `packages/typ/src/session/Session.ml`
- `packages/typ/src/session/Snapshot.ml`
- `packages/riot-check/src/check/session.ml`

Checklist:

- [x] record a before baseline for `riot build typ riot-check --json`
- [ ] record a before baseline for `riot test -p typ --json`
- [ ] record a before baseline for `riot run riot -- check -p kernel-new --json`
- [x] write down the current cold-check path versus rooted-snapshot path in one
      short architecture note inside this file or the checker docs
- [x] add a regression test that fails if the snapshot query path persists
      module typings during read-only work
- [x] add a regression test that fails if `riot-check` cold package checks route
      through rooted-session package reconstruction
- [ ] add a regression test that catches drift between build-path and
      snapshot-path visible-module semantics for the same package
- [x] make the structured event stream expose enough information to prove which
      engine path was used for a given check

Exit criteria:

- [x] we can state exactly which codepath is authoritative today
- [ ] we have tests that lock down the most dangerous drift regressions before
      refactoring

Current architecture note:

- The authoritative build-style module producer today is
  `Typ.Check.fold_package_sources` in `packages/typ/src/check.ml`. It builds
  the package graph, analyzes module groups in dependency order, pairs each
  finished module once, persists canonical `ModuleTypings`, and extends
  `PackageEnv` plus `LoadedModules` for downstream groups.
- The rooted snapshot path is `Session.prepare_snapshot` plus lazy forcing in
  `packages/typ/src/session/Snapshot.ml`. That path prepares a rooted,
  revision-bound query world and lazily computes analyses plus module pairings
  for queries. It should stay read-only and consume authoritative artifacts,
  not define persistence semantics.
- The currently reachable `riot-check` cold-check path goes through
  `packages/riot-check/src/check/session.ml` ->
  `checked_group_for_package_scan` -> `Typ.Check.fold_package_sources`.
  `workspace_module_typings_for_package` uses the same authoritative engine for
  dependency package warmup. The older rooted-session package helper still
  exists in `riot-check`, but `check_target_files` does not select it today.
- Phase 0 explicitly defers imported-resolution unification and
  build-vs-snapshot alias-visibility parity work. Those semantics belong to
  Phase 2 unless they are covered by targeted regressions.

## Phase 1. Freeze the Authority Boundary

Current touchpoints:

- `packages/typ/src/check.ml`
- `packages/typ/src/session/Snapshot.ml`
- `packages/riot-check/src/check/session.ml`

Checklist:

- [ ] define one explicit contract for authoritative module production on the
      build path
- [x] stop persisting module typings from `Snapshot.ensure_module_typings_persisted`
- [x] make snapshot forcing return query results only, not persistence side
      effects
- [x] make it impossible for query-time code to mutate the store accidentally
- [x] make `check.ml` the only place that persists authoritative finished-module
      typings for build-style checking
- [x] remove any end-of-run “persist again” or “reload what we just built”
      behavior from `riot-check`
- [x] remove any fallback package-bundle reconstruction that depends on asking a
      snapshot for authoritative results after the fact
- [x] add tests that prove one finished module is persisted once on the build
      path and zero times on the snapshot path

Exit criteria:

- [x] there is one authoritative producer of persisted module artifacts
- [x] snapshots can no longer define persistence semantics

## Phase 2. Make Imported Resolution Use One World Model

Current touchpoints:

- `packages/typ/src/TypConfig.mli`
- `packages/typ/src/SourceAnalysis.ml`
- `packages/typ/src/PackageEnv.ml`
- `packages/typ/src/ScopeView.ml`
- `packages/typ/src/infer/checker.ml`
- `packages/typ/src/session/Snapshot.ml`

Checklist:

- [x] define the imported-world API that `SourceAnalysis.analyze` should consume
      on every path
- [x] make `PackageEnv + ScopeView` the canonical imported-module input for
      source analysis
- [x] keep `TypConfig.ambient`, `ambient_type_decls`, and
      `ambient_visible_types` as compatibility shims only while migrating
- [x] migrate the build engine to use only the imported-world API
- [x] migrate rooted snapshot forcing to use the same imported-world API
- [x] remove hot-path dependencies on `TypConfig.with_ambient`
- [x] remove hot-path dependencies on `TypConfig.with_ambient_type_decls`
- [x] remove hot-path dependencies on `TypConfig.with_ambient_visible_types`
- [x] make `hidden_export_names` a narrowly scoped export-filter concern, not a
      second imported-module channel
- [x] add tests that prove imported value lookup, type lookup, constructor
      lookup, record lookup, local opens, and include/module-alias behavior all
      work through the same imported-world path on both build and snapshot flows

Exit criteria:

- [x] imported-module resolution no longer depends on replaying flat ambient
      exports or type-decl lists
- [x] build and snapshot flows share the same imported lookup semantics

## Phase 3. Delete Snapshot Ambient Replay

Current touchpoints:

- `packages/typ/src/session/Snapshot.ml`
- `packages/typ/src/TypConfig.ml`
- `packages/typ/src/ModuleSurface.ml`

Checklist:

- [x] remove `loaded_ambient_env_for` from the snapshot hot path
- [x] remove `loaded_ambient_type_decls_for` from the snapshot hot path
- [x] remove `local_ambient_env_for` from the snapshot hot path
- [x] remove `local_ambient_type_decls_for` from the snapshot hot path
- [ ] replace ambient replay caches with caches keyed by visible module ids and
      canonical module results
- [ ] make snapshot local-module visibility produce `ScopeView` entries directly
      instead of concatenating export lists
- [ ] keep any needed caching around qualified/public surfaces as derived view
      caches only, not as checker inputs
- [ ] make snapshot forcing pass imported context through `PackageEnv` lookups
      rather than `TypConfig` ambient mutation
- [ ] add tests that snapshot diagnostics and query answers remain stable after
      removing ambient replay
- [ ] add tests that snapshots no longer allocate ambient payload proportional to
      all visible imported exports

Exit criteria:

- [ ] snapshots no longer rebuild imported state as flat ambient lists
- [ ] snapshot caching is about canonical module results and visibility, not
      replayed semantic env payload

## Phase 4. Separate Dependency Discovery From Typing

Current touchpoints:

- `packages/typ/src/session/Session.ml`
- `packages/typ/src/session/Snapshot.ml`
- `packages/typ/src/check.ml`
- `packages/typ/src/model/FileSummary.ml`
- `packages/typ/src/model/ModuleTypings.ml`

Checklist:

- [ ] document the dependency-discovery inputs that are allowed before typing
- [ ] stop using nested rooted snapshots inside `collect_missing_module_summaries`
- [ ] make missing-requirement discovery use parse deps plus cheap persisted
      module header information
- [ ] decide whether `typ` needs a smaller persisted module header separate from
      full `ModuleTypings`
- [ ] if a header is needed, define exactly what it contains:
      declared modules, exported nested module prefixes, import requirements,
      and any cycle/discovery metadata
- [ ] make snapshot preparation discover missing modules without forcing full
      sibling typing just to learn nested module exports
- [ ] share dependency-closure and module-ordering logic between the package
      engine and rooted snapshot preparation where semantics must match
- [ ] preserve explicit local-module cycle diagnostics while moving cycle
      discovery earlier and cheaper
- [ ] add tests for nested modules, `include`, implicit opens, and local cycles
      so discovery behavior is locked down before the refactor lands

Exit criteria:

- [ ] dependency discovery no longer depends on opportunistic typed forcing
- [ ] snapshot preparation knows what is missing before query forcing begins

## Phase 5. Strengthen Canonical Module Identity

Current touchpoints:

- `packages/typ/src/model/LocalModules.ml`
- `packages/typ/src/model/BindingId.ml`
- `packages/typ/src/model/EntityId.ml`
- `packages/typ/src/model/ModuleTypings.ml`
- `packages/typ/src/PackageEnv.ml`
- `packages/typ/src/Store.ml`

Checklist:

- [ ] decide the canonical persistent module identity that crosses analysis,
      env, and persistence boundaries
- [ ] stop treating `module_name : string` as the only imported authority
- [ ] stop treating `BindingId.Persistent of SurfacePath.t` as a sufficient
      representation of imported binding identity
- [ ] preserve user-visible surface paths for diagnostics and UX, but separate
      them from canonical imported identity
- [ ] push `PackageEnv.ModuleId` or a close equivalent through more of the
      imported-module APIs
- [ ] make store records point at canonical module identity plus display names,
      not only raw strings
- [ ] audit places that still compare or cache by raw module name and decide
      whether those should become canonical-id keyed
- [ ] keep `LocalModules` alias matching logic only as a visibility-resolution
      concern, not as the artifact identity layer
- [ ] add tests that imported identity survives aliasing, public rebinding,
      nested module views, and persistence round trips

Exit criteria:

- [ ] canonical module identity is distinct from presentation paths
- [ ] aliasing no longer requires cloning artifacts to preserve identity

## Phase 6. Replace Cloned Alias/Public Artifacts With Views

Current touchpoints:

- `packages/typ/src/ModuleSurface.ml`
- `packages/typ/src/check.ml`
- `packages/typ/src/session/Snapshot.ml`
- `packages/riot-check/src/check/session.ml`

Checklist:

- [ ] decide the runtime representation for visible-module views over canonical
      module results
- [ ] replace `ModuleSurface.rebind_module_typings` in hot paths with view data
      or alias tables
- [ ] make internal module names, local aliases, and public package names all
      resolve through the same visibility/view layer
- [ ] stop synthesizing new source hashes just to represent alias/public names
- [ ] stop persisting cloned alias/public `ModuleTypings`
- [ ] decide whether package bundles should store:
      canonical modules only, or canonical modules plus a separate public-name
      index
- [ ] remove any remaining host-side rebind logic from `riot-check`
- [ ] keep qualified surface rendering as a derived helper for diagnostics or
      UI, not as a second persisted artifact
- [ ] add tests that local alias exports, public package names, and nested alias
      lookups all behave identically before and after the representation change

Exit criteria:

- [ ] one canonical module artifact can be exposed under many visible names
- [ ] no codepath needs to clone `ModuleTypings` just to express alias/public
      visibility

## Phase 7. Add Import Consistency To Persistence

Current touchpoints:

- `packages/typ/src/Store.ml`
- `packages/typ/src/check.ml`
- `packages/typ/src/session/Session.ml`
- `packages/typ/src/model/ModuleTypings.ml`

Checklist:

- [ ] design the import-consistency payload persisted with each authoritative
      module result
- [ ] record canonical imported module ids and hashes for every persisted module
- [ ] decide how package fingerprinting relates to per-module import
      consistency checks
- [ ] validate hydrated module typings against imported consistency data before
      treating them as authoritative
- [ ] make stale imported dependencies invalidate hydration cleanly instead of
      silently reusing bad data
- [ ] add tests for renamed modules, changed dependencies, and stale package
      bundles
- [ ] make diagnostics/events explicit when hydration is rejected because of
      consistency mismatch

Exit criteria:

- [ ] hydrated artifacts can be trusted against the imported world they claim to
      summarize
- [ ] store semantics are closer in spirit to OCaml `import_crcs` / `imports`

## Phase 8. Decide and Implement the Interface-Inclusion Story

Current touchpoints:

- `packages/typ/src/ModulePairing.ml`
- `packages/typ/src/model/VisibleTypes.ml`
- `packages/typ/src/PackageEnv.ml`

Checklist:

- [ ] decide the target: OCaml-like env-aware signature inclusion, or an
      intentionally narrower Riot-specific subset
- [ ] if the target is narrower, write that limitation down explicitly in the
      checker docs
- [ ] if the target is broader, design the replacement for current
      export/type-decl list comparison in `ModulePairing`
- [ ] move implementation-vs-interface checking closer to imported env-aware
      module-signature reasoning
- [ ] support richer mismatch diagnostics without baking current simplistic
      surface assumptions into the final architecture
- [ ] add dedicated tests for aliasing through signatures, included module
      surfaces, hidden type identity, constructor ownership, and record labels
- [ ] audit whether public/package views can affect signature inclusion results;
      if yes, fix the representation before deepening the inclusion engine

Exit criteria:

- [ ] the project has an explicit, documented answer for what interface
      inclusion means
- [ ] `ModulePairing` is no longer an accidental long-term architecture choice

## Phase 9. Simplify `riot-check` Integration

Current touchpoints:

- `packages/riot-check/src/check/session.ml`
- `packages/typ/src/check.ml`
- `packages/typ/src/session/Session.ml`

Checklist:

- [ ] make cold package checks use the authoritative package engine directly
- [ ] keep rooted snapshots only for explicit rooted query/editor workflows
- [ ] remove `riot-check` code that reconstructs package module results after
      the authoritative engine already had them
- [ ] remove `riot-check` code that rebinds module typings independently of the
      canonical visibility layer
- [ ] delete comments warning about semantic drift once the second path is gone
- [ ] keep `riot-check`’s minimal checked-file payload separate from richer
      query payload
- [ ] add integration tests covering package checks, explicit-file checks, and
      rooted query flows so the split stays intentional

Exit criteria:

- [ ] `riot-check` no longer acts as an architecture patch layer over `typ`
- [ ] there is one build-style engine and one query-style snapshot story

## Phase 10. Cleanup, Docs, and Validation

Current touchpoints:

- `packages/typ/docs/checker/fast_package_check.md`
- `packages/typ/docs/checker/engine.md`
- `packages/typ/AGENTS.md`

Checklist:

- [ ] delete dead ambient-replay helpers
- [ ] delete dead rooted package-check orchestration helpers
- [ ] delete dead public-module cloning helpers
- [ ] update checker docs to reflect the final architecture instead of the
      transition state
- [ ] update any AGENTS guidance that still describes the old split semantics
- [ ] make sure the docs say clearly which data is authoritative, which data is
      derived, and which data is query-only
- [ ] run the `packages/typ/AGENTS.md` validation stack after each coherent
      slice:
      `riot fix ./packages/typ`,
      `riot fix ./packages/riot-check`,
      `riot fmt ./packages/typ`,
      `riot fmt ./packages/riot-check`,
      `riot build typ riot-check`,
      `riot test -p typ`,
      `riot bench -p typ`,
      `riot run riot -- check -p kernel-new`
- [ ] compare before/after timings and memory behavior on the cold package-check
      path

Exit criteria:

- [ ] the docs match the implementation
- [ ] the validation stack passes or has explicitly explained known failures
- [ ] cold package checks are simpler, faster, and semantically single-sourced

## Open Design Decisions

- [ ] do we need a separate persisted module header for discovery, or can a
      cheaper view be derived from canonical `ModuleTypings` without retyping?
- [ ] what exact persistent module-id shape should cross store boundaries?
- [ ] how much OCaml `Includemod` parity do we actually want in the first
      rewrite target?
- [ ] should package bundles store public-name indexes explicitly, or should
      hosts derive those from canonical modules plus a visibility map?
- [ ] what is the minimum query payload that snapshots must retain once build
      checks stop carrying editor baggage?

## Suggested Execution Order

- [ ] Phase 0
- [ ] Phase 1
- [ ] Phase 2
- [ ] Phase 3
- [ ] Phase 4
- [ ] Phase 5
- [ ] Phase 6
- [ ] Phase 7
- [ ] Phase 8
- [ ] Phase 9
- [ ] Phase 10

The dependency order above is deliberate:

- freezing authority before refactoring internals avoids reintroducing a second
  writer
- converging imported resolution before changing identities/views avoids moving
  drift around
- fixing discovery before deep persistence work keeps the store boundary honest
- deciding the inclusion story after identity and views are sane avoids baking
  unstable representation choices into `ModulePairing`
