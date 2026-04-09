# Zort's Backlog

Add future `zort` work here.

This is a date-free execution plan with work split into immediate (`now`), shortly-after (`next`), and planned-but-not-in-cycle (`later`).

Use this order: fill `now` first, then pull from `next`, then `later` only when the `next` lane is healthy.

Each item is capability-level and includes:
- why this is needed for zort's goals,
- what capability is currently missing,
- and what cannot be achieved yet under the current architecture.

## now

- [ ] [P1] Typed plugin registration contract and required-metadata validation policy. Why: a typed contract enables safe optional dynlink while avoiding raw native-link assumptions; What we need: explicit metadata schema, required symbol checks, and deterministic failure shapes; Cannot do yet: complete binary loader parity until we define supported plugin formats in runtime policy.
- [ ] [P1] Compatibility GC-control façade for lifecycle operations and policy knobs. Why: user-facing GC control should be explicit without binding semantics to collector internals; What we need: boundary-facing commands mapped to collector hooks and invariants; Cannot do yet: direct OCaml API parity for all undocumented legacy fields.
- [ ] [P1] Memory-management tuning and runtime-parameter exposure surface. Why: visibility into collector behavior is required for benchmarking and debugging; What we need: stable configuration surface and diagnostics rendering; Cannot do yet: exact numeric compatibility for all vendor/runtime parameters while we intentionally keep knobs subsetted.

## next

- [ ] [P1] Memory-profiling callback pipeline and filtering. Why: callback-based profiling enables low-overhead integration with tooling; What we need: registration lifecycle, payload schema, and filtering strategy by site/category; Cannot do yet: full low-level integration with external profilers until sink formats are standardized.
- [ ] [P1] Structural compare/hash baseline in compatibility layer. Why: typed comparisons are core language observability and deterministic hashing behavior; What we need: invalid-value matrix, traversal budgets, and traversal interruption strategy; Cannot do yet: exhaustive parity for every custom callback mode and all historical edge-cases.
- [ ] [P1] Boundary split for runtime-hosted services. Why: keeps core semantic runtime small and maintainable; What we need: explicit module boundary for channels/sync/io-like capabilities and compatibility contract; Cannot do yet: complete host-primitive replacement until feature owners are agreed.
- [ ] [P1] Aggregated memory-profiling summaries by site/object class. Why: users need actionable summaries beyond raw samples; What we need: aggregation counters, export format, and summary reports; Cannot do yet: long-term stability guarantees for report schema before storage/versioning policy is set.
- [ ] [P1] Deletion-safe root-provider iteration. Why: roots can be updated during traversal in realistic workloads; What we need: tombstone/deferred-removal model with traversal invariants; Cannot do yet: full concurrent GC-thread safety model before multi-domain execution exists.
- [ ] [P1] Runtime-owned synchronization primitives with ownership and error mapping. Why: external code requires deterministic lock-like behavior without exposing raw runtime internals; What we need: first-class resource handles, lifecycle rules, and mapping to canonical exceptions; Cannot do yet: full fairness/perf tuning without dedicated sync stress harness.
- [ ] [P1] Expanded observability with regression-backed metrics artifacts. Why: changes in GC/control semantics need measurable validation; What we need: artifact schema, counters, and targeted regression tests; Cannot do yet: long-horizon dashboard conventions and governance for historical baselines.
- [ ] [P1] Generational mutation stress suite for remembered-edge correctness. Why: nursery/major boundaries are correctness-critical as code scales; What we need: mutation-heavy directed tests and stronger edge coverage; Cannot do yet: full production workload corpus for confidence before next collector milestones.
- [ ] [P1] Hardened external-result contracts and API boundary typing. Why: boundary consumers need stable semantics instead of internal heap encoding leakage; What we need: explicit result/error contract and migration away from legacy encoded assumptions; Cannot do yet: complete end-to-end migration of every external consumer path.

## later

- [ ] [P2] Managed-stack vs system-stack overflow strategy and recovery model. Why: failure behavior must be explicit to avoid silent runaway failures; What we need: policy for stack growth/fault handling and clear ownership boundary; Cannot do yet: platform-complete stack policy until non-Unix/native backends are completed.
- [ ] [P2] Shared-memory-safe atomic mutation primitives for fields. Why: enables future parallel execution and robust low-level concurrency APIs; What we need: atomic operations with explicit ordering and API design; Cannot do yet: full integration with multiple execution domains.
- [ ] [P2] Plugin lifecycle ownership, unload, and identity continuity. Why: plugin ecosystems depend on predictable identity and release behavior; What we need: handle ownership model and unload lifecycle semantics; Cannot do yet: fully safe unload under active closures/continuations without broader graph reachability rules.
- [ ] [P2] Marshaling/code-loading capability model. Why: avoids accidental compatibility traps while keeping clear limits; What we need: policy for accepted/forbidden serialization and continuation-related restrictions; Cannot do yet: exact byte-level parity with legacy compiler artifacts.
- [ ] [P2] Multi-domain GC hardening and forwarding observability plan. Why: prevents regressions when moving from baseline collector policy; What we need: explicit strategy and telemetry for forwarding/shared objects; Cannot do yet: full production-level GC concurrency without stable domain scheduler.
- [ ] [P2] Domain startup/shutdown parity foundations. Why: startup/shutdown ordering is required before reliable lifecycle APIs; What we need: reference-counted lifecycle sequencing and hook points; Cannot do yet: external host orchestration contracts for all embedding modes.
- [ ] [P2] Deterministic runtime shutdown orchestration for multi-context execution. Why: prevents teardown races and leaked work when contexts leave runtime participation; What we need: teardown sequencing model, hook ordering, and idempotency; Cannot do yet: full host coordination for forced termination and external shutdown races.
- [ ] [P2] STW progress handoff for blocking behavior. Why: blocked regions must not stall global coordination; What we need: handoff protocol and handback policy for collection phases; Cannot do yet: complete scheduler-level handoff without domain runtime.
- [ ] [P2] Aggregated global statistics model for multiple runtime participants. Why: observability must scale when runtime participants increase; What we need: aggregation strategy with approximate yet stable reporting; Cannot do yet: exactness guarantees under active migration/adoption.
- [ ] [P2] Cross-heap forwarding representation. Why: preserves sharing behavior across advanced collectors; What we need: forwarding metadata shape and stabilization semantics; Cannot do yet: full correctness proofs without dual-heap collector completion.
- [ ] [P2] Forwarding completion and convergence behavior. Why: stale or partial forwarding states are a major correctness risk; What we need: completion protocol and retry/recheck behavior; Cannot do yet: high-concurrency validation at scale.
- [ ] [P2] Orphan-safe finalizer transfer model. Why: finalizers must remain reliable under context loss/migration; What we need: ownership handoff rules and readiness queue ownership; Cannot do yet: multi-domain testbed that reproduces orphaning at scale.
- [ ] [P2] Platform-aware plugin loading abstraction layer. Why: decouples zort policy from host platform details; What we need: loader interface and metadata translation; Cannot do yet: parity for every native ABI variant and packaging pipeline.
- [ ] [P2] Plugin metadata validation suites. Why: bad plugin metadata should fail fast and clearly; What we need: deterministic negative and positive test matrix; Cannot do yet: exhaustive plugin corpus until build tooling standardizes.
- [ ] [P2] Portable signal-number mapping strategy. Why: signal APIs must remain consistent across Unix/other targets; What we need: canonical mapping table and unavailable-signal policy; Cannot do yet: full non-Unix mapping parity.
- [ ] [P2] Non-Unix signal-stack behavior policy. Why: avoids undefined behavior when features are missing; What we need: explicit capability list and documented fallbacks; Cannot do yet: equivalent native alternatives on all targets.
- [ ] [P2] Exception-aware resource unwind policy. Why: deterministic resource cleanup under exceptional exits is required for reliability; What we need: unwinding contracts for owned resources; Cannot do yet: full integration with all external resource kinds.
- [ ] [P2] Channel-like resource lifecycle governance. Why: deterministic close/error behavior is part of practical runtime semantics; What we need: lifecycle transitions and locked-state cleanup rules; Cannot do yet: complete suite of resource types and failure injections.
- [ ] [P2] Syscall bridge normalization behaviors. Why: host interactions must produce predictable, typed runtime exceptions; What we need: bridge contract for interrupted/bad-path/system-error cases; Cannot do yet: complete coverage of platform-specific syscall errors.
- [ ] [P2] Comparison/hash policy flags and compatibility boundaries. Why: maintainers need controlled behavior without overfitting to legacy internals; What we need: feature flags and documented policy matrix; Cannot do yet: community/embedding agreement on defaults.
- [ ] [P2] Byte/float array mutation + copy semantics. Why: memory semantics become correctness-critical as numeric code grows; What we need: explicit ordering, bounds, and consistency policy; Cannot do yet: final memory model guarantee for every optimization path.
- [ ] [P2] Weak/ephemeron phase parity extension. Why: advanced memory-liveness behavior depends on phase-aware cleaning; What we need: phase model and dead-key/data cleanup semantics; Cannot do yet: full scale stress coverage for complex graphs.
- [ ] [P2] Full GC phase observability model and baselines. Why: phase visibility is needed to reason about long-running collections; What we need: stable phase naming/events and baseline assertions; Cannot do yet: all collectors emitting uniform phase timelines.
- [ ] [P2] Major-collection scheduling and urgent-collection hooks. Why: memory pressure control needs explicit levers; What we need: scheduling API and urgent-work triggers; Cannot do yet: validated policy across diverse workloads.
- [ ] [P2] Runtime diagnostics and verbosity surface. Why: runtime users need operational visibility without core changes; What we need: rendering and parse/emit policy for diagnostic modes; Cannot do yet: complete legacy-field equivalence.
- [ ] [P2] Deletion-aware root-provider traversal invariants. Why: real systems mutate roots during traversal; What we need: deferred-removal pass and traversal invariants; Cannot do yet: strict lock-free concurrent GC path support.
- [ ] [P2] Callback-path parity tests for continuation backtraces and parent chains. Why: backtrace fidelity drives debugging and effect diagnosis; What we need: controlled tests for captured stack and callback boundaries; Cannot do yet: platform-specific callback metadata parity.
- [ ] [P2] Effect-boundary re-perform scenarios under callback/finalizer invocation. Why: ensures control transfer behavior stays coherent under mixed callback semantics; What we need: test matrix for boundary traversal and unhandled-effect behavior; Cannot do yet: full native-stack implementation for every edge case.
