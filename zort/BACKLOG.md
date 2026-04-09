# Zort's Backlog

Add future `zort` work here.

This is a date-free execution plan with work split into immediate (`now`), shortly-after (`next`), and planned-but-not-in-cycle (`later`).

Use this order: fill `now` first, then pull from `next`, then `later` only when the `next` lane is healthy.

## now

- [ ] [P0] Wire OS signal ingress into runtime services and enforce platform-safe callback delivery. Implement signal handler registration and delivery path from low-level signal trap to `RuntimeServices`, then route through `Runtime.deliverPendingActions(...)`.
- [ ] [P0] Implement alternate signal-stack ownership in native setup/teardown, including leak-safe restore behavior and restoration of pre-existing foreign alt-stack when present.
- [ ] [P0] Model signal + blocking interaction so leaving blocking sections (and every safe scheduling point) processes pending actions immediately.
- [ ] [P1] Add callback-boundary enforcement points for all runtime-exposed C/API callback surfaces so finalizer and signal callbacks always cross `ControlKernel` callback boundaries.
- [ ] [P1] Add runtime-service docs/tests for the naming model decision (`runtime-local` vs shared/global named-values table), then lock policy in `spec` and code comments.
- [ ] [P1] Add first-wave tests for blocked-section behavior (`blocking section + pending finalizer/signal` scenarios), with explicit assertions that no callback runs twice and that delivery is ordered.

## next

- [ ] [P1] Add native-dynlink registration contract in the boundary layer: typed `frametable/gc_roots/code-fragment` metadata, mandatory symbol validation, and deterministic failure paths.
- [ ] [P1] Implement `Gc` compatibility façade (`Gc.get`, `Gc.set`, `Gc.minor`, `Gc.major`, `Gc.full_major`, `Gc.compact`, `Gc.stat`) without changing collector internals.
- [ ] [P1] Implement `Gc.Tweak` and runtime-parameters rendering surface, including compatibility-field behavior for non-shippable knobs.
- [ ] [P1] Add memprof callback plumbing (registration/unregistration, payload schema, backtrace-site filtering, and dispatch timing).
- [ ] [P1] Implement compare/hash parity slice: OCaml-invalid cases, NaN ordering/canonicalization, continuation handling, bounded traversal, and pending-action polling hooks.
- [ ] [P1] Split runtime-hosted primitives into explicit boundary modules: decide which sync primitives/channels/sys primitives stay out of core and record the public compatibility contract.
- [ ] [P1] Add memprof summary reporting: allocation-site histograms, kind-aware aggregates, and action logs suitable for `--trace-memprof`.
- [ ] [P1] Implement root-iteration safety for registries in progress (`ROOT_DELETED`-style), with concurrent-safe semantics at collector root scan boundaries.
- [ ] [P1] Add runtime-owned sync custom blocks: mutex/condition handles, OS error mapping, and ownership checks on invalid unlock/relock.
- [ ] [P1] Add summarized GC/collector metrics for this month’s changes to `notes/` artifacts and include regression tests.
- [ ] [P1] Add aggressive remembered-set verification and mutation-heavy generational stress suites so every major-to-nursery edge is recorded, compacted, and honored during minor collection.

## later

- [ ] [P2] Prepare domain/STW scaffolding interfaces: domain registry shim, world-stop hooks, and phase barriers usable by collectors without changing current single-domain runtime behavior.
- [ ] [P2] Add stack-overflow behavior plan and scaffolding for managed-stack vs system-stack overflow separation; document native/platform strategy.
- [ ] [P2] Add atomic field primitive surface (`load`, `exchange`, `cas`, `fetch_add`) and test ordering semantics for multi-thread-ready variants.
- [ ] [P2] Expand dynlink lifecycle work: close/unregister behavior, plugin handle ownership, and code-identity metadata surface.
- [ ] [P2] Add marshal/code-loading parity slices (including continuation marshalling rejection and code identity behavior) after dynlink surface stabilizes.
- [ ] [P2] Finalize hardening tasks: multi-domain GC-adoption path plan and forwarding-state observability notes for generational collector alignment.
