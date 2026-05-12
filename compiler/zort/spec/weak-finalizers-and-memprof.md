# Weak Arrays, Ephemerons, Finalizers, and Memprof

## Source anchors

- `vendor/ocaml/runtime/weak.c`
- `vendor/ocaml/runtime/finalise.c`
- `vendor/ocaml/runtime/memprof.c`
- `vendor/ocaml/runtime/major_gc.c`
- `vendor/ocaml/runtime/minor_gc.c`

## Weak arrays and ephemerons

- Weak arrays are implemented with the ephemeron machinery.
- An ephemeron is an `Abstract_tag` block with:
  - link field
  - data field
  - key fields
- Special sentinels exist for:
  - `caml_ephe_none`
  - `caml_ephe_locked`
- Minor GC may temporarily lock ephemeron keys. Readers must wait until a locked key becomes unlocked.

## Cleaning semantics

- During `Phase_sweep_ephe`, dead keys are removed and data is cleared if any key is dead.
- Getter/setter behavior is designed to appear as if all ephemerons were cleaned atomically, even though cleaning is incremental.
- `Weak.create` validates size; out-of-range accesses raise `Invalid_argument`.

## Finalizers

- Finalizers are tracked in separate “first” and “last” tables.
- After GC, unreachable tracked values are moved to todo queues for callback execution.
- “First” finalizers can darken/re-root their target value before callback execution.
- “Last” finalizers can be invoked with `unit` instead of the original value.
- Finalizer callback execution is reentrant-aware and may itself return an exception result.

## Domain interaction

- Finalizer work is integrated into major-GC phase coordination across domains.
- Terminating domains can orphan finalizer work, which other domains later adopt.

## Memprof

- Memprof samples allocations probabilistically by word count.
- Tracking state lives partly on the OCaml heap and partly in C tables.
- The GC scans memprof-owned roots explicitly rather than registering each one individually as a normal global root.
- Minor/major GC update memprof entry status after each collection.
- Orphaned tracking tables are adopted by surviving domains.

## zort takeaways

- Weak refs, ephemerons, finalizers, and memprof are not independent features; they all hook deeply into GC phase structure.
- If zort omits any of them early on, that should be a staged non-goal.
- If zort implements weak refs before ephemerons or finalizers, it should still reserve room in the collector design for “GC-phase-dependent cleanup”.

## zort managed-liveness baseline

- zort now has a dedicated `ManagedLiveness` subsystem in `src/liveness.zig`.
- The current semantic surface includes:
  - `WeakRefHandle` slots whose targets are cleared after the collector's weak phase if the target block is dead,
  - `EphemeronHandle` slots with explicit key arrays and optional data,
  - `FinalizerHandle` registrations with explicit `first` / `last` mode.
- Collector integration is phase-ordered:
  - normal root/provider marking runs first,
  - ephemerons may darken their data during the weak phase when all keys are live,
  - weak refs clear dead targets during the same weak phase,
  - finalizers queue ready callbacks during the finalizer phase.
- First-finalizer behavior is explicit:
  - the target is marked again before sweep,
  - the ready finalizer queue roots that argument until it is drained.
- zort still diverges deliberately from OCaml here:
  - weak refs and ephemerons are runtime-managed handles, not heap blocks,
  - memprof is a dedicated `src/memprof.zig` subsystem instead of heap/C-table hybrid state,
  - domain adoption/orphan handling is still deferred until real domain/STW work exists.

## zort memprof baseline

- zort now has a dedicated `MemprofState` subsystem in `src/memprof.zig`.
- The current memprof surface is intentionally smaller than OCaml's:
  - samples are keyed by stable `HeapRef`,
  - sampling is probabilistic by allocated word count by default,
  - deterministic interval sampling is kept only as an explicit test/debug mode,
  - sampled lifecycle transitions are `sampled_alloc`, `promoted`, and `reclaimed`,
  - sampled records can optionally carry an allocation backtrace as a slice of `site_id`s from the managed control stack.
- Integration points are explicit:
  - `Runtime` decides whether a fresh allocation should be sampled,
  - `Collector` reports promotion and reclaim transitions,
  - `EventSink` and `TraceRecorder` carry memprof lifecycle events like any other runtime event.
- The current baseline deliberately does not attempt full OCaml parity:
  - memprof does not own GC roots because sampled metadata stores site ids, not heap values,
  - sampling policy is single-runtime only,
  - domain adoption/orphan handling is still deferred,
  - memprof callbacks are not implemented yet.
