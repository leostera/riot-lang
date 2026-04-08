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
