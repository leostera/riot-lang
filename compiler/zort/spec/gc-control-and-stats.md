# GC Control, Stats, and Runtime Knobs

## Source anchors

- `vendor/ocaml/runtime/gc_ctrl.c`
- `vendor/ocaml/runtime/gc_stats.c`
- `vendor/ocaml/runtime/startup_aux.c`

## Compatibility-shaped control surface

- The runtime exposes a compatibility-oriented `Gc` control API, not a cleanly designed one.
- `caml_gc_get` returns an 11-field tuple with legacy slot positions.
- Several fields are compatibility placeholders left at `0`.
- `caml_gc_set` accepts the same shape and updates:
  - minor heap size
  - percent free
  - GC verbosity
  - max stack size
  - custom block ratios/threshold

## `Gc.set` behavior

- Percent-free and custom ratios are normalized to at least `1`.
- Stack-limit changes happen before minor-heap changes.
- Minor-heap resizing comes last because it can:
  - trigger a minor collection
  - invalidate the tuple argument
  - raise `Out_of_memory`
- If the requested minor heap exceeds the global max, the max is raised first.
- Observable consequence: `Gc.set` is not a passive parameter write. It can run collection and fail.

## Explicit collection operations

- `Gc.minor`
  - performs a minor collection
  - then processes pending actions
- `Gc.major`
  - empties minor heaps once
  - finishes one major cycle
  - resets major pacing
  - then processes pending actions
- `Gc.full_major`
  - may run up to 3 major cycles
  - the source comment explicitly says a currently unreachable object may need up to 3 cycles to disappear
- `Gc.compact`
  - also runs the 3-cycle full-major preparation path before final compaction work
- `Gc.stat`
  - forces the full-major path first
  - then returns a fresh `quick_stat`
- These explicit operations emit runtime-event markers.

## Statistics surface

- `caml_gc_counters` returns current minor/promoted/major word counters.
- `caml_gc_quick_stat` returns an 18-field tuple including:
  - minor/promoted/major words
  - minor and major collection counts
  - total heap words
  - live heap words and blocks
  - pool fragmentation words
  - compaction count
  - heap max words
  - current OCaml stack size
  - forced major collection count
  - live stack memory
- Some legacy positions remain `0` placeholders.

## How global stats are computed

- Global GC stats are assembled from:
  - orphaned stats from dead domains
  - sampled stats of non-current domains
  - live allocation stats of the current domain
- Sampled stats are refreshed only during stop-the-world events.
- The source explicitly says the computed instantaneous maximum heap size is approximate and effectively “completely wrong” because it sums per-domain maxima.
- Observable consequence: some stats are exact counters, while others are compatibility approximations.

## Runtime identity and parameter rendering

- `caml_runtime_variant` returns:
  - `""` for normal runtime
  - `"d"` for debug runtime
  - `"i"` for instrumented runtime
- `caml_runtime_parameters` renders the current state as an `OCAMLRUNPARAM`-style string.
- The output includes active GC tweaks as `,Xname=value` fragments.
- One legacy `R` slot is explicitly missing from the formatted output.

## Runtime warnings and tweak knobs

- Runtime warnings can be enabled/disabled independently of other GC knobs.
- Named tweak lookup is string-based.
- Unknown tweak names raise `Invalid_argument("Gc.Tweak: parameter not found")`.
- The currently exposed tweak names come from a small fixed table:
  - `mark_stack_prune_factor`
  - `small_heap_limit`
- The runtime can also list only the tweaks that differ from their startup values.

## Ramp-up / ramp-down

- `Gc.ramp_up` marks a phase where allocation work is suspended and returned later.
- Nested ramp-up phases are explicitly supported.
- The wrapper preserves outer suspended work while an inner ramp-up runs.
- The ML-facing wrapper returns both:
  - the callback result
  - the deferred work count
- If the callback raises, the wrapper ramps down before re-raising so deferred work is not lost.

## Initialization behavior

- GC initialization sets:
  - normalized minor-heap max
  - initial stack limit
  - initial fiber stack size
  - percent-free/custom ratios
- In native builds it also initializes frame descriptors before domains/stats setup.

## zort takeaways

- zort should separate:
  - collector internals
  - stable public control knobs
  - debug/experimental tweak knobs
- If zort wants compatibility with OCaml-facing APIs, it may need a compatibility layer that preserves tuple shapes and odd placeholder fields without infecting the internal runtime design.
- The OCaml runtime treats explicit GC calls as real runtime actions that may run pending callbacks and mutate pacing state, not as pure hints.

## zort debug and stats baseline

Executable model:

- [`runtime/PendingActionDrain.tla`](./runtime/PendingActionDrain.tla)
- [`runtime/README.md`](./runtime/README.md)

- zort now emits collector-scoped observability through `src/event_sink.zig` rather
  than through ad hoc bench-only counters.
- Each collection can emit:
  - `collect.start`
  - one `root_provider` event per provider
  - one `gc_phase` event per explicit collector phase
  - `reclaim` events for reclaimed objects
  - `collect.end`
  - a `gc_snapshot` containing:
    - strategy
    - root count
    - marked counts by object kind
    - promoted counts by object kind
- zort now treats pending-action delivery as an explicit runtime protocol:
  - configured delivery hooks run only at explicit checkpoints,
  - manual `deliverPendingActions(...)` still exists as the explicit compatibility/testing seam,
  - the automatic checkpoint path is guarded against reentrant drain recursion.
    - promoted words
    - reclaimed counts by object kind
    - current live nursery object/word counts
    - current live major object/word counts
    - timings for root enumeration, marking, weak processing, finalizer processing, sweeping, and total collection
- Mutation observability now includes:
  - `barrier` events for remembered-set recording
  - per-case `barrier_records` counters in bench output and profile JSON
- Sampled memory profiling observability now includes:
  - `memprof.sampled_alloc`
  - `memprof.promoted`
  - `memprof.reclaimed`
  - per-case `memprof_samples`, `memprof_promotions`, and `memprof_reclaims` counters
- `Runtime.Config.debugChecks` adds explicit verification modes:
  - `verify_roots`
  - `verify_heap_store`
  - `verify_control_kernel`
  - `verify_after_collect`
- Bench/profile traces can now show:
  - provider counts from `RootRegistry`, `ControlKernel`, `RuntimeServices`, and `ManagedLiveness`
  - phase-by-phase collector timing
  - callback/effect events separately from GC events
  - memprof lifecycle events separately from GC/effect events
- `collect.strategy` now distinguishes:
  - `mark_sweep`
  - `generational`
  - `bump`
- Bench traces are now capped to a fixed number of printed entries per case so `--trace-effects` stays usable on 1000-iteration runs.
- `--trace-memprof` enables memprof sampling for the bench run and prints only sampled allocation/promotion/reclaim events.
- These checks are zort-native debug surfaces, not attempts to mirror OCaml's
  environment-variable-based debugging contract.
