# Minor/Major GC, Promotion, and Phases

## Source anchors

- `vendor/ocaml/runtime/minor_gc.c`
- `vendor/ocaml/runtime/major_gc.c`
- `vendor/ocaml/runtime/memory.c`
- `vendor/ocaml/runtime/domain.c`
- `vendor/ocaml/runtime/caml/mlvalues.h`

## High-level shape

- OCaml uses a generational collector:
  - per-domain minor heap for small allocations
  - shared major heap for promoted and directly-major blocks
- Collection is domain-aware and stop-the-world coordination is part of the runtime contract.

## Minor heap behavior

- Small allocations reserve space by decrementing `young_ptr`.
- Minor collection runs when the young limit is reached or a forced minor GC is requested.
- Young objects are “oldified” into the shared heap.
- Promotion preserves sharing:
  - forwarded objects receive forwarding pointers
  - other domains may observe in-progress updates and spin until forwarding is complete
- `Forward_tag`, `Infix_tag`, and `No_scan_tag` adjacency is relied upon by minor-GC logic.
- `Cont_tag` is also special:
  - oldifying a continuation copies the block and then scans the suspended stack it references
  - suspended computations are therefore promoted by dedicated stack scanning, not by ordinary field traversal

## Major heap behavior

- Major allocations go through `caml_shared_try_alloc`.
- Allocation updates major allocation counters and may request a major slice.
- The major collector is phased, not a simple one-shot sweep.
- Major marking/verification/compaction special-case `Cont_tag` and follow suspended stack roots through `caml_darken_cont` and stack scanning.

## Major GC phase machine

- The major GC exposes these observable phases:
  - `Phase_sweep_main`
  - `Phase_sweep_and_mark_main`
  - `Phase_mark_final`
  - `Phase_sweep_ephe`
- Finalizers and ephemerons are integrated into phase transitions, not bolted on afterward.
- Per-domain mark/sweep/finalizer counters coordinate completion across domains.

## Tuning knobs

- `caml_percent_free` and `caml_small_heap_limit` control major-GC pacing.
- `caml_mark_stack_prune_factor` limits mark-stack growth relative to heap size.
- `caml_adjust_gc_speed` and dependent-resource accounting can request more collection work.
- `OCAMLRUNPARAM` influences heap sizes, percent free, verification, and verbosity.

## Urgent work and pending slices

- `caml_check_urgent_gc` gives the GC a chance to run after large/direct allocations.
- `caml_request_major_slice(1)` requests global major work.
- `caml_request_major_slice(0)` requests domain-local major work.
- Beginning a mark phase may force a minor collection first, so that minor-to-major references are not left unaccounted for.

## Domain interaction

- Stop-the-world sections are used for:
  - minor GC
  - major GC phase changes
- Domains participate through explicit registration and barrier coordination.
- Blocking sections hand responsibility to backup threads so STW requests still get serviced.

## Debug and verification behavior

- The debug runtime fills fresh major/minor scanned fields with canary values.
- `OCAMLRUNPARAM=V=1` enables extra heap verification during major cycles.
- Verbose GC logging is controlled by `OCAMLRUNPARAM=v=...`.

## zort takeaways

- The meaningful OCaml reference is not “mark-sweep” but “domain-local nursery + shared major heap + STW-coordinated phases”.
- If zort experiments with simpler collectors, the explicit questions are:
  - does promotion preserve sharing?
  - are forwarding states observable?
  - are finalizers and ephemerons phase-coupled?
  - how do blocking threads still participate in global coordination?
