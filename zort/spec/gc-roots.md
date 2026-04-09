# Roots, Root Scanning, and Write Barriers

## Source anchors

- `vendor/ocaml/runtime/caml/memory.h`
- `vendor/ocaml/runtime/roots.c`
- `vendor/ocaml/runtime/globroots.c`
- `vendor/ocaml/runtime/memory.c`
- `vendor/ocaml/runtime/callback.c`

## Local roots

- C local roots are represented as linked `caml__roots_block` records.
- `caml_do_local_roots` scans:
  - explicit root tables
  - the current OCaml stack
  - GC registers
- Null root slots are skipped.
- This is the runtime side of the `CAMLparam`, `CAMLlocal`, `Begin_roots`, and related macros.
- For effect-enabled stacks, stack scanning is not limited to the current frame chain:
  - `caml_scan_stack` walks the current stack and every parent fiber
  - each stack contributes its frame roots plus `handle_value`, `handle_exn`, and `handle_effect`
- Before C re-enters OCaml code, `caml_maybe_expand_stack` ensures a free `gc_regs` bucket exists so register roots remain representable across nested runtime entries.

## Global roots

- Mutable global roots live in `caml_global_roots`.
- Generational global roots are split across:
  - `caml_global_roots_young`
  - `caml_global_roots_old`
- The invariant is value-dependent:
  - young pointer => must be in `young`
  - major pointer => may be in `old` or `young`
  - immediate / out-of-heap => in neither list
- `caml_modify_generational_global_root` reclassifies roots as the pointed-to value changes.

## Iteration and deletion semantics

- Global-root tables are protected by a mutex.
- If a root is removed while roots are being iterated, it is tombstoned as `ROOT_DELETED` and physically removed during the current or next scan.
- This means deletion during GC/root traversal is intentionally supported.

## Root scanning extension points

- `caml_scan_roots_hook` allows extra root sources to participate in scanning.
- `caml_do_roots` scans:
  - local roots
  - hook-provided roots
  - finalizer roots

## Remembered sets and field mutation

- `caml_modify` is the runtime’s general heap mutation primitive.
- It performs:
  - major-to-minor remembered-set maintenance
  - darkening during concurrent/incremental marking when needed
  - the memory-model fence needed for OCaml’s stronger-than-C11 semantics
  - a release store of the new value
- `caml_initialize` is only for initial field stores into not-yet-finished blocks.
- `caml_initialize` still records major-to-minor references when needed, but it does not run the full mutation protocol and never triggers GC.

## Atomic field operations

- `caml_atomic_load_field`, `exchange`, `cas`, and `fetch_add` provide heap-visible atomic accessors.
- On a single domain, several operations degrade to simpler non-atomic forms.
- On multiple domains, they use real atomics plus fences.
- Successful exchange/CAS routes through the same write-barrier logic as `caml_modify`.

## Callback-specific rooting rule

- C-to-OCaml callbacks preserve closures/arguments across helper allocations only for as long as necessary.
- The runtime explicitly avoids keeping callback arguments rooted longer than OCaml call semantics require.
- When a callback temporarily clears the current stack parent to block effect propagation, it also keeps the saved parent stack alive as a root until the callback returns.

## zort takeaways

- “Root API” is not a single list abstraction in OCaml. It is the combination of:
  - local root frames
  - global mutable roots
  - generational global roots
  - stack/register scanning
  - finalizer/memprof/ephemeron-owned roots
- A zort root API that is easier to reason about is fine, but it still needs to answer:
  - who owns liveness
  - when mutation is legal
  - how major-to-young references are tracked
  - whether deletion during iteration is supported

## zort RootRegistry notes

- Explicit roots now live in `zort/src/root_registry.zig`, not inline in `Runtime`.
- `RootRegistry` owns:
  - root slot storage,
  - generation counters,
  - registration/unregistration bookkeeping,
  - scoped root handles,
  - debug validation through an external validity hook.
- `RootRegistry` also exposes a `RootProvider` view through `zort/src/root_provider.zig`.
- `Runtime` now builds the collector's root set from providers instead of hard-coding explicit-root slices.
- Scoped root handles are an ownership tool only; they do not yet model stack roots, callback roots, or effect-parent roots.
- This is the seam zort will use for suspended fibers, continuations, callback-owned roots, and other non-registry liveness owners.
- Runtime root ownership is now split across explicit control-state providers instead of one opaque `control_kernel` bucket:
  - `fiber_scheduler` visits the active, runnable, parked, and scheduler-suspended fibers currently owned by per-domain scheduler lanes,
  - `suspended_continuations` visits continuation payloads, captured roots, and managed suspended-stack frames,
  - direct unscheduled live fibers are now treated as an ownership bug instead of a collector fallback path.
- This means collection now answers liveness questions more explicitly:
  - domain-owned parked fibers stay collector-visible through the scheduler provider,
  - fibers suspended by effect capture stay scheduler-owned through a dedicated suspended lane while their captured stacks live in `suspended_continuations`,
  - direct low-level control-kernel usage fails ownership verification instead of silently widening the GC root set.
- `RuntimeServices` is now the next built-in provider:
  - named values stay live through service-owned roots,
  - signal handlers stay live through the same provider,
  - signal and blocking-section bookkeeping stays outside the semantic value core.
- `ManagedLiveness` is now the next built-in provider:
  - registered finalizer callbacks stay live while the registration exists,
  - queued ready-finalizer callbacks and first-finalizer arguments stay live until delivery drains them,
  - weak refs and ephemerons do not root their targets/data unconditionally.
- `MemprofState` is intentionally not a root provider in the current baseline:
  - sampled records store site ids and object metadata only,
  - memprof does not keep sampled heap values alive,
  - any future callback-based memprof surface would need an explicit provider of its own.
- `Mutator` now exposes the future generational seam explicitly:
  - major-to-nursery mutation records remembered-set edges in `src/remembered_set.zig`,
  - barrier events are observable through `src/event_sink.zig`,
  - the `generational` collector baseline now consumes remembered edges during minor collection.
