# Allocation, Heap Layout, and External Memory

## Source anchors

- `vendor/ocaml/runtime/alloc.c`
- `vendor/ocaml/runtime/memory.c`
- `vendor/ocaml/runtime/caml/memory.h`
- `vendor/ocaml/runtime/str.c`

## Observable behavior

- `caml_alloc(wosize, tag)` chooses between:
  - minor-heap allocation for `wosize <= Max_young_wosize`
  - shared-major allocation via `caml_alloc_shr` otherwise
- `caml_alloc(0, tag)` returns `Atom(tag)` instead of allocating.
- For scanned blocks (`tag < No_scan_tag`), fields are initialized to `Val_unit`.
- For raw blocks (`tag >= No_scan_tag`), payload is generally left unspecified unless the constructor adds stronger guarantees.

## Constructor-specific allocation rules

- `caml_alloc_tuple(n)` is just `caml_alloc(n, 0)`.
- `caml_alloc_1` through `caml_alloc_9` preserve arguments across a possible GC before writing fields.
- `caml_alloc_array(f, arr)` computes each element separately and uses `caml_modify` so evaluation order and barriers stay correct across GC.
- `caml_alloc_string(len)`:
  - allocates enough whole words to store `len` bytes plus a terminator/padding area
  - zeroes the final word
  - stores the trailing padding count in the last byte
- `caml_alloc_initialized_string` copies bytes after allocating the string shell.
- `caml_alloc_float_array` and floatarray constructors use `Double_array_tag` when `FLAT_FLOAT_ARRAY` is enabled.

## Major-only / special allocations

- `caml_alloc_shr` allocates directly in the shared major heap and may raise `Out_of_memory`.
- `caml_alloc_shr_noexc` returns `NULL` instead of raising.
- `caml_alloc_shr_check_gc` exists for native-code large scanned allocations and requires callers to finish closure setup before any GC can occur.
- `caml_alloc_final` is a convenience layer over custom blocks with finalizers.

## Initialization and publication rules

- `caml_initialize` is for first stores into a freshly allocated field, especially in major blocks.
- `caml_initialize` never triggers a GC.
- `caml_modify` is for later mutation and performs remembered-set / marking work plus the memory-model fence needed by multicore OCaml.
- Plain stores are only correct when the caller can prove the value is not young or the field is not part of the GC-visible heap protocol.

## External and dependent memory

- The runtime exposes `caml_stat_*` allocation functions for non-moving C-managed storage.
- With the internal pool enabled, those allocations come from a runtime-owned pool; otherwise they degrade to `malloc`-style behavior.
- The pool API is not interchangeable with arbitrary system allocators.
- `caml_alloc_dependent_memory` and `caml_free_dependent_memory` inform the GC about out-of-heap memory whose lifetime depends on heap objects/finalizers.
- `caml_adjust_gc_speed` and `caml_adjust_minor_gc_speed` speed up collection when non-heap resource usage grows.

## zort takeaways

- OCaml’s allocation API is not just “malloc plus trace”; it encodes publication, barrier, and GC-trigger rules.
- A maintainable zort runtime should separate:
  - block construction
  - initial field initialization
  - mutating writes
  - external resource accounting
- If zort keeps only one allocation path, it should still model the semantic difference between first-store and post-publication mutation.

## zort HeapStore notes (Loop 2)

- `Runtime` delegates object storage to `HeapStore` (`zort/src/heap_store.zig`) instead of tracking allocation state inline.
- `HeapStore` owns:
  - slot-based object storage,
  - index/generation handles (`HeapRef`),
  - free-slot tracking,
  - reclamation semantics.
- Reclaim increments slot generation and records slot index for reuse.
- Reuse order is deterministic (last reclaimed slot first, LIFO).
- Fixed-arena collection path reuses slots without deallocating payload buffers (`collectBump` path only).

## zort Mutator notes (Loop 3)

- Allocation and GC-relevant writes now flow through `zort/src/mutator.zig`.
- `Mutator` owns:
  - typed tuple/string/boxed allocation entrypoints,
  - compatibility-tag allocation dispatch,
  - tuple field initialization,
  - tuple field mutation,
  - string fill and bytes writes.
- The distinction between initialize-vs-mutate is represented explicitly in the mutator write path, even though both phases currently share the same low-level behavior.
- `Runtime` delegates allocation and mutation to `Mutator` instead of mutating heap objects inline.
