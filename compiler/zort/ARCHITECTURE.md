# zort Architecture

This document is the reference architecture for `zort`.
It describes the target runtime shape we want to grow into, not the current
flat prototype in `src/`.

The goal is a maintainable Zig-native runtime that is easy to reason about,
easy to test, explicit about ownership and liveness, and capable of supporting
an outer interop shim without making OCaml's historical runtime layout the
center of the design.

Current status note:
`src/caml_compat/*` is a handle-oriented interop boundary, not a claim of raw
OCaml ABI compatibility. The architecture should stay honest about that
distinction until value layout, headers, roots, barriers, and callback
semantics are truly ABI-shaped.

## Design Principles

- Internal runtime data should be semantic and typed.
- OCaml-shaped interop should be an outer encoding layer, not the foundation.
- Liveness should be explicit and owned.
- Allocation, mutation, collection, and control transfer should be separate
  subsystems.
- Raw pointers should not be the long-term public or internal identity model.
- Every runtime mutation with GC implications should pass through one controlled
  path.
- Effects and continuations are a control-flow subsystem, not an exception hack
  and not a GC feature.

## Architecture Boundary

The system should have two universes:

- Internal universe:
  typed values, typed heap objects, stable handles, explicit roots,
  explicit continuations, explicit collectors.
- Interop/shim universe:
  tagged raw words, OCaml-ish tags, named values, primitive lookup,
  legacy `caml_*` entrypoints, and any future FFI shim.

The interop universe may depend on the internal universe.
The internal universe must not depend on the interop universe.

That is the main irreversible architecture rule.

## Core Data Model

The center of the runtime should be a semantic value type, not a raw machine
word:

```zig
const Value = union(enum) {
    int: i63,
    atom: Atom,
    ref: HeapRef,
};
```

`HeapRef` should be a stable object identity, not a raw pointer.
The exact representation can vary:

- slot index
- slot + generation
- packed handle

What matters is that object identity survives future moves, compaction, or
alternate storage backends.

Heap objects should also be semantic:

```zig
const Object = union(enum) {
    tuple: []Value,
    bytes: []u8,
    boxed_i64: i64,
    boxed_f64: f64,
    custom: CustomPayload,
    fiber: FiberState,
    continuation: ContinuationState,
};
```

The runtime may still derive OCaml-like tags at the boundary, but the internal
kernel should work in terms of object kinds, not header-bit numerology.

Current scalar note:
the current `boxed_i64` / `boxed_f64` object kinds are still provisional. If
zort wants first-class unboxed `u8/u16/u32/u64/u128` and corresponding float
widths, the long-term answer is a richer scalar/value representation strategy,
not an ever-growing set of boxed heap cases.

## Runtime Subsystems

The runtime should be built from a small number of explicit subsystems with
non-overlapping responsibilities.

### `Runtime`

`Runtime` is the orchestrator.
It wires together the subsystems, owns global policy, and exposes the ergonomic
API surface.

`Runtime` should not be where allocation, tracing, root management, effect
stack switching, and compatibility encoding all live together.

### `HeapStore`

`HeapStore` owns:

- object allocation
- object lookup by `HeapRef`
- storage-backend selection and backend-neutral traversal hooks
- object slot reuse in the current `slot_registry` backend
- heap iteration for collectors through callback-based traversal/sweep APIs
- explicit payload-storage ownership metadata so host-allocated, static, and
  future page-backed payloads can coexist without hidden free rules
- object destruction/reclamation hooks

`HeapStore` does not own GC policy.
It is storage plus object lifecycle primitives.

Current implementation note:
the only backend today is `slot_registry`. That is still an object registry,
not a packed page heap. New backends should fit behind the `HeapStore`
interface instead of teaching the collector or mutator about backend-specific
arrays or page tables.

Current storage note:
small nursery tuples now use pinned page-backed field storage, and promotion is
currently a non-moving metadata transition for those tuple payloads. That
matches the intended direction better than a copying nursery, but page-local
fragmentation and page-reuse policy are still early.

### `RootRegistry`

`RootRegistry` owns:

- explicit root registration
- scoped root handles
- lexical root frames that own stable root slots
- root generation counters
- debug validation of root operations
- root enumeration for collection

The rest of the runtime should not maintain ad hoc liveness lists.
If something must keep a value alive across collection, it should do so through a
root-owning API.

### `Collector`

`Collector` owns collection policy:

- mark-sweep
- bump/reset
- future copying or generational collectors

The collector should ask other subsystems for roots and trace edges through the
heap store.
It should not own value semantics, effect semantics, or FFI policy.

### `Mutator`

Allocation and heap mutation should happen through a narrow mutator capability.

The mutator owns:

- allocation entrypoints
- field writes
- bytes writes
- barrier calls
- remembered-set updates if needed
- collector handshakes if allocation pressure demands it

The architectural point is simple:
all GC-relevant writes must flow through one layer.

### `ControlKernel`

Effects, fibers, and continuations belong here.

`ControlKernel` owns:

- fiber creation
- continuation capture
- continuation resume/reperform
- handler stacks
- suspended-stack liveness exposure to the collector

The collector should not know effect semantics.
It should only know how to ask `ControlKernel` for additional roots.

### `CompatLayer`

The compatibility layer owns all OCaml-shaped behavior we choose to support:

- raw tagged word encoding/decoding
- tag translation
- named values
- primitive table
- native plugin boundary
- `caml_*` shims

In the current repo this is best understood as an interop shim, not as proven
ABI compatibility.

If the compatibility layer disappears, the internal runtime should still make
sense.

### `HostSubstrate`

The runtime needs one explicit platform boundary below the semantic kernel.

`HostSubstrate` owns:

- domain worker threads
- stop-the-world wake/pause plumbing
- signal ingress
- alternate signal-stack setup/restore
- plugin loading
- clocks and timers
- blocking syscall boundaries

The semantic kernel must not embed target-specific assumptions such as
`std.posix` signal handling or `LoadLibrary` behavior directly.

### `EventSink`

Observability should be explicit.

`EventSink` owns:

- alloc/mutate/collect notifications
- effect/fiber lifecycle events
- debug counters
- benchmark hooks

This avoids scattering logging and stats accounting across unrelated code paths.

## Capability Gating

Cross-platform support should be expressed as a three-layer intersection:

1. `TargetCaps`
2. `BuildCaps`
3. `RuntimePermissions`

### `TargetCaps`

`TargetCaps` are compile-time facts derived from the Zig target.

Examples:

- WASI may have no native signals, no alternate signal stack, and no native
  plugin loading.
- Unix-native targets may support POSIX signals and alternate signal stacks.
- Windows may support threads and plugin loading but require different signal
  and overflow backends.

Unsupported target features should not compile into the binary.

### `BuildCaps`

`BuildCaps` are compile-time reductions chosen by the build.

Examples:

- `-Ddisable-threads`
- `-Ddisable-posix-signals`
- `-Ddisable-native-plugin-loading`

These flags may only subtract capability from the target. They must never
invent support that the target does not have.

### `RuntimePermissions`

`RuntimePermissions` are userland policy.
They should feel like Deno-style access flags:

- `allow-read`
- `allow-write`
- `allow-net`
- `allow-env`
- `allow-run`
- `allow-ffi`
- `allow-hrtime`

Permissions only narrow what an already-supported build may do.
They must never widen a compile-time capability boundary.

### Effective Host Access

The runtime should behave as:

```zig
effective_access = TargetCaps ∩ BuildCaps ∩ RuntimePermissions
```

That gives us the intended behavior:

- unsupported subsystems do not compile into a target build
- product/distribution builds can intentionally reduce capability
- userland can still choose a stricter runtime policy at startup

This split is what keeps zort portable without turning the semantic runtime into
a pile of platform checks.

The intended implementation shape is compile-time backend selection first,
runtime permission checks second:

```zig
const target_caps = PlatformCaps.target();
const build_caps = BuildCaps.fromRoot();
const compiled_caps = target_caps.applyBuildCaps(build_caps);

const signal_backend = if (compiled_caps.posix_signals)
    @import("host/signals_posix.zig")
else
    @import("host/signals_none.zig");
```

That means a target like WASI should not carry POSIX signal code in the binary
at all, while a Unix-native build can still compile the backend in and later
disable access through runtime permissions.

## Data Flow

The runtime should have a small number of explicit flows.

### Allocation Flow

1. Caller gets a `Mutator` view from `Runtime`.
2. Caller requests allocation through semantic constructors.
3. `Mutator` requests storage from `HeapStore`.
4. `HeapStore` returns a stable `HeapRef`.
5. `Mutator` wraps the result as `Value.ref`.
6. `EventSink` observes the allocation.
7. Allocation-pressure logic may request a collection through `Collector`.

The constructor should not manually append to global object arrays or directly
manipulate root lists.

### Mutation Flow

1. Caller invokes a typed mutation API.
2. `Mutator` validates object kind and field bounds.
3. `Mutator` performs the write through one mutation path.
4. Barrier / remembered-set / debug verification runs there if needed.
5. `EventSink` records the mutation if enabled.

All heap writes with GC implications should use this path.
There should be no "safe enough direct store" convention spreading through the
runtime.

### Root / Liveness Flow

1. Long-lived values are pinned by `RootRegistry`.
2. Scoped roots return `RootHandle`s.
3. Collection starts by asking for roots from:
   - `RootRegistry`
   - `ControlKernel`
   - compatibility/FFI global roots if enabled
4. The collector traces through `HeapStore`.

The central liveness question should always be answerable:
who owns the fact that this value must stay alive?

### Collection Flow

1. `Runtime` triggers collection explicitly or due to pressure.
2. `Collector` gathers roots from registered providers.
3. `Collector` traces through `HeapStore`.
4. `Collector` decides survivors/reclamation according to its strategy.
5. `HeapStore` reclaims dead objects.
6. `EventSink` records stats and lifecycle events.

This lets us change collector strategy without rewriting value semantics or
effect semantics.

### Effect / Continuation Flow

1. `perform` enters `ControlKernel`.
2. The current fiber stack is captured into a one-shot continuation handle.
3. Parent handlers are consulted by `ControlKernel`.
4. `resume` consumes a continuation handle exactly once.
5. Suspended computations expose their roots through `ControlKernel`.
6. The collector treats them as additional root providers.

That keeps control transfer and tracing integrated, but not tangled.

### Compatibility / FFI Flow

1. Boundary code receives raw OCaml-like values or C ABI inputs.
2. `CompatLayer` decodes them into internal semantic values/handles.
3. Internal subsystems operate only on semantic forms.
4. Results are encoded back at the boundary.

The internal runtime should never have to care whether a caller came from Zig,
generated OCaml native code, or a test harness.

## Ownership Rules

These rules should stay true even as the implementation changes.

- Only `HeapStore` owns heap slots and object reclamation.
- Only `Collector` decides when unreachable storage dies.
- Only `RootRegistry` owns explicit roots.
- Only `ControlKernel` owns continuation linearity and suspended-stack state.
- Only `CompatLayer` knows about raw tagged-word compatibility.
- Only `Mutator` performs GC-relevant writes.

If a feature does not fit one of those ownership buckets, it probably has the
wrong abstraction boundary.

## Important Invariants

- Internal values are semantic; compatibility encoding is edge-only.
- `HeapRef` remains stable even if object storage strategy changes.
- Continuations are one-shot unless a dedicated cloning API exists.
- All collector-visible edges are discoverable through explicit tracing APIs.
- Root registration is explicit, scoped, and verifiable.
- Heap mutation is centralized.
- Collector strategy is replaceable without changing the public semantic API.

## Architecture Consequences

This architecture implies a few design choices that are worth making explicit.

### Do not center raw pointers

The current pointer-as-value prototype is useful for early iteration, but it is
not the right long-term architecture.

If the runtime grows around raw pointers:

- moving GC becomes painful
- effect stacks become harder to reason about
- aliasing bugs get easier
- future backends become harder

### Do not center OCaml tag numbers

OCaml tags matter for compatibility work, but they should not define the core
type system of the runtime.

Internal code should ask:

- is this a tuple?
- is this bytes?
- is this a continuation?

not:

- is this tag `245`?
- is this tag `252`?

### Do not let `Runtime` become the God object

`Runtime` should compose capabilities.
It should not become a single mutable bag implementing allocation, GC,
compatibility, roots, effects, and stats inline.

## Migration Direction From The Current Prototype

The current prototype is still flat and pointer-centric.
The intended migration direction is:

1. Separate storage from policy.
2. Introduce stable heap references.
3. Extract explicit root ownership.
4. Move collection strategies behind a collector interface.
5. Add a dedicated control kernel for effects/fibers.
6. Push all OCaml-shaped encoding and shim behavior to the edge.

The target is not "many files".
The target is a runtime where data flow, ownership, and mutation boundaries are
obvious.

## Non-Goals

- Preserving OCaml runtime internals as the center of the design.
- Building the entire architecture around the legacy `caml_*` API.
- Treating compatibility tags, primitive indexes, or named values as internal
  truth.
- Letting benchmark/debug hooks leak into every subsystem.

## Short Reference

When in doubt, use this model:

- semantic values in the core
- stable handles for heap identity
- explicit roots for liveness
- collector as policy, heap as storage
- effects as control kernel
- compatibility at the boundary

If a change violates that model, it should be treated as suspect until there is
a clear reason to do otherwise.
