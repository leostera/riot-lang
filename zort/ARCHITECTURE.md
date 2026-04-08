# zort Architecture

This document is the reference architecture for `zort`.
It describes the target runtime shape we want to grow into, not the current
flat prototype in `src/`.

The goal is a maintainable Zig-native runtime that is easy to reason about,
easy to test, explicit about ownership and liveness, and capable of supporting
an outer compatibility shim without making OCaml's historical runtime layout the
center of the design.

## Design Principles

- Internal runtime data should be semantic and typed.
- OCaml compatibility should be an outer encoding layer, not the foundation.
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
- Compatibility universe:
  tagged raw words, OCaml-ish tags, named values, primitive lookup,
  legacy `caml_*` entrypoints, and any future FFI shim.

The compatibility universe may depend on the internal universe.
The internal universe must not depend on the compatibility universe.

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
- object slot reuse
- heap iteration for collectors
- object destruction/reclamation hooks

`HeapStore` does not own GC policy.
It is storage plus object lifecycle primitives.

### `RootRegistry`

`RootRegistry` owns:

- explicit root registration
- scoped root handles
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

If the compatibility layer disappears, the internal runtime should still make
sense.

### `EventSink`

Observability should be explicit.

`EventSink` owns:

- alloc/mutate/collect notifications
- effect/fiber lifecycle events
- debug counters
- benchmark hooks

This avoids scattering logging and stats accounting across unrelated code paths.

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
