# zort Runtime Loop

Start working immediately, following these directives:

1. Lock one subsystem only from the list below.
2. Set concrete exit criteria before writing code.
3. List exact spec docs and architecture sections that constrain the work.
4. Implement the minimum Zig changes in `src/` to satisfy that one subsystem.
5. Update or add spec notes if behavior assumptions changed.
6. Run the required test and benchmark command for that subsystem.
7. Promote done items; keep unresolved risks in explicit todos.
8. Pick the next subsystem and move forward only when criteria pass.

Stop starting a later subsystem if an earlier one misses its exit criteria.

Commands:

- `zig build test`
- `zig build bench -- --iters 200000`
- `zig build bench -- --iters 200000 --filter=<substring>` for focused benches.

## Loop Objective

Rebuild `zort/src` against [`ARCHITECTURE.md`](./ARCHITECTURE.md) and the behavior notes in [`spec/`](./spec/), native-first only.

Do these things every loop:

- Follow the subsystem order in this file.
- Keep internal code typed and semantic.
- Keep OCaml compatibility at the boundary.
- Stop once a subsystem exits cleanly.

## Runtime Direction

- Build only native runtime behavior. Ignore bytecode behavior.
- Keep internal values semantic with stable heap identity.
- Delay effects until storage, roots, and baseline collector boundaries are correct.
- Move `src` toward `ARCHITECTURE.md`; do not keep enlarging `runtime.zig`.
- Let compatibility layers call into the runtime, never the other way around.

## Reference Contract

- Use [`ARCHITECTURE.md`](./ARCHITECTURE.md) as the implementation target.
- Use [`spec/*.md`](./spec/) as the behavior source of truth.
- If implementation and spec diverge, update one immediately or block the change.

## Commit Rule

- Commit each loop with `--no-verify`.
- Prefix implementation commits with `feat(zort):`.

## Stop Conditions

- Tests pass for the touched subsystem.
- Benchmarks run for the touched subsystem slice.
- The touched code is closer to the target architecture.
- Scope does not expand beyond the selected subsystem.

## Completed Foundation

- [x] Map native OCaml runtime behavior into `spec/*.md`.
- [x] Add effects/continuation coverage and notes.
- [x] Capture target architecture in [`ARCHITECTURE.md`](./ARCHITECTURE.md).
- [x] Build a useful prototype runtime surface and benchmark harness.

## Ordered Subsystem Plan

The order below is mandatory. Do not execute a later subsystem before earlier items pass.

### 1. Semantic Core

Refs:
- [`ARCHITECTURE.md`](./ARCHITECTURE.md)
- [`spec/constructors.md`](./spec/constructors.md)
- [`spec/allocator-policy.md`](./spec/allocator-policy.md)
- [`spec/custom-blocks.md`](./spec/custom-blocks.md)

Tasks:
- [x] Replace the raw-pointer internal model with semantic internal values.
- [x] Introduce stable heap identity (`HeapRef` or equivalent).
- [x] Define typed object kinds independent of OCaml tags.
- [x] Remove internal reliance on tagged-word encoding.
- [x] Define an explicit internal atoms/immediates model.

Exit criteria:
- New code uses semantic values and stable heap ids, not raw pointers.
- Internal behavior can be reasoned about without OCaml tag numbers.

Status:
- Done.
- `Value` is semantic (`immediate` or `block`).
- Heap objects are semantic payload kinds in `heap_store.zig`, with OCaml tags treated as compatibility metadata instead of the internal storage model.

### 2. Heap Store

Refs:
- [`ARCHITECTURE.md`](./ARCHITECTURE.md)
- [`spec/allocator-policy.md`](./spec/allocator-policy.md)
- [`spec/gc-strategy.md`](./spec/gc-strategy.md)

Tasks:
- [x] Create a `HeapStore` subsystem that owns allocation records and object lookup.
- [x] Remove object tracking from monolithic runtime state.
- [x] Implement deterministic slot reuse and reclamation hooks.
- [x] Isolate storage ownership from collection policy.
- [x] Keep allocator policy configurable through explicit ownership.

Exit criteria:
- Heap storage can be tested without collector policy.
- Runtime no longer depends on a coupled "allocator + object list" model.

Status:
- Done.
- `Runtime` delegates heap allocation, lookup, and reclamation to `HeapStore`.
- `HeapStore` has direct subsystem tests for add/get/reclaim/clear behavior.

### 3. Mutator And Write API

Refs:
- [`ARCHITECTURE.md`](./ARCHITECTURE.md)
- [`spec/gc-roots.md`](./spec/gc-roots.md)
- [`spec/allocator-policy.md`](./spec/allocator-policy.md)
- [`spec/string-semantics.md`](./spec/string-semantics.md)

Tasks:
- Introduce a mutator capability/subsystem for allocation and writes.
- Route tuple/string/boxed writes through one mutation path.
- Separate constructor entry points from raw storage mutation.
- Add explicit typed initialization/write APIs for fields and byte buffers.
- Ensure future write barriers and debug checks can only run on this path.

Exit criteria:
- No heap mutation bypasses a single mutator API.
- Allocation and mutation are not implicit runtime internals.

### 4. Root Registry

Refs:
- [`ARCHITECTURE.md`](./ARCHITECTURE.md)
- [`spec/gc-roots.md`](./spec/gc-roots.md)
- [`spec/effects-and-continuations.md`](./spec/effects-and-continuations.md)

Tasks:
- Replace ad hoc root lists with explicit `RootRegistry`.
- Add scoped root handles with clear ownership.
- Keep generation counters and debug verification enabled.
- Distinguish explicit roots from derived root providers.
- Expose root ownership in tests.

Exit criteria:
- Every long-lived live value has a clear owner.
- Root add/remove is explicit, testable, and safe.

### 5. Collector Baseline

Refs:
- [`ARCHITECTURE.md`](./ARCHITECTURE.md)
- [`spec/gc-strategy.md`](./spec/gc-strategy.md)
- [`spec/gc-control-and-stats.md`](./spec/gc-control-and-stats.md)

Tasks:
- Implement mark-sweep against `HeapStore` and `RootRegistry` interfaces.
- Keep mark-sweep as the first correct collector.
- Consume roots from providers instead of hard-coded runtime fields.
- Preserve allocation failure and recovery behavior.
- Make reclamation a heap concern and strategy selection a collector concern.

Exit criteria:
- Mark-sweep runs through clean interfaces.
- Collector policy can switch without storage rewrites.

### 6. Language Surface Semantics

Refs:
- [`spec/string-semantics.md`](./spec/string-semantics.md)
- [`spec/numeric-primitives.md`](./spec/numeric-primitives.md)
- [`spec/constructors.md`](./spec/constructors.md)
- [`spec/exceptions-callbacks-and-backtraces.md`](./spec/exceptions-callbacks-and-backtraces.md)

Tasks:
- Rebuild tuple, string, bytes, boxed int, and boxed float APIs on semantic core.
- Implement locale-independent float parse/format behavior.
- Preserve explicit length and bytes semantics.
- Use typed constructor APIs instead of generic tag allocators.
- Add boundary/error tests for each primitive path.

Exit criteria:
- Public API is semantic and typed.
- String and numeric behavior is backed by spec-linked tests.

### 7. Event Sink, Stats, And Bench Integration

Refs:
- [`ARCHITECTURE.md`](./ARCHITECTURE.md)
- [`spec/benchmark-depth.md`](./spec/benchmark-depth.md)
- [`spec/gc-control-and-stats.md`](./spec/gc-control-and-stats.md)

Tasks:
- Move observability into an event/stats sink subsystem.
- Decouple benchmark hooks from core runtime policy.
- Capture benchmark baselines for touched subsystem runs.
- Add lightweight benchmark governance + CSV append flow in `notes/`.

Exit criteria:
- Runtime can run with observability disabled.
- Bench and stats collection uses explicit hooks, not scattered prints.

### 8. Minimal Compatibility Layer

Refs:
- [`ARCHITECTURE.md`](./ARCHITECTURE.md)
- [`spec/primitive-boundary-and-native-dynlink.md`](./spec/primitive-boundary-and-native-dynlink.md)
- [`spec/constructors.md`](./spec/constructors.md)

Tasks:
- Keep legacy `api.zig` as explicit shim only.
- Add compile-time flag to include/exclude the shim.
- Encode/decode OCaml-compat values at the boundary.
- Introduce pointer-safe handles for any C-facing export.
- Stop compatibility logic from entering semantic core modules.

Exit criteria:
- Core runtime builds with shim disabled.
- Dependency direction is one-way: shim -> core.

### 9. Control Kernel: Effects, Fibers, Continuations

Refs:
- [`ARCHITECTURE.md`](./ARCHITECTURE.md)
- [`spec/effects-and-continuations.md`](./spec/effects-and-continuations.md)
- [`spec/gc-roots.md`](./spec/gc-roots.md)
- [`spec/exceptions-callbacks-and-backtraces.md`](./spec/exceptions-callbacks-and-backtraces.md)

Tasks:
- Add dedicated control subsystem for fibers and continuations.
- Implement one-shot typed continuation handles.
- Make suspended stack state an explicit root provider.
- Model handler stack and parent links with testable invariants.
- Add tests for unhandled effects and already-resumed continuations.

Exit criteria:
- Effects are isolated as control flow, not GC/exceptions.
- Suspended stack liveness is explicit and verified.

### 10. Native Boundary Services

Refs:
- [`spec/primitive-boundary-and-native-dynlink.md`](./spec/primitive-boundary-and-native-dynlink.md)
- [`spec/signals-and-stack-overflow.md`](./spec/signals-and-stack-overflow.md)
- [`spec/sync-primitives.md`](./spec/sync-primitives.md)
- [`spec/startup-domains-and-signals.md`](./spec/startup-domains-and-signals.md)

Tasks:
- Add named-values only when boundary requires them.
- Decide and document runtime ownership of primitive table/dynlink.
- Decide and document whether sync primitives stay in core or outer layer.
- Define signal stack and overflow handling per platform.
- Keep native boundary services out of semantic kernel unless required.

Exit criteria:
- Boundary behavior is intentional and spec-backed.
- Core remains understandable without reading boundary layers.

### 11. Extended Semantic Surface

Refs:
- [`spec/comparison-hashing.md`](./spec/comparison-hashing.md)
- [`spec/marshaling-and-code-loading.md`](./spec/marshaling-and-code-loading.md)
- [`spec/weak-finalizers-and-memprof.md`](./spec/weak-finalizers-and-memprof.md)
- [`spec/runtime-hosted-primitives.md`](./spec/runtime-hosted-primitives.md)

Tasks:
- Implement or explicitly defer comparison/hash behavior.
- Implement or explicitly defer marshal/code identity behavior.
- Implement or explicitly defer weak refs, finalizers, ephemerons, memprof hooks.
- Place non-core surfaces in hosted support when appropriate.

Exit criteria:
- Every remaining OCaml surface is implemented or explicitly deferred with rationale.

### 12. Alternate Collectors And Policy Experiments

Refs:
- [`ARCHITECTURE.md`](./ARCHITECTURE.md)
- [`spec/gc-strategy.md`](./spec/gc-strategy.md)
- [`spec/gc-control-and-stats.md`](./spec/gc-control-and-stats.md)

Tasks:
- Keep baseline collector clean before adding experiments.
- Add new collectors behind explicit policy selection.
- Compare strategies through the benchmark command path.
- Preserve semantic core invariants across collectors.

Exit criteria:
- Policy experiments are plug-compatible and do not weaken the core.

## Anti-Goals During Rebuild

- Add behavior to `runtime.zig` for convenience.
- Use raw pointer identity as long-term object identity.
- Build core support for compatibility tags before semantic core is stable.
- Implement effects before heap identity, roots, and baseline GC are stable.
- Let benchmarks drive runtime API shape.

## Loop Outputs

- Update `src/*` with passing tests for each selected subsystem.
- Update matching `spec/*` notes when assumptions change.
- Capture bench snapshots for relevant subsystem changes.
- Keep this file accurate as each task flips to done.
