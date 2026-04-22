# zort

`zort` is an experimental Zig-native runtime prototype for play-and-test work.
It is deliberately focused on native execution, maintainability, and
observability, not full OCaml runtime compatibility.

Today it is closest to a runtime research project for control effects,
observability, and GC experimentation. The `src/caml_compat/*` tree is an
OCaml-shaped interop shim, not a claim of raw OCaml ABI compatibility.

## Phase 1 scope

- Native-only allocation model
- Semantic immediate/block `Value` representation with stable heap identity
- Primitive heap objects (tuples, boxed int64, boxed double, string blocks)
- Baseline mark-sweep GC plus a nursery/major generational baseline
- Explicit weak refs, ephemerons, and finalizer queues on top of collector phase hooks
- Sampled memprof lifecycle tracking for allocation / promotion / reclaim events
- Managed fiber stacks with one-shot continuation capture/resume, `reperform`, explicit managed-stack growth policy, and deep-copy continuation stack snapshots for inspection
- Explicit event sink, trace recorder, and GC/control instrumentation
- Runtime services for named values, signal handlers, pending signals, blocking-section state, and owned alternate signal-stack lifecycle
- Small optional OCaml-shaped interop shim (`src/caml_compat/api.zig`) for legacy `caml_*` entrypoints
- Compile-time platform capabilities plus runtime permissions for Deno-style host access narrowing

## Current representation

- `Value` is a semantic tagged union with immediate and block cases.
- Block values are semantic heap references (`HeapRef`) resolved through
  `HeapStore`.
- `HeapStore` now exposes an explicit backend kind. Today the only real backend
  is `slot_registry`, which is the current debug-friendly object registry.
- Internal object kinds are semantic (`tuple`, `string`, `boxed_i64`,
  `boxed_f64`, `custom`) and are not centered on OCaml tag numbers.
- Strings and bytes are allocated with an explicit trailing NUL sentinel.
- Heap payloads now carry explicit storage ownership metadata (`host_allocator`,
  `static`, and future page-backed owners) instead of assuming every slice came
  from the host allocator.
- Small nursery tuples now allocate their field arrays from pinned page-backed
  storage, and promotion keeps those tuple payload addresses stable by changing
  ownership metadata instead of copying the fields.
- Allocation accounting is split into:
  - `payload_bytes`
  - `storage_bytes`
  - `scan_words`
  - `allocation_cost_units`
- Collector-facing heap traversal now goes through `HeapStore` callback methods
  instead of exposing the slot array directly. That is the seam future paged or
  nursery backends should grow behind.
- OCaml-shaped interop encoding stays at the boundary in `src/caml_compat/codec.zig`.

## API entrypoints

- Main library surface: `src/lib.zig`
- Separate OCaml-shaped interop boundary: `src/caml_compat.zig`
- Optional shim entrypoints: `src/caml_compat/api.zig`
- External primitive dispatch now goes through `PrimitiveRegistry.callWithBoundary(...)`, so shim-driven primitive calls use the same callback-boundary isolation as pending signal/finalizer delivery.
- Mutable effect/fiber control-state setup now goes through `Runtime` helpers such as `pushEffectHandler`, `pushFiberFrame`, `pushFiberFrameRoot`, and `enterCallbackBoundary`.
- `Runtime.controlKernel()` is now the read-only inspection seam for control state rather than the default mutation path.

## Portability model

zort now treats host-facing behavior as the intersection of:

- compile-time target capabilities,
- compile-time build capability reductions,
- runtime permissions.

The effective rule is:

`effective_access = TargetCaps ∩ BuildCaps ∩ RuntimePermissions`

In practice:

- unsupported target features should not compile into the binary,
- `build.zig` flags like `-Ddisable-threads` or `-Ddisable-posix-signals` can intentionally remove support from a capable target,
- `Runtime.Config.permissions` can narrow host access at runtime with Deno-like flags such as:
  - `allow_read`
  - `allow_write`
  - `allow_net`
  - `allow_env`
  - `allow_run`
  - `allow_ffi`
  - `allow_hrtime`

The important rule is that runtime permissions never widen compile-time
capabilities.

### Capability examples

- A `wasm32-wasi` build should compile without native signal ingress, alternate
  signal stacks, native plugin loading, or host-thread domain workers even if
  userland later asks for broad permissions.
- A macOS or Linux build may compile those capabilities in, but
  `-Ddisable-posix-signals` and `-Ddisable-native-plugin-loading` should remove
  them from that build profile entirely.
- `Runtime.Config.permissions = .{ .allow_all = true }` should only enable the
  host access that the compiled build already supports.

### Host access configuration

`Runtime` now exposes the three layers directly:

- `platformCaps()`
- `permissions()`
- `hostAccess()`

Example:

```zig
var rt = Runtime.initWithConfig(std.heap.page_allocator, .{
    .permissions = .{
        .allow_read = true,
        .allow_write = true,
        .allow_hrtime = true,
    },
});
defer rt.deinit();

const compiled = rt.platformCaps();
const requested = rt.permissions();
const access = rt.hostAccess();

_ = compiled;
_ = requested;
_ = access;
```

## Typical constructors

Use these helpers for explicit value construction:
- `tuple(values: []const Value)` creates a tuple and fills all fields.
- `allocString(bytes: []const u8)` creates a full string value from bytes.
- `allocI64`, `allocI32`, and `allocF64` are the canonical boxed numeric constructors.
- Legacy aliases remain available as `allocInt64`, `allocInt32`, and `allocDouble`.

Example:

```zig
var rt = Runtime.init(std.heap.page_allocator);
defer rt.deinit();

const left = try rt.allocI64(7);
const text = try rt.allocString("hello");
const pair = try rt.tuple(&.{ left, text });
```

## Safe root usage

Prefer lexical root frames in ordinary runtime code:

```zig
var frame = rt.beginRootFrame();
defer frame.end();

var root = try frame.bind(try rt.allocI64(123));
root.set(try rt.allocString("updated while rooted"));

rt.collect();
```

Use `beginRootFrame()` for normal runtime code. `registerInteropRoot(&slot)` /
`unregisterInteropRoot(&slot)` / `scopedInteropRoot(&slot)` are the low-level
interop escape hatches when you already own a stable `Value` slot address.
`registerRoot` / `unregisterRoot` remain compatibility aliases for that same
escape hatch.

## Debugging and profiling

- `Runtime.Config.debugChecks` enables post-mutation / post-collection checks for:
  - root validity
  - heap-store invariants
  - control-kernel state
- `Runtime.explainValue(value, trace)` reports:
  - heap handle
  - heap space (`nursery` / `major`)
  - object kind
  - `payload_bytes`
  - `storage_bytes`
  - `scan_words`
  - `allocation_cost_units`
  - explicit root ownership count
  - control-kernel ownership count
  - runtime-service ownership count
  - managed-liveness ownership count
  - optional live memprof sample metadata
  - last recorded object event when a `TraceRecorder` is present
- `TraceRecorder` captures:
  - per-case counters
  - optional event traces
  - last GC snapshot
  - last root-provider counts
  - last object event per heap object
- `RuntimeServices` keeps non-semantic runtime state explicit:
  - runtime-local named values
  - runtime-local signal handlers
  - pending signal bitset
  - blocking-section depth
  - process-global signal-ingress ownership claimed by one runtime at a time
  - alternate signal-stack setup / restore / teardown ownership
- `Runtime` now exposes the signal-ingress capability directly:
  - `installSignalIngress` / `uninstallSignalIngress`
  - `enableAlternateSignalStack` / `disableAlternateSignalStack`
  - `signalIngressSnapshot`
  - `raiseSignal` for local ingress testing
- `ManagedLiveness` keeps GC-phase-dependent behavior explicit:
  - weak refs
  - ephemerons
  - first/last finalizers
- `MemprofState` keeps sampled lifecycle profiling explicit:
  - probabilistic allocation-unit sampling by default
  - deterministic interval sampling as an explicit test/debug mode
  - optional allocation-site backtrace capture
  - promotion and reclaim lifecycle tracking by `HeapRef`
- `Mutator` now exposes remembered-target recording through barrier events.
- `Runtime.snapshotContinuationStack(...)` returns a deep copy of a suspended continuation stack so effects/backtraces can inspect captured managed-stack state without resuming it.
- `FiberScheduler` now exposes per-domain coordination snapshots with:
  - atomic runnable/parked/suspended counters
  - an atomic current-fiber mirror
  - an atomic wake-request flag for future cross-domain scheduling
  - and an atomic owner token for claimed lane mutation rights
- `DomainRegistry` now tracks worker lifecycle explicitly:
  - attached vs detached domain state
  - worker state (`stopped`, `running`, `stopping`)
  - worker owner token
  - shutdown-request state
- `StopTheWorldCoordinator` now exposes coordination snapshots with atomic:
  - active state
  - generation
  - target paused-domain count
  - paused-domain count
  - pause/resume epochs
  - and initiator-domain mirrors
- `Runtime.requestStopTheWorld()` now starts a request/ack/resume handshake:
  - the initiator acknowledges its own safepoint immediately
  - other domains acknowledge through `enterSafepoint(...)`
  - collection uses that same path to quiesce attached domains instead of directly marking them paused
- `Runtime` now bootstraps the main domain worker explicitly and exposes worker lifecycle APIs:
  - `startDomainWorker`
  - `requestDomainWorkerShutdown`
  - `finishDomainWorkerShutdown`
  - worker shutdown only completes once the scheduler lane is quiescent
- Mutable scheduler paths now require an active claimed lane owner:
  - enqueue, activate, suspend, switch, yield, park, and unpark all run through a claimed owner token
  - attached domains without a running worker cannot mutate lane state
- `Runtime` now exposes explicit multicore fiber capabilities:
  - `transferRunnableFiber` moves a runnable fiber to another running domain without choosing balancing policy
  - fibers remain migratable by default across runnable transfer and continuation resume
  - userland can implement domain-affine placement policy by choosing when not to call transfer
- Bench trace modes:
  - `--trace` prints all recorded events
  - `--trace-gc` prints GC-focused events only
  - `--trace-effects` prints control-kernel events only
  - `--trace-memprof` enables memprof sampling for the run and prints sampled lifecycle events
  - `--profile-json=<path>` writes per-case counters and GC snapshots as JSON
  - trace output is capped per case so focused 1000-iteration runs stay readable

Loop and rollout notes are in [`LOOP.md`](./LOOP.md).
Behavior notes and compatibility references for OCaml comparison are in [`spec/`](./spec/).
Target runtime architecture notes are in [`ARCHITECTURE.md`](./ARCHITECTURE.md).
Standalone runtime-linked smoke programs live in [`e2e/`](./e2e/README.md).

## Running

- `cd zort && zig build test`
- `cd zort && zig build e2e`
- `cd zort && zig build test -Dcompat-shim=false`
- `cd zort && zig build test -Ddisable-posix-signals -Ddisable-native-plugin-loading`
- `cd zort && zig build compat`
- `cd zort && zig build -Dtarget=wasm32-wasi`
- `cd zort && zig build -Dtarget=x86_64-windows-gnu`
- `cd zort && zig build bench`
- `cd zort && zig build bench -- --iters 1000`
- `cd zort && zig build bench -- --iters 1000 --gc-strategy=bump`
- `cd zort && zig build bench -- --iters 1000 --gc-strategy=generational`
- `cd zort && zig build bench -- --iters 1000 --gc-strategy=both`
- `cd zort && zig build bench -- --filter=string`
- `cd zort && zig build bench -- --filter=string --csv=notes/benchmarks.csv`
- `cd zort && zig build bench -- --filter=root-churn --trace-gc`
- `cd zort && zig build bench -- --filter=effect-roundtrip --trace-effects`
- `cd zort && zig build bench -- --filter=gc-chain-unrooted --trace-memprof`
- `cd zort && zig build bench -- --filter=root-churn --profile-json=notes/bench-profile.json`
- `cd zort && zig build bench -- --filter=alloc-pressure-small`
- `cd zort && zig build bench -- --iters 1000 --filter=root-churn --gc-strategy=both`
- `cd zort && zig build bench -- --iters 1000 --filter=long-lived-sweep`
- `--filter=<substring>` runs only matching benchmark labels (for example `tuple`, `string`, `gc`).
- `--gc-strategy=<mark-sweep|mark_sweep|generational|bump|both>` selects collection mode (default: `mark-sweep`).
  - `--gc-strategy=both` runs the full suite once per strategy and prints separate strategy labels.
- `--trace-gc` prints collection start/end, root-provider counts, reclaim events, and GC snapshots.
- `--trace-gc` also prints explicit collector phases, promotion counts, live nursery/major usage, and weak/finalizer hook counts.
- `--trace-effects` prints continuation/fiber events only.
- `--trace-memprof` enables memprof sampling and prints sampled allocation/promotion/reclaim events only.
- `--trace` prints all recorded events for the selected cases.
- `--profile-json=<path>` writes a JSON summary with counters, root providers, and the last GC snapshot per case.

For benchmark snapshots, capture rows as CSV with columns:
`timestamp,iterations,label,strategy,ns_per_op,sink,notes`.

## What is next

- Keep shrinking `runtime.zig` toward orchestration-only code.
- Decide which native boundary services belong in zort core versus the outer shim.
- Harden the new per-domain scheduler with stronger transfer invariants, transfer observability, and clearer userland-facing placement hooks.
- Drive the new STW request/ack protocol from real worker threads and explicit domain lifecycle management.
- Use the new scheduler/STW atomic coordination state to drive parallel pause/ack/resume and cross-domain wakeup behavior.
- Keep zort at the capability layer: domain workers and runnable transfer in the runtime, balancing policy in userland.
- Keep every live fiber under explicit scheduler ownership and keep continuation payloads/root snapshots separate from fiber-lane ownership.
- Extend the generational baseline toward a truer nursery/major collector under the new domain/STW control surface.
- Decide how much of weak/finalizer/ephemeron behavior should become heap-visible language surface versus stay runtime-managed.
- Use `zig build test` to run the full test suite (`zig build` does not run tests by default).
