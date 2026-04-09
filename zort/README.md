# zort

`zort` is an experimental Zig-native runtime prototype for play-and-test work.
It is deliberately focused on native execution, maintainability, and
observability, not full OCaml runtime compatibility.

## Phase 1 scope

- Native-only allocation model
- Semantic immediate/block `Value` representation with stable heap identity
- Primitive heap objects (tuples, boxed int64, boxed double, string blocks)
- Baseline mark-sweep GC plus a nursery/major generational baseline
- Explicit weak refs, ephemerons, and finalizer queues on top of collector phase hooks
- Sampled memprof lifecycle tracking for allocation / promotion / reclaim events
- Managed fiber stacks with one-shot continuation capture/resume and `reperform`
- Explicit event sink, trace recorder, and GC/control instrumentation
- Runtime services for named values, signal handlers, pending signals, and blocking-section state
- Small optional compatibility shim (`api.zig`) for legacy `caml_*` entrypoints

## Current representation

- `Value` is a tagged immediate when `(value.raw & 1) == 1`.
- Non-immediate values are semantic block references (`HeapRef`) resolved
  through `HeapStore`.
- Internal object kinds are semantic (`tuple`, `string`, `boxed_i64`,
  `boxed_f64`, `custom`) and are not centered on OCaml tag numbers.
- Strings and bytes are allocated with an explicit trailing NUL sentinel.
- OCaml-shaped compatibility encoding stays at the boundary in `compat.zig`.

## API entrypoints

- Main library surface: `src/lib.zig`
- Optional compatibility layer: `src/api.zig`

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

Register the exact `Value` slot by pointer while it is live:

```zig
var root = try rt.allocI64(123);
try rt.registerRoot(&root);
defer rt.unregisterRoot(&root);

// mutate the root slot as needed while keeping a stable pointer
root = try rt.allocString("updated while rooted");

rt.collect();
```

`collect()` only follows currently registered `root` slots, so unregistering is required
for values that should become unreachable.

## Debugging and profiling

- `Runtime.Config.debugChecks` enables post-mutation / post-collection checks for:
  - root validity
  - heap-store invariants
  - control-kernel state
- `Runtime.explainValue(value, trace)` reports:
  - heap handle
  - heap space (`nursery` / `major`)
  - object kind
  - payload size
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
- `ManagedLiveness` keeps GC-phase-dependent behavior explicit:
  - weak refs
  - ephemerons
  - first/last finalizers
- `MemprofState` keeps sampled lifecycle profiling explicit:
  - probabilistic word-based sampling by default
  - deterministic interval sampling as an explicit test/debug mode
  - optional allocation-site backtrace capture
  - promotion and reclaim lifecycle tracking by `HeapRef`
- `Mutator` now exposes remembered-set recording through barrier events.
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

## Running

- `cd zort && zig build test`
- `cd zort && zig build test -Dcompat-shim=false`
- `cd zort && zig build compat`
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
- Harden the new per-domain scheduler with parked-fiber ownership, work stealing, and cross-domain migration policy.
- Replace the current single-threaded STW coordinator with real parallel safepoint handshakes.
- Make parked fibers and suspended continuations explicit domain-owned root providers for GC.
- Extend the generational baseline toward a truer nursery/major collector under the new domain/STW control surface.
- Decide how much of weak/finalizer/ephemeron behavior should become heap-visible language surface versus stay runtime-managed.
- Use `zig build test` to run the full test suite (`zig build` does not run tests by default).
