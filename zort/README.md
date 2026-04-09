# zort

`zort` is an experimental Zig-native runtime prototype for play-and-test work.
It is deliberately focused on native execution and maintainability, not OCaml compatibility.

## Phase 1 scope

- Native-only allocation model
- Immediate/block `Value` representation
- Primitive heap objects (tuples, boxed int64, boxed double, string blocks)
- Manual mark-sweep GC with explicit roots
- Small optional compatibility shim (`api.zig`) for legacy `caml_*` entrypoints

## Current representation

- `Value` is a tagged immediate when `(value.raw & 1) == 1`.
- Non-immediate values are raw pointers to `Object` allocations.
- Strings are allocated with a trailing NUL byte at `bytes[wosize]`.

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
- `cd zort && zig build bench -- --iters 1000 --gc-strategy=both`
- `cd zort && zig build bench -- --filter=string`
- `cd zort && zig build bench -- --filter=string --csv=notes/benchmarks.csv`
- `cd zort && zig build bench -- --filter=alloc-pressure-small`
- `cd zort && zig build bench -- --iters 1000 --filter=root-churn --gc-strategy=both`
- `cd zort && zig build bench -- --iters 1000 --filter=long-lived-sweep`
- `--filter=<substring>` runs only matching benchmark labels (for example `tuple`, `string`, `gc`).
- `--gc-strategy=<mark-sweep|bump|mark_sweep|both>` selects collection mode (default: `mark-sweep`).
  - `--gc-strategy=both` runs the full suite once per strategy and prints separate strategy labels.

For benchmark snapshots, capture rows as CSV with columns:
`timestamp,iterations,label,strategy,ns_per_op,sink,notes`.

## What is next

- Add `export`ed C ABI shims once the allocator/GC surface stabilizes.
- Add deterministic benchmark snapshots for mark-sweep vs bump strategy under shared workloads.
- Use `zig build test` to run test suite (`zig build` does not run tests by default).
- Use `zig build bench -- <iterations>` for an ad-hoc benchmark run.
