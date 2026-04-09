# Observability, Debug Controls, and Runtime Events

This filename is retained because zort already points at it, but the content now
documents OCaml runtime observability surfaces rather than zort-only benchmark notes.

## Source anchors

- `vendor/ocaml/runtime/HACKING.adoc`
- `vendor/ocaml/runtime/startup_aux.c`
- `vendor/ocaml/runtime/backtrace.c`
- `vendor/ocaml/runtime/runtime_events.c`
- `vendor/ocaml/runtime/sys.c`

## `OCAMLRUNPARAM` / `CAMLRUNPARAM`

- The runtime reads `OCAMLRUNPARAM`, falling back to `CAMLRUNPARAM`.
- Parsed controls include:
  - `b`: backtrace enablement
  - `c`: cleanup on exit
  - `d`: max domains
  - `e`: runtime event log size
  - `l`: max stack size
  - `M`, `m`, `n`: custom block GC ratios / thresholds
  - `o`: percent free
  - `p`: parser trace
  - `s`: minor heap size
  - `t`: trace level
  - `v`: GC verbosity mask
  - `V`: heap verification
  - `W`: runtime warnings
  - `X...`: GC tweaks
- Invalid domain counts are fatal at startup.

## Debug runtime

- The debug runtime enables extra assertions and can materially change timing/behavior.
- `HACKING.adoc` recommends running debug builds explicitly when investigating GC or heap issues.
- `OCAMLRUNPARAM=V=1` enables extra heap sanity checks during major GC.
- `OCAMLRUNPARAM=v=0xffffffff` enables all documented GC logging classes.

## Backtraces

- Backtrace recording is runtime state, not purely a library convention.
- Turning it on resets the stored exception/backtrace state.
- Raw backtraces can be copied out and restored later.
- Uncaught exception printing uses backtrace state if active and debug info exists.

## Runtime events

- Runtime events are emitted to per-domain ring buffers in a memory-mapped `<pid>.events` file.
- The file location can be changed with `OCAML_RUNTIME_EVENTS_DIR`.
- `OCAML_RUNTIME_EVENTS_START` eagerly enables the producer.
- `OCAML_RUNTIME_EVENTS_PRESERVE` leaves the ring file in place on exit.
- Start/stop of the producer uses stop-the-world coordination when multiple domains may be running.
- Pause/resume is a separate runtime state from enabled/disabled.
- After `fork`, the child tears down inherited producer state without deleting the parent's file, then can start its own ring.
- The current process can query the active ring path as an allocated string when events are enabled.
- The ring buffer is a flight recorder:
  - old events are overwritten
  - consumers race with producers and must detect torn reads
  - the file layout scales with the maximum concurrent domains ever seen
- User-defined events are registered into a fixed custom-event table:
  - names must be C-safe
  - names have a maximum length
  - the runtime enforces a maximum number of custom events

## Exit-time observability

- With GC stats verbosity enabled, exit prints cumulative allocation/heap counters.
- Runtime events are torn down before process exit.
- Shutdown hooks may run before fatal uncaught exception reporting exits the process.

## zort takeaways

- zort benchmarks should not only record throughput; they should record the runtime knobs that affect GC and stack behavior.
- A useful replacement runtime wants:
  - stable debug toggles
  - a structured event stream
  - a clear story for heap verification
  - repeatable backtrace capture / restore semantics

## zort event sink notes

- zort now has an explicit `EventSink` subsystem in `src/event_sink.zig`.
- `Mutator`, `RootRegistry`, `Collector`, and `ControlKernel` emit typed events
  instead of burying observability in ad hoc counters or bench-only logic.
- `Runtime` accepts an `eventSink` in `Runtime.Config`; the default remains a no-op sink.
- Bench runs now use `TraceRecorder`, which captures per-case deltas for:
  - allocations
  - field writes
  - bytes writes
  - root registrations/unregistrations
  - collections
  - reclaims
- and can optionally retain:
  - full event traces
  - last root-provider counts
  - last GC snapshot
  - last object event per heap object
- `zig build bench -- --csv=notes/benchmarks.csv` appends per-case rows in a lightweight CSV format under `zort/notes/`.
- `zig build bench -- --trace` prints the full event stream for the selected cases.
- `zig build bench -- --trace-gc` prints GC-focused events only.
- `zig build bench -- --trace-effects` prints control-kernel events only.
- `zig build bench -- --profile-json=<path>` writes per-case JSON including:
  - strategy
  - counters
  - root providers
  - the last GC snapshot seen for the case
- Bench argument forwarding now happens through `zort/build.zig`, so `--filter=` reaches the executable as intended.
- `Runtime.explainValue(value, trace)` uses the trace recorder plus runtime state to
  answer why a block is interesting right now:
  - what object kind it is
  - what heap handle it has
  - how many explicit roots own it
  - how many control-kernel roots own it
  - what the last recorded object event was
