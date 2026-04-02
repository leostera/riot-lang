# Actors Spec

This directory contains a readable TLA+ model of the `packages/actors` core runtime.

The specs are intentionally bounded and semantic. They model the pieces that matter for design review:

- per-worker runnable queues and work stealing
- process lifecycle states
- selective receive with a save queue
- blocking syscalls plus wakeups/timeouts
- links, monitors, and exit propagation
- one-shot and interval timers

It does not try to model:

- OCaml continuation internals in `proc_state.ml`
- backtrace capture
- the concrete `Async.Poll` implementation
- exact nanosecond timing or timer-wheel bucket math

## Layout

- `QueueUtils.tla`: small sequence helpers so the main module stays readable.
- `ActorsCommon.tla`: shared enums, message constructors, and pure helper operators used by the integration model and the slices.
- `ActorsRuntime.tla`: the integration state machine for queue ownership, receive, timers, lifecycle, and syscall interactions.
- `ActorsRuntime.cfg`: a tiny TLC smoke model for the integration spec.
- `ReceiveTimeouts.tla`: a receive-only slice that intentionally preserves the historical timeout bug.
- `ReceiveTimeouts.cfg`: a passing smoke config for the receive slice.
- `ReceiveTimeoutsBug.cfg`: a failing bug-reproduction config for the receive slice.
- `IntervalTimers.tla`: a timer-only slice that intentionally preserves the historical interval-id bug.
- `IntervalTimers.cfg`: a passing smoke config for the timer slice.
- `IntervalTimersBug.cfg`: a failing bug-reproduction config for the timer slice.
- `ExitCleanup.tla`: a lifecycle slice that intentionally preserves the historical survivor-cleanup bug.
- `ExitCleanup.cfg`: a passing smoke config for the lifecycle slice.
- `ExitCleanupBug.cfg`: a failing bug-reproduction config for the lifecycle slice.

## How To Run TLC

From the repo root:

```sh
java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  tlc2.TLC \
  specs/actors/ActorsRuntime.tla \
  -config specs/actors/ActorsRuntime.cfg
```

The configs are deliberately small. They are meant to smoke-test the model and its invariants, not to prove the implementation correct under production-scale workloads.

The plain smoke configs currently use explicit level bounds:

- `ActorsRuntime.cfg` uses `TLCGet("level") < 5`.
- `ReceiveTimeouts.cfg`, `IntervalTimers.cfg`, and `ExitCleanup.cfg` use `TLCGet("level") < 6`.

Those cutoffs are intentional. They keep the smoke runs fast enough to use during refactors, while the matching `*Bug.cfg` files stay unconstrained so they can still produce real counterexamples for the historical bugs.

`ExitCleanup.cfg` also disables deadlock checking. That slice can legitimately terminate in a state where every process is finalized, and for the smoke run we only care that the local invariants still hold under that bounded exploration.

For the slice specs, the intended workflow is:

1. Run the plain `.cfg` first to confirm the smaller model parses and its local type/bounds invariants hold.
2. Run the matching `*Bug.cfg` to confirm the historical design bug is still reproduced by the sliced model.
3. Only then update the slice to the fixed semantics and replace the failing bug config with a passing law-check config.

## Design Intent Captured By The Model

The model states a few things explicitly that are easy to miss in the OCaml code:

- Receive timeouts are about the absence of a matching message, not the absence of any message.
- Interval timer cancellation must work with the original timer id returned to the caller.
- Exit cleanup must remove stale links and monitor metadata from the surviving processes.
- A process can only be queued on one worker at a time.
- A process that is waiting must only become runnable again because of a real wakeup, not because bookkeeping drifted.

## Historical Bug Slices

The three slice specs currently preserve the pre-fix semantics on purpose. That is deliberate: each one has a small passing smoke config and a matching failing bug config. The failing configs are the proof that the refactor still showcases the original design bug before we “fix the spec” to match the repaired runtime.

- `ReceiveTimeoutsBug.cfg` should fail because unmatched saved messages can mask a timeout and unmatched wakeups can rearm the original deadline.
- `IntervalTimersBug.cfg` should fail because interval delivery rearms under a fresh timer id instead of preserving the original one.
- `ExitCleanupBug.cfg` should fail because dead processes can still be referenced by surviving links or monitor tables.

## Bugs The Model Exposed During Review

These were the main design/logic mismatches the model highlighted while reading the runtime. The first two now have direct executable regressions in `packages/actors/tests/design_regression_tests.ml`.

1. Selective receive timeouts were keyed to “mailbox empty” instead of “no matching message”.
   This let unmatched saved messages mask timeouts, and it also let unmatched wakeups rearm the original timeout window. The runtime now keeps the original timeout armed until a matching message is selected or the timeout genuinely expires.

2. Interval timers lost their identity after the first tick.
   Rearming through `Timer_wheel.add_timer` allocated a fresh `Timer_id`, so `Timer.send_interval` only cancelled the first scheduled firing. The runtime now reschedules the same `Timer.t`, preserving the original id across repetitions.

3. Link and monitor metadata were left behind on process exit.
   `handle_exit_proc` delivered `DOWN` and `EXIT`, but survivor-side relation cleanup was missing. The runtime now removes dead links and monitor registrations during exit handling. This still deserves a dedicated regression once the relevant internal state is easier to assert from tests.

4. The scheduler queue bookkeeping is subtle enough that it deserves direct regression tests in addition to the model.
   The current `step_process` source does not show the unconditional `mark_slot_pending` issue I initially suspected on first pass, so I do not currently count that as a confirmed implementation bug. I left the queue/slot invariants in the model because this area is still easy to regress.
