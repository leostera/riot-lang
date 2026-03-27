# Miniriot Spec

This directory contains a readable TLA+ model of the `packages/miniriot` core runtime.

The spec is intentionally bounded and semantic. It models the pieces that matter for design review:

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

## Files

- `QueueUtils.tla`: small sequence helpers so the main module stays readable.
- `MiniriotRuntime.tla`: the actual runtime state machine and its safety invariants.
- `MiniriotRuntime.cfg`: a tiny TLC model with a few processes, timers, and selectors.

## How To Run TLC

From the repo root:

```sh
java -cp "/Applications/TLA+ Toolbox.app/Contents/Eclipse/tla2tools.jar" \
  tlc2.TLC \
  specs/miniriot/MiniriotRuntime.tla \
  -config specs/miniriot/MiniriotRuntime.cfg
```

The config is deliberately small. It is meant to smoke-test the model and its invariants, not to prove the implementation correct under production-scale workloads.

## Design Intent Captured By The Model

The model states a few things explicitly that are easy to miss in the OCaml code:

- Receive timeouts are about the absence of a matching message, not the absence of any message.
- Interval timer cancellation must work with the original timer id returned to the caller.
- Exit cleanup must remove stale links and monitor metadata from the surviving processes.
- A process can only be queued on one worker at a time.
- A process that is waiting must only become runnable again because of a real wakeup, not because bookkeeping drifted.

## Bugs The Model Exposed During Review

These were the main design/logic mismatches the model highlighted while reading the runtime. The first two now have direct executable regressions in `packages/miniriot/tests/design_regression_tests.ml`.

1. Selective receive timeouts were keyed to “mailbox empty” instead of “no matching message”.
   This let unmatched saved messages mask timeouts, and it also let unmatched wakeups rearm the original timeout window. The runtime now keeps the original timeout armed until a matching message is selected or the timeout genuinely expires.

2. Interval timers lost their identity after the first tick.
   Rearming through `Timer_wheel.add_timer` allocated a fresh `Timer_id`, so `Timer.send_interval` only cancelled the first scheduled firing. The runtime now reschedules the same `Timer.t`, preserving the original id across repetitions.

3. Link and monitor metadata were left behind on process exit.
   `handle_exit_proc` delivered `DOWN` and `EXIT`, but survivor-side relation cleanup was missing. The runtime now removes dead links and monitor registrations during exit handling. This still deserves a dedicated regression once the relevant internal state is easier to assert from tests.

4. The scheduler queue bookkeeping is subtle enough that it deserves direct regression tests in addition to the model.
   The current `step_process` source does not show the unconditional `mark_slot_pending` issue I initially suspected on first pass, so I do not currently count that as a confirmed implementation bug. I left the queue/slot invariants in the model because this area is still easy to regress.
