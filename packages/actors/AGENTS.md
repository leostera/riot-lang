# actors AGENTS

`actors` is the actor runtime. It owns process lifecycle, mailboxes, scheduling, timers, and message delivery semantics.

## Rules

1. Preserve scheduler and mailbox invariants before optimizing.
2. Prefer explicit actor loops and selector-based receives.
3. Runtime behavior should stay deterministic where practical. Avoid hidden global state.
4. Keep the public runtime surface small. Push convenience APIs up into `std`.
5. Keep cooperative yielding process-local: manual `Actors.yield` calls and `Proc_state.run` effect stepping should spend the same process-owned reduction budget instead of separate domain-local counters.
6. `actors` owns its package-provided `riot-fix` rules under `fix/`; keep those diagnostics aligned with scheduler fairness and cooperative yielding semantics.
7. Placement classes now matter: `spawn` remains the normal migratable path, `spawn_pinned` must stay scheduler-sticky and non-stealable, and `spawn_blocked` must stay isolated from the normal worker pool.
8. `Process.kill` is cooperative runtime cancellation, not arbitrary async preemption. Keep it as a scheduler-delivered exit request that wakes blocked actors and takes effect at a runtime boundary.

## Validate

`timeout 30 riot build actors`
