# miniriot AGENTS

`miniriot` is the actor runtime. It owns process lifecycle, mailboxes, scheduling, timers, and message delivery semantics.

## Rules

1. Preserve scheduler and mailbox invariants before optimizing.
2. Prefer explicit actor loops and selector-based receives.
3. Runtime behavior should stay deterministic where practical. Avoid hidden global state.
4. Keep the public runtime surface small. Push convenience APIs up into `std`.
5. `miniriot` owns its package-provided `tusk-fix` rules under `fix/`; keep those diagnostics aligned with scheduler fairness and cooperative yielding semantics.

## Validate

`timeout 30 tusk build miniriot`
