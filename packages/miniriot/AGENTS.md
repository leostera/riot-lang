# miniriot AGENTS

`miniriot` is the actor runtime. It owns process lifecycle, mailboxes, scheduling, timers, and message delivery semantics.

## Rules

1. Preserve scheduler and mailbox invariants before optimizing.
2. Prefer explicit actor loops and selector-based receives.
3. Runtime behavior should stay deterministic where practical. Avoid hidden global state.
4. Keep the public runtime surface small. Push convenience APIs up into `std`.

## Validate

`timeout 30 tusk build miniriot`
