# specs AGENTS

`specs/` stores executable design artifacts such as TLA+ and PlusCal models.

## Routing

- `specs/miniriot/AGENTS.md`: `packages/miniriot` runtime semantics, scheduler behavior, mailbox rules, timers, lifecycle, links, and monitors

## Rules

1. Read the matching package `AGENTS.md` before changing a spec so the model stays aligned with the owned runtime behavior.
2. Optimize for readability first: descriptive names, short modules, and comments that map each spec action back to the production concept it represents.
3. Keep abstraction boundaries explicit. Model semantic contracts, not incidental OCaml implementation detail, unless the detail is itself the subject of the bug.
4. Prefer a small shared utility module plus one focused runtime module over a single dense file.
5. Keep TLC configs intentionally small and purpose-built. Add separate configs for separate design questions instead of one broad state-space explosion.
6. Update the local `README.md` files when the spec’s scope, assumptions, or validation commands change.

## Validate

If the spec changed, run the narrowest useful TLC check from the repo root and record any remaining state-space limits in the local README.
