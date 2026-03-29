# specs AGENTS

`specs/` stores executable design artifacts such as TLA+ and PlusCal models.

## Routing

- `specs/miniriot/AGENTS.md`: `packages/miniriot` runtime semantics, scheduler behavior, mailbox rules, timers, lifecycle, links, and monitors
- `specs/tusk/AGENTS.md`: `packages/tusk-*` build planning, action scheduling, cache semantics, pipeline boundaries, and artifact materialization

## Rules

1. Read the matching package `AGENTS.md` before changing a spec so the model stays aligned with the owned runtime behavior.
2. Optimize for readability first: descriptive names, short modules, and comments that map each spec action back to the production concept it represents.
3. Keep abstraction boundaries explicit. Model semantic contracts, not incidental OCaml implementation detail, unless the detail is itself the subject of the bug.
4. Keep one canonical integration spec for cross-feature behavior, and add smaller slice specs per subsystem or semantic concern. Slice by behavior, not by individual invariant.
5. Prefer a small shared utility/common layer plus focused specs over one dense file, but do not over-modularize. In TLA+, shared modules are helpers, not the main organizational tool.
6. Give every constant an `ASSUME` that makes the intended model shape obvious.
7. Separate type/bounds invariants from semantic correctness invariants. “Well-typed” and “correct” should usually be different operators.
8. Keep TLC configs intentionally small and purpose-built. Use separate safety and liveness configs, and smaller constants for liveness.
9. Keep models bounded. If exploration needs a temporary cutoff, prefer an explicit model constraint such as `TLCGet("level") < N`, and document that choice.
10. Prefer decomposed mutable machine state over large functions-of-structs. Use structs mainly for immutable values such as messages.
11. Use auxiliary variables only for history, diagnostics, or bounding. The machine’s real behavior should not depend on them.
12. When refactoring a spec around a known historical bug, preserve a bug-reproduction config that still fails until the model is intentionally updated to the fixed semantics.
13. Keep smoke configs and bug-reproduction configs separate, and name the bug configs so expected failures are obvious.
14. Update the local `README.md` files when the spec’s scope, assumptions, or validation commands change.

## Validate

If the spec changed, run the narrowest useful TLC check from the repo root and record any remaining state-space limits, safety/liveness split, and temporary model constraints in the local README.
