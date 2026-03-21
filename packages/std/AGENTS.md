# std AGENTS

`std` is the mandatory standard library surface for the rest of the repo.

## Rules

1. Favor small, composable APIs over one-off helpers.
2. Do not leak `Stdlib`, `Unix`, `Sys`, or `Obj` back through this surface unless the boundary is already intentional.
3. Changes here have wide blast radius. Prefer additive evolution and stable signatures.
4. If a utility is only useful for one package, keep it out of `std`.
5. `std` owns its package-provided `tusk-fix` rules under `fix/`; keep those diagnostics aligned with the scheduler and `std` ownership rationale.

## Validate

`timeout 30 tusk build std`
