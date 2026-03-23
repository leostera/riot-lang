# tusk-fix-api AGENTS

`tusk-fix-api` is the shared rule-authoring surface used by `tusk-fix` and fused package providers.

## Rules

1. Keep this package small and stable; it is a boundary package, not the place for CLI/reporting logic.
2. Prefer types and helpers that rule providers can use without pulling in the whole `tusk-fix` runtime.
3. Put formatting, JSON rendering, and coordinator/runtime behavior in `tusk-fix`, not here.
4. Keep rule ids and diagnostic messages as lightweight strings; do not reintroduce a built-in diagnostic-code registry here.
5. Changes here affect both built-in rules and fused package rules; preserve compatibility where possible.

## Validate

`timeout 30 tusk build tusk-fix-api`
