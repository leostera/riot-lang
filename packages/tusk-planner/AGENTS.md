# tusk-planner AGENTS

`tusk-planner` turns workspaces and packages into dependency-aware build plans.

## Rules

1. Planning is where graph shape and invalidation rules live. Keep execution concerns out.
2. Preserve deterministic planning output for the same workspace inputs.
3. Changes here often need matching updates in `tusk-model` and `tusk-executor`.
4. Prefer explicit plan and error types over implicit sentinel values.

## Validate

`timeout 30 tusk build tusk-planner`
