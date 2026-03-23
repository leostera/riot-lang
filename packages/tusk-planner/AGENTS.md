# tusk-planner AGENTS

`tusk-planner` turns workspaces and packages into dependency-aware build plans.

## Rules

1. Planning is where graph shape and invalidation rules live. Keep execution concerns out.
2. Preserve deterministic planning output for the same workspace inputs.
3. Scoped package nodes (`pkg.build`, `pkg.runtime`, `pkg.dev`) and their dependency edges are planner-owned behavior; keep those rules explicit.
4. `build-dependencies` should only participate in the build-scope graph. Runtime and dev products should not accidentally inherit build-only edges.
5. Changes here often need matching updates in `tusk-model` and `tusk-executor`.
6. Prefer explicit plan and error types over implicit sentinel values.

## Validate

`timeout 30 tusk build tusk-planner`
