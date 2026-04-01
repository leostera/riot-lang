# tusk-planner AGENTS

`tusk-planner` turns workspaces and packages into dependency-aware build plans.

## Rules

1. Planning is where graph shape and invalidation rules live. Keep execution concerns out.
2. Preserve deterministic planning output for the same workspace inputs.
3. Scoped package nodes (`pkg.build`, `pkg.runtime`, `pkg.dev`) and their dependency edges are planner-owned behavior; keep those rules explicit.
4. `build-dependencies` should only participate in the build-scope graph. Runtime and dev products should not accidentally inherit build-only edges.
5. Changes here often need matching updates in `tusk-model` and `tusk-executor`.
6. Prefer explicit plan and error types over implicit sentinel values.
7. Keep default library planning limited to `.cma`/`.cmxa` outputs. Do not reintroduce unconditional `.cmxs` shared-library actions unless there is an explicit runtime consumer and an opt-in surface for it.
8. Package-plan cache keys must include all compiler inputs that can change produced artifacts, including the resolved toolchain identity for cross builds.
9. `CreateLibrary` inputs must be `.cmx` from OCaml module deps plus `.o` from `Native` C deps only. Do not feed ML companion `.o` files into library archive planning.

## Validate

`timeout 30 tusk build tusk-planner`
