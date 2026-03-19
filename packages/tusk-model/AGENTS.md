# tusk-model AGENTS

`tusk-model` defines the shared types for the build system: workspaces, packages, modules, actions, events, targets, and errors.

## Rules

1. Keep this package free of execution policy. It is the shared vocabulary for the rest of tusk.
2. Prefer structured variants and records over loosely typed payloads.
3. Model changes usually require follow-up in planner, executor, server, and CLI code.
4. Be conservative about breaking public type shapes.

## Validate

`timeout 30 tusk build tusk-model`
