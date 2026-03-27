# tusk-model AGENTS

`tusk-model` defines the shared types for the build system: workspaces, packages, modules, actions, events, targets, and errors.

## Rules

1. Keep this package free of execution policy. It is the shared vocabulary for the rest of tusk.
2. Prefer structured variants and records over loosely typed payloads.
3. Scoped package phases (`Build`, `Runtime`, `Dev`) live here; changes to that shape usually require follow-up in planner, executor, server, and CLI code.
4. Be conservative about breaking public type shapes.
5. Workspace build-path configuration lives in the root `tusk.toml` under `[tusk].target_dir`; treat that as the source of truth for `_build`-style paths.
6. Formatter ignore configuration lives in the root-level `[fmt]` table (`ignore = ["substring", ...]`) on both workspace and package manifests.

## Validate

`timeout 30 tusk build tusk-model`
