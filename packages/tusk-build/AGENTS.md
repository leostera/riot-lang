# tusk-build AGENTS

`tusk-build` is no longer a daemon boundary. It is the in-process build session and orchestration layer used by `tusk-cli`.

## Rules

1. Keep this package one-shot and local. Do not reintroduce background service, RPC, or socket assumptions.
2. Session orchestration belongs here; user-facing argument parsing belongs in `tusk-cli`.
3. If behavior starts looking transport-shaped, first ask whether it should be deleted instead.
4. Changes here commonly affect `tusk-cli`, `tusk-planner`, and `tusk-executor`.
5. Artifact lookup must respect the active build lane (`profile` + `target`) instead of assuming host/default output directories.

## Validate

`timeout 30 tusk build tusk-build`
