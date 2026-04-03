# riot-build AGENTS

`riot-build` is no longer a daemon boundary. It is the in-process build session and orchestration layer used by `riot-cli`.

## Rules

1. Keep this package one-shot and local. Do not reintroduce background service, RPC, or socket assumptions.
2. Session orchestration belongs here; user-facing argument parsing belongs in `riot-cli`.
3. If behavior starts looking transport-shaped, first ask whether it should be deleted instead.
4. Changes here commonly affect `riot-cli`, `riot-planner`, and `riot-executor`.
5. Artifact lookup must respect the active build lane (`profile` + `target`) instead of assuming host/default output directories.
6. Public command entrypoints should expose typed request/event/error APIs from this package; `riot-cli` should only translate `matches` into those requests and render the resulting events.
7. Normal command entrypoints should assume `riot-cli` already validated workspace load errors; do not reintroduce `load_errors` plumbing into public request types unless a command truly needs partial-workspace behavior.
8. Requested profiles must propagate end-to-end through the client/server/worker path. Do not recompute `debug` inside the internal server or build worker once a request already carries `profile`.
9. Test-suite discovery narrowing belongs here. When the CLI parses `package:suite`, carry the suite filter through typed test requests and apply it before building/running binaries instead of forwarding the whole selector as a per-test query string.
10. `build` returns per-package build results. `run`/`install`/`test`/`bench` should consume those returned outputs directly instead of round-tripping through the server/store to rediscover artifacts by name.
11. Prepared workspaces are a valid input to the local build runtime. When a caller has already run dependency preparation and needs to append synthetic packages for the same build lane, reuse that prepared workspace instead of forcing a second `ensure_workspace` pass.

## Validate

`timeout 30 riot build riot-build`
