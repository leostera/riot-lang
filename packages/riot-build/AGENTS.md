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
12. Successful builds now record workspace cache generations through `riot-store`. Keep that hook lightweight and best-effort: build success should not turn into command failure just because cache bookkeeping had a problem. Recorded generations must include the full reachable cache closure needed to keep a warm rebuild warm, including exported action-artifact hashes referenced by cached package artifacts. `riot-build` should always hand successful closures to `riot-store`; `riot-store` owns the cheap state-based decision about whether a cached build can skip generation bookkeeping entirely.
13. `install` promotion failures are fatal. Do not downgrade failed project-root or `~/.riot/bin` promotion into warnings or synthetic success.
14. Workspace-free `run`/`install` entrypoints belong here too. If the CLI resolves a remote source or registry package target, `riot-build` should own the typed PM-event emission, external workspace loading, and then delegate back into the normal local build/install/run path instead of reimplementing that flow in `riot-cli`.
15. External installs only promote into `~/.riot/bin`. Do not force a project-root promotion for detached source or registry installs.
16. External source `run` / `install` requests are cache-first by default. Carry an explicit `update` flag in the typed request when the caller wants to refresh a cached source checkout before building or running it.
17. `test` and `bench` should execute suite binaries through their machine-readable runners (`run-tests --json` / `run-benchmarks --json`) and aggregate case-level results in `riot-build`. Do not treat suite exit codes as the summary source of truth when structured results are available. The final test summary event should include aggregated failed test cases so downstream JSON consumers can surface failures without rescanning the full suite stream.
18. Keep per-suite stdout/stderr as structured payload in the runtime events, but leave human filtering decisions to `riot-cli`. `riot-build` should not reintroduce special-case pretty rendering for zero-match suites.
19. Preserve structured suite metadata in the exported `test_event` / `bench_event` payloads. Test cases should carry `duration_us`, `size`, `reliability`, retry attempts, and timeout-aware status, and suite-completed events should carry `started_at_us`, `completed_at_us`, and `duration_us`.
20. Build locks and any other runtime-owned workspace paths must respect `workspace.target_dir_root`. Do not derive `_build`-style locations directly from `workspace.root`.
21. Internal build-session payloads should carry typed target triples, not reparsed target strings. If a worker/runtime boundary needs a target, pass `Riot_model.Target.t` through and stringify only where the filesystem or toolchain process actually needs it.

## Validate

`timeout 30 riot build riot-build`
