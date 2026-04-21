# riot-build AGENTS

`riot-build` is no longer a daemon boundary. It is the in-process build session and orchestration layer used by `riot-cli`.

## Rules

1. Keep this package one-shot and local. Do not reintroduce background service, RPC, or socket assumptions.
2. Session orchestration belongs here; user-facing argument parsing belongs in `riot-cli`.
3. If behavior starts looking transport-shaped, first ask whether it should be deleted instead.
4. Changes here commonly affect `riot-cli` and `riot-planner`.
5. Artifact lookup must respect the active build lane (`profile` + `target`) instead of assuming host/default output directories.
6. Public command entrypoints should expose typed request/event/error APIs from this package; `riot-cli` should only translate `matches` into those requests and render the resulting events.
7. Normal command entrypoints should assume `riot-cli` already validated workspace load errors; do not reintroduce `load_errors` plumbing into public request types unless a command truly needs partial-workspace behavior.
8. Requested profiles must propagate end-to-end through the local runtime/worker path. Do not recompute `debug` inside the runtime or build worker once a request already carries `profile`.
9. Test-suite discovery narrowing belongs here. When the CLI parses `package:suite`, carry the suite filter through typed test requests and apply it before building/running binaries instead of forwarding the whole selector as a per-test query string.
10. `build` returns a typed `Build_result.t`. Downstream command packages should consume that result directly instead of round-tripping through the runtime/store to rediscover artifacts by name.
11. `Build_result` is package-name keyed, not executor-key keyed. When executor results contain the same package in multiple scopes, merge them and prefer `dev` artifacts/exports over `runtime`, and `runtime` over `build`.
12. Public build entrypoints consume `Riot_model.Workspace.t`, the build-ready workspace shape produced by `riot-deps.ensure_workspace`. Do not reintroduce scanned `Workspace_manifest.t` or separate “prepared workspace” wrapper types into the public `riot-build` request surface.
13. Successful builds now record workspace cache generations through `riot-store`. Keep that hook lightweight and best-effort: build success should not turn into command failure just because cache bookkeeping had a problem. Recorded generations must include the full reachable cache closure needed to keep a warm rebuild warm, including exported action-artifact hashes referenced by cached package artifacts. `riot-build` should always hand successful closures to `riot-store`; `riot-store` owns the cheap state-based decision about whether a cached build can skip generation bookkeeping entirely.
14. `install` promotion failures are fatal. Do not downgrade failed project-root or `~/.riot/bin` promotion into warnings or synthetic success.
15. `run`, `install`, `test`, and `bench` belong in their owning packages on top of `riot-build`. Do not move command-specific orchestration back into `riot-build`.
16. Keep `riot-build` focused on prepared-workspace build resolution and execution. External source or registry loading belongs in `riot-run` / `riot-install`, not in the core build facade.
17. Suite execution and aggregation belong outside `riot-build`. This package should expose build-domain events, not test- or bench-runner policy.
20. Build locks and any other runtime-owned workspace paths must respect `workspace.target_dir_root`. Do not derive `_build`-style locations directly from `workspace.root`. When a caller needs to quiesce the current build root, prefer enumerating the currently materialized lane locks under `workspace.target_dir_root` and acquiring those locks rather than inventing a second global lock surface.
21. Internal build-session payloads should carry typed target triples, not reparsed target strings. If a worker/runtime boundary needs a target, pass `Riot_model.Target.t` through and stringify only where the filesystem or toolchain process actually needs it.
19. Keep the root `Riot_build` facade as build-focused as possible. Build entrypoints, typed build requests, `Build_result`, build events, build errors, and the build lock surface belong at the top level. Do not reintroduce command-runtime modules or other transport-shaped internals through `Riot_build`.
20. Package/workspace scaffolding does not belong here. `riot-build` must not expose package creation helpers; `riot-init` owns that API.
21. Generic incremental dependency-graph execution belongs in `graph_scheduler`. Keep build-domain package/action semantics in `build_work` and related modules instead of re-growing scheduler policy there.
22. Keep `graph_scheduler` behind domain facades. Package build orchestration should flow through `package_scheduler` with package-specific types/results, and action execution should eventually do the same instead of leaking generic scheduler node variants through build-domain modules.
23. `package_scheduler` owns one unified package build graph over `PlanPackage`, `ExecuteAction`, and `FinalizePackage` work. Keep that unified graph package-domain specific; callers should consume package results and build events, not generic graph node outcomes.
24. `package_builder` should expose package-domain helpers for planning, preparing execution, executing one action, and finalizing a package. Keep `action_executor` as the low-level action primitive and do not route build-time package execution back through a nested action scheduler loop.
25. `action_scheduler.run` should still return an action-domain summary (completed actions, first failure, warnings) instead of exposing generic scheduler state or raw worker tables.
26. The legacy `coordinator`, `action_queue`, and `build_scheduler` path has been deleted. Do not reintroduce parallel package/action orchestration outside `package_scheduler`, `action_scheduler`, and `graph_scheduler`.
27. Public build events should expose package-scheduler progress as build-domain phases. Keep package planning/execution visibility in `Event.Phase`, but avoid leaking generic graph-scheduler node or mutation details through the public API.
28. `package_scheduler` should express package dependency readiness through `FinalizePackage(dep) -> PlanPackage(pkg)` graph edges. Keep blocked dependents waiting in scheduler state instead of retry-only package work, and keep any public `deferred_count` fields build-domain progress summaries rather than generic scheduler internals.
29. Keep test suites aligned with ownership boundaries. `package_scheduler` behavior belongs in `package_scheduler_tests`; `build_work` tests should stay narrow and cover lane preparation / helper shaping instead of package-scheduler semantics.
30. Funnel public build events and detailed build telemetry through the same `Std.Telemetry` delivery path before calling the external `on_event` callback. Do not reintroduce mixed direct-callback and telemetry-backed event emission from different runtime threads.
31. Local build-session coordination that only runs under the actor runtime can experiment with actor-owned synchronization inside `riot-build`, but do not promote those primitives to `Std.Sync` until the runtime-facing contract is proven by real builds.
32. Actor-owned helpers inside `riot-build` should not spawn actors at module load time. Defer actor creation until runtime use so test binaries and other callers can finish booting their actor runtime before those helpers come to life.
33. `riot-build` must not print raw progress lines directly. Build-lock waits and similar runtime coordination states belong on `Riot_build.Event.Phase` so `riot-cli --json` stays pure JSONL and human mode can render the same state from structured events.

## Validate

`timeout 30 riot build riot-build`
