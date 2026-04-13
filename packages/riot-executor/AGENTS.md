# riot-executor AGENTS

`riot-executor` runs planned build actions and aggregates results.

## Rules

1. Execution order must respect planner dependencies.
2. Package scheduling is scoped and keyed by `Package.key`; keep `Build`, `Runtime`, and `Dev` completion state distinct.
3. Keep caching, worker coordination, and result reporting explicit.
4. Avoid hiding tool invocation failures. Surface structured errors with enough context to debug builds.
5. There is one package coordinator path and one action executor path; do not reintroduce duplicate schedulers without a clearly separate role.
6. When changing concurrency behavior, re-check interactions with `riot-store` and `riot-toolchain`.
7. Treat `Build_ctx.available_parallelism` as the only execution concurrency budget and thread it into action execution; avoid package-level worker pools competing for parallelism ownership.
8. If executor emits command telemetry, emit it from prepared toolchain invocations and do not bypass `riot-toolchain` by executing raw process commands for compiler actions.
9. Successful `ocamlc` warnings are part of package build results: preserve them through caching and replay them for cached packages, and surface dependency-blocked packages as skipped rather than duplicated failures.
10. Rewrite compiler diagnostic paths here, while sandbox/package context is still available; store and replay the rewritten warning text instead of teaching the CLI about sandbox layout.
11. Sandbox, package output, and cache materialization paths must be rooted at `workspace.target_dir_root`, not `workspace.root`, so prepared or cloned workspaces stay isolated.

## Validate

`timeout 30 riot build riot-executor`
