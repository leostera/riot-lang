# tusk-executor AGENTS

`tusk-executor` runs planned build actions and aggregates results.

## Rules

1. Execution order must respect planner dependencies.
2. Keep caching, worker coordination, and result reporting explicit.
3. Avoid hiding tool invocation failures. Surface structured errors with enough context to debug builds.
4. When changing concurrency behavior, re-check interactions with `tusk-store` and `tusk-toolchain`.

## Validate

`timeout 30 tusk build tusk-executor`
