# riot-eval AGENTS

`riot-eval` owns OCaml evaluation helpers and tooling around interactive evaluation.

## Rules

1. Keep parser and toolchain boundaries clear. Syntax lives in `syn`; compiler invocation details live in `riot-toolchain`.
2. Surface evaluation failures with typed results rather than stringly fallbacks.
3. If evaluation behavior changes, check related command paths that expose it.

## Validate

`timeout 30 riot build riot-eval`
