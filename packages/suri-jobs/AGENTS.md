# suri-jobs AGENTS

`suri-jobs` provides typed background jobs, supervised runners, SQL-backed state,
and Suri dashboard routes.

## Rules

1. Keep queue, worker, and fanout identifiers typed; expose checked constructors where possible.
2. Keep in-memory and SQL-backed behavior aligned through shared package tests.
3. Dashboard routes should compose through `Suri.Middleware.Router.forward` and avoid app-specific paths.
4. When changing persistence schema or retry semantics, update README, tests, and SQL backend together.
