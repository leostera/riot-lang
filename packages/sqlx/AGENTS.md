# sqlx AGENTS

`sqlx` is the higher-level SQL surface built on `sqlx-driver`.

## Rules

1. High-level query APIs belong here; wire-protocol and driver mechanics belong in drivers.
2. Keep the abstraction honest. If behavior only works for one backend, model that explicitly.
3. Re-check driver compatibility when changing shared query or row semantics.
