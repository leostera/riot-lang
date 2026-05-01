# sqlx AGENTS

`sqlx` is the higher-level SQL surface built on `sqlx-driver`.

## Rules

1. High-level query APIs belong here; wire-protocol and driver mechanics belong in drivers.
2. Keep the abstraction honest. If behavior only works for one backend, model that explicitly.
3. Re-check driver compatibility when changing shared query or row semantics.
4. Application schemas should live in migration sources, not inline DDL strings in app packages.
5. Keep `Sqlx.migrate` as the small startup convenience API. Use `Sqlx.Migrate.run` for detailed reports, targeted runs, or custom sources.
6. When migration filename, checksum, locking, dirty-state, or rollback behavior changes, update `README.md`, `src/migrate.mli`, and `tests/migrate_tests.ml` together.
7. Prefer typed wrappers for migration identifiers and table names. Expose `from_*` / `to_*` helpers, and reserve `*_unchecked` for constructors that may panic.
