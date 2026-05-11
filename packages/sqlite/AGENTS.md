# sqlite AGENTS

`sqlite` is the SQLite adapter for the shared SQL layer.

## Rules

1. SQLite-specific behavior should stay here; `sqlx-driver` stays the generic driver interface.
2. Keep backend capability differences explicit.
3. Re-check `sqlx-driver` compatibility when changing row, error, or transaction behavior.
4. Native SQLite bindings live under `native/` and link against the system `sqlite3` library via package target flags.
5. Keep `Sqlite.Testing.with_db` disposable: file-backed configs must use temporary storage and close connections before cleanup.
6. Keep `src/sqlite.ml` as a public facade; implementation should stay split across focused internal modules.
