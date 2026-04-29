# sqlite AGENTS

`sqlite` is the SQLite adapter for the shared SQL layer.

## Rules

1. SQLite-specific behavior should stay here; `sqlx-driver` stays the generic driver interface.
2. Keep backend capability differences explicit.
3. Re-check `sqlx-driver` compatibility when changing row, error, or transaction behavior.
