# postgres AGENTS

`postgres` is the PostgreSQL adapter for the shared SQL layer.

## Rules

1. Protocol or type-system quirks specific to PostgreSQL belong here.
2. Keep backend-specific assumptions in this adapter; `sqlx-driver` stays the generic interface.
3. Re-check connection and row-decoding behavior when changing low-level client code.
