# postgres AGENTS

`postgres` is the PostgreSQL adapter for the shared SQL layer.

## Rules

1. Protocol or type-system quirks specific to PostgreSQL belong here.
2. Do not push backend-specific assumptions into `sqlx-driver`.
3. Re-check connection and row-decoding behavior when changing low-level client code.

## Validate

`timeout 30 riot build -p postgres --all`
