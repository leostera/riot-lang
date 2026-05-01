# postgres AGENTS

`postgres` is the PostgreSQL adapter for the shared SQL layer.

## Rules

1. Protocol or type-system quirks specific to PostgreSQL belong here.
2. Keep backend-specific assumptions in this adapter; `sqlx-driver` stays the generic interface.
3. Re-check connection and row-decoding behavior when changing low-level client code.
4. Cover wire-protocol changes with package tests under `packages/postgres/tests` so malformed frames, encoding edge cases, and NULL handling stay deterministic without requiring a live server.
5. Document any config fields that are reserved or partially implemented; connection settings must not imply security behavior the driver does not actually provide.
