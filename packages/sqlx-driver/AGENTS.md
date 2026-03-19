# sqlx-driver AGENTS

`sqlx-driver` defines the shared database driver boundary.

## Rules

1. Keep the interface small and driver-agnostic.
2. Driver-specific behavior should live in `sqlite` or `postgres`, not in the shared abstraction.
3. Prefer typed request and result shapes over polymorphic escape hatches.

## Validate

`timeout 30 tusk build sqlx-driver`
