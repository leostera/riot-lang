# mime AGENTS

`mime` owns MIME types and related parsing helpers.

## Rules

1. Keep parsing and normalization behavior explicit.
2. Avoid application-specific shortcuts in the shared type table.
3. Small data-driven updates are preferable to ad hoc string logic spread through callers.

## Validate

`timeout 30 riot build mime`
