# mime AGENTS

`mime` owns MIME types and related parsing helpers.

## Rules

1. Keep parsing and normalization behavior explicit.
2. Keep the shared type table application-neutral.
3. Small data-driven updates are preferable to ad hoc string logic spread through callers.
