# blink AGENTS

`blink` is the streaming HTTP client built on actors and `http`.

## Rules

1. Connection lifecycle, message flow, and incremental body delivery are the core contracts.
2. Keep transport concerns here and protocol concerns in `http`.
3. Avoid introducing framework-specific behavior into the client.
4. If you change connection or streaming semantics, re-check callers in `suri`.

## Validate

`timeout 30 tusk build blink`
