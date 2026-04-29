# blink AGENTS

`blink` is the streaming HTTP client built on actors and `http`.

## Rules

1. Connection lifecycle, message flow, and incremental body delivery are the core contracts.
2. Keep transport concerns here and protocol concerns in `http`.
3. Keep framework-specific behavior in `suri`; `blink` should stay a reusable client.
4. If you change connection or streaming semantics, re-check callers in `suri`.
