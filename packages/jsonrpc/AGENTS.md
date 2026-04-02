# jsonrpc AGENTS

`jsonrpc` owns JSON-RPC message framing and codec behavior.

## Rules

1. Keep the package transport-neutral. Session management belongs in callers.
2. Treat schema changes as compatibility changes.
3. Prefer typed request and response helpers over raw JSON assembly when possible.

## Validate

`timeout 30 riot build jsonrpc`
