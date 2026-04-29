# lsp AGENTS

`lsp` owns Language Server Protocol types, codecs, and typed method
descriptors.

## Rules

1. Keep `lsp` protocol-only. Riot server state, editor plugins, and language intelligence belong in their owning packages.
2. Build on `jsonrpc` for generic JSON-RPC envelope behavior.
3. Prefer typed request and notification descriptors over stringly JSON plumbing.
4. Start with the subset Riot actually needs and grow methods as callers use them.
5. Keep UTF-16 and URI helpers explicit in this package so downstream servers share one implementation.
6. Keep machine-readable request/response contract tests on `Std.Test.Snapshot`.
