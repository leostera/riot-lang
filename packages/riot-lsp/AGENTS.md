# riot-lsp AGENTS

`riot-lsp` owns Riot's actual Language Server Protocol server.

## Rules

1. Keep `riot-lsp` built on `jsonrpc` and `lsp` protocol types and codecs.
2. Keep stdout protocol-only. Send logs and human status output through stderr or logging sinks.
3. Keep the early slices syntax-first: lifecycle, document sync, syntax and lint diagnostics first, then hover, definitions, document symbols, formatting, and code actions incrementally.
4. Keep session state explicit and testable. Prefer pure state transitions over hiding behavior in ad hoc I/O loops.
5. Treat request parsing failures as request-scoped failures so the rest of the session can continue.
6. Keep request/response behavior covered by snapshot fixtures.
7. Keep type diagnostics package-scoped when a file URI is available. Mirror `riot check`'s package/session grouping so sibling modules resolve the same way in the editor and the CLI.
8. If you need server logging, keep it file-only and out of the JSON-RPC transport.
