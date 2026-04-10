# riot-lsp AGENTS

`riot-lsp` owns Riot's actual Language Server Protocol server.

## Rules

1. Keep `riot-lsp` built on `jsonrpc` and `lsp`; do not duplicate protocol types or JSON codecs here.
2. Keep stdout protocol-only. Never mix logs or human status output into the LSP stream.
3. Keep the early slices syntax-first: lifecycle, document sync, syntax and lint diagnostics first, then hover, formatting, and code actions incrementally.
4. Keep session state explicit and testable. Prefer pure state transitions over hiding behavior in ad hoc I/O loops.
5. Treat request parsing failures as request-scoped failures. One bad request must not poison the rest of the session.
6. Keep request/response behavior covered by snapshot fixtures.
7. Keep type diagnostics package-scoped when a file URI is available. Mirror `riot check`'s package/session grouping so sibling modules resolve the same way in the editor and the CLI.
8. If you need server logging, keep it file-only and out of the JSON-RPC transport.

## Validate

`timeout 30 riot build riot-lsp`
`timeout 180 riot test riot-lsp:framing_tests`
`timeout 180 riot test riot-lsp:session_fixture_tests`
