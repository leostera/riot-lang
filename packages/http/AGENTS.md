# http AGENTS

`http` owns wire-level HTTP behavior and shared protocol types.

## Rules

1. Treat parser and serializer changes as protocol changes. Preserve framing rules and back-compat where intended.
2. Keep HTTP version boundaries clear. Shared helpers should stay version-agnostic.
3. Do not move web-framework policy into this package.
4. Prefer focused fixtures or protocol-level tests when changing parsing or encoding logic.
5. Hot parser internals may use `Std.IO.StringView`, but keep public HTTP request/response surfaces stable until there is clear benchmark evidence for a broader API change.

## Validate

`timeout 30 riot build http`
