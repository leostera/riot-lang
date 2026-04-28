# http AGENTS

`http` owns wire-level HTTP behavior and shared protocol types.

## Rules

1. Treat parser and serializer changes as protocol changes. Preserve framing rules and back-compat where intended.
2. Keep HTTP version boundaries clear. Shared helpers should stay version-agnostic.
3. Do not move web-framework policy into this package.
4. Prefer focused fixtures or protocol-level tests when changing parsing or encoding logic.
5. Hot parser internals should operate on `Std.IO.IoSlice` or `Iter.Cursor` over slices, but keep public HTTP request/response surfaces stable until there is clear benchmark evidence for a broader API change.
6. Keep parser-side borrowed slice usage explicit about ownership. `Std.IO.IoSlice` and `IoBuffer` are the internal fast path; public request/response values should still materialize ordinary `string` fields at the boundary where HTTP leaves the protocol layer. Borrowed parsers are fine as benchmark-only or internal measurement tools, but do not expose them on the public `Http1.Request` or `Http1.Response` surfaces.
7. Keep HTTP head parsing eager and body materialization lazy. Parsers should build owned request/response head values, but carry payload bytes through `Std.Net.Http.Body.t` or borrowed slices until a higher layer explicitly requests `to_string`, JSON decoding, or some other body materialization.

## Validate

`timeout 30 riot build -p http`
`timeout 30 riot test -p http`
`timeout 30 riot bench -p http --warmup 10 --compare 5`
