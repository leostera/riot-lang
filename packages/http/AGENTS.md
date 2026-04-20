# http AGENTS

`http` owns wire-level HTTP behavior and shared protocol types.

## Rules

1. Treat parser and serializer changes as protocol changes. Preserve framing rules and back-compat where intended.
2. Keep HTTP version boundaries clear. Shared helpers should stay version-agnostic.
3. Do not move web-framework policy into this package.
4. Prefer focused fixtures or protocol-level tests when changing parsing or encoding logic.
5. Hot parser internals should operate on `Std.IO.IoSlice` or `Iter.Cursor` over slices, but keep public HTTP request/response surfaces stable until there is clear benchmark evidence for a broader API change.
6. Keep parser-side borrowed slice usage explicit about ownership. `Std.IO.IoSlice` and `IoBuffer` are the internal fast path; public request/response values should still materialize ordinary `string` fields at the boundary where HTTP leaves the protocol layer. Additive borrowed parser helpers such as `Http1.Request.parse_slices` are fine for internal fast paths and benchmarks, but they must not silently change the ownership contract of the public HTTP surface.

## Validate

`timeout 30 riot build http`
