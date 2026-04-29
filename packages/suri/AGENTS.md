# suri AGENTS

`suri` is the web framework layer. It owns middleware, routing, handlers, liveview support, and web-server integration.

## Rules

1. Framework ergonomics belong here, not in `http` or `blink`.
2. Keep `Http_handler`, protocol detection, and server wiring aligned. Interface drift between those modules breaks builds quickly.
3. When a local `Config` module exists, refer to it explicitly, for example `Super.Config`.
4. Middleware and handler APIs should favor concrete framework types over ad hoc tuples or untyped maps.
5. Follow Std fallibility naming conventions. A fallible operation named plainly
   should return `result` or `option`; use `_unchecked` only for APIs that skip
   checks and may panic by contract; reserve `unsafe_` for memory, FFI, or other
   representation-unsafe boundaries.
