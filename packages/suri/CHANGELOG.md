# Changelog

All notable changes to `suri` are documented here.

## 0.0.35 - 2026-05-11

### Added

- Added fuzz coverage for Suri middleware Accept, body parser, and method override parsing helpers.

## 0.0.34 - 2026-05-10

### Added

- Added route forwarding for composing application routes.

## 0.0.32 - 2026-05-10

### Added

- Added router forwarding support for prefix-mounted applications.
- Added tests for forwarded routes and route flattening order.

### Changed

- Switched HTTP/1 request parsing to the shared `Http.Http1.Request.parse_head` parser.
- Updated route and HTTP handler docstrings to markdown syntax.
- Replaced generated temporary names with meaningful binders.

### Fixed

- Preserved route order when flattening nested router definitions.

## 0.0.32 - 2026-05-04

### Changed

- Interface docs were cleaned up so generated documentation attaches summaries and details to the intended public items instead of carrying stale separator or misplaced doc comments.

## 0.0.26 - 2026-04-28

### Changed

- Suri gained a hardened server limits configuration covering request body limits, keep-alive request limits, websocket frame limits, and socket pool startup validation.
- HTTP request handling now validates host headers, request body framing, request ids, method overrides, forwarded client IPs, query parameters, accept headers, CORS configuration, basic auth, static file paths, router matching, websocket routes, and response serialization.
- Suri now returns typed errors for startup configuration, connection handling, protocol handling, static paths, body parsing, session cookie decoding, config environment lookup, CSRF runtime and token unmasking, CORS preflight, liveview protocol, liveview HTML attributes, HTTP/1 validation, and accept quality parsing.
- Sessions and CSRF handling were hardened with HMAC signing, session secret validation, mandatory sessions before CSRF, and structured liveview token/session validation.
- CORS behavior now handles preflight responses as no-content responses and merges `Vary` headers correctly.
- Static file handling now enforces directory roots, dotfile policy, mount boundaries, partial ranges, and no-body response ETag behavior.
- LiveView support now carries typed event payload errors and serializes LiveView errors structurally.
- WebSocket handshakes, frame limits, message flow, and connection writes are validated more carefully.
- App testing helpers, middleware test helpers, and core testing helpers are now exposed as APIs, while the older top-level testing facade was removed.
- Handler exceptions are recovered into structured responses, and fallback unsent response behavior is covered by tests.
