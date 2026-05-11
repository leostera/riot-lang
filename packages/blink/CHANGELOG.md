# Changelog

All notable changes to `blink` are documented here.

## 0.0.34 - 2026-05-10

### Changed

- The HTTP client runtime was simplified around clearer error handling and client lifecycle behavior.

## 0.0.32 - 2026-05-10

### Added

- Added recorder-backed testing helpers under `Blink.Testing`.
- Added connection close callbacks through `?on_close`.
- Added optional transport read timeouts.

### Changed

- Removed retry, policy, and circuit-breaker behavior from the client runtime.
- Reworked transport and protocol failures into structured `Blink.Error` variants.
- Renamed conversion helpers to the `from_*` style.

### Fixed

- Wrapped IO failures internally before exposing them through `Blink.Error`.
- Ensured closed connections report `Error.Closed`.

## 0.0.30 - 2026-05-02

### Removed

- Removed Blink's built-in managed circuit breaker from the HTTP client surface. Applications should own circuit-breaker policy at their API boundary, where they have the context to choose failure thresholds and reset behavior.

## 0.0.26 - 2026-04-28

### Changed

- Blink now follows the stricter HTTP and WebSocket validation introduced in `http`. Client, transport, websocket, and error paths surface protocol errors more consistently instead of accepting malformed frames or request/response metadata.
- Managed HTTP, SSE, and websocket flows continue to work with the hardened protocol layer, including retry, budget, circuit-breaker, request rendering, and SSE parsing behavior.

### Fixed

- Fixed fixed-length HTTP responses on keep-alive connections. Blink now parses response headers separately from response bodies, so a response with `Content-Length` is not consumed and then waited for a second time.

## 0.0.25 - 2026-04-27

### Added

- Added a managed HTTP client layer with request/response types, retry policy, connection and rate budgets, circuit breaker state, and telemetry hooks.
- Added managed HTTP, SSE, and WebSocket examples, plus property and unit tests for retry, budget, circuit breaker, request rendering, and SSE parsing behavior.
