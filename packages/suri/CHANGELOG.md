# Changelog

All notable changes to `suri` are documented here.

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
