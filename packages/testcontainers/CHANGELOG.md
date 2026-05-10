# Changelog

All notable changes to `testcontainers` are documented here.

## Unreleased

### Added

- Added `Testcontainers.docker_available` for packages that need to skip container-backed tests when Docker is unavailable.

## 0.0.1 - 2026-05-10

### Added

- Added test-oriented container lifecycle helpers on top of `docker-client`.
- Added `Generic_image` builders with labeled, pipeline-friendly combinators.
- Added readiness policies for running containers, log messages, healthchecks, and fixed delays.
- Added `start` and `with_container` entry points with explicit cleanup.
- Added container host, published port, log, URL, and removal helpers.
- Added `Container.host` and `Container.host_port` APIs that return `Std.Net.Addr` values.
- Added `Container.url ~scheme` for building `Std.Net.Uri` values from published container ports.
- Added live Docker lifecycle coverage using a managed busybox container.
