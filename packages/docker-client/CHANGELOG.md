# Changelog

All notable changes to `docker-client` are documented here.

## Unreleased

### Changed

- Hardened Docker host parsing by trimming `DOCKER_HOST` and rejecting empty hosts, empty Unix socket paths, and out-of-range TCP ports.
- Deduplicated exposed container ports when generating Docker create request JSON.
- Grouped repeated Docker port bindings under one container-port key.
- Switched Docker create and inspect JSON handling to `serde-json` with string-keyed dictionary codecs.
- Tightened inspect parsing for Docker port bindings and host port ranges.

## 0.0.34 - 2026-05-10

### Added

- Added a Docker Engine client package for integration workflows that need to create, inspect, and manage containers programmatically.

## 0.0.1 - 2026-05-10

### Added

- Added a minimal Docker Engine API client for Riot packages.
- Added local Docker daemon configuration from `DOCKER_HOST` and `DOCKER_DEFAULT_PLATFORM`.
- Added Unix socket and plain TCP daemon transports.
- Added Docker image pull, container create, start, inspect, logs, remove, and ping helpers.
- Added typed Docker port values and container port mapping helpers.
- Added structured errors for config, transport, HTTP, Docker response, and JSON failures.
- Added testing helpers for deterministic Docker request and inspect parser coverage.
- Added live Docker tests for daemon ping and container lifecycle behavior.
