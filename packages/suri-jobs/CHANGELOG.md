# Changelog

All notable changes to `suri-jobs` are documented here.

## Unreleased

### Added

- Added MySQL-backed `suri-jobs` storage using `packages/mysql`, with matching migrations and testcontainer coverage.

### Changed

- Switched SQL integration tests to `testcontainers` so they start isolated PostgreSQL and MySQL containers instead of relying on external databases.

## 0.1.0 - 2026-05-10

### Added

- Added supervised background job queues for Suri applications.
- Added in-memory and SQLX-backed stores.
- Added PostgreSQL schema migration support with advisory locks.
- Added dashboard routes for queue and job inspection.
- Added fanout, uniqueness, retry, scheduling, and worker supervision tests.

### Changed

- Loaded `.env.test` during tests so live database URLs are available locally.
- Reported store, route, and database failures through structured error variants.
