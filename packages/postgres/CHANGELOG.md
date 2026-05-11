# Changelog

All notable changes to `postgres` are documented here.

## Unreleased

### Changed

- Implemented the SQLx driver migration preparation hook as a single-statement pass-through.

## 0.0.33 - 2026-05-10

### Added

- Added testcontainers-backed PostgreSQL e2e coverage with property tests for value roundtrips, CRUD-style workloads, transactions, and generated operation sequences.
- Added `propane` and `testcontainers` dev dependencies for live container testing.

### Changed

- Split the PostgreSQL adapter into config, driver, and value codec modules behind the existing public facade.
- Replaced public and protocol error JSON helpers with serde serializers and deserializers.
- Moved `bytea` text handling onto the shared `Encoding.Base16` helpers.

## 0.0.32 - 2026-05-10

### Added

- Added structured connection-string parse errors.
- Added structured driver errors for authentication, TLS, and protocol messages.
- Added `.env.test` loading for live PostgreSQL tests through `dotenv`.

### Changed

- Updated protocol and binary-reader docstrings to markdown syntax.
- Returned typed parse errors from `Postgres.Config.from_string`.

### Fixed

- Reused `SURI_JOBS_TEST_POSTGRES_URL` as a live test fallback when package-specific URLs are absent.
