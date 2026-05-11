# Changelog

All notable changes to `postgres` are documented here.

## 0.0.35 - 2026-05-11

### Added

- Added fuzz coverage for PostgreSQL frontend writers, backend message reading, and connection-string parsing.

### Changed

- Implemented the SQLx driver migration preparation hook as a single-statement pass-through.

## 0.0.34 - 2026-05-10

### Changed

- PostgreSQL adapter errors are now structured more consistently, and container-backed coverage was expanded.

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

## 0.0.30 - 2026-05-02

### Changed

- PostgreSQL protocol parsing is more defensive and better documented, with broader coverage for invalid or partial wire messages. Driver behavior is clearer around malformed server input while preserving typed error reporting.

## 0.0.29 - 2026-05-01

### Changed

- PostgreSQL connection writes are now serialized through the driver, preventing concurrent operations from interleaving wire-protocol messages on the same connection.

## 0.0.25 - 2026-04-27

### Added

- Added PostgreSQL password authentication support, including cleartext, MD5, and SCRAM-SHA-256 handshake handling.

### Changed

- Extended protocol parsing and writing for SASL authentication messages while preserving structured PostgreSQL error rendering.
