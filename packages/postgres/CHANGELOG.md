# Changelog

All notable changes to `postgres` are documented here.

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
