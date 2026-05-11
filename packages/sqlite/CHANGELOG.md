# Changelog

All notable changes to `sqlite` are documented here.

## 0.0.35 - 2026-05-11

### Changed

- Implemented the SQLx driver migration preparation hook as a single-statement pass-through.

## 0.0.34 - 2026-05-11

### Added

- Replaced the stub adapter with a native `sqlite3`-backed `sqlx-driver` implementation.
- Added support for connections, prepared parameters, row decoding, affected counts, transactions, and SQLite isolation-level configuration.
- Added `Sqlite.Testing.with_db` for disposable file-backed and in-memory test databases.
- Added native SQLite stubs and package target flags for linking against the system `sqlite3` library.
- Added focused unit, property, fuzz, and trace-smoke coverage for SQLite driver behavior.

### Changed

- Split the SQLite implementation into focused internal modules while keeping `Sqlite` as the public facade.
- Switched SQLite configuration modes from polymorphic variants to concrete variant types.

## 0.0.33 - 2026-05-10

### Changed

- Replaced SQLite driver error JSON callbacks with a serde-backed error serializer.
- Added `serde` as a package dependency to satisfy the updated `sqlx-driver` contract.
