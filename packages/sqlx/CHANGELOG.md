# Changelog

All notable changes to `sqlx` are documented here.

## Unreleased

### Fixed

- Accepted MySQL `TEXT` migration-table fields returned by drivers as byte values when validating applied checksums.
- Passed migration bodies through the active driver's migration preparation hook so backend-specific statement handling stays in drivers.

## 0.0.34 - 2026-05-10

### Changed

- SQLx driver contracts, pool behavior, and migration metadata handling were tightened, including support for MySQL migration metadata bytes.

## 0.0.33 - 2026-05-10

### Changed

- Switched connection and protocol error codecs from `Std.Data.Json` callbacks to `Serde.Ser` serializers.
- Updated SQLX driver wiring and test drivers to use serde-backed error serialization.

## 0.0.32 - 2026-05-10

### Added

- Added structured migration source, version, and table errors.
- Added PostgreSQL advisory-lock migration configuration support.
- Added tests for pool validation and migration edge cases.

### Changed

- Reworked transaction isolation and pool status types to regular variants.
- Switched pool identifier generation to `Std.Random`.
- Hardened pool startup, release, and connection validation paths.

### Fixed

- Dropped invalid or stale pooled connections instead of returning them to circulation.
- Caught supervisor exceptions and reported structured runtime errors.

## 0.0.30 - 2026-05-02

### Changed

- SQLx is marked public again in the release manifest, so it is included in the published package set and remains available through the registry.

## 0.0.29 - 2026-05-01

### Added

- Added `Sqlx.Migrate`, a migration API for discovering, applying, and tracking database migrations from SQLx applications.

### Changed

- Expanded SQLx documentation around connections, pools, transactions, and migrations so users have concrete guidance for wiring migration workflows into applications.
