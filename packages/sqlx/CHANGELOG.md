# Changelog

All notable changes to `sqlx` are documented here.

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
