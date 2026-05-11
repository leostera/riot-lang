# Changelog

All notable changes to `sqlx-driver` are documented here.

## Unreleased

### Added

- Added a driver migration preparation hook for backend-specific statement handling.

## 0.0.33 - 2026-05-10

### Changed

- Replaced driver error JSON callbacks with `Serde.Ser` error serializers.
- Added `serde` as a driver-interface dependency so adapters can share typed error codecs.

## 0.0.32 - 2026-05-10

### Changed

- Reworked driver isolation levels into regular variants.
- Converted public interface documentation to markdown docstrings.
- Tightened row and value interfaces around structured conversion behavior.

### Fixed

- Removed unnecessary polymorphic variant usage from public driver contracts.
