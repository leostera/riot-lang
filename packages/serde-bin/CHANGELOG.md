# Changelog

All notable changes to `serde-bin` are documented here.

## 0.0.35 - 2026-05-11

### Added

- Added fuzz coverage for compact binary decoding.

### Fixed

- Oversized decoded container lengths are rejected before allocating decoder storage.
