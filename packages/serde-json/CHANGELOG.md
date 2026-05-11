# Changelog

All notable changes to `serde-json` are documented here.

## 0.0.35 - 2026-05-11

### Added

- Added fuzz coverage for JSON decoding and schema-driven serde roundtrips.

## 0.0.19 - 2026-04-22

### Changed

- Continued IO/runtime hardening and performance work across `std`, `kernel`, `http`, and `serde-json`, including vectored TLS fallback support and lower-overhead buffer/reader paths.
