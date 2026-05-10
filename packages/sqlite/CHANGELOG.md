# Changelog

All notable changes to `sqlite` are documented here.

## 0.0.33 - 2026-05-10

### Changed

- Replaced SQLite driver error JSON callbacks with a serde-backed error serializer.
- Added `serde` as a package dependency to satisfy the updated `sqlx-driver` contract.
