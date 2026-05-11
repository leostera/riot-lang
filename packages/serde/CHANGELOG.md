# Changelog

All notable changes to `serde` are documented here.

## 0.0.35 - 2026-05-11

### Added

- Added fuzz coverage for the BSON, CBOR, binary, TOML, YAML, and URL-encoded serde codecs.

### Fixed

- `serde-bin` now rejects oversized container lengths before allocating decoder storage.

## 0.0.34 - 2026-05-10

### Added

- Added string-keyed `dict` codecs and renamed the older map backend to match that shape. BSON and URL-encoded codecs now implement the new backend, so generic dictionary serialization works across the release set.

## 0.0.18 - 2026-04-15

### Added

- Added schema-driven codec surface and format packages (`serde-cbor`, `serde-bson`, `serde-yaml`, `serde-urlencoded`) and evolved compact binary codec support.

### Changed

- Expanded `serde-bin` with new codec variants, native fast paths, and additional benchmarked encode/decode scenarios.
- Reworked serde property/test suites and formatter updates for ongoing API and package migrations.
