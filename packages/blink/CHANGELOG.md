# Changelog

All notable changes to `blink` are documented here.

## 0.0.32 - 2026-05-10

### Added

- Added recorder-backed testing helpers under `Blink.Testing`.
- Added connection close callbacks through `?on_close`.
- Added optional transport read timeouts.

### Changed

- Removed retry, policy, and circuit-breaker behavior from the client runtime.
- Reworked transport and protocol failures into structured `Blink.Error` variants.
- Renamed conversion helpers to the `from_*` style.

### Fixed

- Wrapped IO failures internally before exposing them through `Blink.Error`.
- Ensured closed connections report `Error.Closed`.
