# Changelog

All notable changes to `human-units` are documented here.

## 0.0.34 - 2026-05-11

### Added

- Added the `Human_units` package for human-readable byte and duration formatting.
- Added byte parsing for binary IEC units such as `KiB`/`MiB` and decimal units such as `KB`/`MB`.
- Added duration parsing for nanoseconds through years, including `us` and `µs`.
- Added structured parser errors instead of exception-based parse failures.
- Added focused unit, property, and fuzz coverage for the public entrypoints.
