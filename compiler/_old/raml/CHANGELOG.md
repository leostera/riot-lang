# Changelog

All notable changes to `raml` are documented here.

## 0.0.18 - 2026-04-15

### Added

- Added and split the compiler stack into dedicated packages (`asm`, `raml-core`, `raml-native`, `raml-js`, `raml-wasm`, `raml-cli`).
- Added new MIR/LIR/WIR/native/JS back-end features: scheduling, legalization, cse/liveness/passes, stack/home allocation, entity/purity/jir simplifications, and runtime import/module support.
- Added artifact-store and codegen improvements for Wasm/native toolchains, including layout and decoding/encoding-path hardening.
- Added many compiler fixtures, snapshots, and regression cases across native, Wasm, and JS tool paths.

### Changed

- Expanded JS backend lowering and passes (JIR/JST tooling, property/intrinsic/object/records/modules flows) and improved backend-specific test coverage and docs.
