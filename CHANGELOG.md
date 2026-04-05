# Changelog

## 0.0.12 - 2026-04-06

### Added

- Added `Actors.spawn_pinned`, `Actors.spawn_blocked`, and scheduler support for pinned and blocking actor placement.
- Added detached single-package build support so `riot build` now works from a standalone package root with only a package-level `riot.toml`.
- Added `./scripts/release.sh` to automate version bumps, changelog updates, tagging, and Riot release orchestration.

### Changed

- `riot` CLI workspace resolution now scans upward once, preferring an enclosing workspace manifest and otherwise synthesizing a one-package workspace from the nearest package manifest.
- Riot release automation now supports manifest-aware all-target releases from `./scripts/release/riot.sh all`.

### Docs

- Marked implemented RFDs as `implemented`, including the pinned/blocking actor runtime work.

## 0.0.10 - 2026-04-06

### Added

- Added the new `riot-doc` package for documentation generation, HTML rendering, doctree/source transforms, and workspace interface docs generation.
- Added manual cache GC and generation receipts in `riot-store`, plus workspace operational cache config via `.riot/config.toml`.
- Added published toolchain manifests and `riot toolchain list-available` / `riot toolchains list-available`.
- Added JSON event output for `riot doc`.
- Added rooted snapshot preparation and broader session-driven improvements in `typ`.
- Added richer analytics and registry dashboards across the `pkgs.ml` services.

### Changed

- `riot clean` now performs policy-aware cache cleanup, while successful builds record generation metadata without automatically reclaiming cache entries.
- `riot install` now fails when promotion fails instead of reporting a warning and continuing.
- `riot-doc`, `riot-cli`, and related command surfaces gained structured JSON output improvements.
- Package detail, activity, stats, and mobile layouts on `pkgs.ml` were substantially refined.
- The repository now carries a default `.riot/config.toml`.

### Fixed

- Fixed stale exported build artifacts after cache cleanup so warm rebuilds stay warm after `riot clean`.
- Fixed `pkgs.ml` desktop readme/package section behavior and removed the mobile theme toggle.
- Fixed `riot` agent propagation for access analytics and process stamping in the package services.
- Fixed a dead tail in Suri liveview process handling.
- Improved Neovim Riot diagnostic float rendering.

### Docs

- Reworked the `typ` RFD/spec stack with expanded algorithm notes, examples, diagnostics, and semantic slices.
- Split the cache and operational config RFDs into:
  - `RFD0032 - Riot Cache and GC`
  - `RFD0038 - Riot Workspace Operational Config`
- Refreshed workspace documentation around toolchains and typing internals.
