# Changelog

All notable changes to `pkgs-ml` are documented here.

## 0.0.32 - 2026-05-10

### Changed

- Updated registry error rendering for Blink's structured protocol and handshake errors.
- Replaced generated temporary binders in registry parsing with meaningful names.

## 0.0.26 - 2026-04-28

### Changed

- Registry materialization now returns regular result values for cached and downloaded release trees. Callers can distinguish successful reuse, fresh materialization, and transport/cache failures without relying on exceptions.
- Filesystem registry caches now handle stale config, corrupt cached archives, gzipped archives, missing package documents, and publish/yank routes more predictably.
