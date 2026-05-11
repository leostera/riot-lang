# Changelog

All notable changes to `gooey` are documented here.

## 0.0.26 - 2026-04-28

### Changed

- Gooey layout and rendering behavior is more stable for nested layouts, clipping, borders, padding, margins, custom commands, unicode text width, and terminal scissor regions.
- Style and config helpers now reject invalid values more consistently and preserve rendering metadata such as text size, z-index, background, borders, and custom render commands.
