# Changelog

All notable changes to `minttea` are documented here.

## 0.0.26 - 2026-04-28

### Changed

- Minttea FPS ticks are more regular, which makes time-driven terminal programs less dependent on uneven render-loop timing.
- Renderer, IO loop, text input, cursor, sprite, and program paths were tightened so Elm-style terminal apps behave more consistently with the updated Gooey and TTY rendering layers.
