# Changelog

All notable changes to `swisstable` are documented here.

## 0.0.26 - 2026-04-28

### Changed

- SwissTable behavior was tightened for insertion, removal, tombstone reuse, overwrite, clear, resize, entry APIs, collision handling, and iteration.
- Complex record, tuple, variant, and nested keys now behave consistently with the standard hash-map model across long operation sequences.
