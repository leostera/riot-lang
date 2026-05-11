# Changelog

All notable changes to `contentstore` are documented here.

## 0.0.26 - 2026-04-28

### Changed

- Object, tree, and named-object writes are safer under concurrency. Same-hash object writers and same-key named writers now converge on a readable value instead of risking corrupt partial state.
- Named-object overwrites preserve a valid old or new value for readers while the overwrite is in progress. This matters for cache and registry metadata paths that may be read while another process is updating them.
- Store operations now return structured errors for missing objects, permission failures, unwritable namespaces, and failed tree commits, so callers can distinguish absent content from real filesystem failures.
- Temporary files from failed object, named-object, and file saves are cleaned up more reliably, reducing stale cache debris after interrupted writes.
