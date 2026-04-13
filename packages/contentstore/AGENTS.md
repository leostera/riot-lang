# contentstore AGENTS

`contentstore` owns generic content-addressable storage primitives.

## Rules

1. Keep this package generic. Riot-specific artifact manifests, package exports, and build-lane policy belong in higher layers such as `riot-store`.
2. Treat on-disk layout and hash-addressed paths as compatibility-sensitive.
3. Keep writes atomic where practical. Partial cache entries are worse than misses.
4. Prefer small storage primitives over domain-specific helpers. Higher layers should compose these primitives into their own typed caches.
5. This package owns generic object persistence only. Serialization formats, typed decoding, and codec-specific helpers belong in higher layers.
6. `contentstore` is file-backed today. Do not add storage backend abstractions unless a concrete second backend is required.
7. Public namespaces are typed via `Namespace.t`; validate once at the boundary and reuse the typed namespace across store creation.
8. `Store.t` is created from `root + namespace + policy`; store operations should own orchestration and call low-level filesystem helpers directly.
9. Reading should prefer file-handle/object primitives over eager whole-file decode helpers.

## Validate

`timeout 30 riot build contentstore`
`timeout 180 riot test -p contentstore --json`
`timeout 180 riot bench -p contentstore --json`
