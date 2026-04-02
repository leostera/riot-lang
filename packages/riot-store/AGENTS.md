# riot-store AGENTS

`riot-store` owns the artifact cache and its on-disk layout.

## Rules

1. Treat manifest layout and hash addressing as compatibility-sensitive.
2. Keep writes atomic where practical. Partial cache entries are worse than misses.
3. Store logic should not know about CLI or session behavior.
4. Cache roots and export manifests are scoped by build lane (`profile` + `target`); do not assume host-default cache paths when serving cross-build artifacts.
5. Artifact manifests must preserve cached `ocamlc_warnings` so package warnings can be replayed without rebuilding.
6. Package exports now live in the hash-addressed artifact `manifest.json`. Do not reintroduce a second payload manifest for package exports.
7. Do not add package-name export lookup scans over the cache. Callers should hold the build result or hash they need and derive export paths from that explicit data.

## Validate

`timeout 30 riot build riot-store`
