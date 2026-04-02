# riot-store AGENTS

`riot-store` owns the artifact cache and its on-disk layout.

## Rules

1. Treat manifest layout and hash addressing as compatibility-sensitive.
2. Keep writes atomic where practical. Partial cache entries are worse than misses.
3. Store logic should not know about CLI or session behavior.
4. Cache roots and export manifests are scoped by build lane (`profile` + `target`); do not assume host-default cache paths when serving cross-build artifacts.
5. Artifact manifests must preserve cached `ocamlc_warnings` so package warnings can be replayed without rebuilding.
6. Package export manifests participate in warm cached-package reuse. Keep them sufficient for export materialization without forcing planner bundle decode.

## Validate

`timeout 30 riot build riot-store`
