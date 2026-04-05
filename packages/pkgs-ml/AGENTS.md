# pkgs-ml AGENTS

`pkgs-ml` is the reusable library for talking to the pkgs.ml registry surface
and managing on-disk registry cache layout. Keep it registry-focused and free of
Riot-specific workflow policy.

## Rules

1. Keep registry protocol/cache concerns here, not in `riot-model`.
2. Bubble errors up instead of hiding them behind fallback behavior.
3. Prefer small, composable modules that `riot-deps` can build on.
4. Validate each slice with focused build and package tests.
5. Use Riot-owned archive and compression APIs for source materialization instead of shelling out to external `tar` commands.
6. Materialization must validate that `src/<pkg>/<version>/riot.toml` exists before treating a package as present. Legacy repo-snapshot archives should be normalized to package-root layout during extraction instead of leaking nested repo roots into the cache.
7. Build package download URLs only from sparse-index `artifact_base_url` plus `source_key`. Do not hardcode `cdn.pkgs.ml` or reconstruct artifact paths outside the index contract.
8. Keep Riot-agent transport metadata explicit. `pkgs-ml` may expose a small setter for the default `X-Riot-Agent` header, but it should also honor `RIOT_AGENT_HEADER` as a higher-priority override for automation that shells out to `riot` and needs a distinct identity.

## Validate

`timeout 30 riot build pkgs-ml`
`timeout 30 riot test -p pkgs-ml`
