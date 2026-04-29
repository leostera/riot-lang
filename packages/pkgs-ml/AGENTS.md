# pkgs-ml AGENTS

`pkgs-ml` is the reusable library for talking to the pkgs.ml registry surface
and managing on-disk registry cache layout. Keep it registry-focused and free of
Riot-specific workflow policy.

## Rules

1. Keep registry protocol/cache concerns here; `riot-model` should stay the shared data vocabulary.
2. Bubble errors up explicitly.
3. Prefer small, composable modules that `riot-deps` can build on.
4. Use Riot-owned archive and compression APIs for source materialization.
5. Materialization must validate that `src/<pkg>/<version>/riot.toml` exists before treating a package as present. Normalize legacy repo-snapshot archives to package-root layout during extraction.
6. Build package download URLs only from sparse-index `artifact_base_url` plus `source_key`.
7. Keep Riot-agent transport metadata explicit. `pkgs-ml` may expose a small setter for the default `X-Riot-Agent` header, but it should also honor `RIOT_AGENT_HEADER` as a higher-priority override for automation that shells out to `riot` and needs a distinct identity.
8. Mutating registry routes such as publish and yank belong here as small explicit client helpers. Keep them exact-version and bubble registry error messages up unchanged.
9. Filesystem registry/cache initialization errors should stay typed in `pkgs-ml`; render them to strings only at CLI/user-facing edges.
