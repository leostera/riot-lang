# pkgs-ml AGENTS

`pkgs-ml` is the reusable library for talking to the pkgs.ml registry surface
and managing on-disk registry cache layout. Keep it registry-focused and free of
Tusk-specific workflow policy.

## Rules

1. Keep registry protocol/cache concerns here, not in `tusk-model`.
2. Bubble errors up instead of hiding them behind fallback behavior.
3. Prefer small, composable modules that `tusk-deps` can build on.
4. Validate each slice with focused build and package tests.
5. Use Riot-owned archive and compression APIs for source materialization instead of shelling out to external `tar` commands.

## Validate

`timeout 30 tusk build pkgs-ml`
`timeout 30 tusk test -p pkgs-ml`
