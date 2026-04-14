# riot-install AGENTS

`riot-install` owns binary promotion on top of `riot-build`.

## Rules

1. Install should resolve workspace binaries from the workspace model directly. Do not route binary lookup back through `riot-build` client/session helpers.
2. Keep promotion failures fatal. Do not downgrade failed project-root or global promotion into warnings.
3. Source and registry installs should emit PM events through `Riot_build.Event.Pm` payloads so `riot-cli` sees one event stream.
4. Keep path values typed as `Path.t` until the final filesystem or rendering boundary.
5. Model install intent with variants, not boolean flags. Workspace installs and external installs have different invariants; encode that in the request type instead of carrying `local_only` / `promote_*` booleans.

## Validate

`timeout 30 riot build riot-install`
