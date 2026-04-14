# riot-run AGENTS

`riot-run` owns binary selection and execution on top of `riot-build`.

## Rules

1. `riot-run` should consume typed `riot-build` outputs directly instead of round-tripping through store or session APIs to rediscover artifacts by name.
2. Keep binary selection local to the workspace model. Do not reintroduce `riot-build` client/server lookups for executable discovery.
3. PM/source-loading events should be bridged into `Riot_build.Event.Pm` payloads so `riot-cli` can keep rendering one event stream.
4. `Path.t` stays typed until the final process execution boundary.

## Validate

`timeout 30 riot build riot-run`
