# riot-run AGENTS

`riot-run` owns binary selection and execution on top of `riot-build`.

## Rules

1. `riot-run` should consume typed `riot-build` outputs directly.
2. Keep binary selection local to the workspace model and executable data returned by `riot-build`.
3. PM/source-loading events should be bridged into `Riot_build.Event.Pm` payloads so `riot-cli` can keep rendering one event stream.
4. `Path.t` stays typed until the final process execution boundary.
