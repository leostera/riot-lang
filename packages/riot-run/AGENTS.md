# riot-run AGENTS

`riot-run` owns binary selection and execution on top of `riot-build`.

## Rules

1. `riot-run` should consume typed `riot-build` outputs directly.
2. Keep binary selection local to the workspace model and executable data returned by `riot-build`.
3. PM/source-loading and run lifecycle events should flow through the standard `on_event` channel as `Riot_model.Event.t` so `riot-cli` can render one event stream.
4. `Path.t` stays typed until the final process execution boundary.
