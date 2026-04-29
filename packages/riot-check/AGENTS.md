# riot-check AGENTS

`riot-check` owns the `riot check` command implementation and package-aware
typing flow.

## Rules

1. Keep `riot-check` focused on typechecking flows, diagnostics rendering, and
   package-aware typing orchestration.
2. `riot-cli` should stay a thin wrapper over this package.
3. Planner, store, and `typ` integration should live here, with `riot-cli`
   staying as the command wrapper.
4. Keep human and JSON output paths behaviorally aligned.
5. Preserve workspace-relative paths and structured diagnostics in both output
   modes.
6. Prefer small, focused modules over centralizing behavior into a single
   `riot_check.ml` god module. Keep responsibilities split across dedicated
   modules such as checking, diagnostics, reporting, errors, and explain
   flows when the package grows.
7. Keep `riot-check` event-driven. The library should emit structured events
   and typed errors; `riot-cli` decides how those events are rendered in human,
   JSON, or quiet modes.
8. Package-level check progress belongs in the event stream too. When `riot
   check` reuses persisted package typings, emit a cache-hit package event.
9. `riot check` is workspace-scoped. Workspace setup and warmup should flow
   through the normal workspace command path.
