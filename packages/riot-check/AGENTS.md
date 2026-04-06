# riot-check AGENTS

`riot-check` owns the `riot check` command implementation and package-aware
typing flow.

## Rules

1. Keep `riot-check` focused on typechecking flows, diagnostics rendering, and
   package-aware typing orchestration.
2. `riot-cli` should stay a thin wrapper over this package.
3. Planner, store, and `typ` integration should live here rather than
   accreting back into `riot-cli`.
4. Keep human and JSON output paths behaviorally aligned.
5. Preserve workspace-relative paths and structured diagnostics in both output
   modes.

## Validate

`timeout 30 riot build riot-check`
