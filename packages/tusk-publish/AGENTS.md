# tusk-publish AGENTS

`tusk-publish` owns the top-level `tusk publish` command flow. It orchestrates `fmt --check`, `fix --check`, `build`, metadata validation, and final artifact upload by composing `tusk-fmt`, `tusk-fix`, `tusk-build`, and `tusk-deps`.

## Rules

1. Keep the command-level orchestration here, not in `tusk-cli` and not in `tusk-deps`.
2. `tusk-cli` should only parse `matches`, build a `Tusk_publish.publish_request`, call `Tusk_publish.publish`, and render `publish_event`.
3. Preserve the preflight order: `fmt --check`, `fix --check`, `build`, metadata validation, then artifact creation/upload.
4. Dry-run mode must emit the same preflight activity as a real publish, minus the final upload.
5. Prefer reusing typed events from `tusk-fmt`, `tusk-fix`, `tusk-build`, and PM/model surfaces instead of inventing parallel ad hoc status strings.

## Validate

`timeout 30 tusk build tusk-publish`
