# riot-publish AGENTS

`riot-publish` owns the top-level `riot publish` command flow. It orchestrates `fmt --check`, `fix --check`, `build`, metadata validation, and final artifact upload by composing `riot-fmt`, `riot-fix`, `riot-build`, and `riot-deps`.

## Rules

1. Keep the command-level orchestration here, not in `riot-cli` and not in `riot-deps`.
2. `riot-cli` should only parse `matches`, build a `Riot_publish.publish_request`, call `Riot_publish.publish`, and render `publish_event`.
3. Preserve the preflight order: `fmt --check`, `fix --check`, `build`, metadata validation, then artifact creation/upload.
4. Dry-run mode must emit the same preflight activity as a real publish, minus the final upload.
5. Prefer reusing typed events from `riot-fmt`, `riot-fix`, `riot-build`, and PM/model surfaces. Keep enough structure on publish events for `riot-cli publish --json` to render stable JSONL directly.
6. Keep preflight skip behavior explicit and narrow. If a caller skips `fmt` or `fix`, the request should carry a named flag and the skipped stage should be omitted rather than silently reported as successful.
