# riot-fmt AGENTS

`riot-fmt` owns the `riot fmt` command surface and delegates formatting logic to
`krasny`.

## Rules

1. Keep `riot-fmt` thin. File discovery, checking, and reporting primitives should live in `krasny`.
2. `riot-fmt` should only orchestrate workspace roots, flags, and exit codes.
3. Reserve stdout for formatting results and JSONL events; send unsupported-mode guidance to stderr.
4. Keep `--check` quiet on success, and stream per-file failures as they are discovered.
5. Human formatter failures caused by `syn` parse errors should render the direct `Syn.DiagnosticReporter` output, not a flattened `Parse error ...` summary line.
6. Keep `--verify` as a safety preflight over krasny's syntax-hash roundtrip logic. It should report files that would reformat safely separately from files that are unsafe to format.
7. Keep no-flag `riot fmt` as the in-place rewrite path, and keep it quiet on success. With explicit positional paths, format only those files/directories; without them, keep the existing workspace-scan behavior.
8. Keep `--json` machine-readable, line-delimited, timestamped, and incrementally emitted as `start`/`file`/`summary` events. Failed file events should carry structured diagnostics when the formatter failure came from `syn` parse diagnostics.
9. Keep `krasny` as the formatter backend.
10. Keep `riot fmt --explain <id>` as a thin in-process pass-through to `Syn.Error.explain`.
11. Default workspace scans must stay scoped to workspace members only. Traverse member package roots, not materialized registry caches, detached dependency trees, or other non-member roots loaded into the resolved workspace graph.
