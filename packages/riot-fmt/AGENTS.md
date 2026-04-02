# riot-fmt AGENTS

`riot-fmt` owns the `riot fmt` command surface and delegates formatting logic to
`krasny`.

## Rules

1. Keep `riot-fmt` thin. File discovery, checking, and reporting primitives should live in `krasny`.
2. `riot-fmt` should only orchestrate workspace roots, flags, and exit codes.
3. Reserve stdout for formatting results and JSONL events; send unsupported-mode guidance to stderr.
4. Keep `--check` output streaming per file; do not buffer the full workspace before emitting results.
5. Keep `--verify` as a safety preflight over krasny's syntax-hash roundtrip logic. It should report files that would reformat safely separately from files that are unsafe to format.
6. Keep no-flag `riot fmt` as the in-place rewrite path, and keep it quiet on success. With explicit positional paths, format only those files/directories; without them, keep the existing workspace-scan behavior.
7. Keep `--json` machine-readable, line-delimited, timestamped, and incrementally emitted as `start`/`file`/`summary` events.
8. Do not reintroduce an `ocamlformat` dependency here; `krasny` is the formatter backend.

## Validate

`timeout 30 riot build riot-fmt`
`timeout 30 riot test riot-fmt:fmt_tests`
`timeout 30 riot build riot-cli`
