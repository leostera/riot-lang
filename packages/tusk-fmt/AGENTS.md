# tusk-fmt AGENTS

`tusk-fmt` owns the `tusk fmt` command surface and delegates formatting logic to
`krasny`.

## Rules

1. Keep `tusk-fmt` thin. File discovery, checking, and reporting primitives should live in `krasny`.
2. `tusk-fmt` should only orchestrate workspace roots, flags, and exit codes.
3. Reserve stdout for formatting results and JSONL events; send unsupported-mode guidance to stderr.
4. Keep `--check` output streaming per file; do not buffer the full workspace before emitting results.
5. Keep `--json` machine-readable, line-delimited, timestamped, and incrementally emitted as `start`/`file`/`summary` events.
6. Do not reintroduce an `ocamlformat` dependency here; `krasny` is the formatter backend.

## Validate

`timeout 30 tusk build tusk-fmt`
`timeout 30 tusk build tusk-cli`
