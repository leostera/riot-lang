# krasny AGENTS

`krasny` is Riot's owned OCaml formatter. It formats clean `syn` parse results through the streaming writer-oriented formatter.

## Rules

1. Keep one formatting pipeline. `Krasny.format`, `Krasny.write`, `Krasny.stream_format`, and `Krasny.stream_format_to_string` should all use the same stream formatter path.
2. Format only successful parser results. Treat parser recovery or unsupported syntax shapes as formatter diagnostics that need first-class support.
3. Keep the public surface writer-oriented and `Std.IO` friendly. String helpers should stay convenience wrappers around the writer path.
4. Use `Syn.Ast` semantic views and module-specific helpers. Ask `syn` for typed views when formatting needs structure.
5. Keep layout policy explicit in `layout_policy.ml`. Width checks, named layout reasons, and tracing live there; renderers execute the selected mode.
6. Keep rendering context explicit and per invocation. Thread formatter state through the call path.
7. Treat comments and docstrings as structural formatting input. Preserve meaningful comment content and indentation, collapse meaningless whitespace, and let Krasny choose spaces/newlines.
8. Keep workspace formatting runners streaming-friendly. File discovery and per-file check results should flow incrementally.
9. Keep OCaml class/object syntax outside formatter scope while `syn` excludes it.
10. Add focused inline snapshots or fixture coverage before changing broad formatter policy.
11. Keep fixture taxonomy curated. When a real-file regression exposes missing behavior, add the smallest representative case to the relevant fixture family.
12. `--verify` is a normalized syntax-hash safety preflight, not another formatting-state check. Report files that would reformat safely separately from files that are unsafe to format.

Audit fixture taxonomy and duplicate pressure manually when curating the corpus by reviewing `tests/FIXTURES.md` and `tests/format_expectations.txt` together before adding overlapping cases.
