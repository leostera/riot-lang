# krasny AGENTS

`krasny` is Riot's owned OCaml formatter.

## Rules

1. Keep `krasny` as the single rendering pipeline for formatted OCaml output; do not grow a separate fix-only printer beside it.
2. Format only from a successful CST lift; do not pretty-print broken files or add a token-replay fallback for them.
3. Start with deterministic valid OCaml output before chasing aesthetic heuristics.
4. Keep the public surface writer-oriented and `Std.IO`-friendly.
5. Treat comments and trivia as part of the formatter design, not as a post-processing hack.
6. Keep workspace formatting runners streaming-friendly: file discovery and per-file check results should be able to flow incrementally instead of requiring a full precollected file list.
7. Keep the active fixture manifest intentionally curated; prefer one category corpus per supported syntax band and add individual edge-case fixtures only after real code exposes a regression. Use `tests/FIXTURES.md` and `tests/fixture_audit.py` before adding overlapping cases.
8. When a copied real-file regression exposes a missing formatter behavior, add the smallest representative example back into the relevant `0X00` category corpus so the feature is isolated before or alongside the fix.
9. `--verify` is a normalized semantic-hash safety preflight, not another formatting-state check. Report files that would reformat safely separately from files that are unsafe to format.
10. Keep lowering context explicit and per-invocation. Do not reintroduce global mutable source/render state in `Lower`; each format run must be multicore-safe on its own.
11. Preserve standalone top-level docstrings and section headers. Treat odoc section docs and markdown-style `# ...` doc blocks as section boundaries, not declaration-owned docs to be dropped or reassigned.
12. Render variant constructor docs from `Syn.Cst.VariantConstructor.owned_trivia.leading`; do not pull docstrings backward from later gaps or EOF once `syn` has assigned leading-only constructor ownership.
13. Render record field docs from `Syn.Cst.RecordField.owned_trivia.leading`, and preserve terminal `}`-owned comment/doc trivia inside the record body instead of stealing them for the last field.
14. Use `Syn.Cst.Docstring.kind` for normal doc-vs-section decisions when lowering CST-owned docstrings; do not resniff raw docstring text once `syn` has made that distinction explicit.
15. Render top-level source files from the ordered `SourceFile.items` stream plus each item's `owned_trivia`; do not reparse raw source gaps there to rediscover standalone comments/docstrings or declaration docs.
16. Render nested `sig ... end` and `struct ... end` bodies from `Syn.CstBuilder.signature_items_of_module_type` and `Syn.CstBuilder.structure_items_of_module_expression` directly; do not keep nested-only trailing-comment or source-gap recovery once those helper streams are token-order-complete.

## Validate

`timeout 30 tusk build krasny`
`timeout 30 tusk test krasny:format_tests`
`timeout 900 python3 packages/krasny/tests/test_runner.py`

Target individual fixture subsets when needed:
`timeout 900 python3 packages/krasny/tests/test_runner.py --filter 0100`
`timeout 900 python3 packages/krasny/tests/test_runner.py --refresh`

Audit fixture taxonomy and duplicate pressure when curating the corpus:
`python3 packages/krasny/tests/fixture_audit.py`
