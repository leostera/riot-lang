# TODO

This file is _yours_. Keep it up to date after every big change.

## Mission

- [x] Make trivia, comments, and docstrings first-class at the token layer so the CST can derive reliable ownership and `krasny` can become a renderer again
- [ ] Make `krasny` Structural Formatting Only: format from CST structure plus token-attached trivia, never by reparsing or sniffing source text
- [ ] Implement RFD0025 - Snapshot Testing for Riot (./docs/rfds/RFD0025-snapshot-testing-for-riot.md)
- [ ] Implement `tusk build --json` so we get jsonl events for builds instead of human-friendly output (this is good for LLMs and machines)

## Up Next

- [ ] Delete the dead whole-tree `validate_source_file` scaffold from `packages/syn/src/cst_builder.ml` now that normal CST construction no longer calls it.
- [ ] Keep migrating any real CST invariants into the specific builder helpers that own those facts, instead of reviving post-construction validation.
- [ ] Run full `./packages/krasny/tests/test_runner.py --verify-workspace --fail-fast` once the current unrelated workspace breakage is out of the way.

### Krasny 

#### `krasny` Audit

- [ ] Delete dead or redundant formatter branches that were only supporting older broader CST shapes.
  - Focus on `lower.ml`.
  - Prefer removing impossible-state handling that the new CST already forbids.
  - grouped module renderers no longer match impossible empty declaration lists now that the CST head declaration is always explicit
  - `render_let_operator_expression` no longer carries an impossible empty rendered-binding branch now that the leading binding is always explicit

- [ ] Re-audit `lower.ml` exhaustiveness and unsupported-shape branches after each cleanup slice.
  - If a branch is impossible with the current CST, delete it.
  - If a branch is valid syntax, either support it structurally or move the missing fact into `syn`.

#### Validate

- `timeout 120 tusk build syn krasny fixme tusk-fix`
- `timeout 180 tusk test syn:cst_tests`
- `timeout 180 tusk test krasny:format_tests`
- `timeout 300 python3 packages/syn/tests/test_runner.py cst`
- `timeout 300 python3 packages/syn/tests/test_runner.py cst --refresh-clean`
- `timeout 300 python3 packages/krasny/tests/test_runner.py --filter 051`
- `./packages/krasny/tests/test_runner.py --verify-workspace --fail-fast`
