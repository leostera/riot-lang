# TODO

This file is _yours_. Keep it up to date after every big change.

## Mission

- [x] Make trivia, comments, and docstrings first-class at the token layer so the CST can derive reliable ownership and `krasny` can become a renderer again
- [ ] Make `krasny` Structural Formatting Only: format from CST structure plus token-attached trivia, never by reparsing or sniffing source text
- [ ] Implement RFD0025 - Snapshot Testing for Riot (./docs/rfds/RFD0025-snapshot-testing-for-riot.md)
- [ ] Implement specs/TODO.md

## Stable Contracts

- Use token `leading_trivia` only; do not add token `trailing_trivia`.
- Preserve trivia losslessly in `syn`, even when the source placement is weird.
- Keep token spans as token-body-only spans; trivia carries its own spans/text.
- Stop storing trivia as standalone syntax/tree children.
- Derive ownership from token order and item/member sequences, not source-gap archaeology.
- Normalize ugly comment placement in `krasny`, not in `syn`.
- Keep doc ownership leading-only; postfix docstrings stay preserved but standalone.
- Structural formatting only:
  no reparsing source,
  no source sniffing,
  no string heuristics for ownership or rendering decisions.
- If `krasny` cannot format a shape structurally, formatting should fail until `syn` or the CST exposes the missing fact.

## Up Next

- [ ] Do a `krasny` dead-branch cleanup pass now that the CST is tighter.
- [ ] Burn down the remaining unsupported valid syntax in `krasny` one shape at a time.
- [ ] Run full `./packages/krasny/tests/test_runner.py --verify-workspace --fail-fast` once the current unrelated workspace breakage is out of the way.

## `krasny` Audit

- [x] Remove the stale nested non-`struct ... end` module-expression failure path.
  - `render_module_expression_doc` now only requests nested structure-item relift from the real `ModuleExpression.Structure` branch.
  - Non-`struct` nested module expressions continue to render as ordinary module expressions instead of flowing through an impossible nested-item helper path.

- [x] Restore structural support for valid interface-side module aliases.
  - `Syn.Cst.ModuleSignature` now distinguishes `module M : S` from `module M = N`.
  - `.mli` module alias surfaces such as `module Addr = Addr` format structurally again instead of failing during CST lifting.
  - the reported workspace failures in `gooey`, `http2/connection`, `kernel`, and `miniriot` now pass targeted workspace verification again.

- [ ] Delete dead or redundant formatter branches that were only supporting older broader CST shapes.
  - Focus on `lower.ml`.
  - Prefer removing impossible-state handling that the new CST already forbids.
  - grouped module renderers no longer match impossible empty declaration lists now that the CST head declaration is always explicit
  - `render_let_operator_expression` no longer carries an impossible empty rendered-binding branch now that the leading binding is always explicit

- [ ] Re-audit `lower.ml` exhaustiveness and unsupported-shape branches after each cleanup slice.
  - If a branch is impossible with the current CST, delete it.
  - If a branch is valid syntax, either support it structurally or move the missing fact into `syn`.

## `syn` Audit

- [ ] Keep `open` and `include` shared for now, but treat them as the main remaining intentionally broad CST nodes.
- [ ] If either starts forcing formatter guesses later, split or narrow that CST surface then.

## Validate

- `timeout 120 tusk build syn krasny fixme tusk-fix`
- `timeout 180 tusk test syn:cst_tests`
- `timeout 180 tusk test krasny:format_tests`
- `timeout 300 python3 packages/syn/tests/test_runner.py cst`
- `timeout 300 python3 packages/syn/tests/test_runner.py cst --refresh-clean`
- `timeout 300 python3 packages/krasny/tests/test_runner.py --filter 051`
- `./packages/krasny/tests/test_runner.py --verify-workspace --fail-fast`
