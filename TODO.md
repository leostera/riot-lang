# TODO

This file is _yours_. Keep it up to date after every big change.

## Mission

- [x] Make trivia, comments, and docstrings first-class at the token layer so the CST can derive reliable ownership and `krasny` can become a renderer again
- [ ] Make `krasny` Structural Formatting Only: format from CST structure plus token-attached trivia, never by reparsing or sniffing source text
- [ ] Implement RFD0025 - Snapshot Testing for Riot (./docs/rfds/RFD0025-snapshot-testing-for-riot.md)

## Up Next

- [ ] Delete the dead whole-tree `validate_source_file` scaffold from `packages/syn/src/cst_builder.ml` now that normal CST construction no longer calls it.
- [ ] Keep migrating any real CST invariants into the specific builder helpers that own those facts, instead of reviving post-construction validation.
- [ ] Tighten `syn` to follow the token-first CST contract:
  - tokens own trivia
  - CST preserves original syntax tokens and structure
  - do not duplicate token-owned trivia into convenience fields when the same fact is already structurally reachable
- [ ] Remove redundant expression boundary-trivia fields that duplicate token-owned facts:
  - [ ] `fun_expression.body_leading_trivia`
  - [ ] `sequence_expression.expression_leading_trivia`
  - [ ] `let_binding.leading_trivia`
  - [ ] `let_binding.value_leading_trivia`
  - [ ] `binding_operator_binding.bound_value_leading_trivia`
  - [ ] `let_operator_expression.body_leading_trivia`
  - [ ] `let_expression.bound_value_leading_trivia`
  - [ ] `let_expression.body_leading_trivia`
  - [ ] `match_case.body_leading_trivia`
  - [ ] `if_expression.then_branch_trailing_trivia`
  - [ ] `if_expression.else_branch_leading_trivia`
  - [ ] `parenthesized_expression.inner_leading_trivia`
- [ ] Replace CST booleans that collapse real syntax choices with token-backed structure where the original tokens matter:
  - keep auditing for any remaining bool-only syntax shells that still drop modifier trivia
- [ ] Keep local opens token-backed and grammar-true:
  - do not reintroduce type-side local opens into the parser or CST
- [ ] Keep auditing real `tusk fmt` output for destructive regressions only:
  - dropped comments or docstrings
  - duplicated trivia
  - attribute ownership changes
  - invalid OCaml
- [ ] Treat postfix docstrings as source cleanup, not formatter ownership:
  - keep prefix docs preserved structurally
  - normalize postfix docs in source files instead of teaching `krasny` postfix ownership

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
- [ ] Keep adding focused destructive regressions for formatter output that:
  - drops comments or docstrings
  - duplicates trivia
  - changes attribute ownership
  - emits invalid OCaml
- [ ] Keep nested `sig ... end` / `struct ... end` relift trivia loss covered:
  - nested helper lifts must preserve inter-item comments/docstrings, not just declaration nodes
- [ ] Keep branch-body trivia explicit and covered:
  - comments/docstrings between `->` and a match-case body belong on the case body boundary, not in formatter archaeology

#### Validate

- `timeout 120 tusk build syn krasny fixme tusk-fix`
- `timeout 180 tusk test syn:cst_tests`
- `timeout 180 tusk test krasny:format_tests`
- `timeout 300 python3 packages/syn/tests/test_runner.py cst`
- `timeout 300 python3 packages/syn/tests/test_runner.py cst --refresh-clean`
- `timeout 300 python3 packages/krasny/tests/test_runner.py --filter 051`
- `./packages/krasny/tests/test_runner.py --verify-workspace --fail-fast`
