# TODO

This file is _yours_. Keep it up to date after every big change.

## How You Work

1. Read this file from top to bottom and pick the next unchecked item that is unblocked.
2. Work until it is complete.
3. Mark a task complete here only after the listed verification has passed.
4. Commit after every slice with a conventional commit message.
5. Prefer `ceibo` / `syn` changes over new `krasny/lower.ml` trivia heuristics.

## Mission

- [x] Make trivia, comments, and docstrings first-class at the token layer so the CST can derive reliable ownership and `krasny` can become a renderer again

## Invariants

- Use token `leading_trivia` only; do not add token `trailing_trivia`.
- Preserve trivia losslessly in `syn`, even when the source placement is weird.
- Keep token spans as token-body-only spans; trivia carries its own spans/text.
- Stop storing trivia as standalone syntax/tree children.
- Derive ownership from token order and item/member sequences, not source-gap archaeology.
- Normalize ugly comment placement in `krasny`, not in `syn`.
- Keep doc ownership leading-only; postfix docstrings stay preserved but standalone.

## Slice Loop

For every future cleanup slice:

- Add or update the smallest regression first.
- Land the smallest code change that improves the post-migration model.
- Run focused tests first.
- Run the slice build command.
- Commit with a scoped conventional commit message.
- Update this file before starting the next slice.

## Completed State

- [x] `ceibo` stores trivia on tokens instead of as standalone tree children.
- [x] `syn` lexer/parser consume token-attached trivia cleanly.
- [x] Top-level and nested CST ownership come from token boundaries and ordered item streams.
- [x] Member ownership is reliable for grouped `type ... and ...`, variant constructors, and record fields.
- [x] Doc kind is explicit in the CST, and doc ownership is leading-only.
- [x] `krasny/lower.ml` no longer does normal-path trivia archaeology.
- [x] `./packages/krasny/tests/test_runner.py --verify-workspace --fail-fast` passes.

## Current State

- Ceibo green/red tokens carry token-attached `leading_trivia`, and trailing file trivia lives on `EOF.leading_trivia`.
- `Lexer.tokenize` emits only real tokens plus `EOF`; non-EOF trivia is attached to the next real token.
- `Parser.parse_result.tokens` and `syn print-ceibo` preserve the original lexer stream, and `Syn.build_cst` uses that token stream instead of re-lexing source text.
- `Token_cursor` treats the main parser stream as real-token-only end-to-end; explicit trivia consumption only exposes token-shaped trivia for green construction.
- Top-level and nested structure/signature ownership run through shared ordered-item passes built from token order.
- Variant constructors and record fields are the only explicit member-stream grammars today.
- `sig ... end` now has an explicit `SIG_EXPR` syntax kind; `syn` no longer recognizes signature module types by string-sniffing an `IDENT_EXPR` token stream.
- `krasny` renders top-level, nested, grouped-type, and record-body ownership from CST streams plus per-node `owned_trivia`; source-preserving fallbacks rebuild text from real token bodies plus later-token `leading_trivia`.

## Maintenance Backlog

- [x] Decide whether `owned_trivia` should stay public as-is or be renamed/simplified now that the token-trivia model is stable.
- [ ] Keep trimming stale migration-era comments/helpers in `packages/syn` and `packages/krasny` when compiler/readability audits surface them.
- [ ] Add new ownership regressions to `syn:cst_tests` before adding formatter-only fixtures.
- [ ] Keep future formatter fixtures renderer/layout-focused; do not reintroduce `lower.ml` ownership archaeology.

## Validate

- `timeout 120 tusk build ceibo syn krasny`
- `timeout 180 tusk test syn:cst_tests`
- `timeout 180 tusk test krasny:format_tests`
- `timeout 300 python3 packages/syn/tests/test_runner.py cst`
- `./packages/krasny/tests/test_runner.py --verify-workspace --fail-fast`
