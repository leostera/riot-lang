# TODO

This file is _yours_. Keep it up to date after every big change.

## Mission

- [x] Make trivia, comments, and docstrings first-class at the token layer so the CST can derive reliable ownership and `krasny` can become a renderer again

## Stable Contracts

- Use token `leading_trivia` only; do not add token `trailing_trivia`.
- Preserve trivia losslessly in `syn`, even when the source placement is weird.
- Keep token spans as token-body-only spans; trivia carries its own spans/text.
- Stop storing trivia as standalone syntax/tree children.
- Derive ownership from token order and item/member sequences, not source-gap archaeology.
- Normalize ugly comment placement in `krasny`, not in `syn`.
- Keep doc ownership leading-only; postfix docstrings stay preserved but standalone.
- Keep source-preserving fallback in `krasny` limited to unsupported or ambiguity-sensitive rendering shapes, not ownership recovery.

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

## Working Style

- Add or update the smallest regression first.
- Prefer compiler-driven cleanup over speculative rewrites.
- Commit every slice with a scoped conventional commit message.
- Prefer `syn:cst_tests` for ownership bugs and `krasny` fixtures for layout/rendering bugs.
- Do not reintroduce ownership heuristics in `krasny` once the CST already knows the answer.

## Next Steps

- [ ] Audit the remaining source-preserving fallback sites in `packages/krasny/src/lower.ml` and classify them as either necessary syntax preservation or removable cleanup debt.
- [ ] Keep trimming stale rollout-era comments, helper knobs, and redundant branches in `packages/syn` and `packages/krasny` when compiler/readability audits surface them.
- [ ] Add direct `syn:cst_tests` ownership regressions whenever a real workspace formatting case exposes an ownership bug, before adding formatter-only fixtures.
- [ ] Keep future formatter fixtures renderer/layout-focused; do not let them become a substitute for CST ownership coverage.
- [ ] If formatter UX work resumes, make `tusk fmt <file>` default to formatting just that file instead of walking the whole workspace.

## Validate

- `timeout 120 tusk build ceibo syn krasny`
- `timeout 180 tusk test syn:cst_tests`
- `timeout 180 tusk test krasny:format_tests`
- `timeout 300 python3 packages/syn/tests/test_runner.py cst`
- `./packages/krasny/tests/test_runner.py --verify-workspace --fail-fast`
