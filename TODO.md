# TODO

This file is _yours_. Keep it up to date after every big change.

## Mission

- [x] Make trivia, comments, and docstrings first-class at the token layer so the CST can derive reliable ownership and `krasny` can become a renderer again
- [ ] Make `krasny` Structural Formatting Only: format from CST structure plus token-attached trivia, never by reparsing or sniffing source text

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
- `krasny` renders top-level, nested, grouped-type, and record-body ownership from CST streams plus per-node `owned_trivia`.
- top-level type extensions, exception declarations, and floating attributes now lower structurally; unsupported top-level class/class-type/extension items fail explicitly instead of preserving source text.
- module-expression and module-type extensions now fail explicitly instead of falling through raw `doc_of_node` fallback.
- `Format_core.format` no longer falls back to returning the original source when lowering declines to format.
- `lower.ml` still contains source/text heuristics and one remaining source-backed phrase-boundary preservation path that should be treated as debt.

## Working Style

- Add or update the smallest regression first.
- Prefer compiler-driven cleanup over speculative rewrites.
- Commit every slice with a scoped conventional commit message.
- Prefer `syn:cst_tests` for ownership bugs and `krasny` fixtures for layout/rendering bugs.
- Do not reintroduce ownership heuristics in `krasny` once the CST already knows the answer.

## Structural Formatting Debt

- [ ] Remove source-preserving node fallback from `packages/krasny/src/lower.ml`
  - `text_of_syntax_node`
  - `doc_of_node`
  - `doc_of_source_preserved_syntax_node`
  - `doc_of_source_preserved_syntax_node_from_current_source`
  - `doc_of_source_preserved_syntax_node_span_from_current_source`
  - direct source renderers such as `doc_of_core_type`, `doc_of_module_expression`, `doc_of_module_type`, and `render_parameter`
  - attribute/module-type string reconstruction such as `render_attribute`, `render_first_class_module_type`, `strip_outer_parens_once`, and `strip_module_prefix`
  - remaining non-top-level fallback branches in expression/module/module-type lowering that still end in `doc_of_node (...)`
  - keep unsupported shapes on the explicit `Cannot_lower` path; do not reintroduce silent source preservation

- [x] Remove API-level source-preserving fallback from `packages/krasny/src/format_core.ml` and `packages/krasny/src/lower.ml`
  - `Lower.source_file` now returns an explicit lowering result instead of `None`
  - `Format_core.format` now reports `Cannot_lower` instead of returning `original_source`

- [ ] Remove raw source-gap parsing from top-level structure/signature rendering
  - `source_gap_has_only_phrase_separators`
  - `source_gap_leading_phrase_separator`
  - `source_of_relative_span`
  - `separator_doc_between_offsets`
  - expression-run preservation in `render_structure_top_level_items`
  - top-level suffix insertion that still depends on scanning raw source between item spans

- [ ] Remove raw trivia reparsing helpers from `lower.ml`
  - `parse_trivia_between_offsets`
  - `render_trivia_between_spans`
  - `trailing_inline_comment_suffix`
  - `leading_inline_comment_between_offsets`
  - `split_leading_inline_comment_source`
  - if formatting still needs these, the missing structure belongs in `syn`

- [ ] Remove source-sniffing and token-text heuristics used to make rendering decisions
  - `syntax_node_has_explicit_fun_rhs`
  - `inherited_poly_variant_path_doc`
  - `type_declaration_requires_source_preservation`
  - `value_declaration_name_doc`
  - rendered-source substring checks such as `[@` / `[%%expect]` preservation gates
  - multiline/layout heuristics currently driven by `text_of_syntax_node` or `string_contains_substring`
  - token-text scans over `SyntaxNode.tokens`, e.g. searching for `"="` then `"fun"` or reconstructing poly-variant inherit paths from token text lists

- [ ] Remove node-text-driven layout heuristics from `lower.ml`
  - `syntax_node_has_internal_newline`
  - list edge-spacing checks that sniff `"[ "` / `" ]"` from `text_of_syntax_node`
  - `tuple_source_is_long`
  - `apply_expression_is_simple_after_equals`
  - `expression_source_is_long`
  - `expression_source_has_newline`
  - application multiline preference derived from raw node text length/newlines
  - `render_case` deciding line-breaking from raw `"->\\n"` / `"->\\r\\n"` substrings
  - `let exception` rendering that prints `exception_declaration.syntax_node` text directly

- [ ] Remove source-derived “safe to rewrite” gates from top-level formatting
  - `structure_item_requires_source_preservation_before_expression`
  - `render_structure_entry` / `render_signature_entry` `should_preserve_source`
  - expression-run preservation rules in `render_structure_top_level_items`
  - replace “preserve this source because rewrite might change meaning” with explicit CST facts or hard failure

- [ ] Remove source-slice reconstruction from lowering context setup
  - `render_structure_items ?source ~source_node`
  - `render_signature_items ?source ~source_node`
  - do not derive nested/top-level source windows from `ctx.source` + `source_node` spans just to support fallback formatting

- [ ] Remove public/docs-level assumptions that unsupported shapes are preserved from source
  - `packages/krasny/src/Krasny.mli`
  - `packages/krasny/src/format_core.ml`
  - `packages/krasny/AGENTS.md` rules that still describe source-preserving lowering as an accepted steady-state behavior
  - any AGENTS/docs wording that still describes source-preserving fallback as part of the formatter contract

- [ ] Remove impossible-state fallback patterns from formatter hot paths
  - `assert false` branches in item-run collection/rendering
  - any remaining “best effort” fallback that hides missing structural support instead of surfacing it

- [ ] Audit every `ctx.source` / `Source.*` use in `packages/krasny/src/lower.ml`
  - classify each site as:
    remove,
    replace with an explicit CST fact,
    or keep only behind an explicit “unsupported shape” failure boundary while the CST is extended
  - current high-priority sites:
    `Source.source_of_span`,
    `Source.source_of_syntax_node`,
    `Source.source_of_node_from_source`,
    `Source.source_between`,
    `Source.source_of_parameter`,
    `Source.syntax_node_has_comment_like_trivia`

- [ ] Decide which missing structural facts belong in `syn` so `krasny` can stop guessing
  - explicit phrase-separator / top-level phrase-boundary modeling
  - explicit value-declaration printable name modeling
  - explicit ambiguity-sensitive type-declaration shape markers
  - explicit poly-variant inherit path rendering data if needed

- [ ] Add regression coverage before removing each heuristic
  - use `syn:cst_tests` when the missing fact is ownership/structure
  - use `krasny` fixtures when the issue is purely rendering/layout

- [ ] Keep trimming stale rollout-era comments, helper knobs, and redundant branches in `packages/syn` and `packages/krasny` when compiler/readability audits surface them.

- [ ] If formatter UX work resumes, make `tusk fmt <file>` default to formatting just that file instead of walking the whole workspace.

## Validate

- `timeout 120 tusk build ceibo syn krasny`
- `timeout 180 tusk test syn:cst_tests`
- `timeout 180 tusk test krasny:format_tests`
- `timeout 300 python3 packages/syn/tests/test_runner.py cst`
- `./packages/krasny/tests/test_runner.py --verify-workspace --fail-fast`
