# TODO

This file is _yours_. Keep it up to date after every big change.

## Mission

- [x] Make trivia, comments, and docstrings first-class at the token layer so the CST can derive reliable ownership and `krasny` can become a renderer again
- [ ] Make `krasny` Structural Formatting Only: format from CST structure plus token-attached trivia, never by reparsing or sniffing source text
- [ ] Implement RFD0025 - Snapshot Testing for Riot (./docs/rfds/RFD0025-snapshot-testing-for-riot.md) -- that document includes an itemized description of how to implement
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
- nested `sig ... end` and `struct ... end` module bodies now either relift ordered item streams or fail explicitly; `lower.ml` no longer falls back to source-preserved nested body rendering.
- grouped and standalone GADT-style type declarations now lower through the normal structural type renderer; `lower.ml` no longer preserves whole type declarations from source for uppercase constructor/result-type probes.
- top-level type extensions, exception declarations, and floating attributes now lower structurally; unsupported top-level class/class-type/extension items fail explicitly instead of preserving source text.
- module-expression and module-type extensions now fail explicitly instead of falling through raw `doc_of_node` fallback.
- class, local-open, and object core types now lower structurally; core-type extensions fail explicitly instead of falling through raw fallback.
- lazy/operator/poly-variant-inherit/alias/typed/local-open/effect patterns now lower structurally; pattern extensions and typed first-class-module patterns fail explicitly instead of falling through raw fallback.
- module-pack, assert, lazy, while, for, method-call, new, object-override, instance-variable-assign, typed, polymorphic, and coerce expressions now lower structurally; expression/object extensions fail explicitly instead of falling through raw fallback.
- optional parameter defaults and typed binding patterns now survive the `Syn.Cst` lift structurally, and `krasny` renders parameters from CST shape instead of `Source.source_of_parameter`.
- signature `val` declarations now render names from CST token structure; `krasny` no longer reparses declaration source to recover operator names before `:`.
- inherited polymorphic-variant rows now render directly from `Syn.Cst.RowField.Inherit.type_`; `krasny` no longer reconstructs inherited row paths by scanning token text.
- `Syn.Cst.sequence_expression` now exposes per-boundary `separator_tokens`, and binding-operator clauses now expose `equals_token` plus `let_operator_expression.in_token`; the CST surface now carries the token boundaries `krasny` needs for sequence and `let*` trivia without source-gap recovery.
- `let f = fun ...` detection now comes from `Syn.Cst.let_binding` shape instead of scanning tokens after `=`.
- tuple and `let open ... in` line breaking no longer sniff source length or embedded newlines; `krasny` now relies on structural docs there.
- simple apply expressions now decide whether they stay after `=` by recursing over CST callee/argument shape instead of scanning source text for keyword substrings.
- application rendering no longer force-switches layout from raw source length or embedded newlines; it follows structural argument break rules only.
- inline-record constructor arguments no longer preserve multiline layout just because the original record node contained newlines; they format from field structure and owned trivia only.
- sequence-expression trivia now renders from `separator_tokens` plus the next expression's leading trivia, and `let*`/`let+` clause and body trivia now render from `equals_token` / `in_token`; `lower.ml` no longer reparses raw spans for those boundaries.
- match-case layout no longer preserves raw source newlines after `->`; `render_case` now breaks only from rendered body structure and explicit multiline preferences.
- `Format_core.format` no longer falls back to returning the original source when lowering declines to format.
- `Format_core.format` now has an explicit EOF policy: non-empty formatted output ends with a final newline, without inspecting the input source to inherit that behavior.
- top-level structure phrase separators now come from direct source-file separator tokens, not by slicing raw source between item spans or preserving expression runs from source text.
- trivia between `fun ... ->` and the first body token now comes from that body token's `leading_trivia`; `lower.ml` no longer reparses a raw source slice for that path.
- trivia around `if ... then ... else` branches now comes from `else_token.leading_trivia` and the next branch node's first-token `leading_trivia`; `lower.ml` no longer reparses raw source spans for that path.
- `if` conditions now render from ordinary expression structure; `lower.ml` no longer scans token text for `&&` / `||` or comment-like trivia just to format boolean conditions.
- trivia after `=` and `in` in ordinary `let ... in` expressions now comes from the RHS/body node's first-token `leading_trivia`; `lower.ml` no longer reparses raw source spans for those paths.
- local binding layout no longer preserves raw internal newlines from RHS syntax nodes; simple wrapped values now collapse from CST structure instead of staying multiline because the source had embedded newlines.
- singleton list patterns now use explicit formatter edge spacing; `lower.ml` no longer sniffs source text for `"[ "` / `" ]"` to preserve original spacing.
- dead source-preserving helper scaffolding such as `doc_of_node` and `doc_of_source_preserved_syntax_node*` is gone from `lower.ml`; remaining source debt is in live formatting decisions, not unreachable fallback wrappers.
- `render_trivia_between_spans`, `parse_trivia_between_offsets`, `trailing_inline_comment_suffix`, `leading_inline_comment_between_offsets`, and `split_leading_inline_comment_source` are gone from `lower.ml`; the remaining raw-trivia debt is in `doc_of_owned_trivia` separator recovery and source/text heuristics, not generic between-node span replay.
- `packages/krasny/src/source.ml` is trimmed to the remaining live raw source-reconstruction helper only; `Source` is now down to `source_of_syntax_node`.
- `render_structure_items` and `render_signature_items` no longer slice `ctx.source` down to nested/top-level span windows; they use the full available source and only fall back to `Source.source_of_syntax_node` when no source text was provided at all.
- `render_structure_items` and `render_signature_items` now require source text explicitly; the impossible no-source path fails instead of reconstructing node text behind the formatter's back.

## Working Style

- Add or update the smallest regression first.
- Prefer compiler-driven cleanup over speculative rewrites.
- Commit every slice with a scoped conventional commit message.
- Prefer `syn:cst_tests` for ownership bugs and `krasny` fixtures for layout/rendering bugs.
- Do not reintroduce ownership heuristics in `krasny` once the CST already knows the answer.
- At every turn, do a quick audit to see if we can add more clean up todo items 

## Structural Formatting Debt

- [ ] Remove source-preserving node fallback from `packages/krasny/src/lower.ml`
  - `text_of_syntax_node`
  - token/text reconstruction via `Source.source_of_syntax_node`
  - attribute/module-type string reconstruction such as `render_attribute`, `render_first_class_module_type`, `strip_outer_parens_once`, and `strip_module_prefix`
  - remaining non-top-level fallback branches in expression/module/module-type lowering that still end in `doc_of_node (...)`
  - keep unsupported shapes on the explicit `Cannot_lower` path; do not reintroduce silent source preservation

- [x] Remove API-level source-preserving fallback from `packages/krasny/src/format_core.ml` and `packages/krasny/src/lower.ml`
  - `Lower.source_file` now returns an explicit lowering result instead of `None`
  - `Format_core.format` now reports `Cannot_lower` instead of returning `original_source`

- [x] Remove source-derived output policy from `packages/krasny/src/format_core.ml`
  - `Format_core.format` no longer inspects `Source.source_of_result result` to inherit the input file's trailing-final-newline state
  - non-empty formatted output now ends with a final newline by explicit formatter contract
  - `format`, `write`, CLI formatting, and verify now share that explicit EOF policy

- [x] Remove raw source-gap parsing from top-level structure/signature rendering
  - `source_gap_has_only_phrase_separators`, `source_gap_leading_phrase_separator`, and `source_of_relative_span` are gone from `lower.ml`
  - top-level structure phrase separators now come from direct source-file separator tokens instead of scanning raw source between item spans
  - expression-run preservation in `render_structure_top_level_items` is gone; phrase separation is structural

- [ ] Remove raw trivia reparsing helpers from `lower.ml`
  - `doc_of_owned_trivia` should not need `separator_doc_between_offsets` plus raw `source` just to recover spacing between adjacent comment/doc items
  - if formatting still needs these, the missing structure belongs in `syn`

- [ ] Remove source-sniffing and token-text heuristics used to make rendering decisions
  - `type_declaration_requires_source_preservation`
  - rendered-source substring checks such as `[@` / `[%%expect]` preservation gates
  - multiline/layout heuristics currently driven by reconstructed node text
  - token-text scans over `SyntaxNode.tokens`, e.g. searching for `"="` then `"fun"` or reconstructing poly-variant inherit paths from token text lists

- [ ] Remove node-text-driven layout heuristics from `lower.ml`
  - function-binding layout branches in `render_local_binding`
  - `tuple_source_is_long`
  - `apply_expression_is_simple_after_equals`
  - `expression_source_is_long`
  - `expression_source_has_newline`
  - application multiline preference derived from raw node text length/newlines
  - `let exception` rendering that prints `exception_declaration.syntax_node` text directly

- [ ] Remove source-derived “safe to rewrite” gates from top-level formatting
  - `structure_item_requires_source_preservation_before_expression`
  - `render_structure_entry` / `render_signature_entry` `should_preserve_source`
  - expression-run preservation rules in `render_structure_top_level_items`
  - replace “preserve this source because rewrite might change meaning” with explicit CST facts or hard failure

- [x] Remove source-slice reconstruction from lowering context setup
  - `render_structure_items ?source ~source_node`
  - `render_signature_items ?source ~source_node`
  - do not derive nested/top-level source windows from `ctx.source` + `source_node` spans just to support fallback formatting

- [ ] Shrink `packages/krasny/src/source.ml` to the minimal structural-support surface
  - keep `Source` focused on the remaining supported structural utilities, not as a grab-bag for historical source-replay helpers

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
    `Source.source_of_syntax_node`

- [ ] Decide which missing structural facts belong in `syn` so `krasny` can stop guessing
  - explicit phrase-separator / top-level phrase-boundary modeling
  - explicit value-declaration printable name modeling
  - explicit structured attribute/extension payload rendering surface instead of replaying `payload_syntax_node` / `item_syntax_nodes`
  - explicit structured structure/signature payload and module-body views where the public CST still exposes only raw `item_syntax_nodes`
  - explicit inter-trivia separator/layout facts if `owned_trivia` must preserve spacing between adjacent comment/doc items without `separator_doc_between_offsets`
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
