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
- [x] `packages/krasny/src/lower.ml` no longer does raw source reparsing, source sniffing, red-tree token walking, or formatter-side span archaeology.

## Priority Right Now

- Make valid unsupported syntax lower structurally or fail because `syn` still lacks a named CST fact.
- Do not spend time improving layout consistency or formatter aesthetics unless it directly unblocks structural lowering.
- If `krasny` needs to reconstruct a stable fact, move that fact into `syn`.
- Use workspace verify as the closing check, but prioritize the next unsupported valid shape over policy cleanup.

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
- top-level type extensions, exception declarations, floating attributes, floating extension items, and class/class-type declaration items now lower structurally.
- module-expression and module-type extensions now lower structurally from the shared extension shell; extension payloads default to CST-owned opaque token slices instead of OCaml payload relift.
- core-type extensions now lower structurally from that same shared extension shell, including the default opaque payload path.
- class, local-open, object, and extension core types now lower structurally.
- `Syn.Cst.class_declaration` is now an explicit valid-shape sum instead of a `class_type option * class_body option` product; impossible `None/None` declaration states are no longer representable.
- `Syn.Cst.ModuleTypeDeclaration` now models only `module type Name` and `module type Name = ...`; the bogus `is_destructive_substitution` state is gone from that CST node.
- `Syn.Cst.ModuleDeclaration` is now an explicit valid-shape sum for signature-only versus structure-with-body module bindings; the bogus `:=` substitution path and the old `module_type option * module_expression option` product are gone, although the node still spans both structure and signature contexts.
- implementation-side `val` items are no longer representable in `Syn.Cst.StructureItem`; ordinary `value_declaration` nodes now stay signature-only, matching standard OCaml syntax.
- `Syn.Cst.let_binding` is now pattern-first only; the old cached `binding_name` field is gone, and simple binding names are derived from `binding_pattern` when the pattern shape actually carries one.
- expression type annotations and coercions now share one `Syn.Cst.Expression.TypeAscription` node with explicit `Type | Coerce | ConstraintCoerce` variants instead of a split `Typed` node plus nullable `Coerce` record.
- object members now model only valid concrete forms: `object_method.body`, `object_value.value`, and `object_initializer.body` are mandatory expressions, and the old impossible `None` body/value states are gone from the CST.
- class members now split real grammar alternatives explicitly: methods and values carry `ConcreteMethod | VirtualMethod` and `ConcreteValue | VirtualValue` definitions, while `class_initializer.body` is now mandatory.
- lazy/operator/poly-variant-inherit/alias/typed/local-open/effect/first-class-module/extension patterns now lower structurally; default opaque extension payloads in patterns now render directly from CST token slices, while the remaining structured guard case still fails explicitly.
- polymorphic-variant inherit patterns now lift their `type_path` without the leading `#` sigil, and `krasny` renders `#color` / `#M.color` structurally and idempotently instead of collapsing the path to `##`.
- module-pack, assert, lazy, while, for, method-call, new, object, object-override, instance-variable-assign, type-ascription/coercion, polymorphic, extension, and unreachable expressions now lower structurally; plain object expressions support self patterns plus method/value/inherit/initializer/extension members and member attributes, and extension payloads render from CST-owned opaque token slices.
- optional parameter defaults and typed binding patterns now survive the `Syn.Cst` lift structurally, and `krasny` renders parameters from CST shape instead of `Source.source_of_parameter`.
- signature `val` declarations now render names from CST token structure plus `Syn.Cst.Token.is_operator_like_name`; `krasny` no longer reparses declaration source or compares raw token text to recover or parenthesize operator names before `:`.
- inherited polymorphic-variant rows now render directly from `Syn.Cst.RowField.Inherit.type_`; `krasny` no longer reconstructs inherited row paths by scanning token text.
- `Syn.Cst.sequence_expression` now exposes per-boundary `separator_tokens` plus `expression_leading_trivia`, and binding-operator clauses now expose `equals_token` plus `let_operator_expression.in_token`; the CST surface now carries the boundary trivia `krasny` needs for sequence and `let*` rendering without source-gap recovery.
- `let f = fun ...` detection now comes from `Syn.Cst.let_binding` shape instead of scanning tokens after `=`.
- tuple and `let open ... in` line breaking no longer sniff source length or embedded newlines; `krasny` now relies on structural docs there.
- simple apply expressions now decide whether they stay after `=` by recursing over CST callee/argument shape instead of scanning source text for keyword substrings.
- application rendering no longer force-switches layout from raw source length or embedded newlines; it follows structural argument break rules only.
- inline-record constructor arguments no longer preserve multiline layout just because the original record node contained newlines; they format from field structure and owned trivia only.
- sequence-expression trivia now renders from `Syn.Cst.sequence_expression.expression_leading_trivia`, and `let*`/`let+` clause and body trivia now render from `equals_token` / `in_token`; `lower.ml` no longer reparses raw spans or generic boundary-helper spans for those paths.
- match-case layout no longer preserves raw source newlines after `->`; `render_case` now breaks only from rendered body structure and explicit multiline preferences.
- `Format_core.format` no longer falls back to returning the original source when lowering declines to format.
- `Format_core.format` now has an explicit EOF policy: non-empty formatted output ends with a final newline, without inspecting the input source to inherit that behavior.
- `Syn.Cst.SourceFile` now exposes `phrase_separator_tokens`, and top-level structure phrase separators render from that CST token stream instead of slicing raw source between item spans, preserving expression runs, or scanning source-file direct tokens in `krasny`.
- plain `object ... end` expressions now format structurally and idempotently, including empty objects, self patterns, `method`/`val` members, `inherit`, `initializer`, extension members, member attributes, and typed object-expression constraints.
- positional constructor parameter patterns no longer accumulate redundant parentheses across reformat; `krasny` now treats already-parenthesized parameter docs as stable and has focused regression coverage for `fun (Conn conn) -> ...`.
- structure/signature `open!` statements now render the bang from `bang_token`, instead of hardcoding `!` in the formatter.
- trivia between `fun ... ->` and the first body token now comes from that body token's `leading_trivia`; `lower.ml` no longer reparses a raw source slice for that path.
- trivia around `if ... then ... else` branches now comes from `else_token.leading_trivia` and the next branch node's first-token `leading_trivia`; `lower.ml` no longer reparses raw source spans for that path.
- `if` conditions now render from ordinary expression structure; `lower.ml` no longer scans token text for `&&` / `||` or comment-like trivia just to format boolean conditions.
- trivia after `=` and `in` in ordinary `let ... in` expressions now comes from the RHS/body node's first-token `leading_trivia`; `lower.ml` no longer reparses raw source spans for those paths.
- `Syn.Cst` now exposes `leading_trivia_after`, `leading_trivia_before_node`, and `leading_trivia_after_token_before_node`, and `krasny` now uses those helpers instead of walking red-node token streams directly when it needs token-attached boundary trivia around `fun`, `if`, `let`, and sequence bodies.
- local binding layout no longer preserves raw internal newlines from RHS syntax nodes; simple wrapped values now collapse from CST structure instead of staying multiline because the source had embedded newlines.
- the remaining local-binding `=` placement policy now runs through named helper functions, isolating the live style heuristics without changing formatter behavior.
- local-binding header/body placement policy now routes through named helpers for keeping typed parameters in the binding header and forcing multiline recursive local bodies, instead of anonymous booleans inside `render_local_binding`.
- local-binding header/body placement now has focused formatter regression coverage for typed-header retention and multiline recursive local bodies before the next heuristic cleanup.
- the infix-chain cutoff inside the local-binding `=` policy now lives in an explicit named helper/constant, with formatter coverage for the 8-term inline vs 9-term multiline boundary.
- the live local-binding `=` placement policy now has focused formatter regression coverage for long boolean chains and pipelines before the next heuristic cleanup.
- binding-operator clause `=` placement now reuses the same isolated after-equals policy helpers as ordinary local bindings, with focused formatter coverage for explicit `fun` RHS values plus long boolean/pipeline bodies.
- dead inline-string binding special casing is gone; ordinary `expression_is_simple_after_equals` checks now carry the inline decision for both `let` and `let*` bindings.
- `Syn.Cst.CoreType.Alias` now carries an optional `sigil_token`, and labeled/optional parameters now carry `binding_name_matches_label`; `krasny` no longer compares token text for alias binders or named-parameter sugar.
- `Syn.Cst.Token` now exposes `fixed_operator`, `is_operator_like_name`, `is_identifier_like_name`, and `same_text`; `krasny` no longer compares token text for fixed-operator layout checks, operator-like name classification, or same-operator infix-chain grouping.
- `lower.ml` now reads token text only through `doc_of_token` for explicit token/operator rendering, not for layout heuristics or grouping decisions.
- record type and type-definition fields no longer force multiline `:` layout from an arbitrary field-name length threshold; they break only from rendered type structure and normal document width decisions.
- singleton list patterns now use explicit formatter edge spacing; `lower.ml` no longer sniffs source text for `"[ "` / `" ]"` to preserve original spacing.
- dead source-preserving helper scaffolding such as `doc_of_node` and `doc_of_source_preserved_syntax_node*` is gone from `lower.ml`; remaining source debt is in live formatting decisions, not unreachable fallback wrappers.
- stale rollout-era dead helpers continue to shrink during audits; unused locals such as `token_text`, `unwrap_parenthesized_expression`, `flatten_top_level_expression_item`, `render_structure_expression_run`, `expression_needs_multiline_binding`, `trim_trailing_layout_whitespace`, `push_pending_break`, `child_span`, `compare_child_by_span`, `children_in_source_order`, `extract_leading_inline_comment`, `syntax_node_of_apply_argument`, `render_function_expression_inline`, `render_unsugared_named_parameter_binding_pattern`, the dead top-level `doc_with_expression_attributes` duplicate, and unused keyword docs such as `kw_if`/`kw_then`/`kw_match`/`kw_try` are gone from `lower.ml`.
- `render_trivia_between_spans`, `parse_trivia_between_offsets`, `trailing_inline_comment_suffix`, `leading_inline_comment_between_offsets`, and `split_leading_inline_comment_source` are gone from `lower.ml`.
- `doc_of_owned_trivia` now joins owned comments/docstrings with explicit formatter separators instead of recovering whitespace/newline gaps from raw source text.
- `render_structure_items` and `render_signature_items` now render directly from ordered item streams plus owned trivia; they no longer require source text or nested source-window slicing.
- top-level structure/signature rendering now trusts the incoming ordered CST item streams directly; `lower.ml` no longer re-sorts those item lists by reconstructed syntax-node spans before joining docs.
- signature top-level rendering no longer reconstructs per-item spans at all; `render_signature_top_level_items` now joins the ordered CST stream directly because signature entries have no phrase-separator suffix path today.
- `Syn.Cst.Implementation` / `Interface` now carry per-item `trailing_phrase_separator_tokens`, and `krasny` uses those CST-provided boundary tokens directly instead of recomputing top-level structure phrase-separator placement from item spans.
- dead top-level span/owned-trivia boundary helpers such as `span_of_syntax_node_nontrivia_bounds`, `span_of_syntax_node_trim_leading_trivia_keep_trailing_comments`, and `type_declaration_owned_trivia_end` are gone from `lower.ml`; the remaining structural debt is no longer hiding in unused formatter boundary code.
- `lower.ml` no longer uses generic boundary/span helpers for sequence joins or variant-constructor trailing trivia; those paths now run through explicit CST trivia fields or formatter-local booleans instead of reconstructed token offsets.
- inline-record constructor layout no longer reaches through `RecordField.syntax_node` to ask the red tree for a parent node; `krasny` now decides inline-vs-multiline rendering from field presence, owned trivia, standalone record-body trivia, and explicit multiline preference only.
- `Syn.Cst.syntax_kind` now owns diagnostics-only syntax-kind access too, and `krasny` keeps that value typed until `error_to_string` renders it; `packages/krasny/src/lower.ml` no longer references `Ceibo.Red.SyntaxNode` directly even for unsupported-shape reporting.
- `fun`, `if`, ordinary `let`, top-level `let`, class-`let` `and` bindings, and binding-operator CST nodes now carry explicit keyword-bound boundary trivia (`body_leading_trivia`, `value_leading_trivia`, `then_branch_trailing_trivia`, `else_branch_leading_trivia`, and related binding-operator fields), and `krasny` uses those structural fields directly instead of generic boundary-trivia helper calls on those paths.
- `Syn.Cst.sequence_expression` now carries per-step `expression_leading_trivia`, and `krasny` uses that structural list directly instead of reconstructing semicolon-boundary trivia from generic `leading_trivia_after_token_before_node` helper calls.
- `Lower.source_file` and `Format_core.format` no longer thread parse-result source through the normal lowering path just to satisfy dead internal parameters.
- first-class module package-type positions now use an explicit `Syn.Cst.package_type` record in `syn`, including optional package-type attributes, and `krasny` renders those positions from package paths plus `with type` constraints instead of broad module-type variants.
- class-structure and class-type-signature field payloads now carry `owned_trivia`, and `Syn.CstBuilder.class_field_items_of_fields` / `class_type_field_items_of_fields` expose ordered helper streams with standalone trailing `end`-owned comments/docstrings.
- class and class-type declarations now render structurally too, including `class%foo` / `class type%foo` shortcut shell modifiers from explicit CST `declaration_extension` / `declaration_attributes` instead of formatter-side recovery.
- the main lowering path now renders floating attributes and expression-attached attributes from the opaque payload token slice carried by the CST; `krasny` no longer relifts payload bodies as OCaml.
- postfix expression attributes on constructor/apply/infix expressions now parenthesize the attributed expression structurally before rendering `[@attr]`, so payloads stay intact and repeated formatting stays idempotent for shapes like `Some 0 [@inline always]`, `f x [@inline always]`, and `a + b [@inline always]`.
- shared/global core-type, module-type, and module-expression attributes now render no-payload, type-payload, and simple single-expression structure payloads structurally; richer shared/global payload forms fail explicitly instead of replaying raw payload text.
- split-sigil floating attributes such as `[@@@foo]` now lift as payload-free annotations instead of misparsing the trailing `@foo` as raw payload text.
- relifted nested `struct ... end` and `sig ... end` bodies now keep floating attributes as real sibling items after preceding `type` declarations, without double-splitting or dropping them.
- core-type variables now render from `Syn.Cst.CoreType.Var.sigil_token` plus `name_token`; quoted `'a` and bare locally abstract `a` variables no longer need raw syntax-node token replay.
- typed named parameters now normalize through synthesized outer `: type ...` binding annotations without duplicating `~(fn : ...)` inside the unsugared `fun` parameter list.
- `Syn.Cst.index_expression` now carries explicit `opening_tokens` plus `closing_token`, and `krasny` renders `.[ ]`, `.( )`, and extended `.%( )` delimiters from those CST tokens instead of reconstructing punctuation from raw direct-token text.
- int and float constants now carry an optional leading `sign_token`, and `krasny` renders signed literal patterns from that structural token instead of scanning literal-node direct tokens for `+` / `-`.
- operator expressions, operator patterns, and infix/prefix expression operator docs now render directly from CST-carried operator tokens, instead of concatenating raw token text back into operator strings.
- polymorphic-variant expression and pattern heads now render from explicit `tag_token` plus a formatter backtick, instead of replaying raw syntax-node token text.
- `Syn.Cst.CoreType.Poly` now exposes `type_keyword_token`, and `krasny` uses that explicit token instead of scanning raw tokens to decide whether locally abstract types were written with `type`.
- `packages/krasny/src/source.ml` is gone; `krasny` no longer keeps any live raw source-reconstruction helper.
- attribute and extension payloads now use a single `Syn.Cst.Payload.Opaque` constructor, preserving foreign-language bodies and OCaml payload bodies losslessly in the CST without advertising structured payload variants we do not actually support.
- `krasny` now renders attribute and extension payloads from that opaque CST token slice instead of relifting or normalizing payload contents.
- the remaining structural debt is unsupported valid syntax and missing CST facts, not formatter-side source/token/span reconstruction.

## Working Style

- Add or update the smallest regression first.
- Prefer compiler-driven cleanup over speculative rewrites.
- Commit every slice with a scoped conventional commit message.
- Prefer `syn:cst_tests` for ownership bugs and `krasny` fixtures for layout/rendering bugs.
- Do not reintroduce ownership heuristics in `krasny` once the CST already knows the answer.
- Prefer structural completeness over formatter quality for now. A temporarily worse but CST-only formatter is acceptable.
- Treat policy cleanup as deferred unless it directly blocks structural lowering.
- At every turn, do a quick audit to see if we can add more clean up todo items 

## Structural Formatting Debt

- [ ] Burn down the remaining unsupported valid syntax in structural-priority order

- [x] Make annotation payloads structural-by-contract without forcing OCaml relift
  - attribute and extension payloads now use a single `Syn.Cst.Payload.Opaque` constructor
  - `krasny` renders those payloads by replaying the CST-owned token slice exactly
  - foreign payload languages such as `[%sql ...]` no longer depend on accidental OCaml payload parsing
  - ordinary OCaml payload bodies also stay opaque until we intentionally add a different CST contract

- [x] Remove source-preserving node fallback from `packages/krasny/src/lower.ml`
  - `Source.source_of_syntax_node` is gone
  - shared/global attribute payloads no longer replay raw syntax-node text
  - unsupported shared/global payload forms now fail explicitly instead of preserving source

- [x] Remove API-level source-preserving fallback from `packages/krasny/src/format_core.ml` and `packages/krasny/src/lower.ml`
  - `Lower.source_file` now returns an explicit lowering result instead of `None`
  - `Format_core.format` now reports `Cannot_lower` instead of returning `original_source`

- [x] Remove source-derived output policy from `packages/krasny/src/format_core.ml`
  - `Format_core.format` no longer inspects `Source.source_of_result result` to inherit the input file's trailing-final-newline state
  - non-empty formatted output now ends with a final newline by explicit formatter contract
  - `format`, `write`, CLI formatting, and verify now share that explicit EOF policy

- [x] Remove raw source-gap parsing from top-level structure/signature rendering
  - `source_gap_has_only_phrase_separators`, `source_gap_leading_phrase_separator`, and `source_of_relative_span` are gone from `lower.ml`
  - top-level structure phrase separators now come from `Syn.Cst.SourceFile.phrase_separator_tokens` instead of scanning raw source between item spans or source-file direct tokens
  - expression-run preservation in `render_structure_top_level_items` is gone; phrase separation is structural

- [x] Remove raw trivia reparsing helpers from `lower.ml`
  - `separator_doc_between_offsets` is gone
  - `doc_of_owned_trivia` now uses explicit formatter separators instead of raw source gaps between adjacent comment/doc items

- [x] Remove token-text replay and token-text heuristics still used in `lower.ml`
  - `lower.ml` no longer uses token text for layout detection, ownership, or grouping decisions
  - the only remaining token-text read is `doc_of_token`, which is explicit token rendering rather than heuristic logic

- [x] Audit remaining layout heuristics and keep only the ones that are explicit style policy
  - inline `let ... =` placement rules now live in named helpers such as `expression_is_simple_after_equals`, `local_binding_value_stays_after_equals`, `adjust_local_binding_value_after_equals`, and `infix_expression_is_simple_after_equals`
  - `render_local_binding` header/body placement rules now live in named helpers such as `local_binding_keeps_parameters_in_header` and `local_binding_forces_multiline_body`
  - the remaining local-binding layout policy has focused formatter regression coverage for typed-header retention, multiline recursive locals, long boolean/pipeline bodies, and the 8-term infix cutoff

- [x] Remove source-derived “safe to rewrite” gates from top-level formatting
  - top-level item joins/renderers are layout-only now; there are no remaining “preserve because rewrite might change meaning” branches in top-level formatting

- [x] Remove source-slice reconstruction from lowering context setup
  - `render_structure_items ?source ~source_node`
  - `render_signature_items ?source ~source_node`
  - do not derive nested/top-level source windows from `ctx.source` + `source_node` spans just to support fallback formatting

- [x] Shrink `packages/krasny/src/source.ml` to the minimal structural-support surface
  - `source.ml` is deleted; `krasny` no longer keeps a raw source helper module

- [x] Remove obsolete lowering source parameters from internal formatter plumbing
  - `Lower.source_file` no longer takes `~source`
  - `Format_core.format` no longer threads parse-result source into lowering

- [x] Remove public/docs-level assumptions that unsupported shapes are preserved from source
  - `packages/krasny/src/Krasny.mli`, `packages/krasny/src/format_core.ml`, and `packages/krasny/AGENTS.md` now describe explicit failure on unsupported shapes instead of source fallback

- [x] Remove impossible-state fallback patterns from formatter hot paths
  - formatter hot paths no longer carry “best effort” source-preserving fallback branches that mask missing structural support

- [x] Audit every `ctx.source` / `Source.*` use in `packages/krasny/src/lower.ml`
  - `lower.ml` no longer uses `Source.*`
  - any future raw source helper use should be treated as new debt immediately

- [ ] Decide which missing structural facts belong in `syn` so `krasny` can stop guessing
  - explicit public nested signature-body item anchors beyond the current helper-only relift surface, if downstream tools need more than `CstBuilder.signature_items_of_module_type`
  - explicit inter-trivia separator/layout facts if `owned_trivia` must preserve spacing between adjacent comment/doc items without `separator_doc_between_offsets`
  - explicit ambiguity-sensitive type-declaration shape markers

- [ ] Tighten remaining representable-but-invalid CST states
  - tighten `CoreType.FirstClassModule` / package-type positions further if any broad module-type-only states are still representable beyond module-type paths plus `with type` constraints
  - keep auditing broad shared nodes and documenting any remaining intentional invalid-state surface explicitly

- [x] Remove remaining red-tree token/span archaeology from `packages/krasny/src/lower.ml`
  - `lower.ml` no longer references `Ceibo.Red.SyntaxNode` directly
  - generic boundary-helper/span reconstruction for sequence joins and variant-constructor trailing trivia is gone
  - the remaining debt is structural support for unsupported shapes plus explicit style heuristics, not token/span archaeology

- [ ] Add regression coverage before landing each new structural shape
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
