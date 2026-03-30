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
17. Keep `render_structure_top_level_items` and `render_signature_top_level_items` layout-only. Top-level phrase separators should come from `Syn.Cst.SourceFile.phrase_separator_tokens`, not raw source-gap parsing, source-preserved expression runs, or ad hoc direct-token scans; ordered item streams plus `owned_trivia` already decide ownership.
18. Trust the incoming ordered structure/signature item streams. Do not re-sort top-level items by reconstructed syntax-node spans before joining them; if join boundaries still need extra facts, add those facts to the CST instead of re-deriving order in `krasny`.
19. Keep signature top-level joins span-free when there is no signature phrase-separator contract to honor. Do not carry dead per-item span reconstruction through `render_signature_top_level_items` once the ordered CST stream already determines join order and trailing suffixes are always `None`.
20. Render grouped `type ... and ...` members from each member's `Syn.Cst.TypeDeclaration.owned_trivia`; do not reparse between-member source gaps once `syn` has attached `and`-token leading trivia to the following member.
21. Keep adjacent standalone ordinary docstrings visually separate in top-level joins; do not compact them into a single tight run just because they are both trivia items.
22. Render record bodies and inline record constructor arguments from `Syn.CstBuilder.record_field_items_of_fields`; do not inspect raw record syntax children or closing-token trivia to rediscover terminal `}`-owned comments/docstrings.
23. Do not add new source-preserving reconstruction paths in `Lower`. Unsupported shapes should fail formatting until `syn` exposes enough structure for a purely structural lowering.
24. The old top-level parameterized-`let` phrase-boundary preservation path is gone. Do not reintroduce source-preserved expression runs; if another phrase-boundary case needs help, model it structurally.
25. Treat `Syn.Ceibo.Red.SyntaxNode.tokens` and `direct_tokens` as real-token-only streams in `krasny`. Do not keep dead filters for impossible `WHITESPACE` / `COMMENT` / `DOCSTRING` token kinds after the token-trivia migration.
26. Render parameters from `Syn.Cst.Parameter` structure. Do not reintroduce `Source.source_of_parameter` or other raw parameter-text reconstruction once optional defaults and typed binding patterns are preserved structurally.
27. Render signature `val` names from CST token structure. Do not reparse declaration source to recover operator spelling or parentheses once `Syn.Cst.value_declaration.name_token` is available.
28. Render inherited polymorphic-variant rows from `Syn.Cst.RowField.Inherit.type_` directly. Do not scan raw token text to reconstruct row paths that the CST already models as a core type.
29. Distinguish `let f = fun ...` from `let f x = ...` from `Syn.Cst.let_binding` shape, not from scanning tokens around `=` in the original source.
30. `Krasny.format` output policy is explicit: non-empty formatted output ends with a final newline, independent of whether the input source had one.
31. Render trivia around `if ... then ... else` from `else_token.leading_trivia` and the following branch node's leading trivia; do not reparse raw source spans between `then`, `else`, and branch bodies.
32. Render trivia after `=` and `in` in ordinary `let ... in` expressions from the RHS/body node's leading trivia. Do not reparse raw source spans for those boundaries once the CST already exposes `equals_token`, `in_token`, and the branch node.
33. Render sequence-expression trivia from `Syn.Cst.sequence_expression.separator_tokens` plus the following expression's leading trivia; do not recover semicolon-boundary comments/docstrings by reparsing the source gap.
34. Render binding-operator clause and body trivia from `Syn.Cst.binding_operator_binding.equals_token` and `Syn.Cst.let_operator_expression.in_token`; do not reconstruct `let*` / `and*` / `let+` trivia from raw spans once `syn` exposes the tokens explicitly.
35. Singleton list-pattern spacing is explicit formatter policy, not source preservation. Do not sniff original `"[ value ]"` spacing from raw node text to decide pattern edge spaces.
36. Render `if` conditions through ordinary expression lowering. Do not scan token text for `&&` / `||` or token-leading comment trivia to decide boolean-condition layout.
37. Local binding layout should follow rendered RHS structure, not raw source newlines inside the RHS syntax node. Do not keep multiline `let ... =` layouts just because the original subtree text contained embedded newlines.
38. Render first-class module core types and type definitions from structural module-type variants. Do not reconstruct `(module ...)` text from raw module-type syntax-node text; if a first-class module-type form still lacks a structural renderer, fail explicitly.
39. In the main lowering path, render floating and expression-attached attribute payloads from `Syn.Cst.attribute.payload` plus `Syn.CstBuilder.structure_items_of_payload` / `signature_items_of_payload`. Do not replay raw payload syntax text there; unsupported pattern payloads should fail explicitly until `syn` exposes more structure.
40. Ordinary `[@attr? pattern when guard]` payloads should lower through `Syn.CstBuilder.pattern_of_syntax_node` and `expression_of_syntax_node` when `Syn.Cst.pattern_payload` still carries syntax anchors. Do not reparse source text for those payloads; shared/global pattern payloads may stay on an explicit unsupported path until the CST exposes more.
41. Join `owned_trivia` with explicit formatter separators. Do not recover comment/docstring spacing from raw source gaps or thread source text through nested item renderers just to preserve whitespace between trivia items.
42. Render shared core-type, module-type, and module-expression attributes structurally too. Support simple single-expression structure payloads without raw payload replay, and fail explicitly on richer shared/global payload forms until `syn` exposes enough structure.
43. When relifted nested item streams expose a floating attribute immediately after a `type` declaration, keep that join tight on the next line. Do not open a blank paragraph there or re-split the nested body to recover the attribute twice.
44. Render polymorphic-variant expression and pattern heads from the explicit `tag_token` fields in the CST. Do not replay raw direct-token text for the leading backtick tag once `syn` exposes it structurally.
45. Render `CoreType.Poly`'s optional leading `type` prefix from `type_keyword_token`. Do not scan raw poly-type tokens to rediscover whether a locally abstract type was written with `type`.
46. Render `CoreType.Var` from `sigil_token` plus `name_token`. Do not drop quoted `'a` sigils by printing only the bare name token, and do not reintroduce raw token replay to recover them.
47. When `render_local_binding` synthesizes an outer `: type ...` annotation from parameter types, drop duplicate inner type annotations from the unsugared `fun` parameter list too. Normalize `~(fn : a -> b)` to `~fn` once the outer arrow already carries `fn:(a -> b)`.
48. Render index expressions from `Syn.Cst.index_expression.opening_tokens` plus `closing_token`. Do not reconstruct `.[ ]`, `.( )`, or extended index-operator punctuation from raw direct-token text once `syn` exposes the delimiter tokens explicitly.
49. Render signed int/float literals from `sign_token` when the CST constant carries one. Do not rescan literal-node direct tokens for leading `+` / `-`, especially for signed literal patterns.
50. Render operator expressions, operator patterns, and infix/prefix expression operator docs from CST-carried operator tokens directly. Do not concatenate raw token text back into an operator string when the CST already preserves the exact token sequence.
51. Render structure/signature `open!` statements from `bang_token` when present. Do not hardcode `!` in the formatter when the CST already carries the token.
52. Render `CoreType.Alias` binders from `sigil_token` plus `name_token`. Do not synthesize a leading apostrophe for `... as 'a` by inspecting token text once `syn` exposes the alias-binder spelling explicitly.
53. Render named and optional parameter sugar from `binding_name_matches_label` on `Syn.Cst.Parameter`. Do not compare label token text with identifier binding patterns to choose `~label` vs `~label:pattern` once `syn` has made that equivalence explicit.
54. Let record type and type-definition fields break after `:` from rendered type structure and document width only. Do not force multiline field layout from arbitrary field-name length thresholds once the structural renderer already has the field type.
55. Render fixed-operator layout, operator-like name decisions, and same-operator infix-chain grouping from `Syn.Cst.Token` helpers such as `fixed_operator`, `is_operator_like_name`, and `same_text`. Do not compare raw token text for `|>`, `&&`, `||`, `-`, `~-`, keyword operators, or infix-chain grouping once `syn` exposes that classification explicitly.
56. Keep the local-binding infix-chain inline cutoff explicit formatter policy. If `let ... = a + b + ...` keeps using a term-count threshold, name that cutoff and cover its boundary in formatter tests instead of hiding a raw magic number inside `expression_is_simple_after_equals`.
57. Keep local-binding header/body placement policy explicit too. Decisions such as whether typed parameters stay in the binding header or whether a recursive local body forces multiline layout should live in named helpers, not anonymous booleans embedded inside `render_local_binding`.
58. Keep positional function-parameter parens idempotent. If a CST parameter pattern already renders with the necessary outer parentheses, do not wrap it again in `render_positional_parameter_pattern`; constructor-pattern parameters such as `fun (Conn conn) -> ...` must stay stable across repeated formatting.
59. Render typed first-class-module patterns from `Syn.Cst.Pattern.FirstClassModule.module_type` through the structural module-type renderer. Do not keep `(module M : S)` on an explicit unsupported path when the CST already carries the binder and module-type shape.
60. Parenthesize attributed non-atomic expressions before rendering postfix `[@attr]`. Constructor payloads, ordinary applications, and infix expressions such as `Some 0 [@inline always]`, `f x [@inline always]`, and `a + b [@inline always]` must keep their full payloads and remain idempotent across repeated formatting.
61. Render polymorphic-variant inherit patterns by printing `#` separately from `Syn.Cst.Pattern.PolyVariantInherit.type_path`. That path should already exclude the sigil, so `#color` and `#M.color` must not collapse to `##` or duplicate the sigil during formatting.
62. Render plain `object ... end` expressions structurally from `Syn.Cst.Expression.Object`. Support empty objects, self patterns, `method`/`val`/`inherit`/`initializer` members, and postfix member attributes without replaying source text; keep object extension members on an explicit unsupported path until `syn` exposes the remaining structural ownership facts there.
63. When body/branch boundary trivia still comes from token-attached leading trivia, read it through `Syn.Cst.leading_trivia_after`, `leading_trivia_before_node`, and `leading_trivia_after_token_before_node`. Do not walk `Ceibo.Red.SyntaxNode.tokens` in `lower.ml` just to rediscover the first token and its attached trivia.

## Validate

`timeout 30 tusk build krasny`
`timeout 30 tusk test krasny:format_tests`
`timeout 900 python3 packages/krasny/tests/test_runner.py`

Target individual fixture subsets when needed:
`timeout 900 python3 packages/krasny/tests/test_runner.py --filter 0100`
`timeout 900 python3 packages/krasny/tests/test_runner.py --refresh`

Audit fixture taxonomy and duplicate pressure when curating the corpus:
`python3 packages/krasny/tests/fixture_audit.py`
