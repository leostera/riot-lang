# syn AGENTS

`syn` is the OCaml lexer, parser, CST, and diagnostics layer.

## Rules

1. Preserve lossless parsing. Token and trivia retention matter.
2. Parser recovery changes are user-facing because tooling builds on diagnostics.
3. Keep syntax tree changes coordinated with any tooling that consumes `syn`, especially `tusk-fix` and `tusk-eval`.
4. Prefer explicit syntax kinds and spans over inferred structure.
5. Keep `Syn.Cst` faithful to the successful `Ceibo` parse. If a syntax family cannot be lifted precisely, bail from the builder instead of introducing public placeholder nodes.
6. Keep the CST root explicit about implementation vs interface files; do not collapse `.ml` and `.mli` structure into one ambiguous top-level shape.
7. Keep file-level item families split between `StructureItem` and `SignatureItem`; do not reintroduce a shared mixed top-level item enum.
8. Keep `cst.ml` focused on public types, `cst_builder.ml` focused on lifting, and `cst_json.ml` focused on fixture serialization.
9. Keep shared CST recursion in `visit.ml`; syntax consumers should not each reimplement their own expression and type walkers.
10. Keep parsing and CST construction split: `Parser.parse_*` and `Syn.parse*` return Ceibo trees plus diagnostics, while `Syn.build_cst` performs the explicit faithful lift.
11. Keep `Ceibo` sourced from `packages/ceibo`; do not reintroduce a vendored `packages/syn/src/ceibo` copy.
12. Keep pattern attributes orthogonal to pattern shape; attach them via `Pattern.attributes` instead of a `Pattern.Attribute` wrapper node.
13. Keep expression attributes orthogonal to expression shape; attach them via `Expression.attributes` instead of wrapper nodes or postfix-shell `Apply` artifacts.
14. Keep record-expression fields parsetree-like: always lift a field value expression, and preserve punning with explicit metadata instead of `None`.
15. Keep packed first-class module expressions direct: `Expression.ModulePack.module_expression` should be the packed payload itself, and any `: S` ascription should stay in `Expression.ModulePack.module_type` instead of being rewritten as an inner `ModuleExpression.Constraint`.
16. Keep grouped `type ... and ...` items on `TypeDeclaration.and_declarations`; do not reintroduce a separate `TypeMutualDeclaration` CST node.
17. Keep standalone top-level comments and docstrings explicit in the CST item stream; do not bury their ownership in enclosing declaration spans.
18. Keep nested `sig ... end` and `struct ... end` syntax-node lifts normalized the same way as file-level lifts; callers should not have to provide extra source text just to get correct trivia ownership.
19. Keep raw trivia ownership explicit on declaration nodes that can carry inline comments/docstrings, even before higher-level sequence normalization decides whether adjacent docstrings stay standalone or attach to a neighbor.
20. Keep token-attached trivia as the source of truth; do not reintroduce standalone trivia tree children in Ceibo once the migration lands.
21. Keep `Lexer.tokenize` emitting only real tokens plus `Token.EOF`, with trailing file trivia on `Token.EOF.leading_trivia`.
22. Keep `Parser.parse_result.tokens` as the original lexer token stream during the migration so tools can stay lossless before trivia children disappear from the tree, and use that stream as the source of truth for file-level standalone trivia instead of re-lexing source text.
23. Keep parser-built green trees trivia-free in normal paths: parser control flow may still consume token-attached leading trivia explicitly during the migration, but green child arrays should represent trivia through token `leading_trivia`, not standalone trivia elements.
24. Keep top-level structure/signature doc ownership on one shared ordered-item pass; do not let those normalization rules fork by item family again.
25. Keep top-level standalone comment/doc ordering driven by token order: derive file items from each item’s first-token leading trivia plus `EOF.leading_trivia`, not by subtracting syntax-node spans from the file.
26. Keep nested `sig ... end` and `struct ... end` bodies exposed through CST-node helpers such as `CstBuilder.signature_items_of_module_type` and `CstBuilder.structure_items_of_module_expression`; callers should not have to relift raw nested syntax anchors by hand just to get normalized item ownership.
27. Keep grouped `type ... and ...` ownership member-driven: normalize each member from its own `TYPE_DECL` token stream, then reassemble the public grouped declaration, instead of redistributing docs/headings out of the grouped parent node by source-span slicing.
28. Keep red traversal aligned with that same contract for parser-built trees: `SyntaxNode.children`, `direct_tokens`, and `tokens` should already be trivia-free, with comments/docstrings reachable through `SyntaxToken.leading_trivia`.
29. Keep member doc ownership leading-only: constructor docstrings in inter-member gaps should attach to the next constructor's `owned_trivia.leading`, while postfix comments may still bubble backward onto the previous constructor and terminal constructor docstrings stay standalone.
30. Keep record field doc ownership leading-only: field docstrings in inter-field gaps should attach to the next field's `owned_trivia.leading`, while postfix comments may still bubble backward onto the previous field and terminal `}`-owned docs/comments must be preserved without stealing them for the last field.
31. Keep explicit member-stream normalization limited to repeated member grammars with public member `owned_trivia` today: variant constructors and record fields. Exception declarations stay on the ordinary ordered-item path, and object type fields should not gain member-stream ownership rules until the CST gives them `owned_trivia` and a renderer contract.

## Validate

`timeout 30 tusk build syn`
`timeout 180 tusk test syn:cst_tests`
`timeout 900 python3 packages/syn/tests/test_runner.py fixtures`
`timeout 900 python3 packages/syn/tests/test_runner.py cst`
