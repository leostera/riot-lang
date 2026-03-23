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
9. Keep pattern attributes orthogonal to pattern shape; attach them via `Pattern.attributes` instead of a `Pattern.Attribute` wrapper node.
10. Keep expression attributes orthogonal to expression shape; attach them via `Expression.attributes` instead of wrapper nodes or postfix-shell `Apply` artifacts.
11. Keep record-expression fields parsetree-like: always lift a field value expression, and preserve punning with explicit metadata instead of `None`.
12. Keep packed first-class module expressions direct: `Expression.FirstClassModule.module_expression` should be the packed payload itself, and any `: S` ascription should stay in `Expression.FirstClassModule.module_type` instead of being rewritten as an inner `ModuleExpression.Constraint`.

## Validate

`timeout 30 tusk build syn`
`timeout 180 tusk test syn:cst_tests`
`timeout 900 python3 packages/syn/tests/test_runner.py fixtures`
`timeout 900 python3 packages/syn/tests/test_runner.py cst`
