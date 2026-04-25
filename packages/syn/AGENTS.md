# syn AGENTS

`syn` is the OCaml lexer, streaming parser, lossless syntax tree, diagnostics,
and Ast typed-view layer.

## Rules

1. Keep one parser path. `Syn.Parser` is the streaming parser implementation,
   and public parse entrypoints should accept source slices rather than strings.
2. Do not reintroduce the old Ceibo parser, typed `Cst`, CST builder, CST JSON
   snapshots, or CST visitor/traversal stack. Shared traversal should go
   through the Ast-driven `Syn.Visitor`.
3. Preserve lossless parsing. Raw tokens, spans, diagnostics, comments, and
   docstrings must stay recoverable from the streaming parser tree.
4. Keep parser-built syntax trees trivia-free in child arrays. Comments,
   docstrings, and whitespace belong in token-attached leading trivia.
5. Keep formatter-facing Ast whitespace trivia collapsed. Exact raw whitespace
   can stay in the backing tree for spans and diagnostics, but Ast token views
   should expose whitespace as a structural marker.
6. Keep docstring delimiter structure explicit on `Ast.Token`: opening,
   delimiter-free content, and optional closing delimiter must be available
   without rescanning raw comment text.
7. Prefer explicit `SyntaxKind` facts and spans over inferred syntax shape.
8. When syntax support grows, add parser/Ast coverage before relying on that
   syntax from downstream packages.
9. Keep `Syn.Deps` on the Ast path and differential against `ocamldep`-style
   expectations for dependency behavior.
10. Prefer `Std.Test.FixtureRunner` plus `Std.Test.Snapshot` for fixture-backed
    parser and diagnostic suites.

## Validate

`timeout 120 riot build -p syn --json`
`timeout 180 riot test -p syn -f deps --json`
`timeout 180 riot test -p syn -f fixture --json`
`timeout 180 riot test -p syn -f diagnostic --json`
`timeout 180 riot test -p syn -f ast --json`
