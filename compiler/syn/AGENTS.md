# syn AGENTS

`syn` is the OCaml lexer, streaming parser, lossless syntax tree, diagnostics,
and Ast typed-view layer.

## Rules

1. Keep one parser path. `Syn.Parser` is the streaming parser implementation,
   and public parse entrypoints should accept source slices.
2. Keep the streaming parser, lossless tree, and Ast-driven `Syn.Visitor` as
   the single parser/traversal path.
3. Preserve lossless parsing. Raw tokens, spans, diagnostics, comments, and
   docstrings must stay recoverable from the streaming parser tree.
4. Use `Syn.Span.t` for source ranges exposed by tokens, diagnostics, syntax
   trees, and Ast views. Keep Syn self-contained; do not re-export generic
   red/green tree packages through the public facade.
5. Keep parser-built syntax trees trivia-free in child arrays. Comments,
   docstrings, and whitespace belong in token-attached leading trivia.
6. Keep formatter-facing Ast whitespace trivia collapsed. Exact raw whitespace
   can stay in the backing tree for spans and diagnostics, but Ast token views
   should expose whitespace as a structural marker.
7. Keep docstring delimiter structure explicit on `Ast.Token`: opening,
   delimiter-free content, and optional closing delimiter must be available
   without rescanning raw comment text.
8. Prefer explicit `SyntaxKind` facts and spans over inferred syntax shape.
9. When syntax support grows, add parser/Ast coverage before relying on that
   syntax from downstream packages.
10. Keep the OCaml class/object subset outside the supported grammar:
   `class`, `object`, `method`, `new`, `virtual`, `inherit`, `initializer`,
   object types, and object method calls.
11. Keep `Syn.Deps` on the Ast path and differential against `ocamldep`-style
   expectations for dependency behavior.
12. Prefer `Std.Test.FixtureRunner` plus `Std.Test.Snapshot` for fixture-backed
    parser and diagnostic suites.
