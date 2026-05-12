# Syn Replacement Parser

This branch treats the current parser and handwritten CST builder as disposable.
The package name stays `syn`; the implementation is replaced in place.

## Decisions

1. Build one concrete tree.
   The lossless syntax tree is the only materialized syntax tree. The typed CST
   programming model is preserved as views over syntax nodes, not as a second
   object graph.

2. Parse through events.
   Parser functions recognize grammar and emit events. They do not allocate tree
   nodes, CST records, child lists, or trivia structures directly.

3. Keep recursive descent as the default style.
   Prefer one parser function per grammar family. Use Pratt/precedence parsing
   only for expression, pattern, and type precedence islands where recursive
   descent would make precedence rules less explicit.

4. Keep token kinds lexical.
   Leaf kinds are exact tokens such as `IDENT`, `LET_KW`, and `EQ`. Grammar
   nodes are interior kinds such as `LET_EXPR`, `TYPE_DECL`, and `PATH_EXPR`.
   The parser may remap contextual identifiers to soft keyword tokens in the
   contexts where they are keywords.

5. Keep trivia raw.
   Comments, docstrings, and whitespace live in the raw token stream. Formatter,
   documentation, and lint ownership are query-layer decisions, not parser facts.

6. Keep tokens as source ranges.
   Token leaves carry raw-token ranges and source offsets. Token text is sliced
   from the original source on demand instead of copied into every tree leaf.

7. Do not implement classes or objects.
   This parser intentionally rejects or recovers over OCaml class/object syntax.
   Riot does not need that grammar surface for its own source and tools.

## Target Pipeline

```text
source
-> raw token array, including trivia
-> significant-token index array
-> recursive-descent / Pratt event parser
-> packed green tree
-> typed CST-style views over syntax nodes
```

## Public Programming Model

OCaml users should still be able to write code that feels like CST pattern
matching:

```ocaml
match Ast.Expr.view expr with
| Ast.Expr.Let { bindings; body; _ } -> ...
| Ast.Expr.Apply { callee; argument; _ } -> ...
| Ast.Expr.If { condition; then_branch; else_branch; _ } -> ...
| Ast.Expr.Unknown node -> ...
```

`Ast.Expr.t` is a wrapper around a syntax node. Calling `view` classifies the
node and returns lightweight wrappers for its children.

## First Grammar Slice

The first replacement slice should cover enough real Riot code and formatter
fixtures to validate the substrate:

- implementation and interface source roots
- structure and signature item lists
- `let`, `type`, `module`, `module type`, `open`, `include`, `external`, `val`
- literals, paths, tuples, lists, arrays, records
- `fun`, `function`, `if`, `match`, `try`, `let ... in`
- application, prefix, infix, postfix selectors, assignment
- core types with arrows, tuples, constructors, variables, aliases
- attributes/extensions as lossless opaque syntax shells
- recovery with missing tokens and diagnostics

The parser should not block on complete OCaml parity before replacing the old
pipeline. Unsupported syntax gets an error node and a diagnostic, while the tree
remains lossless.
