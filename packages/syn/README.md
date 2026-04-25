# syn

`syn` is Riot's lossless OCaml lexer, streaming parser, diagnostics, and typed
Ast view layer.

It exists for tooling, not compilation. The package keeps source spans, raw
tokens, comments, docstrings, diagnostics, and a lossless syntax tree available
from one parser path. Typed Ast modules are lightweight views over that tree.

## Current shape

`syn` works in layers:

1. `Lexer` tokenizes source code for debugging and token-oriented tools.
2. `Parser` consumes an `IO.IoVec.IoSlice.t` and streams parser events.
3. `SyntaxTree` builds the lossless tree from raw tokens and parser events.
4. `Ast` exposes typed views over the same lossless tree.
5. `Deps` extracts syntactic module dependencies from Ast views.

Two invariants matter:

- parsing always returns a syntax tree, even for malformed input
- typed Ast views do not allocate a second concrete tree

In other words:

```ocaml
let source =
  IO.IoVec.IoSlice.from_string contents
  |> Result.expect ~msg:"failed to create source slice"
in
let result = Syn.parse ~filename:(Path.v "file.ml") source in

if Vector.length result.Syn.Parser.diagnostics = 0 then
  let root = Syn.Ast.SourceFile.make result.Syn.Parser.tree in
  ignore root
else
  ignore result.Syn.Parser.diagnostics
```

## Public API

The high-level entrypoints are:

- `Syn.tokenize`
- `Syn.parse`
- `Syn.parse_implementation`
- `Syn.parse_interface`

`Syn.parse*` APIs accept `IO.IoVec.IoSlice.t`. Keep string-to-slice conversion
at the edge of callers so the parser can share source storage with downstream
tools.

`Syn.parse` returns a `Parser.parse_result` with:

- `source`: the original source slice
- `kind`: implementation or interface
- `tokens`: source-backed raw token stream
- `tree`: vector-backed lossless syntax tree
- `diagnostics`: structured parse diagnostics

Use `Syn.SyntaxTree` when you need raw lossless traversal. Use `Syn.Ast` when
you want grammar-oriented typed views over the same tree.

## CLI helpers

The package ships CLI surfaces used by the fixture suite:

```sh
riot run syn -- parse --json path/to/file.ml
riot run syn -- token-stream --json path/to/file.ml
```

`parse --json` prints the lossless tree and diagnostics. `token-stream --json`
prints token records, including leading trivia.

## Testing

The main validation surfaces are:

```sh
timeout 120 riot build -p syn --json
timeout 180 riot test -p syn -f deps --json
timeout 180 riot test -p syn -f fixture --json
timeout 180 riot test -p syn -f diagnostic --json
timeout 180 riot test -p syn -f ast --json
```

When extending `syn`, add parser and Ast coverage before relying on new syntax
from downstream packages.
