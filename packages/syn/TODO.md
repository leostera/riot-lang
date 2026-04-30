# syn TODO

## Parser Streaming Follow-up

The current parser streams grammar events directly into `Syntax_tree.Builder`,
but the lexer still tokenizes the full source into an intermediate token list
before `Raw_token.stream` is built.

Next parser cleanup:

1. Add a demand-driven token source over `IO.IoVec.IoSlice.t`.
2. Move trivia attachment into that token source.
3. Preserve parser `current`/`peek`/`bump` semantics through a buffered token
   window.
4. Keep `Syntax_tree.Builder.token ~raw_index` as the tree-builder interface.
5. Benchmark eager tokenization against demand-driven tokenization before
   making the demand-driven path the default.

## Incremental Parsing Exploration

If Syn grows incremental parsing, keep it Syn-native:

1. Keep the public `Syntax_tree.t` and `Ast` handles stable.
2. Add reusable internal tree storage only where it proves useful.
3. Reuse complete syntax islands conservatively, starting at structure and
   signature item boundaries.
4. Verify every incremental update against a full parse of the edited source.
