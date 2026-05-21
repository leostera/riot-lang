# riotc

`riotc` is the first Riot ML compiler written in Riot ML. It is built by
`compiler/stage0` while the language bootstraps.

## Bootstrap Milestone

The first milestone is deliberately small:

1. Compile and run `src/main.ml` with stage0.
2. Return a deterministic exit code.
3. Classify or print tokens from a hardcoded source string.
4. Parse a tiny AST from that hardcoded source.
5. Only then add real file input and broader compiler behavior.

## Module Map

- `src/main.ml` owns the executable entrypoint.
- `src/cli.ml` will interpret command-line arguments and select commands.
- `src/pipeline.ml` will orchestrate source loading, lexing, parsing, checking,
  and later lowering/code generation.
- `src/source.ml` will model source paths, contents, and spans.
- `src/diagnostic.ml` will model structured diagnostics and stable rendering.
- `src/syntax/token.ml` defines token data.
- `src/syntax/lexer.ml` turns source text into tokens.
- `src/syntax/parser.ml` turns tokens into the surface AST.
- `src/syntax/ast.ml` defines the parsed Riot ML surface tree.
- `src/check/env.ml` models checker environments.
- `src/check/type.ml` defines compiler type data.
- `src/check/infer.ml` will own inference and checking.
- `src/check/typed_tree.ml` defines checked syntax.
- `src/interface/rsig.ml` models structured interfaces corresponding to stage0
  `.rsig` concepts.
- `src/lambda/lambda.ml` defines the lambda IR.
- `src/lambda/simplify.ml` owns lambda simplification.
- `src/actor/air.ml` defines actor IR.
- `src/backend/llvm/llvm.ml` and `src/backend/native/link.ml` will eventually
  own backend integration.

Riotc should discover and document the next stage0 gaps without turning stage0
into the final package-aware compiler.
