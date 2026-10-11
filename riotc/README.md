# RiotML compiler bootstrap source

This is the repository-root source tree for the RiotML compiler written in
TinyML. Rust Stage 0 compiles `main.ml` and its source-module graph to JS:

```sh
cargo run -p tinyml -- compile riotc/main.ml -I riotc -o build/riotc.mjs
```

The JS entry point exports `main`; run it from a small Node launcher so
`Host.args` sees ordinary command-line arguments. The current development slice
accepts `compile SOURCE -o OUTPUT`, typechecks an i32 entry expression, emits C,
and invokes Clang with the checked-in host runtime. For example:

```js
// build/run-compiler.mjs
import { main } from "./riotc.mjs";
process.exitCode = Number(main(undefined));
```

```sh
node build/run-compiler.mjs compile path/to/Smoke.ml -o build/smoke
./build/smoke
```

## Source organization

- `syntax/lexer.ml`: source tokenization and source spans.
- `syntax/ast.ml`: domain-specific names, types, patterns, expressions, and
  module declarations.
- `syntax/parser.ml`: validates the initial top-level `main : unit -> i32`
  entry contract after parsing the source module.
- `syntax/parser/source_module.ml`: parses imports, type declarations, and
  top-level value declarations into `SourceModule`.
- `modules.ml`: loads imported source files in dependency order using importer
  directories, ordered include roots, and bundled library roots.
- `syntax/parser/common.ml`: token-stream results, delimiter helpers, and
  source-aware lexer error mapping.
- `syntax/parser/types.ml`: named, generic, tuple, type-variable, and arrow
  type expressions.
- `syntax/parser/patterns.ml` and `pattern_parts.ml`: patterns and pattern
  lists.
- `syntax/parser/expressions.ml`, `expression_parts.ml`, and `literals.ml`:
  expression precedence, nested expressions, record/list productions, and
  string literal decoding.
- `check/typer.ml`: type checking and native entry-point validation.
- `lambda/ir.ml`, `lambda/lower.ml`: checked AST to Lambda IR.
- `codegen/c/emit.ml`: C emission for the native slice.
- `runtime.h`, `runtime.c`: native value and host runtime linked by Clang.
- `main.ml`: compiler pipeline and CLI.

The native compiler remains intentionally incomplete. The current entry
expression subset includes `i32` literals, parentheses, unary minus, arithmetic
and comparisons, plus exhaustive boolean `match` expressions. The frontend can
parse complete source modules and load their dependency graph, but the native
compile pipeline still accepts only a standalone module containing `main`; it
does not yet typecheck or compile the loaded graph or the compiler's own source
tree. The required JS-to-native self-compilation gate and the RFD0050
completion criteria remain open; this slice is not a substitute for them.
