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

- `syntax/lexer.ml`: source tokenization.
- `syntax/parser.ml`: parser for the current `main : unit -> i32` slice.
- `syntax/ast.ml`: source AST.
- `check/typer.ml`: expression type checking and entry-point validation.
- `lambda/ir.ml`, `lambda/lower.ml`: checked AST to Lambda IR.
- `codegen/c/emit.ml`: C emission for the current subset.
- `runtime.h`, `runtime.c`: native value and host runtime linked by Clang.
- `main.ml`: compiler pipeline and CLI.

The native compiler is intentionally incomplete: it currently supports integer
literals, parentheses, unary minus, `+`, `-`, `*`, and `/` in the entry
expression. It does not yet parse or compile its own source tree, resolve
RiotML modules, or support the full documented language subset. The required
JS-to-native self-compilation gate and the existing RFD0050 completion criteria
remain open; this slice is not a substitute for them.
