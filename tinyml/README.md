# TinyML Stage 0

TinyML is Riot's small implementation language and the Rust Stage 0 compiler
that checks TinyML source and emits runnable JavaScript ES modules for Node.js.
Stage 0's only compile target is `js`; the required native RiotML backend belongs
to the later RiotML compiler written in TinyML. See
[`docs/rfds/RFD0050-tinyml-bootstrap.md`](../docs/rfds/RFD0050-tinyml-bootstrap.md)
for the bootstrap contract and full acceptance criteria.

## Build and test

From the repository root:

```sh
cargo test -p tinyml
cargo run -p tinyml -- --help
```

The tests cover checker acceptance/rejection and execute generated JS with Node.

## CLI

All commands take one TinyML source file. `-I DIR` adds a module search root;
repeat it to add roots in order.

```sh
cargo run -p tinyml -- lex path/to/Main.ml
cargo run -p tinyml -- parse path/to/Main.ml
cargo run -p tinyml -- check path/to/Main.ml -I path/to/modules
cargo run -p tinyml -- emit --stage ast --stage tir --stage lambda path/to/Main.ml
cargo run -p tinyml -- emit --target js path/to/Main.ml -I path/to/modules
cargo run -p tinyml -- compile path/to/Main.ml -o build/Main.mjs -I path/to/modules
node build/Main.mjs
```

- `lex` prints tokens and byte spans; `parse` prints the AST.
- `check` resolves and typechecks the complete reachable module graph. It prints
  `ok` only when every module passes.
- `emit` writes requested `.ast`, `.tir`, or `.lambda` inspection artifacts
  beside the input. `--target js` emits the reachable JS module graph using the
  default output location.
- `compile` accepts only `--target js` (the default). It checks the full graph
  before writing JavaScript. `-o` selects the root `.mjs` path; dependency
  modules are written beside it as `<ModuleName>.mjs`. Without `-o`, the root
  artifact is `<input-stem>.mjs` in the invocation directory.
- Errors are reported with a non-zero command status. Type and module failures
  stop compilation before JS artifacts are written.

Module paths follow the RFD0050 naming and search rules: the importing
file's directory, ordered `-I` roots, bundled `tinyml/lib/`, then bundled
`tinyml/lib/std/`. `Host` is a compiler-provided intrinsic with Node
implementations for arguments, UTF-8 file reads/writes, stdout/stderr, and
safe process invocation (argument vectors, no shell interpolation).

## Current language boundary

The Stage 0 subset includes HM inference, top-level recursive functions,
annotations, selected source imports, tuples, records, recursive variants,
pattern matching, `unit`, `bool`, strings, and exact `i8/i16/i32/i64` and
`u8/u16/u32/u64` arithmetic. Integer arithmetic wraps at the annotated width;
JavaScript lowering uses `BigInt`. `string + string` concatenates. Floating
point, general FFI, actors, distribution, package management, WASM, and native
code generation by Stage 0 are unsupported. Unsupported or ill-typed input
must be rejected rather than treated as successful output.
