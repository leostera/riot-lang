# TinyML Stage 0

TinyML is Riot's small implementation language and the Rust Stage 0 compiler.
Stage 0 currently retains its JavaScript/Node target and has an experimental C
source target for the closed `main : unit -> i32` smoke subset. The C target is
being expanded for the required C-first, no-JavaScript bootstrap; it does not
yet compile the full Riotc source graph. The required native RiotML backend
belongs to the later RiotML compiler written in TinyML. See
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
cargo run -p tinyml -- compile path/to/Main.ml --target js -o build/Main.mjs -I path/to/modules
node build/Main.mjs
cargo run -p tinyml -- compile path/to/Main.ml --target c -o build/Main.c -I path/to/modules
clang -std=c11 build/Main.c -o build/Main
```

- `lex` prints tokens and byte spans; `parse` prints the AST.
- `check` resolves and typechecks the complete reachable module graph. It prints
  `ok` only when every module passes.
- `emit` writes requested `.ast`, `.tir`, or `.lambda` inspection artifacts
  beside the input. `--target js` emits the reachable JS module graph using the
  default output location.
- `compile` currently defaults to `--target js`; `--target c` selects the
  experimental C bootstrap emitter. Both targets check the full module graph
  before writing output. JavaScript emits the root `.mjs` and dependency modules
  beside it. C currently accepts only a single closed `main : unit -> i32`
  module with supported i32 expressions; it rejects imports and other
  constructs before writing a C artifact. Without `-o`, the selected target
  chooses `.mjs` or `.c` in the invocation directory.
- Errors are reported with a non-zero command status. Type and module failures
  stop compilation before target artifacts are written.

Module paths follow the RFD0050 naming and search rules: the importing
file's directory, ordered `-I` roots, bundled `tinyml/lib/`, then bundled
`tinyml/lib/std/`. `Host` is a compiler-provided intrinsic with Node
implementations for arguments, UTF-8 file reads/writes, stdout/stderr, and
safe process invocation (argument vectors, no shell interpolation).

## Current language boundary

The checker subset includes HM inference, top-level recursive functions,
annotations, selected source imports, tuples, records, recursive variants,
pattern matching, `unit`, `bool`, Unicode text strings, and exact
`i8/i16/i32/i64` and `u8/u16/u32/u64` arithmetic. Integer arithmetic wraps at
the annotated width; the JavaScript backend uses `BigInt`. The experimental C
emitter currently implements only the closed i32 entry slice described above.
`string + string` concatenates in the JavaScript backend. The bundled `String` module also provides `concat`, `length`,
`get`, `slice`, and `equal`; indices count Unicode scalar values, and `get`
returns `Option<i32>` containing a code point. Host text-file I/O uses strict
UTF-8 decoding and encoding. Floating point, general FFI, actors, distribution,
package management, WASM, and native code generation by Stage 0 are
unsupported. Unsupported or ill-typed input must be rejected rather than
treated as successful output.
