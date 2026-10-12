# TinyML Stage 0

TinyML is Riot's small implementation language and the Rust Stage 0 compiler.
Stage 0 has a JavaScript/Node target and a C source target. The C target lowers
checked module graphs through TinyML LambdaIR into a single C translation unit
linked with the Riotc C runtime. It emits C for the complete reachable
`riotc/main.ml` source graph and is the required no-JavaScript bootstrap
bridge. It supports a narrower TinyML subset than the JS target and rejects
unsupported constructs before writing output. It is not the Riotc-owned native
backend: once bootstrapped, Riotc source must use its own IR and AArch64
pipeline. See
[`docs/rfds/RFD0050-tinyml-bootstrap.md`](../docs/rfds/RFD0050-tinyml-bootstrap.md)
for the bootstrap contract and full acceptance criteria.

## Build and test

From the repository root:

```sh
cargo test -p tinyml
cargo run -p tinyml -- --help
```

The tests cover checker acceptance/rejection, generated JS behavior, and the
C-first Riotc bootstrap. The test
`compile_c_first_bootstraps_riotc_without_javascript` emits the compiler graph
twice, builds the C-hosted compiler, and compiles/runs a smoke program with
Node excluded from `PATH`. On macOS/AArch64,
`c_hosted_bootstrap_compiles_native_direct_call_graph_without_javascript`
additionally uses that C-hosted compiler to compile and run bounded direct-call
and recursive AArch64 graphs with Node excluded; this is not native Riotc
self-rebuild.

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
clang -std=gnu11 -I riotc build/Main.c riotc/runtime.c -o build/Main
RIOTML_RUNTIME_DIR="$PWD/riotc" build/Main
```

- `lex` prints tokens and byte spans; `parse` prints the AST.
- `check` resolves and typechecks the complete reachable module graph. It prints
  `ok` only when every module passes.
- `emit` writes requested `.ast`, `.tir`, or `.lambda` inspection artifacts
  beside the input. `--target js` emits the reachable JS module graph using the
  default output location.
- `compile` currently defaults to `--target js`; `--target c` selects the
  Stage 0 bootstrap emitter. Both targets check the full reachable module graph
  before writing output. JavaScript emits the root `.mjs` and dependency
  modules beside it. C emits a graph as one translation unit and supports the
  compiler source graph plus a narrower subset of TinyML (including checked
  imports, curried closures, aggregate patterns, i32 arithmetic, NUL-aware
  strings, and selected Host bindings). It rejects unsupported constructs
  before writing a C artifact. Link generated C with `riotc/runtime.c` and
  include `riotc/runtime.h`. Generated programs read `RIOTML_RUNTIME_DIR` at
  startup to expose the runtime directory through `Host.runtime_directory`.
  Without `-o`, the selected target chooses `.mjs` or `.c` in the invocation
  directory.
- Errors are reported with a non-zero command status. Type and module failures
  stop compilation before target artifacts are written.

Module paths follow the RFD0050 naming and search rules: the importing
file's directory, ordered `-I` roots, bundled `tinyml/lib/`, then bundled
`tinyml/lib/std/`. `Host` is a compiler-provided intrinsic with JavaScript/Node
implementations for arguments, UTF-8 file reads/writes, stdout/stderr, and safe
process invocation (argument vectors, no shell interpolation). The C runtime
provides a subset of those operations for the no-JavaScript bridge.

## Current language boundary

The checker subset includes HM inference, top-level recursive functions,
annotations, selected source imports, tuples, records, recursive variants,
pattern matching, `unit`, `bool`, Unicode text strings, and exact
`i8/i16/i32/i64` and `u8/u16/u32/u64` arithmetic. Integer arithmetic wraps at
the annotated width; the JavaScript backend uses `BigInt`. The C emitter
lowers only its supported subset through the existing dynamic `RtValue`
runtime and does not implement all integer-width semantics. `string + string`
concatenates in both backends where supported.
The bundled `String` module also provides `concat`, `length`,
`get`, `slice`, and `equal`; indices count Unicode scalar values, and `get`
returns `Option<i32>` containing a code point. Host text-file I/O uses strict
UTF-8 decoding and encoding. The C runtime preserves embedded NUL bytes in
string values, file contents, and stdout/stderr; `Host` paths, process names,
and process arguments reject NUL because the underlying OS APIs require C
strings. Floating point,
general FFI, actors, distribution,
package management, WASM, and native code generation by Stage 0 are
unsupported. Unsupported or ill-typed input must be rejected rather than
treated as successful output.
