# stage0 AGENTS

`compiler/stage0` is the Rust bootstrap compiler for the next Riot ML compiler.

## Current Scope

- Compile one or more `.ml` source files to one native executable with
  `stage0 compile <file>... -o <output>`.
- Compile one or more `.ml` source files to module artifacts with
  `stage0 compile-lib <file>... --out-dir <dir>`, which emits `<Module>.rsig`
  and `<Module>.o` for each input.
- Emit compiler artifacts with `stage0 emit <pass> <file>`. Supported passes
  are `cst`, `typed`, `rsig`, `ir`, `actor-ir`, `llvm`, `assembly`,
  `object`, and `all`.
- Keep `.rsig` as the binary typed interface artifact. Use `stage0 emit all`
  when a human-readable canonical signature view is needed in tests or review.
- Source file stems must be lowercase, for example `hello.ml` and
  `hello_world.ml`; module names are derived as `Hello` and `HelloWorld`.
- Multi-source `compile` and `compile-lib` parse every input before lowering,
  build a small module graph from `use`, `pub mod`/`mod`, and `include`
  declarations, and process inputs in topological order.
- Keep the pipeline compiler-shaped: parse, typed HIR/signature, lambda IR,
  actor-frame IR, LLVM codegen, link.
- Treat `emit actor-ir` as a compiler boundary snapshot: it should expose actor
  frame layout, captures, resume states, receive points, and terminal states.
- Prefer established Rust compiler crates over ad hoc infrastructure.
- Link generated objects against `compiler/rt` through its exported C ABI.

## Rules

1. Keep diagnostics source-backed and span-backed.
2. Keep frontend, typed IR, lowered IR, and backend boundaries explicit even
   while the accepted language is tiny.
3. Use LLVM through Rust bindings for native codegen.
4. Preserve `fn main() { ... }` as the executable entrypoint. Non-main
   functions and externals are exported through `.rsig`.
5. Keep the fixture harness generated from `tests/fixtures`, which is a symlink
   to `compiler/fixtures`; `build.rs` should continue emitting one cargo test
   per `.ml` fixture.
6. `use Name` resolves `Name.rsig` from the source directory and `--sig-dir`
   entries, and qualified access should stay explicit as `Name.value`.
7. Review compiler-output snapshot changes under `tests/snapshots` with
   `cargo insta review` or update them intentionally with `INSTA_UPDATE`.
8. Keep the parser explicit: `logos` tokenization followed by recursive descent
   with precedence-focused expression parsing.
