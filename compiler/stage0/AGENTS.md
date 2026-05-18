# stage0 AGENTS

`compiler/stage0` is the Rust bootstrap compiler for the next Riot ML compiler.

## Current Scope

- Compile one `.ml` source file to one native executable.
- Keep the pipeline compiler-shaped: parse, typecheck, lower, LLVM codegen, link.
- Prefer established Rust compiler crates over ad hoc infrastructure.
- Link generated objects against `compiler/rt` through its exported C ABI.

## Rules

1. Keep diagnostics source-backed and span-backed.
2. Keep frontend, typed IR, lowered IR, and backend boundaries explicit even
   while the accepted language is tiny.
3. Use LLVM through Rust bindings for native codegen.
4. Preserve `fn main() { ... }` as the only entrypoint form until the language
   design grows a module/package model.
