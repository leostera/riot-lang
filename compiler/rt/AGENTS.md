# rt AGENTS

`compiler/rt` is the Rust runtime linked by generated Riot ML programs.

## Current Scope

- Export stable C ABI symbols for generated native code.
- Keep runtime entrypoints small and explicit while stage0 is bootstrapping.
- Prefer ordinary Rust `std` while the runtime design is moving quickly.
- Avoid Rust panics across FFI boundaries.
- Stage0 actors are compiler-emitted native state machines. The runtime stores
  opaque actor frames, owns mailboxes/scheduling, and calls compiler-provided
  resume function pointers; it must not interpret source-level actor programs.
- Actor frames use the v2 ABI: generated code passes size, alignment, and
  optional drop hooks so the runtime owns frame teardown after actor completion.

## Rules

1. Mark exported runtime functions with stable unmangled symbols.
2. Treat pointer/length pairs from generated code as unsafe inputs.
3. Keep ownership rules explicit at the ABI boundary.
4. Add runtime behavior here only when generated code needs it.
5. Keep actor behavior composable: avoid adding hardcoded actor variants for
   source-level patterns that should lower through compiler IR passes.
