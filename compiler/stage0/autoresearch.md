# Autoresearch rules

Goal: grow the shared RiotML compiler fixture corpus by translating small old `compiler/syn/tests/fixtures/*.ml` and `compiler/typ/tests/fixtures/corpus/*.ml` examples into new RiotML fixtures under `compiler/fixtures/programs`, then extend `compiler/stage0` only as needed to compile/run them.

Validation command:

```sh
env LLVM_SYS_221_PREFIX=/opt/homebrew/opt/llvm cargo test --manifest-path compiler/stage0/Cargo.toml
```

Loop constraints:

- Prefer smallest next language/compiler feature.
- Keep provenance in imported fixture filename; do not add metadata comments to `.ml` files.
- Add `.stdout` when executable output is deterministic.
- Keep stage0 deliberately narrow; do not implement broad features for one fixture.
- Do not weaken existing positive fixtures.
- Preserve unrelated worktree changes.
