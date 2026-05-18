# compiler fixtures AGENTS

`compiler/fixtures` is the shared source fixture tree for Riot ML compilers.
Fixtures here must be reusable by `stage0`, `riotc`, and future compiler
frontends.

## Layout

- `programs/**/*.ml`: positive fixtures that should compile.
- `programs/**/*.stdout`: optional expected stdout sidecar for a matching
  `.ml` program fixture.
- `diagnostics/**/*.ml`: negative fixtures that should fail with diagnostics.

## Rules

1. Keep fixtures compiler-neutral. Avoid expectations that only make sense for
   one compiler implementation unless they are isolated in a clearly named
   subdirectory.
2. Prefer many small fixtures over one large fixture when testing one language
   feature or diagnostic.
3. Put larger programs in grouped subdirectories once they exercise multiple
   features together.
4. Do not embed test metadata in `.ml` source comments; keep metadata in
   sidecar files so the source remains ordinary Riot ML.
5. Preserve the `programs` and `diagnostics` roots unless stage0's generated
   fixture harness is updated at the same time.
