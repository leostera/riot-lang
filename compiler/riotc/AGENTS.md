# riotc AGENTS

`compiler/riotc` is the self-hosted Riot ML compiler written in Riot ML and built by `compiler/stage0` during bootstrap.

## Current Scope

- Keep riotc as the place for the real compiler architecture: CLI, source loading, diagnostics, syntax, checking, interfaces, lowering, and backend orchestration.
- Keep stage0 as a bootstrap compiler. Add stage0 features only when a riotc slice immediately needs them.
- Prefer small, valid Riot ML modules that stage0 can compile as soon as the relevant language feature exists.
- Keep `.rsig` concepts structured and binary-compatible in spirit; avoid stringly compiler internals.

## Rules

1. Use `use`, not `import`, in Riot ML source.
2. Prefer explicit domain names such as `SourcePath`, `ModuleName`, `BindingId`, and `FunctionTable` over anonymous/stringly structures.
3. Keep slices vertical when possible: source model or syntax -> typed representation -> lowering/backend/runtime only if needed -> smoke fixture.
4. Do not add package-aware behavior to stage0 to support riotc; put final compiler behavior in riotc.
5. Commit every value-adding riotc slice with a conventional commit and concrete validation evidence.
