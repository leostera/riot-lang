# Riot Repository Conventions

Use these conventions when contributing inside this repository. Package-local `AGENTS.md` files may add stricter rules.

## Riot ML Style

- Prefer explicit `Result` and `Option` flows for expected failure.
- Use `Result` / `Option` for normal control flow.
- Prefer `from_*` / `to_*` conversion names over `of_*` where this stack has a choice.
- Prefer structured error types over polymorphic variants for new error surfaces unless an existing public API requires otherwise.
- Keep unsafe behavior visible in names such as `unsafe_*` or `_unchecked`.
- Prefer package-owned abstractions over raw `Stdlib`, `Unix`, `Sys`, or `Obj` leakage through public surfaces.

## Collections And Hot Paths

- Prefer `Vector` over `ref []` when growing collections.
- Preallocate vectors with `Vector.with_capacity` when any size hint is available.
- Use `Vector` or preallocated collection builders for append-heavy performance-sensitive paths.
- Keep parser, formatter, dependency, and planner hot paths allocation-aware; benchmark before and after broad rewrites.

## Syn And AST APIs

- Prefer semantic AST view helpers over direct token or node spelunking.
- Use module-specific helpers such as `Ast.RecordField.span`, `Ast.RecordType.field_count`, or `Ast.Ident.*` for operations that belong on the typed view.
- Keep identifiers as `Ast.Ident.t`, not loose token lists.
- Push recovery into cast results and unknown nodes; valid views should expose required fields as required.
- Traversal helpers should support accumulator-style folds and early return where useful.

## Formatter Work

- Formatting policy should be explicit and covered by fixtures.
- Prefer vertical layouts when in doubt, especially where future diffs become smaller.
- Width should be a central veto: an inline layout is only valid if it fits the configured width.
- Comments and docstrings are meaningful layout facts, not raw whitespace preservation.
- Test source snippets should be intentionally unformatted so the expected output demonstrates formatter behavior.
- In tests, prefer multiline OCaml quoted strings such as:

```ocaml
{ocaml|
let x=1
|ocaml}
```

## Public Contracts

- Use public package surfaces.
- If a public API becomes more flexible or performant, prefer reader/writer/slice-based paths over string-only APIs where the package design supports it.
- Add intermediate compatibility layers only when a downstream migration needs them; remove old paths once the branch intentionally replaces them.

## Documentation

- Keep AGENTS guidance current when behavior or contracts change.
- For release notes, explain what changed, why it matters, and any user-visible migration detail.
