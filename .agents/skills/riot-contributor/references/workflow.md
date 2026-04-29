# Contributor Workflow

Use this reference when making changes inside the Riot repository.

## Start

1. Identify the touched domains.
2. Read root `AGENTS.md` as the router.
3. Read the nearest package, compiler, editor, docs, or specs `AGENTS.md` before editing.
4. Run `git status --short` before changing files and keep unrelated user work intact.

The repo-local `AGENTS.md` files are maintained alongside the code. When behavior, public contracts, validation commands, or ownership expectations change, update the relevant `AGENTS.md` in the same slice.

## Editing

- Prefer existing package patterns and local helper APIs over new abstractions.
- Keep edits scoped to the package or contract under discussion.
- Use public package surfaces.
- Thread explicit context values for per-run compiler, parser, formatter, or linting context.
- Prefer small behavior-driven slices with focused tests over broad unrelated cleanup.
- If a file already has user edits, work with them and preserve unrelated changes.

## Tests And Snapshots

- Add focused coverage before broad formatter, parser, lint, or planner policy changes.
- Prefer inline snapshots for small focused examples.
- Prefer fixture snapshots for real-file coverage, corpus behavior, and formatter policy.
- Snapshot mismatches create `.expected.new` files. Compare approved and pending output before accepting.
- Approve or reject snapshots only after comparing approved and pending output.
- Stale `.expected.new` files can keep a run failing after behavior is fixed; reject stale candidates before rerunning.

## Formatting

- Use `riot run riot -- fmt --check` for status or timing.
- Run `riot run riot -- fmt` only when in-place formatting is intended.
- For formatter development, stabilize with small policy fixtures first, then verify broader fixture suites.
- A full workspace format should be idempotent: repeated runs should converge with no extra changes.

## Commits

- Use conventional commits, scoped by package or area when practical:
  - `fix(syn): ...`
  - `feat(krasny): ...`
  - `docs(agents): ...`
- Commit cohesive slices grouped by behavior or package.
