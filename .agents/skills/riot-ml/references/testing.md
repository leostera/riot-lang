# Riot Testing Workflow

Use this reference when authoring or debugging Riot tests.

## Commands

- Prefer JSON output: `riot test --json`.
- Narrow early with packages: `riot test -p <package> --json`.
- List tests before guessing selectors: `riot test -p <package> --list --json`.
- Filter by substring with `-f`: `riot test -p <package> -f <filter> --json`.
- Use `-p <package>` and `-f <filter>` for tight iteration, then broaden validation once the focused slice is green.
- Use `fmt --check` when you only need formatter status or timing; run `fmt` itself only when changing workspace formatting is intended.

## Naming

Riot composes suite names and case names in selectors/output.

Do not repeat the suite name in each case. For a suite named
`typ:infer-module-interface`, use case names like:

```ocaml
case "empty env renders empty interface" test_empty_env_renders_empty_interface
```

Avoid:

```ocaml
case "infer-module-interface: empty env renders empty interface" test_empty_env_renders_empty_interface
```

That would produce duplicated selectors such as
`typ:infer-module-interface:infer-module-interface: ...`.

## Fixtures And Snapshots

- Snapshot mismatches are not compile failures. Report them separately from build errors.
- Snapshot mismatches produce `.expected.new` files. Do not approve them by default.
- Compare the approved `.expected` and pending `.expected.new` output:
  - if they match after rerunning, move on;
  - if they differ, fix behavior or intentionally review/approve the new snapshot.
- For policy-driven formatter or lint output, add or update focused snapshot coverage before changing broad behavior.
- Fixture runners may skip files with pending `.expected.new` snapshots. If a filtered fixture appears to run zero tests, check for a pending snapshot first.
- Stale `.expected.new` files can keep a run failing after the underlying behavior is fixed. Remove or reject stale candidates before rerunning.
- Use `riot snapshots reject` to clear pending candidates when you need a clean
  rerun.
- Use focused filters for frontier fixtures, then run the package suite once the
  slice is stable.
- Prefer inline snapshots for small focused cases, and fixture snapshots when real-file coverage or corpus behavior matters.
