# Testing and benchmarking

Use this reference when the task involves `riot test`, `riot bench`, suite
selection, or repository-shared test policy.

## Mental model

At the top level, `riot test` and `riot bench` are Riot commands that:

1. build the needed packages once
2. discover suite binaries
3. run those suite binaries through their machine-readable contracts
4. aggregate the results

That means the right user workflow is usually:

- narrow by package or suite first
- then narrow by query if needed
- use `--json` when tooling needs structured results

## Test selection

These are different selectors:

- `riot test`
  Runs the default test set.
- `riot test <query>`
  Filters test cases by substring.
- `riot test <package:suite>`
  Narrows suite discovery before running cases.
- `riot test --small`
  Runs only cases marked small.
- `riot test --large`
  Runs only cases marked large.
- `riot test --flaky`
  Runs only cases marked flaky.

Do not confuse package or suite selection with case-name selection.

## Shared test policy

Repository-shared test policy lives in `.riot/config.toml`:

```toml
[riot.test]
small_test_timeout = "500ms"
flaky_max_retries = 3
```

Use that file for repository-local policy. Do not put this policy in
`riot.toml`, and do not assume every user should store it in
`~/.riot/config.toml`.

## Advanced note: suite binaries

If you are debugging the generated suite binary directly, Riot test suites
typically expose subcommands such as:

- `list-tests`
- `run-tests [query]`

Benchmark binaries similarly expose:

- `list-benchmarks`
- `run-benchmarks [query]`

Most user tasks should still go through `riot test` or `riot bench` first.

## When to use `--json`

Use `--json` when:

- you need to feed results into tooling
- you need reliable machine-readable timing or status output
- scraping human output would be fragile

Examples:

```sh
riot test --json
riot bench --json
```
