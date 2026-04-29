# Riot Benchmarking Workflow

Use this reference when authoring, running, or interpreting Riot benchmarks.

## Commands

- Prefer JSON output: `riot bench --json`.
- Narrow by package: `riot bench -p <package> --json`.
- List benchmarks before guessing filters: `riot bench -p <package> --list --json`.
- Filter by substring with `-f`: `riot bench -p <package> -f <filter> --json`.

## Regression Checks

- Use stable explicit controls when checking for regressions:
  - `riot bench -p <package> --warmup 10 --compare 5 --json`
- Report both benchmark failures and missing comparison history.
- If no historical comparison rows are returned, summarize the current means directly.

## Comparison Semantics

- `riot bench --compare N` compares the current run with previous recorded comparable runs.
- It is not how to compare unrelated benchmark functions inside one current run.
- A benchmark comparison group should contain functions that do the same work.
- Split pipeline stages into separate comparison groups when the stages differ.

Good comparison groups:

- parse implementation A vs parse implementation B
- source representation A vs source representation B
- lower implementation A vs lower implementation B
- solve implementation A vs solve implementation B

Avoid mixing unlike stages in one comparison group, such as comparing a full formatter against a pre-solved document formatter.
