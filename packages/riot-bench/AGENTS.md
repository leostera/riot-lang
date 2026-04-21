# riot-bench AGENTS

`riot-bench` owns Riot-specific benchmark history storage and future regression tracking helpers.

## Rules

1. Keep the on-disk bench history format append-only and self-contained.
2. Store benchmark history package-first under `.riot/bench/<package>/<suite>/runs/<run-id>.json`.
3. Each stored suite-run file must stand on its own; do not require a separate global index to interpret it.
4. Persist normalized aggregate benchmark statistics, not raw per-iteration samples.
5. `riot-bench` should not know about CLI rendering or build orchestration. It owns the storage contract, path layout, and serialization.
6. Filtered or package-narrowed runs must be marked as partial in stored metadata so later regression checks do not confuse them with full-suite baselines.
7. History comparison logic belongs here too. Match prior runs by package, suite, profile, and target, and align benchmark history by benchmark case name or comparison-case `(description, case)` pair.

## Validate

`timeout 30 riot build riot-bench`
`timeout 30 riot test riot-bench:history_tests`
