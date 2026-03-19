# pubgrub AGENTS

`pubgrub` owns the version solving algorithm implementation.

## Rules

1. Solver determinism and explanation quality matter more than clever internal shortcuts.
2. Changes to conflict resolution should be paired with focused solver cases.
3. Keep package-management policy out of the core algorithm where possible.

## Validate

`timeout 30 tusk build pubgrub`
