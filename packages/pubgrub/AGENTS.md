# pubgrub AGENTS

`pubgrub` owns the version solving algorithm implementation.

## Rules

1. Solver determinism and explanation quality matter more than clever internal shortcuts.
2. Changes to conflict resolution should be paired with focused solver cases.
3. Keep package-management policy out of the core algorithm where possible.
4. `Pubgrub.Trace` is the supported structured debugging surface; prefer extending it over ad hoc prints.
5. Snapshot tests are appropriate for solver traces, and should cover stable high-signal scenarios.
6. Solver code should stay quiet in normal operation. Capture debugging through `Pubgrub.Trace` and tests.
