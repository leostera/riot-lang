# tusk-cli AGENTS

`tusk-cli` owns command parsing, user-facing output, and the top-level build workflow.

## Rules

1. Keep the CLI thin. It should orchestrate, not duplicate planner or executor logic.
2. Prefer direct local calls into tusk libraries over protocol-shaped wrappers.
3. User-facing messages should stay concise and actionable.
4. When adding commands, update completions and help output in the same change.
5. Built-in commands that own domain logic elsewhere should delegate into their package library.
6. Commands that touch build artifacts must resolve the workspace root and honor `[tusk].target_dir` instead of assuming `_build` or `./target`.
7. Keep rule-oriented and diagnostic-oriented fix surfaces distinct: `--list-rules` should describe rules, while `--list-diagnostics` should describe diagnostic codes.
8. Keep `tusk test <query>` substring-based across package names, suite names, and test case names; preserve the no-query fast path separately from the query-discovery path.

## Validate

`timeout 30 tusk build tusk-cli`
`timeout 30 tusk test tusk-cli:test_selection_tests`
