# tusk-cli AGENTS

`tusk-cli` owns command parsing, user-facing output, and the top-level build workflow.

## Rules

1. Keep the CLI thin. It should orchestrate, not duplicate planner or executor logic.
2. Prefer direct local calls into tusk libraries over protocol-shaped wrappers.
3. User-facing messages should stay concise and actionable.
4. When adding commands, update completions and help output in the same change.
5. Built-in commands that own domain logic elsewhere should delegate into their package library.

## Validate

`timeout 30 tusk build tusk-cli`
