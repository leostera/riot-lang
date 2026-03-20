# tusk-fix AGENTS

`tusk-fix` is the linting and auto-fix pipeline built on `syn`.

## Rules

1. Prefer safe, explicit rewrites over clever transformations.
2. Diagnostics and fixes should stay paired so users can understand the rewrite.
3. Parser assumptions should be pushed down into `syn` when they become structural.
4. `tusk fix` applies only fixes with clear package-owned replacements; ambiguous migrations stay diagnostics-only.

## Validate

`timeout 30 tusk build tusk-fix`
