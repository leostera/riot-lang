# tusk-fix AGENTS

`tusk-fix` is the linting and auto-fix pipeline built on `syn`.

## Rules

1. Prefer safe, explicit rewrites over clever transformations.
2. Diagnostics and fixes should stay paired so users can understand the rewrite.
3. Parser assumptions should be pushed down into `syn` when they become structural.
4. `tusk fix` applies only fixes with clear package-owned replacements; ambiguous migrations stay diagnostics-only.
5. Package-provided rules are fused into a generated runtime under `_build`; avoid designs that require one subprocess per rule or per file.
6. Shared rule-authoring types live in `tusk-fix-api`; keep `tusk-fix` runtime/reporting helpers layered on top of that shared surface.
7. Explain text belongs with the rule definition. `tusk-fix-api` should carry string codes and explanation types, not a built-in diagnostic-code registry.
8. `--list-rules` is rule-oriented and `--list-diagnostics` is diagnostic-oriented; keep that distinction clear in CLI and reporting code.

## Validate

`timeout 30 tusk build tusk-fix`
`timeout 180 tusk test tusk-fix:runner_tests`
