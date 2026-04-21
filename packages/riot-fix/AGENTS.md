# riot-fix AGENTS

`riot-fix` is the linting and auto-fix pipeline built on `syn`.

## Rules

1. Prefer safe, explicit rewrites over clever transformations.
2. Diagnostics and fixes should stay paired so users can understand the rewrite.
3. Parser assumptions should be pushed down into `syn` when they become structural.
4. `riot fix` applies only fixes with clear package-owned replacements; ambiguous migrations stay diagnostics-only.
5. Package-provided rules are compiled into a generated `fixme-runner` under `_build`; keep that runner as a direct lint engine, not a second wrapper that re-enters generated-runner selection, and avoid designs that require one subprocess per rule or per file.
6. Materialize the generated runner as a synthetic package inside the active workspace build lane, not as a detached synthetic workspace. It should reuse the real workspace lockfile, target dir, and shared build cache.
7. Shared rule-authoring types live in `fixme`; keep `riot-fix` runtime/reporting helpers layered on top of that shared surface.
8. Explain text belongs with the rule definition. `fixme` should carry typed `Rule_id.t` values, descriptions, and explanation types, not a built-in diagnostic-code registry.
9. `--explain` works on rule ids. Keep the CLI surfaces user-facing and consistent about package-qualified ids like `riot:snake-case-type-names`.
10. `--list-rules` is rule-oriented and `--list-diagnostics` is diagnostic-oriented; today they are both keyed by rule id because each built-in rule emits one diagnostic kind.
11. Prefer `Rule_query` and `Syn.Visit` over hand-written `match ctx.cst` boilerplate or bespoke recursive descent inside individual rules.
12. Do not run rules on parse results alone; build a typed CST first and skip lint execution when `Syn.build_cst` fails.
13. `riot fix --json` owns the machine-readable contract. Keep it as JSONL events on stdout, and send human-oriented control output anywhere else.
14. Apply `riot.fix.ignore` during discovery, not after collection. Ignored subtrees should be pruned before they ever reach the worker queue.
15. Keep `Riot_fix` as the reusable library boundary. The top-level package should own `fix_request`, `fix`, and `Event.to_json`; `Cli` is the standalone adapter layered on top.
16. Keep fixture-backed `riot-fix` coverage on `Std.Test.FixtureRunner` plus adjacent `.expected` snapshots. Do not add a second package-local fixture harness when the native snapshot suite can own the contract.
17. Keep autofix fixtures proving that safe rewrites still parse after application. Add at least one autofix fixture for each builtin rule that returns a concrete fix.

## Validate

`timeout 30 riot build riot-fix`
`timeout 180 riot test riot-fix:runner_tests`
`timeout 180 riot test riot-fix:autofix_fixture_tests`
`timeout 180 riot test riot-fix:fixture_tests`
