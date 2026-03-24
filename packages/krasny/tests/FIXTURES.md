# Krasny Fixtures

The active `krasny` harness is the manifest in
[format_expectations.txt](/Users/leostera/Developer/github.com/leostera/riot/packages/krasny/tests/format_expectations.txt).
That curated corpus should stay small enough to be readable and broad enough to cover every formatter
heuristic we rely on.

The larger `fixtures/` directory still contains historical and exploratory cases. Treat that wider corpus as
raw material, not as the default harness.

The active manifest now follows two rules:

- keep one category corpus fixture per supported syntax band
- defer individual edge-case fixtures until a real codebase example exposes a formatter regression

## Active Prefixes

- `01xx`: atoms and basic expressions
  Examples: literals, identifier shapes, paths, and simple application.

- `02xx`: operators and parens
  Examples: infix normalization, precedence, explicit parens, comparison and boolean chains.

- `03xx`: bindings and control flow
  Examples: top-level lets, `let ... in`, `if`, `seq`, `begin`, assignment in sequences.

- `04xx`: functions, match, and patterns
  Examples: `fun`, `function`, match lowering, constructor/list/tuple/or/guard patterns.

- `05xx`: labeled and optional arguments
  Examples: labeled arguments, labeled parameters, optional arguments, defaulted optional parameters.

- `06xx`: collections and updates
  Reserved for the curated harness when we start pulling array/list/record update fixtures into it.

- `07xx`: types and signatures
  Reserved for curated type-expression and declaration fixtures.

- `08xx`: modules, first-class modules, objects, and methods
  Reserved for curated module/object formatting fixtures.

- `09xx`: trivia and mixed top-level preservation
  Examples: top-level comments/docstrings, mixed supported and unsupported items, type-trivia regressions.

## Curation Rules

- Keep one category corpus fixture per supported syntax band in the active manifest.
- Inside a category corpus, keep one representative example per syntax group.
- Add an individual edge-case fixture only after a real formatting failure from repo code or a smoke corpus run
  shows that the category corpus is too broad to localize the bug.
- If two fixtures are exact `source + expected` duplicates, keep the clearest name and drop the rest from the
  active manifest.
- If two fixtures are near-duplicates, keep both only when the differing token changes the formatter behavior
  being asserted.
- Prefer names that describe the formatter behavior, not the parser feature inventory.

## Audit

Use the audit script to inspect both exact duplicates and near-duplicate families:

`python3 packages/krasny/tests/fixture_audit.py`

Useful variants:

`python3 packages/krasny/tests/fixture_audit.py --duplicates pair`
`python3 packages/krasny/tests/fixture_audit.py --near-threshold 0.92`
`python3 packages/krasny/tests/fixture_audit.py --near-field pair`
