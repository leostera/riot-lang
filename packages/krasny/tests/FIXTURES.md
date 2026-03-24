# Krasny Fixtures

The active `krasny` harness is the manifest in
[format_expectations.txt](/Users/leostera/Developer/github.com/leostera/riot/packages/krasny/tests/format_expectations.txt).
That curated corpus should stay small enough to be readable and broad enough to cover every formatter
heuristic we rely on.

The larger `fixtures/` directory still contains historical and exploratory cases. Treat that wider corpus as
raw material, not as the default harness.

## Active Prefixes

- `01xx`: atoms and literals
  Examples: negative literals, chars, unit, identifier shapes.

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

- Keep one canonical fixture per formatting heuristic.
- Add an extra fixture only when it exercises a genuinely different CST path or a different formatting
  decision.
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
