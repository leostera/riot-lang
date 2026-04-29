# fixme AGENTS

`fixme` is the shared rule-authoring surface used by `riot-fix` and generated fixme-runner providers.

## Rules

1. Keep this package small and stable as the shared rule-authoring boundary.
2. Prefer types and helpers that rule providers can use without pulling in the whole `riot-fix` runtime.
3. Put formatting, JSON rendering, and coordinator/runtime behavior in `riot-fix`, not here.
4. Keep rule ids lightweight and use `Rule_id.t` across the shared rule-authoring surface. Rule packages own diagnostic-code registries.
5. Changes here affect both built-in rules and generated fixme-runner providers; preserve compatibility where possible.
