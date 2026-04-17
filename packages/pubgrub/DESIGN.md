# PubGrub Design

## Structure

`ranges.ml` models version sets, `term.ml` models package constraints,
`incompatibility.ml` models clauses and derivations, `partial_solution.ml`
tracks the decision trail, and `new_solver.ml` drives the PubGrub loop.

## Solver Flow

1. Decide the root package version.
2. Add dependency incompatibilities for newly selected versions.
3. Run unit propagation until no package gets a tighter derived constraint.
4. When an incompatibility becomes satisfied, resolve it into a prior cause and
   backtrack to the right decision level.
5. Choose the next version from the provider using the currently effective
   constraints.

## Important Invariants

- Package identity is structural string equality, never physical equality.
- `Term.is_any` only drops tautologies: `positive full` and `negative empty`.
- `Partial_solution.add_derivation` must only be called for a package that is
  present in the incompatibility being derived from.
- Public solutions and trace output should be deterministic.

## Debugging

`Pubgrub.Trace` is the supported debugging surface. Tests should prefer focused
trace snapshots and exact behavior checks over ad hoc logging.
