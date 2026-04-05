# Typ Pattern Analysis

This document specifies exhaustiveness, redundancy, refutation, and fragile
match behavior for `typ`.

This builds on top of [nominal_data.md](./nominal_data.md),
[gadts.md](./gadts.md), [polyvariants.md](./polyvariants.md), and
[effects.md](./effects.md).

The point here is simple: pattern typing and pattern analysis are related, but
they are not the same thing.

Typing asks:

- does this pattern make sense at this type?

Pattern analysis asks:

- does this match cover all reachable cases?
- is any clause unreachable or redundant?
- is a refutation clause actually justified?

Those are separate questions, and `typ` should keep them separate.

## 1. Scope

This document covers:

- exhaustiveness checking
- redundancy checking
- redundant subpatterns
- refutation clauses
- guarded clauses
- fragile matches over extensible families
- the interaction with typed patterns

This document does not cover:

- the core typing of patterns
- constructor declaration typing
- row unification itself

Those are covered elsewhere. This document starts after patterns are already
typed.

## 2. Inputs

Pattern analysis runs on typed patterns and typed cases.

That means its inputs already know:

- the pattern shape
- the expected scrutinee type
- constructor and label identity
- whether a case has a guard
- whether a case is a refutation clause

This matters because exhaustiveness is not purely syntactic once GADTs,
extensible variants, and polymorphic variants exist.

## 3. Exhaustiveness

A match is exhaustive if every value inhabiting the scrutinee type is matched by
at least one case.

That is the semantic rule.

The operational rule should still be pattern-matrix based:

1. build the matrix of typed cases
2. simplify and minimize the matrix where possible
3. compute whether a witness value remains uncovered
4. if so, keep the match marked partial and report a witness when possible

This keeps the checker close to upstream `parmatch` without tying it to the
exact implementation details.

### Example

```ocaml
match x with
| Some y -> y
```

is partial because `None` remains uncovered.

The important point is that this judgment happens after the patterns are typed,
so the witness is attached to the real scrutinee type, not just to raw syntax.

## 4. Guards

Guards do not make a case unavailable for typing, but they do weaken
exhaustiveness.

If every clause is guarded, then the match should be considered partial unless
another clause proves total coverage independently.

That means `typ` should not treat:

```ocaml
match x with
| p when cond -> e
```

as exhaustive just because `p` looks exhaustive on its own.

## 5. Redundancy

A clause is redundant if every value it could match is already covered by
earlier clauses.

That is source-order sensitive.

So the checker should analyze cases left to right and report:

- fully redundant clauses
- redundant subpatterns inside a still-useful clause
- unreachable clauses after type refinement

These are not the same warning, and `typ` should keep them distinct in
structured diagnostics.

### Example

```ocaml
match x with
| _ -> 0
| Some y -> y
```

The second clause is redundant because the first clause already covers the full
space.

## 6. Refutation

A refutation clause says, in effect:

"this pattern space is empty."

So `typ` should only accept a refutation clause if the typed pattern really is
uninhabited under the current constraints.

That matters especially once GADTs and local equalities enter the picture.

Refutation is not syntax sugar for "raise an error here." It is a semantic claim
about the emptiness of the remaining match space.

## 7. Witnesses

When a match is partial, `typ` should try to preserve a witness pattern showing
what is missing.

That witness may be:

- exact
- simplified
- partially abstract

depending on the feature involved.

The important rule is:

pattern analysis should produce structured witness information when it can, not
just a formatted string.

That keeps diagnostics useful for both humans and tools.

### Pseudocode

```ocaml
let analyze_match cases scrutinee_ty =
  let typed_cases = type_cases cases scrutinee_ty in
  let matrix = build_pattern_matrix typed_cases |> minimize in
  let partiality = check_exhaustive matrix in
  let redundancy = check_redundant matrix in
  { partiality; redundancy }
```

## 8. Fragile Matches

Matching over extensible families is special.

A match may be exhaustive for the constructors currently known in scope and
still be semantically fragile because more constructors may appear later.

So `typ` should preserve the distinction between:

- truly exhaustive over a closed family
- exhaustive only relative to the currently-known constructors of an extensible
  family

This is why extensible variants and exceptions usually need a wildcard case for
robust exhaustiveness.

## 9. Variants And Pressure

Pattern analysis is also where open variant spaces get narrowed.

For polymorphic variants and other row-like pattern families, pattern analysis
may need to "pressure" the row so the checker can:

- close obviously-used cases
- finalize row information
- make later exhaustiveness and redundancy checks meaningful

That is a real semantic step. It should not be treated as mere warning logic.

## 10. GADTs

GADT matches make pattern analysis type-sensitive in a stronger way.

That means:

- some syntactically possible cases are unreachable after refinement
- some clauses become redundant only after local equalities are applied
- some exhaustiveness checks must respect refined pattern spaces

So `typ` should run pattern analysis over typed, refined patterns, not raw
surface patterns.

## 11. Effects

Effect cases are part of the pattern-analysis story too, but they form a
separate case family.

The checker should treat:

- value cases
- exception cases
- effect cases

as distinct spaces where the language makes them distinct, even if some
diagnostics are reported together.

## 12. References

The main upstream extraction points here are:

- `parmatch.mli`
  the public boundary for exhaustiveness, redundancy, witnesses, and variant
  pressure
- `parmatch.ml`
  `check_partial`, `check_unused`, and fragile-match handling
- `typecore.ml`
  the places where typed patterns feed into exhaustiveness and redundancy

The contract we want to keep is straightforward:

- type patterns first
- analyze the resulting pattern space explicitly
- preserve structured outcomes instead of collapsing everything into strings
