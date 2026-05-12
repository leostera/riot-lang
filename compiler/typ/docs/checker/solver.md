# Typ Solver

This document specifies the shared solver layer for `typ`.

The point here is simple: all the other checker docs talk about typing rules,
but those rules rely on a common algebra.

That algebra is where `typ` decides:

- what a fresh variable is
- when a type variable may be generalized
- how substitutions compose
- how types unify
- how rows unify
- how GADT refinement is represented locally
- how dependencies on temporary module binders get erased

If this layer is vague, the rest of the specs are fake precision.

## 1. Scope

This document covers:

- type variables and row variables
- substitutions
- scopes and levels
- instantiation
- generalization
- ordinary unification
- row unification
- GADT-aware unification hooks
- rigidity and reification
- dependency erasure through `nondep`

This document does not cover:

- concrete source syntax
- lowering from CST
- session, snapshot, or store behavior
- exact diagnostic rendering

Those are all separate concerns. They should be specified separately.

## 2. What This Layer Is

`checker.md`, `nominal_data.md`, `modules.md`, `gadts.md`, and
`polyvariants.md` all talk in terms of environments and typing rules.

This document describes the reusable machinery underneath them.

That means this layer is not itself “the typechecker”.

It is the shared engine that those other typing relations rely on.

The easiest way to think about it is:

- the checker docs say what must be true
- this doc says what common solver operations the checker is allowed to use to
  make it true

## 3. Semantic Objects

For the purposes of this document, the solver manipulates these broad classes
of things:

```text
tau ::= ...
      | alpha
      | tau -> tau
      | TCon(name, tau list)
      | Variant(row)
      | Package(pack)
      | DepArrow(label, binder, pack, tau)

row ::= Row(fields, more, closed, fixed, name)

sigma ::= Forall(alpha_1, ..., alpha_n). tau
```

This is the shared type language across the previous spec slices.

The solver also manipulates:

- substitutions over type variables
- row tails and row fields
- local equalities introduced by GADT patterns
- local abstract types introduced by reification

## 4. Variables, Levels, And Scopes

The solver needs at least three distinct notions:

- flexible variables
- rigid variables
- scoped local abstract names

### Flexible Variables

Flexible variables are the ordinary inference variables introduced by:

- fresh unknowns during inference
- instantiation of quantified schemes
- open rows
- existential instantiation in constructor expressions

These variables may later unify with other types.

### Rigid Variables

Rigid variables are variables the solver is not allowed to solve away.

They matter in places like:

- checking a user-written type annotation
- checking that quantified variables stay distinct
- checking that a supposedly closed row does not silently open up again

### Scoped Local Abstract Names

Some reasoning steps need more than “rigid variable”.

In particular:

- GADT pattern refinement
- unpacked module binders
- reified row variables

need names that behave like local abstract types with a real scope boundary.

Those names may appear while solving one local problem, but they must not leak
past their scope.

### Levels

The solver must associate fresh variables with levels.

The point of levels is simple:

- fresh variables created under a local inference context should not be
  generalized outside that context by accident
- generalization should quantify exactly the variables that are fresh relative
  to the surrounding environment

This document does not care whether the implementation uses Rémy-style levels,
pools, or some other efficient internal encoding.

It does care that the behavior matches that contract.

## 5. Substitutions

The solver needs substitutions:

```text
S : variable -> tau
```

with the usual operations:

- apply to a type
- apply to a scheme, avoiding quantified variables
- apply to environments
- compose substitutions

Composition is written:

```text
S2 o S1
```

meaning “apply `S1`, then `S2`”.

That same convention is already used in [checker.md](./checker.md).

## 6. Instantiation

Instantiation turns a scheme into a fresh monomorphic type.

Conceptually:

```text
instantiate(Forall(a1, ..., an). tau)
```

creates fresh flexible variables for `a1 ... an` and substitutes them into
`tau`.

This is the operation behind:

- variable lookup in the core calculus
- constructor lookup for ordinary variants
- constructor lookup for GADTs
- exported value use across module boundaries

The important contract is:

every use gets fresh variables.

No call site gets to share the quantified variables from the declaration
directly.

## 7. Generalization

Generalization turns a monomorphic inferred type into a scheme at a binding
boundary.

Conceptually:

```text
generalize(E, tau) = Forall(fresh_relative_to_E(tau)). tau
```

subject to value-restriction rules.

The important contract is:

- variables free in the environment are not quantified
- only variables fresh relative to the environment may be quantified
- expansive expressions do not get the same generalization power as
  non-expansive ones

This applies directly to [checker.md](./checker.md), and later slices inherit
it.

## 8. Ordinary Unification

The solver needs an ordinary unification operation:

```text
unify(tau1, tau2) -> substitution or failure
```

At a minimum, this must:

- unify identical base types
- unify arrows componentwise
- unify named type applications componentwise when their heads match
- solve flexible variables when allowed
- reject cyclic solutions through an occurs check

This is the default solver operation used by:

- core expressions
- constructor expressions
- record operations
- module-summary comparisons over value types

Nothing surprising here. This is the HM-ish core.

## 9. Rigidity

Sometimes ordinary flexible unification is too permissive.

So the solver needs a rigidity operation:

```text
rigidify(tau) -> rigid_vars
```

whose job is:

- walk a type
- mark its flexible variables as rigid for the duration of a check
- later verify that they still behave like distinct variables

This matters for:

- checking annotated schemes
- comparing constrained forms without accidentally solving them
- row checks where openness must stop changing

The implementation details can vary. The behavioral contract is the important
part.

## 10. Reification

Some local equalities should not be represented by mutating outer flexible
variables directly.

So the solver also needs:

```text
reify(local_env, tau) -> tau'
```

whose job is:

- replace selected flexible variables with fresh local abstract names
- keep those names scoped to the current local reasoning problem

This matters especially for:

- GADT pattern refinement
- reified row variables
- local equality solving inside patterns

Reification is one of the main ways `typ` avoids turning a local proof into a
global type mutation.

## 11. GADT-Aware Unification

GADTs need more than ordinary unification.

So the solver layer must expose a distinct operation:

```text
unify_gadt(pattern_type, expected_type) -> equated_pairs
```

This operation must:

- unify the constructor result with the expected scrutinee type
- collect the equalities learned during that match
- use reification when necessary so the equalities stay local

The important difference from ordinary `unify` is that the result is not just
success or failure.

It is success or failure plus the set of local equalities the branch may rely
on.

That is the core of [gadts.md](./gadts.md).

## 12. Row Unification

Polymorphic variants need another specialized operation:

```text
unify_row(row1, row2) -> substitution or failure
```

This operation must:

- merge fields by tag
- unify present payloads
- reconcile `Either`, `Present`, and `Absent`
- respect row openness through `more`
- reject illegal extension of fixed or closed rows

Row unification is not a bolt-on after ordinary unification.

It is part of the main solver contract once `Variant(row)` exists in the type
language.

That is why [polyvariants.md](./polyvariants.md) is its own document.

## 13. Dependency Erasure

The solver also needs one module-related operation:

```text
nondep(E, binders, tau) -> tau'
```

The purpose of `nondep` is:

- take a type or module type that depends on temporary binders
- erase that dependency while keeping a sound supertype

This matters in two places we have already specified:

- anonymous functor application in [modules.md](./modules.md)
- anonymous packed-module application in
  [first_class_modules.md](./first_class_modules.md)

If the dependency cannot be erased soundly, the program is ill-typed.

This is not an optimization. It is a semantic rule.

## 14. Escape Checks

The solver must enforce that scoped local names do not escape.

That applies to:

- constructor existentials in GADT branches
- local equalities introduced by GADT refinement
- reified row variables
- local module binders introduced by package unpacking

The contract is simple:

if a local abstract name belongs only to one branch or one local reasoning
context, it must not appear in the type exported outside that context.

This is where many “looks fine locally” algorithms go wrong.

## 15. Failures

This layer may fail in a handful of structured ways:

- unification failure
- occurs-check failure
- illegal generalization
- rigid-variable escape
- local abstract type escape
- impossible dependency erasure
- illegal row extension

The exact diagnostic surface is a separate concern, but the solver must expose
enough structure that the checker can report these failures without collapsing
everything into one giant string.

## 16. Mapping To `typ`

This document implies a few architectural constraints for `typ`.

1. `TypeRepr` needs to be rich enough for:

- arrows
- named type application
- rows
- packages
- dependent arrows

2. The inference layer needs explicit query-local mutable state for:

- fresh supply
- unification tables or equivalent
- local equalities
- diagnostic accumulation

That mutation is fine. It just must not escape the query boundary.

3. The persistent semantic artifacts should contain solved, serializable types
and schemes, not live solver state.

4. The module layer, GADT layer, and row layer should all reuse the same common
solver vocabulary instead of inventing three different ones.

## 17. Relationship To Upstream OCaml

This document is extracted mainly from `typing/ctype.ml`, with support from the
other typing modules.

That upstream file is where OCaml keeps:

- instantiation
- level management
- ordinary unification
- row unification
- GADT refinement support
- rigidification
- reification
- nondep transformations

What we want to preserve is the contract, not the exact implementation shape.

So the real takeaways are:

- use one shared solver layer
- keep local reasoning local
- make row solving first-class
- make dependency erasure first-class
- keep generalized facts serializable and query-facing

That is the shared solver contract underneath the rest of the `typ` specs.
