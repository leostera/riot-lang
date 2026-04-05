# Typ Polymorphic Variants

This document specifies the polymorphic-variant slice for `typ`:

- row-typed variant values
- polymorphic variant expressions
- polymorphic variant patterns
- open and closed rows
- row unification
- row closing after pattern analysis

This builds on top of [checker.md](./checker.md) and [gadts.md](./gadts.md).

The point here is simple: polymorphic variants are not ordinary variants with
backticks.

Ordinary variants are nominal. Polymorphic variants are row-typed.

That means the checker is no longer just looking up constructors in a nominal
environment. It is solving constraints over rows of tags, openness, and
payloads.

That is a different algebra. It needs its own contract.

## 1. Scope

This slice includes:

- polymorphic variant expression forms such as `` `Tag`` and `` `Tag x``
- polymorphic variant patterns
- row types with open and closed forms
- row-field unification
- row closing and pressure after pattern analysis
- interaction with type inference through row variables

This slice does not include:

- ordinary nominal variants
- objects
- private row types as a separate feature story
- user-facing subtyping as a separate judgment
- exhaustiveness-reporting details beyond the row-closing contract

Those are all separate extensions. They should be specified separately.

## 2. What This Slice Adds

`nominal_data.md` gave us named variants.

This slice adds variant values that are typed structurally by their tags instead
of nominally by a declared owner type.

That means the checker now needs to model:

1. rows of tags instead of named constructors
2. row openness through a row variable
3. tag payload compatibility
4. row closing after enough information has been learned from patterns

The important point is number 4.

If the checker never closes rows, every match stays too open and downstream
typing loses precision. If it closes too aggressively, it rejects valid code.

So row pressure and finalization are part of the real typing contract here, not
an implementation detail.

## 3. Semantic Objects

This slice extends the type language with row-typed variants:

```text
tau ::= ...
      | Variant(row)

row ::= Row(fields, more, closed, fixed, name)
```

where:

```text
fields ::= (tag * row_field) list

row_field ::= Present(payload option)
            | Either(no_arg, payloads, matched)
            | Absent
```

The important pieces are:

- `fields`
  the known tags and their current state
- `more`
  the row variable or row tail that says what other tags may still exist
- `closed`
  whether new tags may still be added
- `fixed`
  why the row is rigid, if it is rigid

This matches the broad shape upstream exposes through `row_desc`,
`row_field`, and `row_more`.

## 4. Row Intuition

The quickest way to think about rows is:

- `[ `A | `B ]` is a closed row with exactly those tags
- `[> `A | `B ]` is an open row that has at least those tags
- `[< `A | `B ]` is a closed upper bound used mostly in input positions and
  pattern reasoning

The `more` part is what carries that openness.

When `more` is:

- a row variable, the row is still open
- `Tnil`, the row is static and closed
- another row, the type is accumulating more tags structurally

This is why polymorphic variants need row-specific machinery instead of just
being encoded as ordinary sums.

## 5. Variant Expressions

For an expression like:

```text
`Tag
```

or:

```text
`Tag e
```

the checker must produce a row type that says:

- this tag is present
- if there is an argument, its payload has the inferred payload type
- the row is open unless stronger expected-type information closes it

Conceptually:

```text
`Tag      : Variant(Row([Tag -> Present(None)], more = alpha, closed = false))
`Tag e    : Variant(Row([Tag -> Present(Some tau)], more = alpha, closed = false))
```

where `alpha` is a fresh row variable.

If the expected type is already a row type with a known payload for `Tag`, the
checker may use that to type the argument directly.

That is exactly the fast path upstream takes when the expected type already
contains a `Tvariant` row with a present field for the tag.

## 6. Variant Patterns

Variant patterns start out more permissive than variant expressions because the
checker is trying to learn from them.

For a pattern like:

```text
`Tag
```

or:

```text
`Tag p
```

checked against an expected scrutinee type, the checker must:

1. create a row containing the matched tag
2. represent that matched tag initially as `Either`, not immediately as
   `Present`
3. unify that row with the expected scrutinee type
4. if there is a payload pattern, type it against the inferred tag payload type

The `Either` state matters.

It means:

- this tag is known to matter for the match
- its final shape may still be refined by later row reasoning
- the checker should not commit too early to the row being fully closed or the
  tag being the only possible case

That is exactly why upstream builds pattern rows with `rf_either ... ~matched:true`.

## 7. Row Unification

Polymorphic variant typing requires a dedicated row unification story.

At a high level, unifying:

```text
Variant(row1)
```

with:

```text
Variant(row2)
```

must:

1. merge the field sets by tag
2. unify compatible present payloads
3. reconcile `Either` fields with present or absent information
4. respect row openness through `more`
5. reject additions to fixed or closed rows when the tags do not fit

This is not ordinary constructor unification.

The interesting cases are:

- `Present` with `Present`
  payloads must unify
- `Either` with `Either`
  payload alternatives may need to merge
- `Either` with `Absent`
  this is only legal if the row is not fixed against adding or removing that
  tag
- `Either` with `Present`
  the payload alternatives narrow down to the present payload

This is the part of upstream `ctype` where row fields, fixedness, and row tails
all interact.

## 8. Open, Closed, And Fixed Rows

This slice needs three separate concepts:

- open
- closed
- fixed

They are related, but not the same thing.

### Open

An open row may still gain information through `more`.

This is the ordinary state during inference for expressions like:

```ocaml
let x = `A
```

where the checker should infer something like `[> `A ]`.

### Closed

A closed row does not admit arbitrary new tags.

This is the state needed once a pattern match or a type annotation has fixed the
available tags tightly enough.

### Fixed

A fixed row is one where the checker is not allowed to mutate the row shape in
the relevant direction.

This matters especially when rows have been rigidified or reified by other
typing machinery, including GADT-related reasoning.

So “closed” is about the logical shape of the row, while “fixed” is about what
the solver is allowed to change.

## 9. Pressure And Finalization

This is the operational rule that keeps polymorphic variant pattern typing from
staying forever vague.

After typing computation patterns involving polymorphic variants, the checker
must apply pressure and then finalize the row information.

Conceptually:

```text
pressure_variants(patterns)
finalize_variants(patterns)
```

The purpose of pressure is:

- look at the set of variant patterns being used
- decide when a row should be treated as closed enough for matching
- mark missing tags as absent when the match shape justifies that

The purpose of finalization is:

- turn matched `Either` fields into `Present` when appropriate
- erase the temporary “matched” state used only during pattern analysis
- leave the final row shape in a stable post-match form

This is part of the spec because it changes what type the surrounding match
expression ends up with.

## 10. Example

Consider:

```ocaml
let f = function
  | `A -> 0
  | `B -> 1
```

The important point is not just that `f` accepts variants tagged `A` or `B`.

The important point is that the checker should not leave the input as “some
open row with maybe `A` and maybe `B` forever”.

After pattern pressure and finalization, the input type should behave like a
row constrained by the matched tags, not like an unconstrained open row.

That is the difference between useful inference and uselessly vague inference
here.

## 11. Interaction With Other Slices

Polymorphic variants interact with the rest of the type system in a few
important ways.

1. They are not nominal constructors.

So constructor environments from [nominal_data.md](./nominal_data.md) do not
apply here.

2. They can interact with rigidification and reification.

So GADT-related local reasoning may force a row variable to become fixed or
reified instead of staying flexible.

3. They interact with variance-sensitive generalization.

This is why upstream lowers contravariant information around polymorphic
variants before generalization.

## 12. Queries And Summaries

This slice matters for reusable semantic outputs too.

If an exported value has a polymorphic-variant type, its summary needs to carry
the row shape in structured form:

- known tags
- payload types
- whether the row is open or closed
- fixedness when relevant
- exact origin data for definition queries

Flattening that into a human string would throw away semantic information the
LSP and later passes may actually need.

## 13. Mapping To `typ`

This slice implies a few architectural constraints for `typ`.

1. The type representation needs an explicit row-variant form.

2. The solver needs row-field states, not just a flat map from tags to payload
types.

3. Pattern typing needs an explicit finalization step for polyvariant rows.

4. Diagnostics should be structured around row mismatch facts:

- missing tag
- unexpected tag
- payload mismatch
- illegal extension of a fixed row

That is much more useful than collapsing the whole thing into “variant type
mismatch”.

## 14. Relationship To Upstream OCaml

This slice is extracted mainly from:

- `typing/types.mli`
  for the row representation and fixedness story
- `typing/typecore.ml`
  for polymorphic variant expressions, patterns, and row finalization
- `typing/ctype.ml`
  for row unification
- `typing/parmatch.ml`
  for row pressure and closing during pattern analysis

What we want to preserve is the contract:

- polymorphic variants are row-typed, not nominal
- expressions introduce open rows
- patterns use temporary row states to learn from matches
- row unification respects openness, closure, and fixedness
- pressure and finalization are part of the typing story

That is the polyvariant story `typ` needs to implement.
