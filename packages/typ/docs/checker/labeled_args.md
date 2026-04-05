# Typ Labeled Arguments

This document specifies labeled and optional arguments for `typ`.

This builds on top of [checker.md](./checker.md),
[solver.md](./solver.md), and [generalization.md](./generalization.md).

The point here is simple: labeled and optional arguments are not surface sugar
that can be erased completely.

Some surface forms absolutely should normalize together, but labels survive in
the type language and in application matching. If `typ` erases that too early,
it will get function typing, partial application, and diagnostics wrong.

## 1. Scope

This document covers:

- labeled arrows in function types
- optional arguments
- parameter defaults
- call-site argument matching
- commuting and reordering by label
- omitted optional arguments
- diagnostics for wrong labels and incoherent argument order

This document does not cover:

- first-class-module parameters
- effect handlers
- class methods
- object methods

Those may reuse some of the same machinery, but they are separate slices.

## 2. Semantic Model

Lowering still happens by semantic equivalence classes, but labels are part of
the semantics.

So these do normalize together:

- `let f x = body`
- `let f = fun x -> body`

But these do not normalize all the way together:

- `fun ~x -> body`
- `fun x -> body`

because the label changes call-site matching and type compatibility.

For this slice, arrow types should carry argument-label information:

```text
tau ::= ...
      | tau -[label]-> tau
```

where `label` is one of:

- `Nolabel`
- `Labelled(name)`
- `Optional(name)`

The exact concrete representation can differ. The semantic distinction must
survive.

## 3. Parameter Typing

Typing a function parameter with label `l` should:

1. split the expected function type at the next arrow/functor boundary
2. require that the next parameter label is compatible with `l`
3. bind the parameter pattern against the parameter type
4. continue typing the remaining parameters and the body

This is the contract behind upstream OCaml's `split_function_ty`.

The important rule is:

function typing is label-aware from the start. Labels are not bolted on after
ordinary arrow typing succeeds.

### Example

```ocaml
let connect ~host ~port = ...
```

should preserve the labeled spine:

```text
~host:string -> ~port:int -> connection
```

not flatten it into:

```text
string -> int -> connection
```

## 4. Optional Parameters

Optional parameters have two views:

- an external view, visible at the function boundary
- an internal view, visible inside the function body

If a function has:

```ocaml
fun ?x:(default) -> body
```

then:

- externally, `x` behaves like an optional parameter
- internally, after defaulting, the body sees the underlying payload type

That means the checker should treat optional-default parameters as having:

- an external optional arrow type
- an internal non-optional parameter type once defaulting is applied

This is not a cosmetic distinction. It affects pattern typing, body typing, and
the final function type.

### Example

```ocaml
let read ?timeout:(seconds = 30) file = ...
```

Externally, `timeout` is optional.

Inside the body, `seconds` should already have the payload type:

```text
int
```

not an unresolved optional wrapper.

## 5. Application Matching

Call-site typing does not just "zip parameters with arguments from left to
right."

The algorithm should:

1. inspect the current function type spine
2. look at the next expected parameter label
3. find the matching source argument by label when labels are in play
4. allow commutation when that preserves the labeled-application contract
5. diagnose mismatches explicitly when the next argument cannot fit

This means argument order at the source level may differ from parameter order,
but only within the label rules of the language.

### Pseudocode

```ocaml
let rec match_apply_args fun_ty source_args =
  match next_parameter fun_ty with
  | None ->
      [], rebuild_remaining_function_type fun_ty []
  | Some param -> (
      match find_matching_argument param.label source_args with
      | Some arg ->
          let checked = check_argument param arg in
          let rest_args = remove_argument arg source_args in
          let applied, result_ty =
            match_apply_args param.result_ty rest_args
          in
          checked :: applied, result_ty
      | None when is_optional param.label ->
          let omitted = mark_omitted param in
          let applied, result_ty =
            match_apply_args param.result_ty source_args
          in
          omitted :: applied, result_ty
      | None ->
          raise (Wrong_label_or_arity (param.label, source_args)) )
```

## 6. Omitted Optional Arguments

Optional arguments may be omitted.

When an optional parameter is omitted, the function does not necessarily finish
application. The result may still be another function waiting for the remaining
arguments.

So the application algorithm must preserve omitted optional parameters in the
result type rather than pretending they were never there.

Conceptually:

```text
f : ?x:int -> y:string -> bool
```

applied as:

```text
f ~y:"ok"
```

still has the behavior of a value where the optional `x` was omitted rather
than supplied explicitly.

## 7. Wrong Labels

If the next source argument cannot match the current parameter label, `typ`
should not collapse that into a generic unification failure.

This slice requires structured diagnostics for:

- wrong label
- non-optional supplied where an optional label was expected
- incoherent label order
- too many arguments
- applying something that is not actually a function

These should remain structured even if the human reporter later renders them as
friendly prose.

### Example

```ocaml
f ~port:8080 "localhost"
```

against:

```ocaml
f : ~host:string -> ~port:int -> unit
```

should not collapse into a generic unification failure. The checker should know
that the unlabeled argument is in the wrong place relative to the labeled
parameter spine.

## 8. Defaults

Default expressions for optional parameters are typed against the underlying
payload type, not against the outer optional-arrow wrapper.

So if the external type is conceptually:

```text
?x:int -> tau
```

then the default is typed as `int`, not as `int option`.

The checker may internally model optional parameters with option-like payloads
or with dedicated optional-arrow metadata. The behavioral contract is the same:
the body sees the payload type after defaulting.

## 9. Partial Application

Partial application remains label-aware.

That means:

- supplying some labeled arguments may still leave a function type
- omitting optional arguments may still leave a function type
- the result arrow spine must preserve the labels of the remaining parameters

This matters for both typing and diagnostics.

If `typ` erases the remaining labels too early, later applications and hovers
will be wrong.

## 10. Lowering Contract

Lowering should normalize by semantic equivalence classes, but not past the
point where label meaning is lost.

So:

- nested single-parameter `fun` chains may normalize to one n-ary function
  representation if label information is preserved
- application spines may normalize to one callee-plus-arguments shape if label
  information is preserved
- exact source ordering, punctuation, and token spelling stay in origins

The key rule is:

labels survive semantically; most other surface details do not.

## 11. References

The main upstream extraction points here are:

- `typecore.ml`
  `collect_apply_args`, `collect_unknown_apply_args`,
  `split_function_ty`, and `type_function`
- `ctype.ml`
  `filter_arrow` and arrow-shape filtering

Those functions are implementation details upstream, but they expose the
contract we want `typ` to keep:

- labels are part of function typing
- optional arguments have real semantics
- application matching is not plain positional zipping
