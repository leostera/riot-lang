# RFD0018 - Syn Matchers, Traversal, and Visitor

- Feature Name: `syn_matchers_traversal_and_visitor`
- Start Date: `2026-03-23`
- Status: `presented`
- RFD PR: [leostera/riot#0000](https://github.com/leostera/riot/pull/0000)
- Riot Issue: [leostera/riot#0000](https://github.com/leostera/riot/issues/0000)

## Summary
[summary]: #summary

This RFD proposes a shared syntax-consumer support layer on top of the faithful
`Syn.Cst` introduced in [RFD0015](./RFD0015-syn-typed-cst.md).

The proposal introduces four distinct layers:

- `Syn.Cst`
  - the faithful, typed CST
- `Syn.Matchers`
  - small shape helpers such as unwrap/flatten/extract functions
- `Syn.Traversal`
  - shared child, iter, and fold helpers for recursive CST families
- `Syn.Visit`
  - an explicit, reentrant visitor API with full traversal control

The central design requirement is:

- explicit traversal control in the visitor API is fundamental

This RFD does **not** propose pushing more lint-convenience helpers into
`Syn.Cst` itself. The goal is the opposite: keep `Syn.Cst` faithful and move
ergonomics into dedicated shared infrastructure.

## Motivation
[motivation]: #motivation

The faithful CST work is already paying off, but the current rule code still
shows the same structural smell repeatedly:

- rules unpack `SourceFile` manually
- rules reconstruct entrypoint plumbing from `structure_items` or
  `signature_items`
- rules hand-roll recursive descent over `expression`, `pattern`, and
  `core_type`
- rules drop down to raw `Ceibo` tokens and spans for common named-node tasks
- heavier rules are becoming mini analysis frameworks of their own

That means the CST has improved rule authoring, but the support layer on top of
it is still incomplete.

This matters for Riot because `syn` is now a shared foundation for:

- `tusk-fix`
- parser-backed diagnostics
- future formatting
- future macros
- future typechecking
- future source-to-source tooling

If every consumer keeps rebuilding traversal, matchers, and query helpers
independently, Riot will get:

- repeated bugs
- inconsistent coverage of syntax families
- rules that are longer than they should be
- needless coupling between consumers and CST implementation details

The main objective of this RFD is to centralize that repeated structural work.

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

Contributors should think of the post-CST syntax stack as four layers:

1. `Syn.Cst`
2. `Syn.Matchers`
3. `Syn.Traversal`
4. `Syn.Visit`

Each layer has a different job.

### 1. `Syn.Cst` stays faithful

`Syn.Cst` should continue to describe what was written, not what rules would
find convenient.

That means:

- concrete distinctions stay explicit
- lossless tokens and nodes remain available
- helper predicates such as `is_function` should not keep growing inside the
  core CST surface

The CST should remain the parser-shaped source of truth for successful parses.

### 2. `Syn.Matchers` provides small shape helpers

`Syn.Matchers` is for local syntactic conveniences that are useful across
consumers but do **not** define traversal policy.

Examples:

- `Syn.Matchers.Expression.unwrap_parens`
- `Syn.Matchers.Expression.flatten_apply`
- `Syn.Matchers.Pattern.unwrap_alias_typed_parens`
- `Syn.Matchers.CoreType.unwrap`

These helpers should be:

- small
- composable
- syntax-local
- unsurprising

They should not try to replace traversal or analysis.

### 3. `Syn.Traversal` provides shared recursion helpers

`Syn.Traversal` is the shared recursion layer for recursive CST families.

Examples:

- `children_of_expression`
- `iter_expression`
- `fold_expression`
- `exists_expression`
- the same for `pattern`, `core_type`, `module_expression`, and `module_type`

This is the layer rule authors should use when they want:

- a fold
- an `exists`
- a mechanically correct child walk

without having to write recursion themselves.

### 4. `Syn.Visit` is an explicit visitor

`Syn.Visit` is the reentrant visitor API for consumers that need custom
traversal order or selective recursion.

This RFD intentionally rejects an implicit “return `Continue` and the framework
will walk children for you” design for the main visitor API.

The visitor should work like this instead:

- callbacks receive the current context
- callbacks receive a `walker`
- callbacks decide whether to recurse
- callbacks decide which children to recurse into
- callbacks decide the traversal order

The key distinction is:

- the **visitor** is the hook table
- the **walker** is the traversal engine

This matches the mental model used by systems such as Rust `syn`:

- “visit” methods are hooks
- a separate default walker performs standard recursion
- continuing traversal is explicit, not automatic

## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

## 1. Module responsibilities

The proposed module layout is:

```ocaml
module Syn.Cst
module Syn.Matchers
module Syn.Traversal
module Syn.Visit
```

The responsibilities are:

### `Syn.Cst`

- faithful public syntax tree
- tokens and spans where they are part of the tree
- no traversal policy
- no rule-specific query logic

### `Syn.Matchers`

- wrapper removal
- flattened application helpers
- arrow decomposition
- small extraction helpers for common local shapes

### `Syn.Traversal`

- child enumeration
- iter/fold/exists helpers
- mechanically correct recursion over recursive CST families

### `Syn.Visit`

- explicit visitor hooks
- explicit reentrant `walker`
- full-spectrum node-family coverage
- no implicit child traversal hidden behind a control return value

## 2. `Syn.Visit` should be explicit and reentrant

The main visitor shape should be:

```ocaml
module Syn.Visit : sig
  type 'ctx walker = {
    source_file : 'ctx -> Cst.source_file -> unit;
    implementation : 'ctx -> Cst.implementation -> unit;
    interface : 'ctx -> Cst.interface -> unit;
    structure_item : 'ctx -> Cst.StructureItem.t -> unit;
    signature_item : 'ctx -> Cst.SignatureItem.t -> unit;
    attribute : 'ctx -> Cst.attribute -> unit;
    extension : 'ctx -> Cst.extension -> unit;
    pattern : 'ctx -> Cst.Pattern.t -> unit;
    expression : 'ctx -> Cst.Expression.t -> unit;
    core_type : 'ctx -> Cst.CoreType.t -> unit;
    module_expression : 'ctx -> Cst.module_expression -> unit;
    module_type : 'ctx -> Cst.module_type -> unit;
    class_expression : 'ctx -> Cst.class_expression -> unit;
    class_type : 'ctx -> Cst.class_type -> unit;
    ...
  }

  type 'ctx visitor = {
    visit_source_file :
      'ctx -> 'ctx walker -> Cst.source_file -> unit;
    visit_structure_item :
      'ctx -> 'ctx walker -> Cst.StructureItem.t -> unit;
    visit_signature_item :
      'ctx -> 'ctx walker -> Cst.SignatureItem.t -> unit;
    visit_attribute :
      'ctx -> 'ctx walker -> Cst.attribute -> unit;
    visit_pattern :
      'ctx -> 'ctx walker -> Cst.Pattern.t -> unit;
    visit_expression :
      'ctx -> 'ctx walker -> Cst.Expression.t -> unit;
    visit_core_type :
      'ctx -> 'ctx walker -> Cst.CoreType.t -> unit;
    ...
  }

  val default : 'ctx visitor
  val walker : 'ctx visitor -> 'ctx walker
end
```

Important properties:

- the callback receives the traversal engine, not the raw visitor record
- the callback decides whether to recurse
- default traversal happens only when explicitly invoked
- traversal order is entirely under callback control

This means:

- subtree skipping is just “do not call the walker on children”
- custom order is straightforward
- specialized rule traversals do not need to fight a framework-level default

## 3. Why the visitor should not auto-traverse by return value

This RFD explicitly rejects making `Syn.Visit` look like:

```ocaml
type 'ctx control =
  | Continue of 'ctx
  | Skip_children of 'ctx
  | Stop of 'ctx
```

with a contract like:

- return `Continue` and the framework walks children
- return `Skip_children` and the framework does not

That design is convenient for small folds, but it has the wrong feel for the
main visitor API because:

- traversal order is implicit
- rule authors need to remember hidden framework behavior
- callbacks are less obviously in control
- it does not match the normal visitor model used elsewhere

That design is still appropriate for some fold-oriented traversal helper, but
not for the core visitor abstraction.

## 4. `Syn.Traversal` should stay separate from `Syn.Visit`

This RFD does not want one abstraction to do both jobs.

`Syn.Traversal` should stay fold-oriented and recursion-oriented:

```ocaml
module Syn.Traversal : sig
  val children_of_expression :
    Cst.Expression.t -> Cst.Expression.t list
  val fold_expression :
    ('a -> Cst.Expression.t -> 'a) -> 'a -> Cst.Expression.t -> 'a
  val exists_expression :
    (Cst.Expression.t -> bool) -> Cst.Expression.t -> bool
end
```

`Syn.Visit` should stay visitor-oriented and explicit.

Consumers should choose based on need:

- use `Traversal` for folds and structural predicates
- use `Visit` for explicit visitor control

## 5. Full-spectrum visitor coverage

`Syn.Visit` should expose callbacks across the meaningful public CST node
families, not only the few node kinds current rules happen to use.

That includes:

- file roots
- structure and signature items
- attributes, extensions, and payloads
- patterns and parameters
- expressions and their recursive subfamilies
- core types, module types, and class types
- module/class expressions
- declarations such as `let`, `type`, `module`, `class`, `include`, and
  `open`

The goal is that a consumer should not have to fall back to bespoke recursion
just because it cares about a less common public node family.

## 6. Rule authoring conventions

Once these layers exist, rule-authoring conventions should be:

1. prefer `Syn.Visit` for explicit custom traversal
2. otherwise prefer `Syn.Traversal` for small structural folds
3. use `Syn.Matchers` for local unwrap/flatten/extract helpers instead of
   rebuilding those ad hoc in each consumer
4. only write bespoke recursion when the shared layers truly cannot express the
   behavior

That should be treated as an intentional style bar for new rule code.

## Drawbacks
[drawbacks]: #drawbacks

This adds more public surface area:

- another `syn` API layer to maintain
- more node families to keep in sync with CST evolution
- more documentation burden for traversal semantics

There is also a real design burden in keeping:

- `Matchers`
- `Traversal`
- `Visit`

distinct enough that they do not collapse into one confusing pile of helpers.

That separation is worth it, but it will require discipline.

## Rationale and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

### Alternative: keep adding helper methods to `Syn.Cst`

Rejected.

That would make the CST more convenient in the short term, but it pushes
interpretive and consumer-specific logic into the core tree surface. That is
exactly what [RFD0015](./RFD0015-syn-typed-cst.md) tries to avoid.

### Alternative: use only `Syn.Traversal` folds and skip a visitor API

Rejected.

Some consumers need explicit traversal order and selective recursion. A
fold-only API is not expressive enough for those cases.

### Alternative: make the main visitor auto-traverse children by return value

Rejected.

That style is workable, but it hides traversal control behind framework
semantics and does not match the explicit visitor model used in other
ecosystems.

### Alternative: pass the raw visitor record into callbacks

Rejected.

Callbacks need the traversal engine, not the hook table. The raw visitor record
does not itself perform recursion. The separate `walker` abstraction makes the
distinction explicit.

## Prior art
[prior-art]: #prior-art

The most relevant prior art is Rust `syn`:

- hook methods define custom behavior
- the default walker is a separate traversal function
- continuing traversal is explicit

That separation between:

- hook table
- traversal engine

is the right model for Riot as well.

## Unresolved questions
[unresolved-questions]: #unresolved-questions

- whether `Syn.Visit` should expose only `visit_*` hooks or also optional
  `leave_*` hooks
- how much of `Syn.Traversal` should be generated versus hand-written
- whether canonical `span` / `name_site` accessors should be proposed in a
  follow-up RFD or folded into this one later

## Future possibilities
[future-possibilities]: #future-possibilities

Once these layers exist, Riot can build further analysis support without
polluting `Syn.Cst` itself:

- scope-aware read/write collectors
- binding-site helpers
- path and field-root analysis
- future formatter or macro passes built on shared traversal primitives

That is the intended long-term payoff: a faithful CST with a strong support
layer, rather than a CST that every consumer has to rediscover structurally on
its own.
