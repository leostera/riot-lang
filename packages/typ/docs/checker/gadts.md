# Typ GADTs

This document specifies the GADT slice for `typ`:

- generalized constructor declarations
- constructor result annotations
- existential constructor variables
- GADT constructor expressions
- GADT constructor patterns
- pattern refinement through local equalities

This builds on top of [checker.md](./checker.md),
[nominal_data.md](./nominal_data.md), and [modules.md](./modules.md).

The point here is simple: GADTs are not “ordinary variants with extra syntax”.

The declaration surface looks close to ordinary variants, but the typing story
is different in one crucial way:

using a GADT constructor in a pattern can refine the type of the scrutinee and
introduce local equalities that later pattern subterms and branch bodies get to
use.

That is a different subsystem. It needs its own contract.

## 1. Scope

This slice includes:

- constructor declarations with explicit result types
- existential variables introduced by GADT constructors
- constructor expressions for GADT constructors
- constructor patterns for GADT constructors
- expected-type refinement from GADT patterns
- local abstract types created while solving GADT constraints

This slice does not include:

- extensible GADTs as a separate feature story
- private GADT constructors
- exhaustiveness and redundancy checking details
- first-class equality witnesses as a user-facing feature
- dependent pattern typing outside the constructor-pattern fragment

Those are all separate extensions. They should be specified separately.

## 2. What This Slice Adds

`nominal_data.md` gave us ordinary variants and records.

That was enough for nominal data, but not for type refinement.

This slice adds the missing bit:

constructors may now declare an explicit result type, and pattern-matching on
such a constructor may teach the checker something new about the scrutinee
type.

That means the checker now needs to model:

1. constructor declarations whose result type is not just “the owner type with
   fresh parameters”
2. existential variables local to one constructor use
3. a pattern-typing mode that can add local equalities
4. escaping checks so those equalities and existentials do not leak out

That is why GADTs sit halfway between nominal data and local reasoning.

## 3. Semantic Objects

This slice refines the constructor-declaration model from
[nominal_data.md](./nominal_data.md).

For a generalized constructor, we need at least:

```text
constructor_decl ::= ConstructorDecl(
  name,
  owner,
  universals,
  existentials,
  args,
  result_type,
  generalized
)
```

where:

- `owner` is the declared type constructor
- `universals` are the owner parameters
- `existentials` are variables introduced only by this constructor
- `args` are the payload argument types
- `result_type` is the declared result of the constructor
- `generalized` says whether this is an ordinary constructor or a GADT one

The key difference from ordinary variants is `result_type`.

For an ordinary constructor, the result type is just:

```text
TCon(owner, universals)
```

For a GADT constructor, the result type may be a more specific instance of the
owner type, such as:

```text
int expr
bool expr
('a * 'b) expr
```

and may quantify existential variables that do not appear in the owner type
parameters.

## 4. Declaration Elaboration

For a declaration like:

```ocaml
type _ expr =
  | Int  : int -> int expr
  | Pair : 'a expr * 'b expr -> ('a * 'b) expr
```

the checker must elaborate each constructor into a constructor declaration with
an explicit result type.

This slice requires two rules.

### 4.1 Result-Type Head Rule

The result type of a GADT constructor must still have the declared owner at its
head.

So for a constructor of type `expr`, the result type must be some instance of
`expr`, not an unrelated type.

That is a syntactic and semantic sanity check during declaration elaboration.

### 4.2 Existential Extraction

Any constructor-local type variable that appears in the constructor arguments or
explicit result but is not one of the owner parameters becomes a constructor
existential.

That existential is fresh for each use of the constructor.

So for:

```ocaml
type _ box =
  | Box : 'a -> 'a box
```

there is no existential.

But for:

```ocaml
type _ packed =
  | Pack : 'a * ('a -> string) -> string packed
```

`'a` is existential with respect to the result `string packed`.

## 5. Constructor Expressions

GADT constructor expressions are still constructor expressions. The difference
is what gets instantiated.

For:

```text
C(e1, ..., en)
```

where `C` is generalized, the checker must:

1. look up the constructor declaration
2. instantiate its universals with fresh variables
3. instantiate its existentials with fresh flexible variables
4. type-check the payload expressions against the instantiated argument types
5. use the instantiated `result_type` as the expression type
6. unify that result with the expected type when one is available

So constructor expressions do not themselves add local equalities. They just
produce an expression whose type is the instantiated constructor result.

The refinement story becomes interesting in patterns, not in expressions.

## 6. Constructor Patterns

This is the heart of the slice.

For a constructor pattern:

```text
C(p1, ..., pn)
```

checked against an expected scrutinee type `tau_expected`, the checker must:

1. look up the constructor declaration
2. if the constructor is generalized, first ensure the expected scrutinee has
   the right owner head
3. instantiate the constructor declaration
4. if the constructor has existentials, turn them into fresh local abstract
   types for this pattern occurrence
5. unify the instantiated constructor result with `tau_expected` in GADT mode
6. collect any equalities discovered by that unification
7. type-check the payload patterns against the instantiated argument types
8. make the discovered equalities available while typing those payload patterns
   and the branch body

That is the actual GADT rule.

Everything else is support machinery around it.

## 7. GADT Unification

Ordinary pattern unification is not enough here.

When a GADT constructor pattern says:

```ocaml
Int : int -> int expr
```

and the scrutinee has type `'a expr`, matching `Int _` should teach the checker
that `'a = int` in that branch.

So this slice requires a distinct GADT-aware unification mode:

```text
unify_gadt(pat_type, expected_type) -> equated_type_pairs
```

The purpose of `unify_gadt` is not just to say yes or no.

It must also collect the type equalities induced by the match, because later
payload patterns and the branch body depend on them.

If plain unification is enough, great.

If not, the checker may need to reify some variables into fresh local abstract
types and retry in pattern mode. That is the upstream shape too.

## 8. Reification And Local Abstract Types

Some GADT equalities cannot be represented by leaving ordinary flexible type
variables around.

So the checker needs a way to turn certain variables into fresh local abstract
types that live only for this pattern-solving context.

This slice requires an operation with the shape:

```text
reify(pattern_env, tau) -> tau'
```

whose job is:

- replace flexible variables that cannot safely stay flexible
- introduce fresh local abstract names for them
- use those names while solving local equalities

This is how the checker avoids “solving” a local GADT equality by mutating some
outer flexible type variable that should never have been refined globally.

## 9. Existential Scope

Constructor existentials are local to one constructor use.

That means:

- they may be used while typing the constructor payload pattern
- they may be used while typing the branch body
- they must not escape the branch

So this slice requires an explicit escape check:

after typing a GADT pattern and its branch, constructor existentials and
branch-local equalities must not appear in the type exported outside that
branch.

This is the same broad discipline upstream enforces with local abstract types,
pattern environments, and escape checks.

## 10. Example

Consider:

```ocaml
type _ expr =
  | Int  : int -> int expr
  | Bool : bool -> bool expr

let eval : type a. a expr -> a = function
  | Int n -> n
  | Bool b -> b
```

The important bit is not the surface syntax. The important bit is the branch
logic.

In the `Int n` branch:

- the scrutinee has expected type `a expr`
- the constructor result is `int expr`
- GADT unification refines `a = int`
- the branch body is then checked against `int`

In the `Bool b` branch:

- the scrutinee has expected type `a expr`
- the constructor result is `bool expr`
- GADT unification refines `a = bool`
- the branch body is checked against `bool`

That is the whole point of GADTs.

## 11. Queries And Summaries

This slice matters for reusable semantic outputs too.

If a constructor is generalized, the reusable summary for its defining module
must retain:

- that the constructor is generalized
- its explicit result type
- its argument types
- its constructor-local existentials
- its exact definition origin and span

Otherwise downstream typing and LSP queries would lose the information that
makes the constructor a GADT constructor in the first place.

## 12. Mapping To `typ`

This slice implies a few architectural constraints for `typ`.

1. `TypeDecl` or its eventual replacement must be able to distinguish ordinary
constructors from generalized ones.

2. Constructor typing in the semantic tree cannot treat all constructors as
ordinary variant constructors.

3. Pattern typing needs a real pattern environment with local equalities and
local abstract types.

4. Diagnostics for GADT mismatches should be structured around:

- constructor used
- expected scrutinee type
- constructor result type
- equated types when available

That is much more useful than a flattened string.

## 13. Relationship To Upstream OCaml

This slice is extracted mainly from:

- `typing/typedecl.ml`
  for constructor elaboration and explicit result-type handling
- `typing/typecore.ml`
  for `solve_Ppat_construct`, constructor expressions, and pattern refinement
- `typing/ctype.ml`
  for existential instantiation, GADT unification, reification, and rigidification

What we want to preserve is the contract:

- GADT constructors have explicit result types
- constructor use instantiates fresh existentials each time
- constructor patterns may refine the scrutinee type
- refinement happens through local equalities, not global mutation
- existentials and local equalities do not escape

That is the GADT story `typ` needs to implement.
