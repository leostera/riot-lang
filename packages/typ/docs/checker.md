# Typ Checker

This document specifies the first checker slice for `typ`.

The point of this document is not to explain the current implementation. The
point is to say what checker we are actually trying to build.

This is the core ML-ish fragment we want `typ` to implement first. Later slices
can extend it, but they should not quietly contradict it.

## 1. Scope

This first slice includes:

- integer literals
- boolean literals
- unit
- variables
- unary and multi-argument functions after lowering
- function application
- non-recursive `let`
- a restricted `let rec`

This first slice does not include:

- modules
- type declarations
- algebraic data types
- records
- pattern matching beyond variable binders
- labeled or optional arguments
- objects or classes
- polymorphic variants
- effect handlers
- subtyping

Those are all separate extensions. They should be specified separately.

## 2. Semantic Surface

`typ` does not type-check arbitrary surface syntax directly. It type-checks a
small semantic language produced by lowering.

Lowering happens by semantic equivalence classes.

That means different surface forms which mean the same thing should lower to the
same semantic form.

Examples:

- `let f x = x` and `let f = fun x -> x` lower to the same semantic shape
- `let rec f x = body` and `let rec f = fun x -> body` lower to the same
  semantic shape
- extra parentheses do not survive semantically
- comments and exact token choices do not survive semantically; they survive in
  origins

For this slice, we assume the lowered expression language is:

```text
e ::= Int(n)
    | Bool(b)
    | Unit
    | Var(x)
    | Fun(x, e)
    | App(e, e)
    | Let(x, e, e)
    | LetRec(f, e, e)
```

This is intentionally small.

In particular:

- `Let(x, e1, e2)` means `let x = e1 in e2`
- `LetRec(f, e1, e2)` means `let rec f = e1 in e2`
- multi-argument functions are nested `Fun`
- multi-argument application is nested `App`

## 3. Types And Schemes

For this slice, the type language is:

```text
tau ::= int
      | bool
      | unit
      | alpha
      | tau -> tau
```

Type schemes are:

```text
sigma ::= Forall(alpha_1, ..., alpha_n). tau
```

A monomorphic type is just a scheme with zero quantifiers.

We will also refer to:

- `ftv(tau)`: the free type variables of a type
- `ftv(sigma)`: the free type variables of a scheme
- `ftv(Gamma)`: the free type variables in the environment

## 4. Environments

The value environment is:

```text
Gamma : name -> sigma
```

So `Gamma[x -> alpha]` is shorthand for binding `x` to the monomorphic scheme
`Forall(). alpha`.

For this first slice, the environment contains only value bindings. There are
no type declarations, module bindings, constructors, labels, or opens here.

## 5. Declarative Typing Rules

The declarative relation is:

```text
Gamma |- e : tau
```

This is the language we want the algorithm to implement.

### Constants

```text
Gamma |- Int(n)  : int
Gamma |- Bool(b) : bool
Gamma |- Unit    : unit
```

### Variables

If `x : sigma` is in `Gamma`, and `tau` is an instance of `sigma`, then:

```text
Gamma |- Var(x) : tau
```

### Functions

If `Gamma, x : tau1 |- e : tau2`, then:

```text
Gamma |- Fun(x, e) : tau1 -> tau2
```

### Application

If:

```text
Gamma |- e1 : tau1 -> tau2
Gamma |- e2 : tau1
```

then:

```text
Gamma |- App(e1, e2) : tau2
```

### Let

If:

```text
Gamma |- e1 : tau1
Gamma, x : Gen(Gamma, e1, tau1) |- e2 : tau2
```

then:

```text
Gamma |- Let(x, e1, e2) : tau2
```

`Gen(Gamma, e1, tau1)` is value-restriction aware. It is defined later in this
document.

### Let Rec

For this first slice, `LetRec` is intentionally restricted.

The binder must be a variable, and the implementation may reject recursive
definitions that are not function-shaped.

Semantically, the rule is:

1. assume `f` has a fresh monomorphic type `alpha`
2. type-check `e1` under `Gamma, f : alpha`
3. require `alpha` to unify with the inferred type of `e1`
4. generalize the resulting type using the same value-restriction rule as `let`
5. type-check the body under the extended environment

This keeps the first slice aligned with ordinary ML recursion without dragging
in the full OCaml recursive-value story yet.

## 6. Algorithmic Contract

The algorithmic judgment is:

```text
infer(Gamma, e) = (S, tau)
```

where:

- `S` is a substitution
- `tau` is the inferred type of `e` after applying `S`

Substitution composition is written `S2 o S1`, meaning "apply `S1`, then apply
`S2`".

The implementation will also carry fresh-variable supply and structured
diagnostics, but those do not change the mathematical contract.

### Variables

```text
infer(Gamma, Var(x)):
  sigma = lookup(Gamma, x)
  tau = instantiate(sigma)
  return (Id, tau)
```

If `x` is not found, this is a failure.

### Constants

```text
infer(Gamma, Int(n))  = (Id, int)
infer(Gamma, Bool(b)) = (Id, bool)
infer(Gamma, Unit)    = (Id, unit)
```

### Functions

```text
infer(Gamma, Fun(x, body)):
  alpha = fresh()
  (S1, tau_body) = infer(Gamma[x -> alpha], body)
  return (S1, S1(alpha) -> tau_body)
```

### Application

```text
infer(Gamma, App(e1, e2)):
  (S1, tau1) = infer(Gamma, e1)
  (S2, tau2) = infer(S1(Gamma), e2)
  beta = fresh()
  U = unify(S2(tau1), tau2 -> beta)
  return (U o S2 o S1, U(beta))
```

### Let

```text
infer(Gamma, Let(x, e1, e2)):
  (S1, tau1) = infer(Gamma, e1)
  sigma = generalize(S1(Gamma), e1, tau1)
  (S2, tau2) = infer(S1(Gamma)[x -> sigma], e2)
  return (S2 o S1, tau2)
```

### Let Rec

For this first slice:

```text
infer(Gamma, LetRec(f, e1, e2)):
  alpha = fresh()
  Gamma' = Gamma[f -> Mono(alpha)]
  (S1, tau1) = infer(Gamma', e1)
  U = unify(S1(alpha), tau1)
  sigma = generalize(U(S1(Gamma)), e1, U(tau1))
  (S2, tau2) = infer(U(S1(Gamma))[f -> sigma], e2)
  return (S2 o U o S1, tau2)
```

This is the minimal contract. An implementation may use an approximation step
before full checking if it wants to mirror OCaml's recursive-typing shape more
closely, but the observable result should still match this contract for the
supported recursive fragment.

## 7. Unification

`unify(tau1, tau2)` returns a substitution `S` such that:

```text
S(tau1) = S(tau2)
```

or fails with a type error.

For this first slice, unification must handle:

- type variables
- base types
- arrow types

The rules are the usual ones:

- unifying a type variable with itself succeeds
- unifying a type variable with some type succeeds if the occurs check passes
- unifying two equal base types succeeds
- unifying two arrow types succeeds by recursively unifying parameter and result
- any other combination fails

An implementation may use mutable union-find internally. That is an
implementation detail. The contract is still unification over the type language
above.

## 8. Instantiation And Generalization

### Instantiation

`instantiate(Forall(alpha_1, ..., alpha_n). tau)` replaces each quantified
variable with a fresh type variable.

If a scheme has no quantified variables, instantiation is the identity.

### Generalization

`generalize(Gamma, e, tau)` closes over type variables which are:

- free in `tau`
- not free in `Gamma`
- permitted by the value restriction

The result is a type scheme.

## 9. Value Restriction

This first slice keeps the ordinary ML/OCaml shape:

- nonexpansive bindings may be generalized
- expansive bindings may not be generalized

For this slice, the following expressions are nonexpansive:

- constants
- variables
- functions

Everything else in this slice is expansive.

So:

```ocaml
let id = fun x -> x in ...
```

may generalize, but:

```ocaml
let f = g h in ...
```

may not.

This rule is part of the observable checker behavior. It is not optional.

## 10. Recursive Bindings

This first slice is deliberately conservative.

The checker may reject recursive bindings outside the supported recursive
fragment with a structured `IllegalRecursiveBinding` style diagnostic.

At minimum, the supported fragment should include function recursion through the
lowered `Fun` form.

Examples that should be in-bounds:

```ocaml
let rec loop = fun x -> loop x
```

```ocaml
let rec id = fun x -> x
```

Examples that may be rejected in this first slice:

```ocaml
let rec x = x
```

```ocaml
let rec x = (fun y -> y) x
```

We can widen this later. The point for now is to keep the recursive contract
small and explicit.

## 11. Core Failures

The first slice needs, at minimum, structured failures for:

- unbound names
- type mismatches
- occurs-check failures
- illegal recursive bindings
- unsupported syntax that never lowered into this semantic language

This document does not freeze the exact diagnostic payload shape. It does freeze
that these failures are semantic checker outcomes, not renderer strings.

## 12. Mapping To `typ`

This document is intentionally implementation-agnostic, but the intended
mapping into `typ` is straightforward:

- `Syn.Cst` lowers into the semantic language above
- origins remain attached separately through origin data
- the inferencer operates on the lowered semantic language, not directly on
  `Syn.Cst`
- query APIs such as `type_at` and `definition_at` read results produced from
  this semantic layer

## 13. Relationship To Upstream OCaml

This slice is extracted from the same broad algorithmic family as the OCaml
checker, but deliberately stripped down into a cleaner contract.

The most relevant upstream implementation points are:

- `typing/typecore.ml` for expression typing and `let`
- `typing/ctype.ml` for instantiation, levels, and unification

We should treat upstream OCaml as:

- a source of algorithmic constraints
- a source of edge cases
- a source of tests

We should not treat the current OCaml checker as the spec.
