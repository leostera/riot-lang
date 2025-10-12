# OCaml's Type Checking Algorithm

## Overview

OCaml uses **Hindley-Milner (HM) type inference** with several extensions for advanced features. The core algorithm has these beautiful properties:

1. **Complete type inference** - No type annotations needed (though allowed)
2. **Principal types** - Every expression has a most general type
3. **Decidable** - Always terminates with an answer
4. **Let-polymorphism** - Automatic generalization of types

## The Core Algorithm: Hindley-Milner

### Key Idea: Constraint Generation + Unification

Type inference works in phases:

1. **Generate fresh type variables** for unknowns
2. **Collect constraints** (type equations) from the code
3. **Solve constraints** via unification
4. **Generalize** at let-bindings (make polymorphic)

### Example Walkthrough

Let's type-check: `let f x = x + 1`

```ocaml
let f x = x + 1
```

**Step 1: Generate Type Variables**
```
f : 'a          (unknown)
x : 'b          (unknown)
(+) : int -> int -> int  (known from prelude)
1 : int         (known constant)
```

**Step 2: Collect Constraints**
```
From (x + 1):
  - x must have type int (because + requires int)
  - Result is int

From function:
  - f has type 'b -> int
```

**Step 3: Unify**
```
'b = int        (from constraint)
'a = int -> int (function type)
```

**Step 4: Result**
```
f : int -> int
```

## The Five Core Type Checking Operations

### 1. **Unification** - Make Two Types Equal

Unification finds substitutions to make types equal.

```ocaml
unify('a, int)           → 'a := int
unify('a -> 'b, int -> string) → 'a := int, 'b := string
unify(int, string)       → ERROR: type mismatch
unify('a, 'a -> int)     → ERROR: occurs check (infinite type)
```

**Algorithm (simplified):**
```ocaml
unify(t1, t2) =
  match (t1, t2) with
  | Variable v, t → 
      if v occurs in t then ERROR
      else v := t
  | t, Variable v → unify(Variable v, t)
  | Arrow(a1, r1), Arrow(a2, r2) →
      unify(a1, a2)
      unify(r1, r2)
  | Constructor(c1, args1), Constructor(c2, args2) →
      if c1 ≠ c2 then ERROR
      for each pair (a1, a2) in (args1, args2):
        unify(a1, a2)
  | _, _ → ERROR
```

**The Occurs Check** is crucial:
```ocaml
(* Without occurs check: *)
let f x = f x  (* Would try: 'a = 'a -> 'b *)
               (* Creates infinite type! *)

(* With occurs check: *)
let f x = f x  (* ERROR: 'a occurs in 'a -> 'b *)
```

### 2. **Instantiation** - Fresh Copy of Polymorphic Type

When using a polymorphic value, create fresh type variables.

```ocaml
(* Definition *)
let id = fun x -> x    (* Type: ∀'a. 'a -> 'a *)

(* Usage 1 *)
id 42                  (* Instantiate: 'a₁ -> 'a₁ where 'a₁ = int *)

(* Usage 2 *)
id "hello"             (* Instantiate: 'a₂ -> 'a₂ where 'a₂ = string *)
```

**Algorithm:**
```ocaml
instantiate(∀'a₁...'aₙ. t) =
  let 'b₁...'bₙ = fresh type variables
  return t['a₁ := 'b₁, ..., 'aₙ := 'bₙ]
```

### 3. **Generalization** - Make Type Polymorphic

At let-bindings, make type variables polymorphic (add ∀).

```ocaml
let id = fun x -> x
     (* Type: 'a -> 'a at level 1 *)
     (* Generalize: ∀'a. 'a -> 'a *)
```

**Key Rule:** Only generalize variables not constrained by the environment.

```ocaml
let r = ref None           (* 'a option ref - DON'T generalize 'a *)
let _ = r := Some 42       (* Now 'a = int *)
let _ = r := Some "hi"     (* ERROR: 'a is already int *)
```

**Algorithm:**
```ocaml
generalize(t, env) =
  let free_vars = free_type_vars(t) - free_type_vars(env)
  return ∀free_vars. t
```

### 4. **Type Levels** - Preventing Escape

Type levels track nesting depth to prevent type variables from escaping their scope.

```ocaml
(* Level 0: top-level *)
let f =                    (* Enter level 1 *)
  let g x = x in           (* x at level 2 *)
  g                        (* Exit level 2, generalize: ∀'a. 'a -> 'a *)
                           (* Exit level 1, generalize: ∀'a. 'a -> 'a *)
```

**Rule:** When unifying a level-L1 variable with a type containing level-L2 variables (L2 > L1), lower L2 to L1.

**Why?** Prevents polymorphic types from escaping:
```ocaml
let f () =
  let r = ref None in           (* 'a ref at level 1 *)
  let g x = r := Some x in      (* x at level 2 *)
  g                             (* Can't return g - x would escape! *)
  (* ERROR: x's type can't be generalized (constrained by r) *)
```

### 5. **Value Restriction** - Soundness for Refs

The **value restriction** prevents unsound generalization with references.

```ocaml
(* DANGEROUS (without value restriction): *)
let r = ref None           (* Would be: ∀'a. 'a option ref *)
let _ = r := Some 42       (* Use as int option ref *)
let _ = !r                 (* Use as string option ref - UNSOUND! *)

(* SAFE (with value restriction): *)
let r = ref None           (* Type: 'a option ref - NOT generalized *)
                           (* 'a is a "weak type variable" *)
let _ = r := Some 42       (* Now fixed: int option ref *)
```

**Rule:** Only generalize at **syntactic values**:
- Variables
- Constants
- Functions (fun x -> ...)
- Constructors (Some e) if e is a value

**NOT values:**
- Function calls: `f x`
- References: `ref e`
- Operations: `x + y`

## OCaml's Extensions to HM

### 1. **Labeled and Optional Arguments**

```ocaml
let f ~x ~y = x + y        (* Type: x:int -> y:int -> int *)
let f ?x y = ...           (* Type: ?x:int -> int -> int *)
```

Type checker tracks argument labels as part of function types.

### 2. **Polymorphic Variants**

```ocaml
let f = function
  | `A -> 1
  | `B -> 2
(* Type: [< `A | `B] -> int *)
```

Uses **row polymorphism** - types track which tags are present/absent.

### 3. **GADTs** (Generalized Algebraic Data Types)

```ocaml
type _ expr =
  | Int : int -> int expr
  | Add : int expr * int expr -> int expr
  | Bool : bool -> bool expr
```

Type checker performs **local type inference** and tracks **type equations**.

### 4. **First-Class Modules**

```ocaml
let m = (module M : S)     (* Package module as value *)
let module M = (val m) in  (* Unpack module *)
```

Type checker tracks module types separately from value types.

## The Type Checking Algorithm (Detailed)

### Algorithm W (Damas-Milner)

This is the classic HM algorithm OCaml is based on:

```
W(Γ, e) = (S, τ)
  where:
    Γ = type environment
    e = expression to type-check
    S = substitution (solution to constraints)
    τ = type of expression
```

**Rules:**

**Constant:**
```
W(Γ, 42) = (∅, int)
```

**Variable:**
```
x : ∀'a₁...'aₙ. τ ∈ Γ
W(Γ, x) = (∅, instantiate(τ))
```

**Function:**
```
W(Γ, fun x -> e) =
  let 'a = fresh
  let (S, τ) = W(Γ + {x : 'a}, e)
  (S, S('a) -> τ)
```

**Application:**
```
W(Γ, e1 e2) =
  let (S1, τ1) = W(Γ, e1)
  let (S2, τ2) = W(S1(Γ), e2)
  let 'a = fresh
  let S3 = unify(S2(τ1), τ2 -> 'a)
  (S3 ∘ S2 ∘ S1, S3('a))
```

**Let (non-recursive):**
```
W(Γ, let x = e1 in e2) =
  let (S1, τ1) = W(Γ, e1)
  let τ' = generalize(S1(Γ), τ1)    (* Key: generalization! *)
  let (S2, τ2) = W(S1(Γ) + {x : τ'}, e2)
  (S2 ∘ S1, τ2)
```

## Implementation in RAML

Our implementation follows this algorithm:

### 1. Type Representation (`Types` module)
```ocaml
type type_expr = {
  desc : type_desc;
  level : int;        (* For type levels *)
  id : int;           (* For occurs check *)
}

type type_desc =
  | Variable of string option
  | Arrow of arg_label * type_expr * type_expr
  | Constructor of ModulePath.t * type_expr list
  | Link of type_expr  (* For unification *)
```

### 2. Unification (`Unification` module)
```ocaml
let rec unify ~ctx t1 t2 =
  let t1 = follow_links t1 in
  let t2 = follow_links t2 in
  match (t1.desc, t2.desc) with
  | Variable _, Variable _ → (* Link variables *)
  | Variable _, _ → (* Occurs check, then link *)
  | Arrow(...), Arrow(...) → (* Unify components *)
  | Constructor(...), Constructor(...) → (* Check same, unify args *)
  | _ → ERROR
```

### 3. Generalization (`Unification` module)
```ocaml
let generalize ~level ty =
  (* Convert level > level variables to universal variables *)
  iter_type_expr (fun t ->
    if t.level > level then
      t.desc <- UniversalVariable None
  ) ty
```

### 4. Instantiation (`Unification` module)
```ocaml
let instance ~ctx ty =
  (* Create fresh copies of universal variables *)
  let subst = HashMap.create () in
  let rec inst ty =
    match ty.desc with
    | UniversalVariable _ → get_or_create_fresh subst ty
    | Arrow(l, t1, t2) → Arrow(l, inst t1, inst t2)
    | Constructor(p, args) → Constructor(p, List.map inst args)
    | _ → ty
  in inst ty
```

### 5. Type Checking (`TypeChecker` module)
```ocaml
let rec type_check_expression ~ctx expr =
  match expr with
  | Constant c → constant_type c
  | Variable x → instantiate(lookup(env, x))
  | Fun(x, body) →
      let arg_ty = fresh_var() in
      let env' = add(env, x, arg_ty) in
      let body_ty = type_check env' body in
      Arrow(arg_ty, body_ty)
  | App(f, arg) →
      let f_ty = type_check env f in
      let arg_ty = type_check env arg in
      let ret_ty = fresh_var() in
      unify(f_ty, Arrow(arg_ty, ret_ty));
      ret_ty
  | Let(x, e1, e2) →
      let e1_ty = type_check (level + 1) env e1 in
      generalize level e1_ty;  (* Key: generalization! *)
      let env' = add(env, x, e1_ty) in
      type_check level env' e2
```

## Why This Algorithm is Beautiful

1. **Automatic inference** - You write `let f x = x + 1`, compiler figures out `int -> int`
2. **Principal types** - There's always a "best" type (most general)
3. **Efficient** - Nearly linear time in practice
4. **Predictable** - Same code always gets same types
5. **Supports polymorphism** - `let id x = x` works on any type!

## Common Patterns

### Pattern 1: Constraint Accumulation
```ocaml
let f x y = x + y
(* Constraints: x : int, y : int, result : int *)
```

### Pattern 2: Substitution Threading
```ocaml
type_check e1   (* Returns S1, τ1 *)
type_check e2   (* Returns S2, τ2 - must apply S1 to environment first! *)
(* Final: S2 ∘ S1 *)
```

### Pattern 3: Level Management
```ocaml
(* Level 0 *)
let f =                    (* Level 1 *)
  let g x = x in           (* Level 2 *)
  g                        (* Generalize vars at level > 1 *)
(* Generalize vars at level > 0 *)
```

## Further Reading

- **Papers:**
  - Damas & Milner (1982) - "Principal type-schemes for functional programs"
  - Rémy (1989) - "Type checking records and variants in a natural extension of ML"
  
- **Books:**
  - Pierce - "Types and Programming Languages" (Chapter 22)
  - Appel - "Modern Compiler Implementation in ML" (Chapter 16)
  
- **OCaml Source:**
  - `typing/typecore.ml` - Expression type checking
  - `typing/ctype.ml` - Unification
  - `typing/btype.ml` - Type operations

## Summary

OCaml's type checker:
1. Generates fresh type variables for unknowns
2. Collects constraints from code structure
3. Solves via unification (with occurs check)
4. Generalizes at let-bindings (with levels to prevent escape)
5. Instantiates polymorphic values with fresh variables

**Result:** Complete type inference with no annotations needed, while still being decidable and efficient!
