# Rémy's Algorithm: Efficient Type Generalization

## Overview

In 1988, **Didier Rémy** discovered an elegant algorithm for efficient type generalization that is the foundation of OCaml's type checker. This document explains Rémy's key insight and how we implement it in RAML.

**Source:** Based on Oleg Kiselyov's excellent article "How OCaml type checker works" and Rémy's 1992 paper.

## The Problem: Naive Generalization is Slow

### What is Generalization?

Generalization `GEN(Γ, t)` quantifies free type variables in type `t` that don't appear in environment `Γ`:

```ocaml
let id = fun x -> x    (* Infer: 'a -> 'a *)
(* Generalize: ∀'a. 'a -> 'a *)
```

### Why It Must Check the Environment

Without checking the environment, we get **unsound** types:

```ocaml
fun x -> let y = x in y
```

**Naive (wrong):**
1. Infer `x : 'a` (environment: `{x : 'a}`)
2. Infer `y : 'a`  
3. Generalize to `∀'a. 'a` **← WRONG! 'a is in environment**
4. Result: `'a -> 'b` (unsound - can convert any type to any other!)

**Correct:**
1. Infer `x : 'a` (environment: `{x : 'a}`)
2. Infer `y : 'a`
3. DON'T generalize (α is in environment)
4. Result: `'a -> 'a` ✓

### The Performance Problem

Original Caml **scanned the entire environment** on each generalization:

```ocaml
let rec typeof env = function
  | Let (x, e, body) ->
      let ty_e = typeof env e in
      let ty_x = gen env ty_e in  (* Scan entire env! *)
      typeof ((x, ty_x) :: env) body
```

**Cost:** Environments grow large (every `let` adds bindings). Scanning on each `let`:
- Linear time per `let`: O(n)
- Typechecking whole program: **O(n²)**

**Real impact:** Bootstrapping Caml compiler took **20 minutes** for two mutually recursive functions!

## The Key Insight: Generalization as Memory Management

Rémy realized: **Unsound generalization is like premature deallocation!**

### Type Variables are Resources

- **Allocate:** Create fresh type variable (`newvar()`)
- **Use:** Unify with other types
- **Deallocate:** Quantify (convert to ∀)

### Unsound Generalization = Use-After-Free

```ocaml
fun x ->              (* Allocate ty_x at depth 1 *)
  let y = x in y      (* Deallocate ty_x at depth 2 *)
                      (* Use ty_x at depth 1 - DANGLING! *)
```

This is **exactly like**:

```c
char *ptr;
void bad() {
    char local[] = "data";
    ptr = local;          // Pointer to local
}                         // local deallocated
// Using ptr now is undefined behavior!
```

### The Solution: Ownership Tracking (Regions!)

Instead of scanning all memory (environment) to check if something is in use, use **regions**:

- Each `let` creates a new region
- Type variables belong to their owning `let`
- Only deallocate (quantify) variables owned by the exiting region

This is exactly like **region-based memory management** (Tofte & Talpin)!

## Rémy's Algorithm: Type Levels

### Levels = Region IDs

A **level** is the nesting depth of `let` expressions:

```ocaml
(* Level 0: implicit top-level *)
let f =              (* Level 1 *)
  let g x =          (* Level 2 *)
    let h y = x      (* Level 3 *)
```

### Type Variables Track Their Owner

```ocaml
type tv = Unbound of string * level

let current_level = ref 0

let newvar () = 
  TVar (ref (Unbound (gensym(), !current_level)))
```

Each type variable knows which `let` owns it!

### Generalization Only Affects Dead Regions

```ocaml
let rec gen ty =
  match ty with
  | TVar {contents = Unbound (name, l)} when l > !current_level ->
      (* This variable belongs to a deeper (now dead) region *)
      QVar name  (* Quantify it *)
  | TVar {contents = Link t} -> gen t
  | TArrow (t1, t2) -> TArrow (gen t1, gen t2)
  | _ -> ty
```

**Key rule:** Only generalize variables whose level > current_level (they're from dead regions).

### Example Walkthrough

```ocaml
fun x ->              (* Level 1, allocate ty_x/1 *)
  let y = x in y      (* Level 2, infer y : ty_x/1 *)
                      (* Exit level 2, generalize *)
                      (* ty_x/1 has level 1 = current *)
                      (* DON'T generalize *)
```

Result: `'a -> 'a` ✓

Another example:

```ocaml
fun x ->                  (* Level 1, ty_x/1 *)
  let y = fun z -> z      (* Level 2, ty_z/2 *)
  in y                    (* Exit level 2 *)
                          (* ty_z/2 > 1, generalize! *)
                          (* ty_x/1 = 1, don't *)
```

Result: `'a -> ('b -> 'b)` ✓

### Level Updates During Unification

When unifying, levels may need updating:

```ocaml
(* ty_x at level 1, ty_y at level 2 *)
unify ty_x (TArrow ty_y ty_y)
(* ty_y must be lowered to level 1! *)
(* Otherwise it would escape when level 2 exits *)
```

**The occurs check can be combined with level updates:**

```ocaml
let rec occurs tvr ty =
  match ty with
  | TVar {contents = Unbound (name, l')} ->
      if tvr == ty then panic "occurs check"
      else
        (* Update level to minimum *)
        let min_level = min (level_of tvr) l' in
        ty := Unbound (name, min_level)
  | TArrow (t1, t2) ->
      occurs tvr t1; occurs tvr t2
  | ...
```

## Further Optimizations: Lazy Level Updates

Rémy's full algorithm has additional optimizations:

### 1. Composite Types Have Levels Too

Not just type variables:

```ocaml
type typ =
  | TVar of tv ref
  | TArrow of typ * typ * levels

and levels = {
  mutable level_old : level;  (* Upper bound on component levels *)
  mutable level_new : level;  (* Desired level after update *)
}
```

**Benefits:**
- Skip traversing types whose level ≤ current (nothing to generalize)
- Skip instantiating types whose level < generic (no quantified vars)
- Improves sharing!

### 2. Generic Level

Use special level (ω = 100000000) for quantified variables:

```ocaml
let generic_level = 100000000

(* Type variable at generic_level is quantified *)
| TVar {contents = Unbound (_, l)} when l = generic_level ->
    (* This is ∀'a *)
```

### 3. Delayed Level Updates

Don't immediately traverse composite types on unification:

```ocaml
let to_be_level_adjusted = ref []

let update_level l ty =
  match ty with
  | TVar {...} -> (* Update immediately *)
  | TArrow (_, _, ls) as ty ->
      if l < ls.level_new then begin
        if ls.level_new = ls.level_old then
          to_be_level_adjusted := ty :: !to_be_level_adjusted;
        ls.level_new <- l
      end
```

Force updates before generalization:

```ocaml
let gen ty =
  force_delayed_adjustments ();
  (* Now actually generalize *)
  ...
```

## Levels Prevent Type Escapes

The same levels that make generalization efficient **also prevent types from escaping their scope**!

```ocaml
let x = ref []
module M = struct
  type t
  let _ = (x : t list ref)  (* ERROR: t escapes! *)
end
```

**How it works:**
1. Each type constructor has a **binding time** (its identifier's timestamp)
2. When entering a type declaration, set timestamp = current_level
3. During unification, check: `ty.level ≥ constructor.binding_time`
4. If violated, the type escaped!

This is the **same mechanism** as generalization - just applied to type constructors instead of variables!

## Complexity Analysis

### Naive Algorithm
- Scan environment on each generalization: O(n)
- Total: **O(n²)**

### Rémy's Algorithm
- Track levels incrementally during unification: O(1) per unification
- Generalization only traverses type, not environment: O(size of type)
- Total: **Nearly O(n)** (linear in practice)

**Real impact:** Caml compilation went from 20 minutes to seconds!

## Implementation in RAML

We implement Rémy's algorithm in RAML:

### 1. Context Threading (No Global State)

```ocaml
type Types.context = {
  type_id_counter : int;
  type_level : int;        (* Current level, NOT global ref! *)
  identifier_ctx : Identifier.context;
}

let type_check_let ~ctx recursive bindings body =
  (* Enter new level *)
  let ctx = { ctx with type_level = ctx.type_level + 1 } in
  
  (* Type check bound expression *)
  let typed_expr, ctx = type_check_expression ~ctx expr in
  
  (* Exit level *)
  let ctx = { ctx with type_level = ctx.type_level - 1 } in
  
  (* Generalize *)
  Unification.generalize ~level:ctx.type_level typed_expr.exp_type;
  
  (* Continue with body *)
  ...
```

### 2. Type Operations

```ocaml
(* TypeOperations module *)

let update_level current_level ty =
  (** Lower type variable levels to prevent escape.
      
      When unifying level-L1 variable with type containing
      level-L2 variables (L2 > L1), lower L2 to L1.
  *)
  iter_type_expr
    (fun t ->
      let t = follow_links t in
      if t.level > current_level then
        t.level <- current_level)
    ty
```

### 3. Unification With Level Updates

```ocaml
(* Unification module *)

let rec unify ~ctx t1 t2 =
  let t1 = TypeOperations.follow_links t1 in
  let t2 = TypeOperations.follow_links t2 in
  
  match (t1.desc, t2.desc) with
  | Variable _, _ ->
      if TypeOperations.occurs_in_type t1.id t2 then
        Error (OccursCheck (t1, t2))
      else
        (* Update levels before linking *)
        TypeOperations.update_level t1.level t2;
        t1.desc <- Link t2;
        Ok ctx
  | ...
```

### 4. Generalization

```ocaml
let generalize ~level ty =
  (** Convert level > level variables to universal.
      
      After exiting a let-region, all type variables still
      owned by that region can be quantified.
  *)
  let rec gen ty =
    let ty = TypeOperations.follow_links ty in
    if ty.level > level then
      ty.desc <- UniversalVariable None
  in
  TypeOperations.iter_type_expr gen ty
```

## Key Takeaways

1. **Generalization = Resource Management**
   - Type variables are allocated/deallocated resources
   - Unsound generalization = use-after-free
   - Solution: ownership tracking (regions)

2. **Levels = Region IDs**
   - Each `let` creates a region (level)
   - Type variables track their owner
   - Generalize only dead region's variables

3. **No Environment Scanning**
   - O(1) overhead per unification
   - O(type size) for generalization
   - Nearly linear time overall

4. **Bonus: Type Escape Prevention**
   - Same mechanism prevents type constructors from escaping
   - Unification maintains level invariants

5. **Elegance**
   - Simple idea (ownership tracking)
   - Multiple applications (generalization, escape checking, MLF)
   - Efficient implementation

## References

- **Didier Rémy (1992):** "Extension of ML Type System with a Sorted Equational Theory on Types"
- **Oleg Kiselyov:** "How OCaml type checker works" (the document you just read!)
- **Kuan & MacQueen (2007):** "Efficient ML Type Inference Using Ranked Type Variables"
- **Fluet & Morrisett (2006):** "Monadic Regions" (shows connection to region-based memory management)

## Why This Matters for RAML

Understanding Rémy's algorithm helps us:

1. **Make correct design choices** - Thread context instead of global refs
2. **Optimize appropriately** - Know where to spend optimization effort
3. **Extend the system** - Levels work for GADTs, existentials, etc.
4. **Appreciate the elegance** - Type checking is like GC!

The fact that generalization, escape checking, and region management all use the **same mechanism** (levels) is beautiful. It's not three separate ad-hoc tricks - it's one elegant idea applied consistently.
