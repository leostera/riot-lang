# RAML Type Checker - Implementation Roadmap

## Current Status: ✅ Phase 1 Complete (Basic Type System)

### ✅ What Works Now

1. **Basic expressions**: constants, variables, let bindings
2. **Functions**: anonymous functions with parametric polymorphism  
3. **Function application**: with full unification
4. **Tuples**: multi-element products
5. **Pattern matching**: variable, wildcard, constant patterns
6. **Type inference**: Hindley-Milner with unification

## Phase 2: Algebraic Data Types (Priority: HIGH)

### 2.1 Regular Variants (Standard ADTs)

**What:** Sum types with named constructors
```ocaml
type color = Red | Green | Blue
type option = None | Some of int
type tree = Leaf | Node of tree * int * tree
```

**Implementation Steps:**
1. ✅ Type representation already exists (`Types.type_kind = Variant`)
2. ✅ UntypedTree support exists (`ExprConstruct`, `PatternConstruct`)
3. ✅ TypedTree support exists (`ExpressionConstruct`, `PatternConstructor`)
4. ⏳ Need: Type declarations in checker
5. ⏳ Need: Constructor lookup in environment
6. ⏳ Need: Pattern exhaustiveness checking
7. ⏳ Need: Syn parser support for type definitions

**Difficulty:** Medium (infrastructure exists, need integration)

### 2.2 Records

**What:** Product types with named fields
```ocaml
type point = { x : int; y : int }
type person = { name : string; age : int; mutable score : int }
```

**Implementation Steps:**
1. ✅ Type representation exists (`Types.type_kind = Record`)
2. ✅ UntypedTree support exists (`ExprRecord`, `PatternRecord`)
3. ⏳ Need: Field lookup in type checker
4. ⏳ Need: Mutable field handling
5. ⏳ Need: Record update syntax (`{ r with x = 10 }`)
6. ⏳ Need: Syn parser support

**Difficulty:** Medium (similar to variants)

### 2.3 Pattern Matching

**What:** Match expressions with exhaustiveness checking
```ocaml
match expr with
| None -> 0
| Some x -> x + 1
```

**Implementation Steps:**
1. ✅ UntypedTree support exists (`ExprMatch`)
2. ⏳ Need: Exhaustiveness checking
3. ⏳ Need: Redundancy detection
4. ⏳ Need: Guard expressions
5. ⏳ Need: Or-patterns (`p1 | p2`)
6. ⏳ Need: When clauses

**Difficulty:** Medium-High (pattern analysis is complex)

## Phase 3: Advanced Features (Priority: MEDIUM)

### 3.1 Polymorphic Variants

**What:** Variants without type declarations
```ocaml
let x = `Red
let f = function `A -> 1 | `B -> 2
```

**Type Representation:**
```ocaml
type type_desc =
  | ...
  | RowUniform of type_expr  (* [> `A of int | `B ] *)
  | RowPresent of (string * type_expr list) list * type_expr option
```

**Challenges:**
- Row polymorphism with open/closed rows
- Subtyping (`[> `A] <: [> `A | `B]`)
- Type inference with constraints

**Difficulty:** HIGH (requires row polymorphism)

### 3.2 Recursive Types

**What:** Types that reference themselves
```ocaml
type 'a list = Nil | Cons of 'a * 'a list
```

**Implementation Steps:**
1. ⏳ Occurs check must allow recursive type names
2. ⏳ Regular tree grammars for infinite types
3. ⏳ Cycle detection in unification

**Difficulty:** Medium (need to handle cycles carefully)

## Phase 4: GADTs (Priority: MEDIUM-LOW)

### 4.1 Basic GADTs

**What:** Type constructors that refine result types
```ocaml
type _ expr =
  | Int : int -> int expr
  | Bool : bool -> bool expr
  | Add : int expr * int expr -> int expr
  | If : bool expr * 'a expr * 'a expr -> 'a expr
```

**Type Representation:**
```ocaml
type constructor_declaration = {
  cd_name : string;
  cd_args : constructor_arguments;
  cd_res : type_expr option;  (* Return type refinement *)
}
```

**Challenges:**
- Type equations from pattern matching
- Local type inference (not full HM)
- Existential types in patterns

**Example:**
```ocaml
let eval : type a. a expr -> a = function
  | Int n -> n           (* a = int *)
  | Bool b -> b          (* a = bool *)
  | Add (x, y) -> eval x + eval y  (* a = int from context *)
```

**Difficulty:** HIGH (requires local type inference + existentials)

### 4.2 Existential Types

**What:** Types hidden in constructors
```ocaml
type showable = Show : 'a * ('a -> string) -> showable
```

**Challenges:**
- Scope escape prevention
- Freshness of type variables

**Difficulty:** HIGH (scope management is tricky)

## Phase 5: Advanced Type System (Priority: LOW)

### 5.1 Module System

**What:** First-class modules, functors
```ocaml
module type S = sig val x : int end
module M = struct let x = 42 end
let m = (module M : S)
```

**Difficulty:** VERY HIGH (separate type checker phase)

### 5.2 Objects & Classes

**What:** OOP features
```ocaml
class point = object
  val mutable x = 0
  method get_x = x
end
```

**Difficulty:** VERY HIGH (complex subtyping rules)

### 5.3 Effects System

**What:** Algebraic effects (OCaml 5.0+)
```ocaml
effect E : int -> string
let x = perform (E 42)
```

**Difficulty:** VERY HIGH (research-level feature)

## Implementation Priority Order

### Immediate (Next 1-2 weeks)
1. **Binary operators** - Very common, easy to add
2. **Regular variants** - Essential for real programs
3. **Records** - Essential for real programs
4. **Pattern matching** - Completes basic ADT support

### Short-term (Next month)
5. **Recursive types** - Needed for lists, trees
6. **Type aliases** - `type t = int * string`
7. **Parametric types** - `type 'a list = ...`

### Medium-term (Next 2-3 months)
8. **Polymorphic variants** - Nice to have
9. **Recursive let bindings** - `let rec`
10. **Mutual recursion** - `let rec ... and ...`

### Long-term (Research)
11. **GADTs** - Advanced feature
12. **Module system** - Major undertaking
13. **Objects/Classes** - If needed

## Testing Strategy

For each feature:
1. Add AST support (if not exists)
2. Implement type checker logic
3. Add Syn parser support
4. Write tests with `raml check`
5. Verify compilation works
6. Add to examples documentation

## Success Metrics

**Phase 2 Complete:**
- Can type-check OCaml programs with variants and records
- Pattern exhaustiveness checking works
- Match expressions compile correctly

**Phase 3 Complete:**
- Can handle polymorphic variants
- Recursive types work correctly
- Type aliases and parametric types supported

**Phase 4+ Complete:**
- GADTs type-check correctly
- Advanced OCaml features supported
- Research-grade type system
