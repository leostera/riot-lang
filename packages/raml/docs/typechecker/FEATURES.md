# OCaml Type System Features

Comprehensive list of features to implement in RAML's type checker, based on OCaml's `typing/` directory.

## Expression Types (35 forms)

### Core Expressions
- [ ] `Pexp_ident` - Variable/identifier lookup (`x`, `List.map`)
- [ ] `Pexp_constant` - Constants (int, float, string, char, bool, unit)
- [ ] `Pexp_let` - Let bindings (`let x = 1 in x + 2`)
- [ ] `Pexp_function` - Anonymous functions with pattern matching
- [ ] `Pexp_apply` - Function application (`f x y`)
- [ ] `Pexp_match` - Pattern matching (`match x with ...`)
- [ ] `Pexp_try` - Exception handling (`try ... with ...`)

### Data Structures
- [ ] `Pexp_tuple` - Tuples (`(1, "hello", true)`)
- [ ] `Pexp_construct` - Variant construction (`Some 42`, `None`)
- [ ] `Pexp_variant` - Polymorphic variants (`` `Tag 42``)
- [ ] `Pexp_record` - Record literals (`{x = 1; y = 2}`)
- [ ] `Pexp_field` - Record field access (`r.x`)
- [ ] `Pexp_setfield` - Record field update (`r.x <- 3`)
- [ ] `Pexp_array` - Array literals (`[|1; 2; 3|]`)

### Control Flow
- [ ] `Pexp_ifthenelse` - Conditionals (`if x then y else z`)
- [ ] `Pexp_sequence` - Sequencing (`e1; e2`)
- [ ] `Pexp_while` - While loops (`while cond do body done`)
- [ ] `Pexp_for` - For loops (`for i = 1 to 10 do ... done`)

### Type Annotations & Coercions
- [ ] `Pexp_constraint` - Type constraints (`(e : int)`)
- [ ] `Pexp_coerce` - Type coercions (`(e :> t)`)

### Objects & Classes (Future)
- [ ] `Pexp_send` - Method invocation (`obj#method`)
- [ ] `Pexp_new` - Object instantiation (`new MyClass`)
- [ ] `Pexp_setinstvar` - Instance variable assignment
- [ ] `Pexp_override` - Method overriding
- [ ] `Pexp_object` - Object literals (`object ... end`)

### Modules
- [ ] `Pexp_letmodule` - Local modules (`let module M = ... in ...`)
- [ ] `Pexp_letexception` - Local exceptions
- [ ] `Pexp_pack` - First-class modules (`(module M)`)
- [ ] `Pexp_open` - Local opens (`let open List in ...`)

### Advanced Features
- [ ] `Pexp_assert` - Assertions (`assert cond`)
- [ ] `Pexp_lazy` - Lazy evaluation (`lazy expr`)
- [ ] `Pexp_poly` - Explicitly polymorphic (`(e : 'a. 'a -> 'a)`)
- [ ] `Pexp_newtype` - Locally abstract types (`fun (type t) -> ...`)
- [ ] `Pexp_letop` - Let operators (let+, and+, etc.)
- [ ] `Pexp_extension` - Extension nodes (`[%ext]`)
- [ ] `Pexp_unreachable` - Unreachable code (`.`)

## Pattern Types (19 forms)

### Basic Patterns
- [ ] `Ppat_any` - Wildcard (`_`)
- [ ] `Ppat_var` - Variable binding (`x`)
- [ ] `Ppat_alias` - Pattern alias (`(p as x)`)
- [ ] `Ppat_constant` - Constant patterns (`42`, `"hello"`)
- [ ] `Ppat_interval` - Interval patterns (`'a'..'z'`)

### Structured Patterns
- [ ] `Ppat_tuple` - Tuple patterns (`(x, y, z)`)
- [ ] `Ppat_construct` - Variant patterns (`Some x`, `Node (l, v, r)`)
- [ ] `Ppat_variant` - Polymorphic variant patterns (`` `Tag x``)
- [ ] `Ppat_record` - Record patterns (`{x; y = z}`)
- [ ] `Ppat_array` - Array patterns (`[|x; y; z|]`)

### Pattern Combinators
- [ ] `Ppat_or` - Or patterns (`p1 | p2`)
- [ ] `Ppat_constraint` - Type constraints (`(p : int)`)

### Advanced Patterns
- [ ] `Ppat_type` - Type patterns (`#int`)
- [ ] `Ppat_lazy` - Lazy patterns (`lazy p`)
- [ ] `Ppat_unpack` - Module unpacking (`(module M)`)
- [ ] `Ppat_exception` - Exception patterns (`exception E`)
- [ ] `Ppat_effect` - Effect patterns (effects system)
- [ ] `Ppat_extension` - Extension patterns
- [ ] `Ppat_open` - Local opens in patterns (`M.(p)`)

## Type Definitions

### Type Kinds
- [ ] `Ptype_abstract` - Abstract types (`type t`)
- [ ] `Ptype_variant` - Variant types (`type t = A | B of int`)
- [ ] `Ptype_record` - Record types (`type t = {x: int; y: string}`)
- [ ] `Ptype_open` - Extensible variants (`type t = ..`)

### Type Features
- [ ] Type aliases (`type t = int * string`)
- [ ] Type parameters (`type 'a list`)
- [ ] Multiple type parameters (`type ('a, 'b) map`)
- [ ] Type constraints (`type 'a t constraint 'a = int`)
- [ ] Variance annotations (`type +'a t`, `type -'a t`)
- [ ] Private types (`type t = private int`)
- [ ] Inline records in variants (`type t = A of {x: int}`)

## Type System Features

### Core Type Inference
- [ ] **Hindley-Milner** - Type inference with unification
- [ ] **Let-polymorphism** - Generalization at let bindings
- [ ] **Occurs check** - Prevent infinite types
- [ ] **Level-based generalization** - Efficient polymorphism (Rémy's algorithm)

### Advanced Type Features
- [ ] **Type variables** - `'a`, `'b`, etc.
- [ ] **Type constructors** - `list`, `option`, custom types
- [ ] **Function types** - `'a -> 'b -> 'c`
- [ ] **Tuple types** - `int * string * bool`
- [ ] **Record types** - `{x: int; y: string}`
- [ ] **Variant types** - Sum types with constructors
- [ ] **Polymorphic variants** - `` `Tag of int``
- [ ] **Object types** - Structural typing for objects
- [ ] **Class types** - Nominal typing for classes

### Type System Extensions
- [ ] **GADTs** - Generalized Algebraic Data Types
- [ ] **First-class modules** - Modules as values
- [ ] **Modular implicits** - Type-directed resolution
- [ ] **Private row types** - Constrained polymorphic variants
- [ ] **Local abstract types** - `fun (type t) -> ...`

### Subtyping & Coercion
- [ ] **Subtyping** - For objects and polymorphic variants
- [ ] **Type coercion** - Explicit upcasts `(e :> t)`
- [ ] **Width subtyping** - Adding fields to records/objects
- [ ] **Depth subtyping** - Contravariance in function args

## Module System

### Module Types
- [ ] **Module signatures** - Interface specifications
- [ ] **Module implementations** - Concrete modules
- [ ] **Functors** - Parameterized modules
- [ ] **First-class modules** - `(module S with type t = int)`
- [ ] **Module type of** - `module type of List`

### Module Features
- [ ] **Include** - Include another module
- [ ] **With constraints** - `S with type t = int`
- [ ] **Destructive substitution** - `S with type t := int`
- [ ] **Module aliases** - `module L = List`
- [ ] **Local modules** - `let module M = ... in ...`

## Pattern Matching

### Pattern Matching Features
- [ ] **Exhaustiveness checking** - Warn on missing cases
- [ ] **Redundancy checking** - Warn on unreachable patterns
- [ ] **Or-patterns** - `A | B -> ...`
- [ ] **When guards** - `| x when x > 0 -> ...`
- [ ] **As patterns** - `Some x as opt -> ...`
- [ ] **Nested patterns** - `Some (x, (y, z))`
- [ ] **Record patterns** - Matching record fields
- [ ] **Array patterns** - Matching array elements

### Advanced Pattern Features
- [ ] **GADT pattern matching** - Type refinement
- [ ] **Lazy pattern matching** - Force evaluation
- [ ] **Exception patterns** - `exception Not_found -> ...`
- [ ] **Module unpacking** - `(module M : S) -> ...`

## Type Classes & Interfaces

### Objects (Structural Typing)
- [ ] **Object types** - `< x: int; y: string >`
- [ ] **Method types** - Method signatures
- [ ] **Inheritance** - Object type extension
- [ ] **Polymorphism** - Parametric objects

### Classes (Nominal Typing)
- [ ] **Class types** - Nominal class interfaces
- [ ] **Class definitions** - Class implementations
- [ ] **Inheritance** - Class extension
- [ ] **Virtual methods** - Abstract methods
- [ ] **Private methods** - Encapsulation
- [ ] **Multiple inheritance** - Via mixins

## Effects & Handlers (OCaml 5.0+)

- [ ] **Effect declarations** - `effect E : t -> t`
- [ ] **Effect handlers** - `match e with effect E k -> ...`
- [ ] **Deep handlers** - Resumable continuations
- [ ] **Shallow handlers** - One-shot continuations
- [ ] **Effect typing** - Track effects in types

## Error Reporting

### Type Errors
- [ ] **Unification errors** - Type mismatch details
- [ ] **Occurs check errors** - Infinite type explanation
- [ ] **Missing field errors** - Record/object field errors
- [ ] **Arity errors** - Wrong number of arguments
- [ ] **Variance errors** - Variance constraint violations

### Error Traces
- [ ] **Error trace** - Show unification steps
- [ ] **Expected vs actual** - Clear comparison
- [ ] **Location tracking** - Precise error locations
- [ ] **Hints** - Suggestions for fixes
- [ ] **Diff formatting** - Show type differences

### Warnings
- [ ] **Unused variables** - Warn on unused bindings
- [ ] **Partial match** - Warn on non-exhaustive patterns
- [ ] **Redundant match** - Warn on unreachable cases
- [ ] **Fragile match** - Warn on order-dependent patterns
- [ ] **Name shadowing** - Warn on variable shadowing

## Optimizations

### Type System Optimizations
- [ ] **Levels** - Efficient generalization (Rémy)
- [ ] **Type caching** - Cache unification results
- [ ] **Sharing** - Share type representations
- [ ] **Weak type variables** - Relaxed generalization

### Compilation Optimizations
- [ ] **Inline records** - Unboxed record fields
- [ ] **Unboxed types** - Remove boxing for immediates
- [ ] **GADT optimization** - Eliminate runtime checks
- [ ] **Flambda integration** - Cross-module inlining

## Implementation Priority

### Phase 1: Core (Weeks 1-2)
1. Constants, variables, let bindings
2. Functions (definition and application)
3. Tuples
4. If-then-else
5. Basic pattern matching (var, wildcard, constant, tuple)

### Phase 2: Data Types (Weeks 3-4)
6. Variant types and constructors
7. Record types (literals, access, update)
8. Lists (as variant)
9. Pattern matching (variants, records, nested)
10. Type definitions (alias, variant, record)

### Phase 3: Advanced (Weeks 5-6)
11. Recursive functions and let rec
12. Polymorphic variants
13. Arrays
14. Exceptions (try/with)
15. Advanced patterns (or, as, when)

### Phase 4: Modules (Weeks 7-8)
16. Module signatures
17. Module implementations
18. Functors
19. Module type of
20. First-class modules

### Phase 5: Advanced Features (Weeks 9-12)
21. GADTs
22. Objects and classes
23. Polymorphic methods
24. Local abstract types
25. Effects and handlers

### Phase 6: Polish (Weeks 13-16)
26. Comprehensive error messages
27. Error traces and hints
28. Warnings
29. Performance optimization
30. Documentation

## Testing Strategy

For each feature:
1. ✅ Positive tests - Valid programs
2. ✅ Negative tests - Type errors
3. ✅ Edge cases - Corner cases
4. ✅ Inference tests - Type inference works
5. ✅ Generalization tests - Polymorphism works

## References

- **OCaml Typing**: `ocaml/compiler/typing/`
- **Main type checker**: `typecore.ml` (7092 lines)
- **Type operations**: `ctype.ml` (5674 lines)
- **Environment**: `env.ml` (3726 lines)
- **Pattern matching**: `parmatch.ml` (2363 lines)
- **Type definitions**: `typedecl.ml` (2305 lines)
- **Classes**: `typeclass.ml` (2197 lines)
