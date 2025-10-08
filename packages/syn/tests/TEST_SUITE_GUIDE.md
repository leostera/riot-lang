# Syn Parser Test Suite Guide

## Overview

This test suite comprehensively covers OCaml syntax parsing for the Syn CST (Concrete Syntax Tree) parser. The tests are organized into phases based on feature completeness and priority.

## Test Organization

### Test Numbering Convention
- Tests are numbered sequentially: `NNNN_description.ml`
- Each test file contains a minimal OCaml code snippet
- Expected output is in `NNNN_description.ml.expected` (JSON format)
- Tests without `.expected` files indicate unimplemented features

### Test Phases

Total: **250 new tests** (0801-1050) covering missing parser features

---

## Phase 1: Type Expressions (0801-0850)
**50 tests covering type syntax**

### Type Variables (0801-0803)
- Single type variable: `type 'a t = 'a`
- Multiple type variables: `type ('a, 'b) pair = 'a * 'b`

### Arrow Types (0804-0810)
- Simple arrows: `int -> string`
- Curried functions: `int -> string -> bool`
- Parenthesized: `(int -> string) -> bool`
- Polymorphic: `'a -> 'a`

### Tuple Types (0811-0814)
- Pairs: `int * string`
- Triples: `int * string * bool`
- Nested: `(int * string) * bool`
- Polymorphic: `'a * 'a`

### Named Types (0815-0818)
- Built-in: `int list`, `string option`
- Parameterized: `(int, string) result`

### Parameterized Types (0819-0822)
- Generic: `'a list`, `'a option`
- Multiple params: `('a, 'b) result`
- Nested: `'a list list`

### Complex Combinations (0823-0828)
- Arrow with tuple: `int * string -> bool`
- Nested containers: `int option list`
- Higher-order: `(int -> string) list`

### Polymorphic Variant Types (0829-0833)
- Closed: `[ \`A | \`B ]`
- With payload: `[ \`Int of int | \`String of string ]`
- Open: `[> \`A | \`B ]`
- Constrained: `[< \`A | \`B ]`

### Type Constraints (0834-0835)
- As constraint: `'a list as 'a`
- With wildcards: `_ list`

### Advanced (0836-0850)
- Labeled arguments: `x:int -> string`
- Optional arguments: `?x:int -> string`
- Deeply nested: `((int * string) option list, error) result`
- Higher-order functions: `('a -> 'b) -> 'a list -> 'b list`

---

## Phase 2: Type Definitions (0851-0930)
**80 tests covering type declarations**

### Type Aliases (0851-0860)
- Simple: `type point = int * int`
- Arrow: `type predicate = int -> bool`
- Polymorphic: `type 'a ptr = 'a ref`
- Complex: `type handler = (string -> unit) option`

### Variant Types (0861-0880)
**Simple Variants (0861-0870)**
- Empty constructors: `type color = Red | Green | Blue`
- With payloads: `type msg = Text of string`
- Mixed: `type shape = Circle of float | Rectangle of int * int | Point`
- Polymorphic: `type 'a option_t = None | Some of 'a`
- Recursive: `type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree`

**Complex Payloads (0871-0880)**
- Tuple payloads: `Point2D of int * int`
- Arrow payloads: `Callback of (int -> unit)`
- Inline records: `Student of { name: string; age: int }`
- Nested: `type expr = Num of int | Add of expr * expr`

### Record Types (0881-0900)
**Simple Records (0881-0890)**
- Basic: `type point = { x: int; y: int }`
- Polymorphic: `type 'a box = { value: 'a }`
- Mutable fields: `type counter = { mutable count: int }`
- Arrow fields: `type handler = { on_click: int -> unit }`

**Complex Records (0891-0900)**
- Nested records
- Tuple fields
- Multiple polymorphic parameters
- Result/option fields
- Variant fields
- Constraints

### Recursive Types (0901-0910)
- Lists: `type 'a mylist = Nil | Cons of 'a * 'a mylist`
- Trees: `type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree`
- Expressions: `type expr = Const of int | Add of expr * expr`
- Record-based: `type node = { value: int; next: node option }`
- JSON-like: `type json = Null | Bool of bool | Array of json list | ...`

### Mutually Recursive Types (0911-0920)
- Two types: `type a = A of b and b = B of a`
- Tree/forest: `type tree = Node of int * forest and forest = tree list`
- Expression/operator
- Person/company (records)
- Three+ types
- Polymorphic mutual recursion

### Type Parameters & Constraints (0921-0930)
- Variance annotations: `+'a covariant`, `-'a contravariant`
- Type constraints: `constraint 'a = int`
- Nonrec: `type nonrec list = int list`
- Abstract types: `type t`
- GADTs: `type _ expr = Int : int -> int expr`
- Extensible types: `type error = ..`

---

## Phase 3: Pattern Features (0931-0970)
**40 tests covering advanced pattern matching**

### Or Patterns (0931-0940)
- Simple: `| 1 | 2 -> "small"`
- Constructors: `| Some _ | None -> "option"`
- Nested: `| (1 | 2), (3 | 4) -> true`
- Mixed: `| 0 | 1 | Some 2 -> true`
- Poly variants: `| \`A | \`B | \`C -> 1`

### As Patterns (0941-0950)
- Simple: `| (y as z) -> y + z`
- List: `| x :: xs as list -> list`
- Tuple: `| (a, b) as pair -> pair`
- Constructor: `| Some x as opt -> opt`
- With or: `| (1 | 2 | 3) as n -> n`
- Record: `| { name; age } as person -> person`

### Typed Patterns (0951-0958)
- Function args: `let f (x : int) = x`
- Match: `| (y : int) -> y + 1`
- Tuple: `let f (x, y : int * string) = (x, y)`
- Constructor: `| (Some x : int option) -> x`
- Polymorphic: `let f (x : 'a) = x`

### Lazy Patterns (0959-0962)
- Simple: `| lazy y -> y`
- Nested: `| lazy (Some y) -> y`
- Tuple: `| lazy (a, b) -> a + b`

### Exception Patterns (0963-0966)
- Simple: `| exception Not_found -> None`
- With args: `| exception Failure msg -> Error msg`
- Multiple: Multiple exception handlers
- Nested: `| exception (Failure _ as e) -> Error e`

### Range Patterns (0967-0968)
- Char range: `| 'a'..'z' -> "lowercase"`
- Multiple ranges: `| '0'..'9' -> "digit" | 'a'..'f' -> "hex"`

### Module-Qualified Patterns (0969-0970)
- Simple: `| Option.Some y -> y`
- Nested: `| Result.Ok (Option.Some y) -> y`

---

## Phase 4: Expression Features (0971-1000)
**30 tests covering expression-level features**

### Type Annotations (0971-0978)
- Simple: `let x = (42 : int)`
- Tuple: `let p = ((1, "a") : int * string)`
- Arrow: `let f = ((fun x -> x + 1) : int -> int)`
- Option: `let opt = (Some 42 : int option)`
- Polymorphic: `let id = ((fun x -> x) : 'a -> 'a)`

### Type Coercions (0979-0982)
- Simple: `let x = (obj :> parent)`
- With source: `let x = (obj : child :> parent)`
- On expressions: `let x = ((create ()) :> base)`

### Assignment Operators (0983-0990)
- Ref: `r := 42`
- Field: `obj.field <- 10`
- Array: `arr.(0) <- 5`
- String: `s.[0] <- 'a'`
- Nested: `obj.data.value <- 100`
- In sequence: `x := 1; y := 2; z := 3`

### Record Update (0991-0996)
- Single field: `{ p with x = 10 }`
- Multiple fields: `{ p with x = 10; y = 20 }`
- Expression: `{ p with x = p.x + 1 }`
- Nested: `{ obj with data = { obj.data with value = 42 } }`

### Method Calls (0997-1000)
- Simple: `obj#method_name`
- With args: `obj#method_name arg1 arg2`
- Chaining: `obj#method1#method2#method3`
- Nested: `(get_obj ())#method_name`

---

## Phase 5: Module System (1001-1050)
**50 tests covering modules and functors**

### Module Structures (1001-1010)
- Empty: `module M = struct end`
- With values: `struct let x = 42 end`
- With types: `struct type t = int end`
- Nested modules
- Exceptions
- Open/include in structures

### Module Signatures (1011-1020)
- Empty: `module type S = sig end`
- Value signatures: `sig val x : int end`
- Type signatures: `sig type t end`
- Abstract types
- Nested signatures
- Include in signatures

### Module Type Ascription (1021-1025)
- Simple: `module M : S = struct ... end`
- Inline: `module M : sig val x : int end = ...`
- With constraints: `: S with type t = int`
- Opaque: `: (S with type t = int)`
- Private types

### Module Functors (1026-1035)
- Simple: `module F (X : S) = struct ... end`
- Multiple parameters: `module F (X : S1) (Y : S2) = ...`
- Result signature: `module F (X : S) : Result = ...`
- Nested functors
- Functor application: `module M = F(X)`
- Chained: `module M = F(G(X))`
- Functor types: `functor (X : S) -> ...`

### Include/Open (1036-1040)
- Top-level: `include List`, `open List`
- Module paths: `open List.Set`
- Open bang: `open! List`
- In signatures

### Local Opens (1041-1045)
- Expression: `List.(length [1; 2; 3])`
- List: `List.[1; 2; 3]`
- Record: `M.{ field = 42 }`
- Array: `M.[| 1; 2; 3 |]`
- Nested: `A.(B.(C.value))`

### First-Class Modules (1046-1050)
- Pack: `(module M : S)`
- Unpack: `let module M = (val m : S) in ...`
- Pattern: `let f (module M : S) = ...`
- Local: `let module M = struct ... end in ...`
- With types: `(type a) (module M : Monad with type t = a)`

---

## Running the Tests

### Run All Tests
```bash
./packages/syn/tests/run_tests.sh
```

### Run Specific Phase
```bash
# Phase 1: Type Expressions
./target/debug/syn parse --json ./packages/syn/tests/fixtures/0801_*.ml

# Phase 2: Type Definitions
./target/debug/syn parse --json ./packages/syn/tests/fixtures/08[5-9]*.ml
./target/debug/syn parse --json ./packages/syn/tests/fixtures/09[0-2]*.ml

# And so on...
```

### Generate Expected Outputs
When the parser is implemented for a feature, run:
```bash
./target/debug/syn parse --json file.ml > file.ml.expected
```

## Implementation Roadmap

### Priority 1: Type System (High Priority)
1. Implement type expression parsing (Phase 1: 0801-0850)
2. Implement type definition parsing (Phase 2: 0851-0930)

### Priority 2: Patterns (High Priority)
3. Implement or patterns (0931-0940)
4. Implement as patterns (0941-0950)
5. Implement typed patterns (0951-0958)

### Priority 3: Expressions (Medium Priority)
6. Implement type annotations on expressions (0971-0978)
7. Implement assignment operators (0983-0990)
8. Implement record updates (0991-0996)

### Priority 4: Advanced Features (Lower Priority)
9. Implement module structures (1001-1010)
10. Implement module signatures (1011-1020)
11. Implement functors (1026-1035)

## Test Structure

### Input File Format
```ocaml
(* packages/syn/tests/fixtures/0801_type_var_single.ml *)
type 'a t = 'a
```

### Expected Output Format
```json
{
  "tree": {
    "type": "node",
    "kind": "SOURCE_FILE",
    "width": 14,
    "children": [
      {
        "type": "node",
        "kind": "TYPE_DECL",
        "width": 14,
        "children": [...]
      }
    ]
  },
  "diagnostics": []
}
```

### Test Success Criteria
1. No ERROR nodes in the tree
2. No MISSING nodes in the tree
3. Output matches expected JSON exactly
4. Empty diagnostics array

## Parser Architecture Notes

The Syn parser follows these principles:
- **Lossless**: Every byte preserved (including whitespace/comments)
- **Error Recovery**: Never fails, creates ERROR/MISSING nodes
- **Red-Green Trees**: Via Ceibo library
- **CST not AST**: Preserves exact source structure

## Contributing Tests

When adding new tests:
1. Use next available number
2. Follow naming: `NNNN_feature_description.ml`
3. Keep test minimal and focused
4. One feature per test
5. Add to appropriate phase section in this guide

## Debugging Failed Tests

When a test fails:
1. Check if parser produces ERROR/MISSING nodes
2. Run manually: `tusk run syn -- parse file.ml`
3. Examine tree structure
4. Compare with expected output
5. Use `tusk fmt` to ensure valid OCaml

## Statistics

- **Total tests**: 1050 (including original 800)
- **New tests**: 250 (0801-1050)
- **Phase 1 (Types)**: 50 tests
- **Phase 2 (Definitions)**: 80 tests
- **Phase 3 (Patterns)**: 40 tests
- **Phase 4 (Expressions)**: 30 tests
- **Phase 5 (Modules)**: 50 tests

## Next Steps

1. Run `tusk fmt` to validate all test files compile
2. Implement type expression parsing (Phase 1)
3. Generate expected outputs as features are implemented
4. Run test suite regularly during development
5. Fix errors and iterate
