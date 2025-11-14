# Datalog - High-Performance Query Engine for OCaml

A modern Datalog implementation for OCaml with pluggable storage backends, inspired by Datafrog, Crepe, and DataScript.

## Status

**Current**: ✅ Week 1 Complete - Storage & Foundation  
**Build**: ✅ Zero warnings, 3-second builds  
**Tests**: ✅ 17/17 passing (core + universe)

```
Foundation (Week 1)        ✅ COMPLETE
├─ Parser                  ✅ 150 tests passing
├─ Core types              ✅ Value, Term, AST, Relation
├─ Storage interface       ✅ Pluggable backends
├─ InMemory storage        ✅ HashMap-based default
├─ Universe (functor)      ✅ Base + derived facts
└─ Substitution            ✅ Variable bindings

Evaluation Engine (Week 2) 🔨 IN PROGRESS
├─ Unification             ⏳ Next (pattern matching)
├─ Joins                   ⏳ Next (relation merging)
├─ Variables               ⏳ Next (semi-naive tracking)
└─ Evaluator               ⏳ Next (fixed-point iteration)
```

## Quick Start

### Using InMemory Storage (Default)

```ocaml
open Std
open Datalog

(* Create universe with in-memory storage *)
let universe = Universe.InMemory.of_facts [
  ("edge", [[Int 1; Int 2]; [Int 2; Int 3]; [Int 3; Int 4]]);
] in

(* Add rules (coming in Week 2) *)
let rule = Ast.rule 
  ~head:(Ast.atom ~predicate:"reachable" ~args:[Var "X"; Var "Y"])
  ~body:[Ast.Atom (Ast.atom ~predicate:"edge" ~args:[Var "X"; Var "Y"])]
in

let universe = Universe.InMemory.add_rule universe rule in

(* Evaluate (coming in Week 2) *)
let universe = Evaluator.eval universe in

(* Query results *)
let facts = Universe.InMemory.get_facts universe ~predicate:"reachable" in
Relation.iter (fun tuple -> (* process results *)) facts
```

### Using Custom Storage (e.g., Poneglyph)

```ocaml
open Std
open Datalog

(* Implement storage interface *)
module PoneglyphStorage : Storage.STORAGE = struct
  type t = Poneglyph.graph
  
  let get_facts graph ~predicate =
    match predicate with
    | "edge" -> 
        let edges = Poneglyph.all_edges graph in
        let tuples = List.map (fun e -> 
          [Int e.from; Int e.to; String e.label]
        ) edges in
        Relation.of_list tuples
    | _ -> Relation.empty ()
  
  let predicates _graph = ["edge"; "node"; "triple"]
  let iter_facts graph ~predicate f = (* ... *)
  let get_facts_matching graph ~predicate ~pattern = (* ... *)
end

(* Create universe with Poneglyph backend *)
module U = Universe.Make(PoneglyphStorage)

let graph = Poneglyph.load "data.ttl" in
let universe = U.create graph in

(* Add rules, evaluate, query - no data copying! *)
```

## Features

### ✅ Available Now

- **Pluggable Storage** - Works with any backend (InMemory, Poneglyph, SQLite, etc.)
- **Zero-Copy Design** - Facts stay in original storage, accessed via interface
- **Lazy Evaluation** - Only fetch predicates when rules need them
- **Sorted Relations** - O(n+m) set operations (merge, diff, intersect)
- **Type-Safe API** - Functorized Universe over storage type
- **Complete Parser** - 150 tests passing, full Datalog syntax

### 🔨 Coming Soon (Week 2)

- **Unification** - Pattern matching between terms and atoms
- **Joins** - Efficient merging of relations on shared variables
- **Semi-Naive Evaluation** - Only process Δ (new) facts each iteration
- **Fixed-Point Iteration** - Evaluate rules until no new facts derived
- **Query Engine** - Execute queries and return results

### 📅 Planned (Week 3-4)

- **Optimization** - Galloping search, indexing, stratification
- **Runtime Tests** - 500+ test cases from fixtures
- **Poneglyph Integration** - Complete integration with graph database
- **Performance Tuning** - Meet targets (10K facts < 10ms)

## Architecture

```
┌─────────────────────────────────────────────┐
│           Datalog Query Engine              │
│  (Evaluator, Unification, Joins, etc.)      │
└──────────────────┬──────────────────────────┘
                   │
         ┌─────────▼──────────┐
         │  Storage Interface │ ← Minimal 4-function API
         └─────────┬──────────┘
                   │
    ┏━━━━━━━━━━━━━━┻━━━━━━━━━━━━━━┓
    ┃                              ┃
┌───▼────────┐  ┌────────▼─────┐  ┌──▼──────┐
│  InMemory  │  │  Poneglyph   │  │  SQLite │
│  (HashMap) │  │   (Graph)    │  │  (Disk) │
└────────────┘  └──────────────┘  └─────────┘
```

### Key Components

- **Storage Interface** (`storage.mli`) - 4-function pluggable backend API
- **Universe** (`universe.mli`) - Functor over storage, manages facts and rules
- **Substitution** (`substitution.mli`) - Variable-to-value bindings
- **Relation** (`relation.mli`) - Sorted tuple storage with set operations
- **AST** (`ast.mli`) - Complete Datalog syntax representation

## Storage Interface

The minimal interface any backend must implement:

```ocaml
module type STORAGE = sig
  type t
  
  (* Fetch all facts for a predicate *)
  val get_facts : t -> predicate:string -> fact_tuple Relation.t
  
  (* List all available predicates *)
  val predicates : t -> string list
  
  (* Stream facts without materializing *)
  val iter_facts : t -> predicate:string -> (fact_tuple -> unit) -> unit
  
  (* Pattern queries (optional optimization) *)
  val get_facts_matching : t -> predicate:string -> 
    pattern:Value.t option list -> fact_tuple Relation.t
end
```

**See `STORAGE_INTERFACE.md` for complete implementation guide.**

## Documentation

### For Users
- **`README.md`** - This file (overview and quick start)
- **`PLAN.md`** - 4-week implementation roadmap
- **`DESIGN.md`** - Architecture and design decisions
- **`TESTING.md`** - Test strategy and coverage

### For Integrators
- **`STORAGE_INTERFACE.md`** - Quick reference for implementing storage
- **`PONEGLYPH_INTEGRATION.md`** - Complete integration guide with examples
- **`src/storage.mli`** - Storage interface with detailed documentation

### For Developers
- **`PROGRESS_WEEK1_DAY3.md`** - Detailed progress report
- **`SESSION_SUMMARY.md`** - Session achievements and next steps
- **`src/*.mli`** - Full API documentation in interface files

## Examples

### Transitive Closure

```datalog
% Facts
edge(1, 2).
edge(2, 3).
edge(3, 4).

% Rules
reachable(X, Y) :- edge(X, Y).
reachable(X, Z) :- edge(X, Y), reachable(Y, Z).

% Query: reachable(1, X)
% Results: X = 2, 3, 4
```

### Path Finding

```datalog
% 2-hop paths
path_2(X, Z) :- edge(X, Y), edge(Y, Z).

% 3-hop paths  
path_3(X, W) :- edge(X, Y), edge(Y, Z), edge(Z, W).
```

### Triangle Detection

```datalog
triangle(X, Y, Z) :- 
  edge(X, Y), edge(Y, Z), edge(Z, X),
  X < Y, Y < Z.
```

**More examples coming in Week 3!**

## Performance Targets

| Dataset Size | Target | Status |
|-------------|--------|--------|
| 10K facts | < 10ms | ⏳ Week 3 |
| 100K facts | < 1s | ⏳ Week 3 |
| 1M facts | < 10s | ⏳ Week 4 |

**Techniques**:
- Semi-naive evaluation (only process Δ facts)
- Galloping search (O(log n) in sorted arrays)
- Sorted relations (fast merges and joins)
- SwissTable HashMap (SIMD lookups)

## Testing

```bash
# Build package
tusk build datalog

# Run core tests (5 tests)
tusk test datalog:core_tests

# Run universe tests (12 tests)
tusk test datalog:universe_tests

# Run all tests
tusk test datalog:...
```

**Current**: 17/17 tests passing ✅

## API Reference

### Core Modules

```ocaml
module Value        (* Int | String | Uri *)
module Term         (* Var | Const | Wildcard *)
module Ast          (* Atoms, rules, programs *)
module Relation     (* Sorted tuple storage *)
module Storage      (* Pluggable backend interface *)
module Universe     (* Functor over storage *)
module Substitution (* Variable bindings *)
```

### Creating a Universe

```ocaml
(* Default: InMemory storage *)
let universe = Universe.InMemory.create_empty () in

(* From facts *)
let universe = Universe.InMemory.of_facts [
  ("edge", [[Int 1; Int 2]; [Int 2; Int 3]]);
] in

(* Custom storage *)
module U = Universe.Make(MyStorage)
let universe = U.create my_storage in
```

### Working with Facts

```ocaml
(* Add facts to InMemory storage *)
let storage = InmemoryStorage.create () in
InmemoryStorage.add_fact storage 
  ~predicate:"edge" ~tuple:[Int 1; Int 2];

(* Get facts (base + derived) *)
let facts = Universe.InMemory.get_facts universe ~predicate:"edge" in

(* Iterate over facts *)
Relation.iter (fun tuple ->
  match tuple with
  | [Int from; Int to_] -> printf "edge(%d, %d)\n" from to_
  | _ -> ()
) facts
```

### Working with Rules

```ocaml
(* Create a rule *)
let rule = Ast.rule
  ~head:(Ast.atom ~predicate:"path" ~args:[Var "X"; Var "Y"])
  ~body:[
    Ast.Atom (Ast.atom ~predicate:"edge" ~args:[Var "X"; Var "Y"])
  ]
in

(* Add to universe *)
let universe = Universe.InMemory.add_rule universe rule in

(* Get all rules *)
let rules = Universe.InMemory.rules universe in
```

## Integration

### Poneglyph Integration

Poneglyph can provide zero-copy access to graph data:

1. Implement `PoneglyphStorage : Storage.STORAGE`
2. Create `Universe.Make(PoneglyphStorage)`
3. Query graphs using Datalog rules
4. No data copying - facts stay in Poneglyph!

**Estimated Time**: 2-4 hours  
**Guide**: See `PONEGLYPH_INTEGRATION.md`

### SQLite Integration

Similar approach for disk-backed storage:

1. Implement `SQLiteStorage : Storage.STORAGE`
2. Map SQL tables to Datalog predicates
3. Use SQL indexes in `get_facts_matching`
4. Persistent Datalog facts!

## Contributing

The project is under active development. Current focus:

- **Week 2**: Evaluation engine (unification, joins, evaluator)
- **Week 3**: Testing and optimization
- **Week 4**: Production readiness

See `PLAN.md` for detailed roadmap.

## License

(TODO: Add license information)

## Acknowledgments

- **Datafrog** - Inspiration for semi-naive evaluation
- **Crepe** - Rust Datalog with stratification
- **DataScript** - ClojureScript Datalog for inspiration
- **Riot ML** - Excellent OCaml standard library

---

**Status**: 🚀 Week 1 Complete, Week 2 In Progress

**Next**: Implement Unify and Join modules (evaluation engine)

**Contact**: See project issues for questions and discussions
