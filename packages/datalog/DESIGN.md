# Datalog Package Design

## Overview

This package implements a Datalog engine for OCaml, inspired by three excellent reference implementations:
- **Datafrog** (Rust) - Low-level, embedded Datalog with explicit iteration
- **Crepe** (Rust) - Macro-based declarative Datalog with compile-time code generation
- **DataScript** (Clojure) - Full-featured database with Datalog query engine

## API Design

The API follows a hybrid approach combining the best ideas from all three:

```ocaml
module Datalog : sig
  type universe
  type fact
  
  val empty : unit -> universe
  val query : universe -> query:string -> fact list
end
```

### Target Usage

```ocaml
open Std

let universe = Datalog.empty ()

let universe = 
  universe
  |> Datalog.add_fact "edge(1, 2)"
  |> Datalog.add_fact "edge(2, 3)"
  |> Datalog.add_rule "path(X, Y) :- edge(X, Y)"
  |> Datalog.add_rule "path(X, Z) :- edge(X, Y), path(Y, Z)"

let results = Datalog.query universe ~query:"path(X, Y)"
(* results: [
  {"X": 1, "Y": 2},
  {"X": 1, "Y": 3},
  {"X": 2, "Y": 3}
] *)
```

## Architecture

### Core Data Structures

#### 1. Fact Representation
```ocaml
type term =
  | Var of string
  | Const of value
  | Wildcard

and value =
  | Int of int
  | String of string
  | Atom of string

type fact = {
  predicate: string;
  args: term list;
}
```

#### 2. Rule Representation
```ocaml
type rule = {
  head: fact;
  body: clause list;
}

and clause =
  | Fact of fact
  | Negated of fact
  | Builtin of string * term list
```

#### 3. Universe (Database)
```ocaml
type universe = {
  facts: fact list;
  rules: rule list;
  indices: index_map;
}
```

### Evaluation Strategy

**Semi-Naive Evaluation** (from Datafrog and Crepe):

1. **Initialization**: Load all base facts
2. **Iteration Loop**:
   - Track "recent" facts (added in last iteration)
   - Track "stable" facts (from previous iterations)
   - For each rule:
     - Join recent facts with stable facts
     - Derive new facts
   - Continue until no new facts are derived (fixed point)

### Key Algorithms

#### 1. Join Algorithm (from Datafrog)
```ocaml
val join : 
  ('k * 'v1) relation -> 
  ('k * 'v2) relation -> 
  ('k -> 'v1 -> 'v2 -> 'a) -> 
  'a relation
```

Uses sorted relations and galloping search for efficiency.

#### 2. Stratification (from Crepe)
```ocaml
val stratify : rule list -> rule list list
```

Groups rules into strata based on dependencies, enabling stratified negation.

#### 3. Index Generation (from Crepe)
Automatically creates indices based on rule patterns:
- If rule has `edge(X, Y), edge(Y, Z)`, create index on second argument of edge
- Indices speed up joins by avoiding full table scans

## Implementation Phases

### Phase 1: Core Engine (Current)
- [x] Design API
- [x] Create test fixtures
- [ ] Implement parser for Datalog syntax
- [ ] Implement core data structures
- [ ] Implement semi-naive evaluation

### Phase 2: Optimization
- [ ] Add indexing
- [ ] Implement galloping joins
- [ ] Add stratification for negation

### Phase 3: Advanced Features
- [ ] Aggregation (count, sum, min, max)
- [ ] Built-in predicates (>, <, !=, etc.)
- [ ] Query optimization

## Test Fixtures

14 test fixtures have been created in `./tests/fixtures/`:

1. **Basic (0001-0005)**: Empty universe, simple facts, binary relations, joins
2. **Rules (0006-0014)**: Simple rules, transitive closure, ancestor relationships
3. **Advanced (0015-0029)**: Negation, complex recursion, constants
4. **Performance (0030+)**: Large graphs, multiple predicates, stress tests

Each fixture consists of:
- `.datalog` file with facts and rules
- `.datalog.expected` file with expected JSON output

## Key Design Decisions

### 1. String-Based Query API (from DataScript)
**Why**: Flexibility and ease of use
**Alternative**: Type-safe API (rejected for v1 complexity)

### 2. Immutable Universe (from DataScript)
**Why**: Functional programming style, easier reasoning
**Alternative**: Mutable state with atoms (may add later for performance)

### 3. Explicit Iteration (from Datafrog)
**Why**: User controls when computation happens
**Alternative**: Automatic evaluation (may add as convenience)

### 4. No Schema Required (from DataScript)
**Why**: Flexibility, easier to get started
**Alternative**: Required schema (may add as optional feature)

## Performance Considerations

### Memory
- Use persistent data structures from `Std`
- Share structure between iterations
- Limit intermediate result size

### CPU
- Indices for fast lookups
- Galloping search for sorted data
- Semi-naive evaluation to avoid redundant work

### Scalability Targets
- 10K facts: Should be instant
- 100K facts: Should be fast (< 1 second)
- 1M facts: Should be reasonable (< 10 seconds)

## Future Extensions

1. **Persistent Storage**: Store facts on disk
2. **Incremental Updates**: Add/remove facts without full re-evaluation
3. **Pull API**: DataScript-style hierarchical queries
4. **Aggregation**: count, sum, min, max, etc.
5. **Modules**: Namespaced predicates
6. **Type System**: Optional static typing for facts

## References

- **Datafrog**: https://github.com/rust-lang/datafrog
- **Crepe**: https://github.com/ekzhang/crepe
- **DataScript**: https://github.com/tonsky/datascript
- **Datalog Tutorial**: http://www.learndatalogtoday.org/
- **Semi-Naive Evaluation**: "Datalog and Recursive Query Processing" (Foundations and Trends)
