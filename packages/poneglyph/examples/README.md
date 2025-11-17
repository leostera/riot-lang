# Poneglyph Datalog Query Examples

This directory contains runnable examples demonstrating the Datalog query API.

## Running Examples

```bash
# Run all examples
tusk run poneglyph:basic_datalog_query
tusk run poneglyph:transitive_dependencies
tusk run poneglyph:multi_attribute_query
tusk run poneglyph:build_system_metadata
```

## Examples

### 1. Basic Datalog Query (`basic_datalog_query.ml`)

**What it demonstrates:**
- Creating a graph with module dependencies
- Running basic Datalog queries
- Using `query()` to get raw substitutions
- Using `query_entities()` to extract URIs

**Key concepts:**
- `query()` returns an iterator over variable bindings
- `query_entities()` extracts URIs from a specific variable
- String constants in queries use double quotes: `"module:A"`

### 2. Transitive Dependencies (`transitive_dependencies.ml`)

**What it demonstrates:**
- Defining Datalog rules for transitive relationships
- Querying transitive paths through dependency graphs
- Using the classic path/reachability rules

**Key concepts:**
- Rules extend the query language with derived predicates
- The classic transitive closure pattern:
  ```datalog
  path(X, Y) :- depends_on(X, Y).
  path(X, Z) :- depends_on(X, Y), path(Y, Z).
  ```

### 3. Multi-Attribute Queries (`multi_attribute_query.ml`)

**What it demonstrates:**
- Storing multiple types of facts about entities
- Querying based on multiple attributes
- Using `query_facts()` to get full entity information

**Key concepts:**
- Multi-attribute queries: `formatted(F, "true"), has_tests(F, "true")`
- Wildcards: `depends_on(F, _)` matches any dependency
- `query_facts()` streams all facts for entities matching a query

### 4. Build System Metadata (`build_system_metadata.ml`)

**What it demonstrates:**
- Real-world use case: tracking build artifacts
- Finding files that need rebuilding
- Analyzing stale dependencies

**Key concepts:**
- Practical application of graph queries
- Combining multiple predicates to answer complex questions
- Incremental build analysis

## API Quick Reference

### Core Query Functions

```ocaml
(* Execute Datalog query, get variable bindings *)
val query : 
  t -> 
  rules:string list -> 
  query:string -> 
  (Datalog.Substitution.t Iter.MutIterator.t, string) result

(* Extract URIs from query results *)
val query_entities : 
  t -> 
  rules:string list -> 
  query_str:string -> 
  var:string -> 
  (Uri.t Iter.MutIterator.t, string) result

(* Get all facts for entities from query *)
val query_facts : 
  t -> 
  rules:string list -> 
  query_str:string -> 
  entities_from:string -> 
  (Fact.t Iter.MutIterator.t, string) result
```

### Datalog Syntax

**Queries:**
- Variables: `X`, `Y`, `Module`
- Constants: `"string"`, `123`
- Wildcards: `_`
- Examples: `depends_on(X, Y)`, `formatted("file:A", true)`

**Rules:**
- Format: `head :- body1, body2, ...`
- Example: `path(X, Z) :- edge(X, Y), path(Y, Z)`

## Notes

- All queries return iterators for memory-efficient streaming
- Results are lazy - only computed as you consume them
- String constants must use double quotes: `"module:A"` not `'module:A'`
- The Datalog evaluator supports complex queries, though some transitive
  closure features may have limitations in the current implementation

## Learn More

- See `../src/poneglyph.mli` for full API documentation
- See `../tests/datalog_query_tests.ml` for more usage examples
- See `packages/datalog/` for Datalog engine documentation
