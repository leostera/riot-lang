# Poneglyph Integration Guide

## Overview

Datalog now has a **pluggable storage interface** that allows it to work with different backends. Poneglyph can implement this interface to provide zero-copy access to graph data for Datalog queries.

## Key Benefits

1. **Zero-Copy**: No need to copy facts from Poneglyph into Datalog
2. **Lazy Evaluation**: Facts are only fetched when rules need them
3. **Native Performance**: Poneglyph's graph indexes used directly
4. **Clean Separation**: Datalog = query engine, Poneglyph = storage

## The Storage Interface

Located in `packages/datalog/src/storage.mli`, the interface is minimal:

```ocaml
module type STORAGE = sig
  type t
  
  val get_facts : t -> predicate:string -> fact_tuple Relation.t
  val predicates : t -> string list
  val iter_facts : t -> predicate:string -> (fact_tuple -> unit) -> unit
  val get_facts_matching : t -> predicate:string -> 
    pattern:Value.t option list -> fact_tuple Relation.t
end
```

Where `fact_tuple = Value.t list` and `Value.t` is:
```ocaml
type t = 
  | Int of int
  | String of string
  | Uri of string
```

## Implementation Steps for Poneglyph

### Step 1: Define Predicate Mapping

Decide how Poneglyph concepts map to Datalog predicates:

```ocaml
(* Example mapping *)
"triple"   -> RDF triple (subject, predicate, object)
"edge"     -> Graph edge (from, to, label)
"node"     -> Graph node (id, type, label)
"property" -> Node property (node_id, key, value)
```

### Step 2: Implement the Storage Module

Create `packages/poneglyph/src/datalog_storage.ml`:

```ocaml
open Std
open Datalog

module PoneglyphStorage : Storage.STORAGE = struct
  type t = Poneglyph.graph
  
  (* Core: Get all facts for a predicate *)
  let get_facts graph ~predicate =
    match predicate with
    | "triple" ->
        (* Fetch all RDF triples from Poneglyph *)
        let triples = Poneglyph.all_triples graph in
        let tuples = List.map (fun triple ->
          [
            node_to_value triple.subject;
            Value.String triple.predicate;
            node_to_value triple.object;
          ]
        ) triples in
        Relation.of_list tuples
    
    | "edge" ->
        (* Fetch all edges *)
        let edges = Poneglyph.all_edges graph in
        let tuples = List.map (fun edge ->
          [
            Value.Int edge.from_id;
            Value.Int edge.to_id;
            Value.String edge.label;
          ]
        ) edges in
        Relation.of_list tuples
    
    | "node" ->
        (* Fetch all nodes *)
        let nodes = Poneglyph.all_nodes graph in
        let tuples = List.map (fun node ->
          [
            Value.Int node.id;
            Value.String node.node_type;
            Value.String node.label;
          ]
        ) nodes in
        Relation.of_list tuples
    
    | _ -> Relation.empty ()
  
  (* List available predicates *)
  let predicates _graph =
    ["triple"; "edge"; "node"; "property"]
  
  (* Stream facts without materializing *)
  let iter_facts graph ~predicate f =
    match predicate with
    | "edge" ->
        Poneglyph.iter_edges graph (fun edge ->
          let tuple = [
            Value.Int edge.from_id;
            Value.Int edge.to_id;
            Value.String edge.label;
          ] in
          f tuple)
    | _ ->
        (* Fall back to materializing *)
        let facts = get_facts graph ~predicate in
        Relation.iter f facts
  
  (* Optional: Indexed access for better performance *)
  let get_facts_matching graph ~predicate ~pattern =
    match predicate, pattern with
    | "edge", [Some (Value.Int from_id); None; None] ->
        (* Use Poneglyph's index: all edges from specific node *)
        let edges = Poneglyph.edges_from graph from_id in
        let tuples = List.map (fun edge ->
          [Value.Int edge.from_id; Value.Int edge.to_id; Value.String edge.label]
        ) edges in
        Relation.of_list tuples
    
    | "edge", [None; Some (Value.Int to_id); None] ->
        (* Use Poneglyph's index: all edges to specific node *)
        let edges = Poneglyph.edges_to graph to_id in
        let tuples = List.map (fun edge ->
          [Value.Int edge.from_id; Value.Int edge.to_id; Value.String edge.label]
        ) edges in
        Relation.of_list tuples
    
    | _ ->
        (* Fall back to full scan with filter *)
        let facts = get_facts graph ~predicate in
        Relation.filter (Storage.matches_pattern pattern) facts
end

(* Helper: Convert Poneglyph node to Datalog value *)
let node_to_value node =
  match node with
  | Poneglyph.Node.Int id -> Value.Int id
  | Poneglyph.Node.Uri uri -> Value.Uri uri
  | Poneglyph.Node.String s -> Value.String s
```

### Step 3: Create Universe with Poneglyph Backend

```ocaml
(* In Poneglyph code *)
module DatalogUniverse = Datalog.Universe.Make(PoneglyphStorage)

let query_with_datalog graph =
  (* Create universe backed by Poneglyph *)
  let universe = DatalogUniverse.create graph in
  
  (* Add rules *)
  let reachable_rule1 = Datalog.Ast.rule
    ~head:(Datalog.Ast.atom ~predicate:"reachable" 
      ~args:[Term.Var "X"; Term.Var "Y"])
    ~body:[Datalog.Ast.Atom 
      (Datalog.Ast.atom ~predicate:"edge" 
        ~args:[Term.Var "X"; Term.Var "Y"])]
  in
  
  let reachable_rule2 = Datalog.Ast.rule
    ~head:(Datalog.Ast.atom ~predicate:"reachable" 
      ~args:[Term.Var "X"; Term.Var "Z"])
    ~body:[
      Datalog.Ast.Atom (Datalog.Ast.atom ~predicate:"edge" 
        ~args:[Term.Var "X"; Term.Var "Y"]);
      Datalog.Ast.Atom (Datalog.Ast.atom ~predicate:"reachable" 
        ~args:[Term.Var "Y"; Term.Var "Z"])
    ]
  in
  
  let universe = DatalogUniverse.add_rules universe 
    [reachable_rule1; reachable_rule2] in
  
  (* Evaluate rules (computes transitive closure!) *)
  let universe = Datalog.Evaluator.eval universe in
  
  (* Query results *)
  let reachable_facts = DatalogUniverse.get_facts universe 
    ~predicate:"reachable" in
  
  Relation.iter (fun tuple ->
    match tuple with
    | [Value.Int from; Value.Int to_] ->
        printf "Node %d can reach node %d\n" from to_
    | _ -> ()
  ) reachable_facts
```

## Example Use Cases

### 1. Transitive Closure (Reachability)

```datalog
% Base case
reachable(X, Y) :- edge(X, Y).

% Recursive case
reachable(X, Z) :- edge(X, Y), reachable(Y, Z).
```

**Query**: `reachable(1, Y)` - Find all nodes reachable from node 1

### 2. Path Finding

```datalog
% 2-hop paths
path_2(X, Z) :- edge(X, Y), edge(Y, Z).

% 3-hop paths
path_3(X, W) :- edge(X, Y), edge(Y, Z), edge(Z, W).

% All paths up to N hops
path(X, Y, 1) :- edge(X, Y).
path(X, Z, N+1) :- path(X, Y, N), edge(Y, Z).
```

### 3. Common Neighbors

```datalog
% Nodes that both X and Y connect to
common_neighbor(X, Y, Z) :- edge(X, Z), edge(Y, Z), X != Y.
```

### 4. Triangle Detection

```datalog
% Find all triangles in graph
triangle(X, Y, Z) :- 
  edge(X, Y), edge(Y, Z), edge(Z, X),
  X < Y, Y < Z.
```

## API Reference

### Required Functions

#### `get_facts`
```ocaml
val get_facts : t -> predicate:string -> fact_tuple Relation.t
```
Fetch all facts for a predicate. This is the core function.

**Performance**: O(n) where n = number of facts for predicate
**Caching**: Consider caching if predicates are queried repeatedly

#### `predicates`
```ocaml
val predicates : t -> string list
```
Return list of all available predicate names.

**Performance**: O(1) if using a fixed list

#### `iter_facts`
```ocaml
val iter_facts : t -> predicate:string -> (fact_tuple -> unit) -> unit
```
Stream facts without materializing entire relation.

**Use case**: Large predicates where memory is a concern

### Optional Optimization

#### `get_facts_matching`
```ocaml
val get_facts_matching : t -> predicate:string -> 
  pattern:Value.t option list -> fact_tuple Relation.t
```
Use indexes for pattern queries. Pattern uses `Some v` for constants, `None` for wildcards.

**Example patterns**:
- `[Some (Int 1); None; None]` - All edges from node 1
- `[None; None; Some (String "friend")]` - All edges with label "friend"

**Default**: Falls back to full scan + filter if not optimized

## Performance Considerations

### 1. Lazy Loading
Only fetch predicates when rules need them:
```ocaml
(* This only fetches "edge" facts, not "node" or "triple" *)
let rule = reachable(X, Y) :- edge(X, Y)
```

### 2. Index Usage
If Poneglyph has indexes on edge endpoints:
```ocaml
(* This can use index on from_id *)
let facts = get_facts_matching graph ~predicate:"edge"
  ~pattern:[Some (Int 1); None; None]
```

### 3. Incremental Evaluation
Datalog uses semi-naive evaluation:
- Only processes new facts each iteration
- Avoids recomputing stable facts
- Converges quickly for most queries

### 4. Caching
Consider caching `get_facts` results:
```ocaml
let cache = HashMap.create () in

let get_facts graph ~predicate =
  match HashMap.get cache predicate with
  | Some facts -> facts
  | None ->
      let facts = compute_facts graph predicate in
      HashMap.insert cache predicate facts |> ignore;
      facts
```

## Testing

Use the existing Datalog test suite:

```bash
# Build Poneglyph with Datalog storage
tusk build poneglyph

# Run tests
tusk test poneglyph:datalog_tests
```

Example test:
```ocaml
let test_reachability () =
  let graph = Poneglyph.create () in
  Poneglyph.add_edge graph ~from:1 ~to_:2 ~label:"link";
  Poneglyph.add_edge graph ~from:2 ~to_:3 ~label:"link";
  
  module U = Datalog.Universe.Make(PoneglyphStorage) in
  let universe = U.create graph in
  
  (* Add reachability rules *)
  let universe = U.add_rules universe reachability_rules in
  let universe = Datalog.Evaluator.eval universe in
  
  (* Check that node 1 can reach node 3 *)
  let reachable = U.get_facts universe ~predicate:"reachable" in
  assert (Relation.contains reachable [Value.Int 1; Value.Int 3])
```

## Next Steps

1. **Implement `PoneglyphStorage`** module in Poneglyph package
2. **Add convenience functions** for common queries
3. **Benchmark** against manual graph traversal
4. **Add examples** showing Datalog queries on real graphs
5. **Document** predicate schema for users

## Questions?

The storage interface is designed to be minimal and flexible. If you need:
- Different value types (e.g., floats, timestamps)
- Custom predicate schemes
- Streaming/pagination for huge datasets
- Transaction support

These can all be accommodated by extending the interface or adding storage-specific features.

## Current Status

✅ **Storage interface** - Complete and documented
✅ **InMemory backend** - Working with 12 tests passing
⏳ **Evaluation engine** - Coming in Week 2 (unification, joins, evaluator)
⏳ **Poneglyph implementation** - Ready for you to implement!

Once the evaluator is complete (Week 2), you'll be able to run full Datalog queries on Poneglyph graphs!
