# Datalog Storage Interface - Quick Reference

## TL;DR for Poneglyph

**What you need**: Implement 4 functions that return tuples of values.

**Where**: Create `packages/poneglyph/src/datalog_storage.ml`

**Time estimate**: 2-4 hours

**When**: Now! (Evaluator comes Week 2, but you can start integration today)

---

## The Complete Interface

```ocaml
open Std
open Datalog

module PoneglyphStorage : Storage.STORAGE = struct
  type t = Poneglyph.graph  (* Or whatever your graph type is *)
  
  (* 1. REQUIRED: Get all facts for a predicate *)
  let get_facts graph ~predicate =
    (* Return tuples as a Relation *)
    Relation.of_list [
      [Value.Int 1; Value.Int 2];    (* Example: edge(1, 2) *)
      [Value.Int 2; Value.Int 3];    (* Example: edge(2, 3) *)
    ]
  
  (* 2. REQUIRED: List all predicates *)
  let predicates _graph =
    ["edge"; "node"; "triple"; "property"]
  
  (* 3. REQUIRED: Stream facts (can delegate to get_facts) *)
  let iter_facts graph ~predicate f =
    let facts = get_facts graph ~predicate in
    Relation.iter f facts
  
  (* 4. OPTIONAL: Pattern matching (can delegate to get_facts + filter) *)
  let get_facts_matching graph ~predicate ~pattern =
    let facts = get_facts graph ~predicate in
    Relation.filter (Storage.matches_pattern pattern) facts
end
```

---

## Value Types

You can return 3 types of values in tuples:

```ocaml
type Value.t =
  | Int of int           (* Node IDs, counts, etc. *)
  | String of string     (* Labels, names, etc. *)
  | Uri of string        (* URIs, URLs, etc. *)
```

Example tuple: `[Value.Int 42; Value.String "alice"; Value.Uri "http://example.com"]`

---

## Example Implementations

### 1. Simple Edge Predicate

```ocaml
let get_facts graph ~predicate =
  match predicate with
  | "edge" ->
      let edges = Poneglyph.get_all_edges graph in
      let tuples = List.map (fun edge ->
        [Value.Int edge.from; Value.Int edge.to; Value.String edge.label]
      ) edges in
      Relation.of_list tuples
  | _ -> Relation.empty ()
```

**Result**: Predicate `edge(From, To, Label)` with tuples like:
- `edge(1, 2, "friend")`
- `edge(2, 3, "colleague")`
- `edge(3, 1, "family")`

### 2. RDF Triple Predicate

```ocaml
let get_facts graph ~predicate =
  match predicate with
  | "triple" ->
      let triples = Poneglyph.get_all_triples graph in
      let tuples = List.map (fun triple ->
        [
          Value.Uri triple.subject;
          Value.Uri triple.predicate;
          value_of_object triple.object;  (* Your conversion function *)
        ]
      ) triples in
      Relation.of_list tuples
  | _ -> Relation.empty ()
```

**Result**: Predicate `triple(Subject, Predicate, Object)` with tuples like:
- `triple("alice", "knows", "bob")`
- `triple("bob", "age", 30)`
- `triple("alice", "type", "Person")`

### 3. Node Properties

```ocaml
let get_facts graph ~predicate =
  match predicate with
  | "node" ->
      let nodes = Poneglyph.get_all_nodes graph in
      let tuples = List.map (fun node ->
        [Value.Int node.id; Value.String node.type_; Value.String node.label]
      ) nodes in
      Relation.of_list tuples
  | _ -> Relation.empty ()
```

**Result**: Predicate `node(Id, Type, Label)` with tuples like:
- `node(1, "Person", "Alice")`
- `node(2, "Person", "Bob")`  
- `node(3, "Company", "Acme Corp")`

---

## Optimization: Using Indexes

If Poneglyph has indexes, you can make pattern queries faster:

```ocaml
let get_facts_matching graph ~predicate ~pattern =
  match predicate, pattern with
  
  (* Pattern: edge(1, ?, ?) - All edges FROM node 1 *)
  | "edge", [Some (Value.Int from); None; None] ->
      let edges = Poneglyph.edges_from graph from in  (* Use index! *)
      let tuples = List.map (fun edge ->
        [Value.Int edge.from; Value.Int edge.to; Value.String edge.label]
      ) edges in
      Relation.of_list tuples
  
  (* Pattern: edge(?, 5, ?) - All edges TO node 5 *)
  | "edge", [None; Some (Value.Int to_); None] ->
      let edges = Poneglyph.edges_to graph to_ in  (* Use index! *)
      let tuples = List.map (fun edge ->
        [Value.Int edge.from; Value.Int edge.to; Value.String edge.label]
      ) edges in
      Relation.of_list tuples
  
  (* Pattern: edge(?, ?, "friend") - All "friend" edges *)
  | "edge", [None; None; Some (Value.String label)] ->
      let edges = Poneglyph.edges_with_label graph label in  (* Use index! *)
      let tuples = List.map (fun edge ->
        [Value.Int edge.from; Value.Int edge.to; Value.String edge.label]
      ) edges in
      Relation.of_list tuples
  
  (* Default: Fall back to full scan *)
  | _ ->
      let facts = get_facts graph ~predicate in
      Relation.filter (Storage.matches_pattern pattern) facts
```

**Performance**: O(index_size) instead of O(all_facts)

---

## Usage Example (Once Evaluator is Done)

```ocaml
(* Create universe backed by Poneglyph *)
module U = Datalog.Universe.Make(PoneglyphStorage)

let find_all_reachable graph start_node =
  let universe = U.create graph in
  
  (* Add transitive closure rules *)
  let rules = [
    (* Base case: direct edges are reachable *)
    rule { reachable(X, Y) :- edge(X, Y) };
    
    (* Recursive case: transitive closure *)
    rule { reachable(X, Z) :- edge(X, Y), reachable(Y, Z) };
  ] in
  
  let universe = U.add_rules universe rules in
  
  (* Evaluate to fixed point *)
  let universe = Evaluator.eval universe in
  
  (* Query: what can start_node reach? *)
  let pattern = [Some (Value.Int start_node); None] in
  let reachable = U.get_facts_matching universe 
    ~predicate:"reachable" ~pattern in
  
  (* Return list of reachable node IDs *)
  Relation.to_list reachable
  |> List.filter_map (fun tuple ->
      match tuple with
      | [Value.Int _from; Value.Int to_] -> Some to_
      | _ -> None)
```

**What happens**:
1. Datalog fetches `edge` facts from Poneglyph (via `get_facts`)
2. Evaluates rules to compute `reachable` facts
3. Returns all nodes reachable from `start_node`
4. **No copying** - edges stay in Poneglyph!

---

## Testing Your Implementation

```ocaml
(* Test file: packages/poneglyph/tests/datalog_tests.ml *)
open Std
open Datalog

let test_storage () =
  let graph = Poneglyph.create () in
  
  (* Add test data *)
  Poneglyph.add_edge graph ~from:1 ~to_:2 ~label:"link";
  Poneglyph.add_edge graph ~from:2 ~to_:3 ~label:"link";
  
  (* Test storage *)
  let storage = PoneglyphStorage.create graph in
  let facts = PoneglyphStorage.get_facts storage ~predicate:"edge" in
  
  assert (Relation.length facts = 2);
  
  let preds = PoneglyphStorage.predicates storage in
  assert (List.mem "edge" preds);
  
  printf "✅ Storage tests passed!\n"
```

---

## Predicate Design Patterns

### Pattern 1: Binary Relations
```ocaml
"edge"   -> (from:Int, to:Int)
"friend" -> (person1:String, person2:String)
"parent" -> (parent:String, child:String)
```

### Pattern 2: Ternary Relations  
```ocaml
"edge"     -> (from:Int, to:Int, label:String)
"property" -> (entity:Int, key:String, value:String)
"triple"   -> (subject:Uri, predicate:Uri, object:Value)
```

### Pattern 3: Property Tables
```ocaml
"person"  -> (id:Int, name:String, age:Int)
"company" -> (id:Int, name:String, industry:String)
"node"    -> (id:Int, type:String, label:String)
```

**Recommendation**: Start with Pattern 2 (ternary edge relation), it's most flexible.

---

## Performance Tips

### 1. Lazy Construction
Don't materialize all facts upfront. `get_facts` is called only when needed.

```ocaml
(* Bad: Materializes everything on startup *)
let storage = {
  edges = Poneglyph.get_all_edges graph;  (* Expensive! *)
  nodes = Poneglyph.get_all_nodes graph;
}

(* Good: Fetches on demand *)
let get_facts graph ~predicate =
  match predicate with
  | "edge" -> Poneglyph.get_all_edges graph  (* Fetched when needed *)
  | "node" -> Poneglyph.get_all_nodes graph
```

### 2. Caching
If same predicate queried multiple times, cache it:

```ocaml
type t = {
  graph : Poneglyph.graph;
  cache : (string, fact_tuple Relation.t) HashMap.t;
}

let get_facts storage ~predicate =
  match HashMap.get storage.cache predicate with
  | Some facts -> facts  (* Cache hit! *)
  | None ->
      let facts = compute_facts storage.graph predicate in
      HashMap.insert storage.cache predicate facts |> ignore;
      facts
```

### 3. Streaming
For huge predicates, use `iter_facts` to avoid materializing:

```ocaml
let iter_facts graph ~predicate f =
  match predicate with
  | "edge" ->
      (* Stream edges without creating list *)
      Poneglyph.iter_edges graph (fun edge ->
        f [Value.Int edge.from; Value.Int edge.to; Value.String edge.label])
  | _ -> 
      (* Fall back to materialized *)
      let facts = get_facts graph ~predicate in
      Relation.iter f facts
```

---

## Common Questions

### Q: What if my graph is huge?
**A**: Use `iter_facts` for streaming, and `get_facts_matching` with indexes.

### Q: Can I have predicates with different arities?
**A**: Yes! `edge` can be arity-2, `triple` can be arity-3, etc.

### Q: What if I need floats or dates?
**A**: Encode as strings or ints for now. We can extend `Value.t` later.

### Q: How do I handle NULL values?
**A**: Use a special value like `Value.String "NULL"` or omit the tuple.

### Q: Can predicates be virtual (computed on the fly)?
**A**: Yes! `get_facts` can compute anything, not just fetch from storage.

---

## Checklist for Implementation

- [ ] Create `datalog_storage.ml` in Poneglyph package
- [ ] Implement `type t` (probably just `Poneglyph.graph`)
- [ ] Implement `get_facts` for at least one predicate (e.g., "edge")
- [ ] Implement `predicates` (return list of supported predicates)
- [ ] Implement `iter_facts` (can delegate to `get_facts`)
- [ ] Implement `get_facts_matching` (can delegate to `get_facts` + filter)
- [ ] Create test file `datalog_tests.ml`
- [ ] Test that facts are returned correctly
- [ ] Test that predicates list is correct
- [ ] Optimize with indexes (optional)
- [ ] Add caching (optional)

**Estimated time**: 2-4 hours for basic implementation

---

## Next Steps

1. **Now**: Implement `PoneglyphStorage` module
2. **Week 2**: Wait for Datalog evaluator (coming soon!)
3. **Week 3**: Start writing Datalog queries for Poneglyph
4. **Week 4**: Benchmark and optimize

---

## Need Help?

The storage interface is designed to be simple. If you're stuck:

1. Look at `InmemoryStorage` implementation as reference
2. Check `PONEGLYPH_INTEGRATION.md` for detailed examples
3. Run Datalog tests to see how storage is used
4. The interface is minimal - only 4 functions!

---

## Summary

**What Poneglyph provides**:
- Graph data (edges, nodes, triples)
- Indexes for fast lookups
- Iteration over entities

**What Datalog provides**:
- Storage interface to access graph data
- Query engine for complex queries
- Transitive closure, path finding, etc.

**How they connect**:
- Implement 4 functions
- Datalog fetches facts via interface
- Zero copying, lazy loading, indexed access

**Result**: Powerful graph queries with no data duplication! 🚀
