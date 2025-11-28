# Collections + Iterator-Centric Design

## Core Philosophy

**Collections are data containers. Iterators are for processing.**

Instead of duplicating `map`, `filter`, `fold` on every collection, we have:
- Collections provide `into_iter` / `to_mut_iter` 
- Iterator module provides rich combinators
- Change the collection type, keep the iteration code the same

## Key Benefits

1. **Flexibility**: Swap `Vector` for `List` without changing iteration logic
2. **Consistency**: All iteration uses same API regardless of collection
3. **Composability**: Iterator combinators work with any collection
4. **Simplicity**: Collections focus on storage, Iterator focuses on processing

## Revised Signature Conventions

### Collections API (minimal, storage-focused)

```ocaml
(* Common to all collections *)
val create : unit -> 'a t
val of_list : 'a list -> 'a t
val to_list : 'a t -> 'a list
val length : 'a t -> int
val is_empty : 'a t -> bool

(* Iterator conversion - THE KEY OPERATION *)
val into_iter : 'a t -> 'a Iterator.t
val to_mut_iter : 'a t -> 'a MutIterator.t

(* Collection-specific operations *)
(* e.g., for Vector: *)
val push : 'a t -> 'a -> unit
val pop : 'a t -> 'a option
val get : 'a t -> int -> 'a option

(* e.g., for HashMap: *)
val insert : ('k, 'v) t -> key:'k -> value:'v -> 'v option
val get : ('k, 'v) t -> key:'k -> 'v option
val remove : ('k, 'v) t -> key:'k -> 'v option
```

### Iterator API (rich combinators)

```ocaml
(* Core *)
val next : 'a t -> 'a option * 'a t
val size : 'a t -> int
val to_list : 'a t -> 'a list

(* Transformation *)
val map : 'a t -> fn:('a -> 'b) -> 'b t
val filter : 'a t -> fn:('a -> bool) -> 'a t
val filter_map : 'a t -> fn:('a -> 'b option) -> 'b t
val flat_map : 'a t -> fn:('a -> 'b t) -> 'b t

(* Reduction *)
val fold : 'a t -> init:'acc -> fn:('acc -> 'a -> 'acc) -> 'acc
val reduce : 'a t -> fn:('a -> 'a -> 'a) -> 'a option
val sum : int t -> int
val product : int t -> int

(* Search *)
val find : 'a t -> fn:('a -> bool) -> 'a option
val position : 'a t -> fn:('a -> bool) -> int option
val any : 'a t -> fn:('a -> bool) -> bool
val all : 'a t -> fn:('a -> bool) -> bool

(* Combinators *)
val take : 'a t -> int -> 'a t
val drop : 'a t -> int -> 'a t
val take_while : 'a t -> fn:('a -> bool) -> 'a t
val drop_while : 'a t -> fn:('a -> bool) -> 'a t
val zip : 'a t -> 'b t -> ('a * 'b) t
val enumerate : 'a t -> (int * 'a) t
val chain : 'a t -> 'a t -> 'a t

(* Collectors *)
val collect_list : 'a t -> 'a list
val collect_vec : 'a t -> 'a Vector.t
val collect_set : 'a t -> 'a HashSet.t
val collect_map : ('k * 'v) t -> ('k, 'v) HashMap.t

(* Side effects *)
val for_each : 'a t -> fn:('a -> unit) -> unit
```

### MutIterator API (same operations, mutable)

```ocaml
(* Same as Iterator but mutating *)
val next : 'a t -> 'a option
val map : 'a t -> fn:('a -> 'b) -> 'b t
val filter : 'a t -> fn:('a -> bool) -> 'a t
val fold : 'a t -> init:'acc -> fn:('acc -> 'a -> 'acc) -> 'acc
(* etc... *)
```

## Usage Examples

### Before (duplicated on each collection)

```ocaml
(* With Vector *)
let doubled = Vector.map (fun x -> x * 2) vec in
let evens = Vector.filter (fun x -> x mod 2 = 0) doubled in
let sum = Vector.fold (fun acc x -> acc + x) evens 0

(* Want to switch to List? Rewrite everything! *)
let doubled = List.map (fun x -> x * 2) list in
let evens = List.filter (fun x -> x mod 2 = 0) doubled in
let sum = List.fold_left (fun acc x -> acc + x) 0 evens
```

### After (Iterator-centric)

```ocaml
(* With Vector *)
let result =
  vec
  |> Vector.into_iter
  |> Iterator.map ~fn:(fun x -> x * 2)
  |> Iterator.filter ~fn:(fun x -> x mod 2 = 0)
  |> Iterator.fold ~init:0 ~fn:(fun acc x -> acc + x)

(* Switch to List? Just change first line! *)
let result =
  list
  |> List.into_iter
  |> Iterator.map ~fn:(fun x -> x * 2)
  |> Iterator.filter ~fn:(fun x -> x mod 2 = 0)
  |> Iterator.fold ~init:0 ~fn:(fun acc x -> acc + x)

(* Or even Deque, Queue, whatever - same code! *)
let result =
  deque
  |> Deque.into_iter
  |> Iterator.map ~fn:(fun x -> x * 2)
  |> Iterator.filter ~fn:(fun x -> x mod 2 = 0)
  |> Iterator.fold ~init:0 ~fn:(fun acc x -> acc + x)
```

### Collecting back to collections

```ocaml
(* Start with Vector, end with List *)
let result: int list =
  vec
  |> Vector.into_iter
  |> Iterator.filter ~fn:(fun x -> x > 0)
  |> Iterator.map ~fn:(fun x -> x * 2)
  |> Iterator.collect_list

(* Start with List, end with HashSet *)
let unique_evens: int HashSet.t =
  list
  |> List.into_iter
  |> Iterator.filter ~fn:(fun x -> x mod 2 = 0)
  |> Iterator.collect_set

(* Build a HashMap from pairs *)
let word_counts: (string, int) HashMap.t =
  words
  |> Vector.into_iter
  |> Iterator.map ~fn:(fun word -> (word, String.length word))
  |> Iterator.collect_map
```

### Short-circuiting and efficiency

```ocaml
(* Find first matching element - no iteration beyond what's needed *)
let first_even =
  vec
  |> Vector.into_iter
  |> Iterator.find ~fn:(fun x -> x mod 2 = 0)
  (* Returns Some(2) or None *)

(* Check if any element matches *)
let has_negative =
  vec
  |> Vector.into_iter
  |> Iterator.any ~fn:(fun x -> x < 0)
  (* Returns bool, stops on first true *)

(* Take only first N items *)
let first_ten =
  large_vec
  |> Vector.into_iter
  |> Iterator.take 10
  |> Iterator.collect_list
```

## Naming Conventions

### For function arguments: Use `~fn` everywhere

```ocaml
(* NOT ~predicate, ~mapper, ~folder - just ~fn *)
val map : 'a t -> fn:('a -> 'b) -> 'b t
val filter : 'a t -> fn:('a -> bool) -> 'a t  
val fold : 'a t -> init:'acc -> fn:('acc -> 'a -> 'acc) -> 'acc
val find : 'a t -> fn:('a -> bool) -> 'a option
```

**Rationale**: 
- Simpler, shorter
- Less to remember
- Context makes purpose clear (`filter ~fn` is obviously a predicate)
- OCaml stdlib uses `f` frequently

### For map operations: Use `~key` and `~value`

```ocaml
val insert : ('k, 'v) t -> key:'k -> value:'v -> 'v option
val get : ('k, 'v) t -> key:'k -> 'v option
val update : ('k, 'v) t -> key:'k -> fn:('v option -> 'v option) -> unit
```

### For iterators: Use `~init` for initial accumulator

```ocaml
val fold : 'a t -> init:'acc -> fn:('acc -> 'a -> 'acc) -> 'acc
```

## Migration Strategy

### Phase 1: Expand Iterator API
1. Add rich combinators to Iterator module:
   - map, filter, filter_map, flat_map
   - fold, reduce, sum, product
   - find, position, any, all
   - take, drop, take_while, drop_while
   - zip, enumerate, chain
   - collect_list, collect_vec, collect_set, collect_map
   - for_each

2. Add same to MutIterator module

### Phase 2: Add `into_iter` / `to_mut_iter` to all collections
- Vector, List, Array, HashMap, HashSet, Queue, Deque, Heap, Stream
- Keep existing collection-specific methods for now

### Phase 3: Update collection signatures (breaking)
- Change `iter` → remove or rename to `for_each`
- Change `fold` → remove (use `into_iter |> Iterator.fold`)
- Keep mutation operations: push, pop, insert, remove, etc.
- Update all call sites to use Iterator API

### Phase 4: Deprecation
- Mark old methods as deprecated
- Provide migration guide
- After grace period, remove deprecated methods

## What Collections Keep

Collections should keep:
- Construction: `create`, `of_list`, `with_capacity`
- Conversion: `to_list`, `into_iter`, `to_mut_iter`
- Queries: `length`, `is_empty`, `contains`
- Mutation: `push`, `pop`, `insert`, `remove`, `get`, `set`
- Collection-specific: `union`, `intersection` (sets), `peek` (queue/heap)

Collections should NOT have:
- Generic iteration: `iter`, `map`, `filter`, `fold` → use Iterator
- Search: `find`, `exists`, `for_all` → use Iterator
- Transformation: `rev`, `take`, `drop` → use Iterator (or keep if core operation)

## HashMap/HashSet Special Case

For HashMap/HashSet, keep high-level algebra operations:

```ocaml
(* HashSet *)
val union : 'a t -> 'a t -> 'a t
val intersection : 'a t -> 'a t -> 'a t
val difference : 'a t -> 'a t -> 'a t

(* HashMap - these are more efficient than iterator-based *)
val merge : ('k, 'v) t -> ('k, 'v) t -> fn:('k -> 'v -> 'v -> 'v) -> ('k, 'v) t
```

These are collection-specific operations that don't make sense on Iterator.

## Open Questions

1. **Fold argument order**: `fn:('acc -> 'a -> 'acc)` (accumulator first)
   - More common in Rust, F#, etc.
   - Easier to use with partial application
   - Matches reduce semantics

2. **Array/Stream wrapping**: Keep as stdlib re-export or wrap?
   - Need `into_iter` at minimum
   - Could provide thin wrapper just for that

3. **Naming**: `into_iter` vs `iter` vs `to_iter`?
   - `into_iter` - Rust naming, implies consumption
   - `to_iter` - Clearer for immutable case
   - `iter` - Shorter, but conflicts with for-each operation
   - **Proposal**: `into_iter` for owned, `to_iter` for borrowed (if we add that later)

4. **MutIterator vs Iterator**: When to use each?
   - MutIterator: Performance-critical, single-pass
   - Iterator: When need backtracking, multiple passes
   - **Proposal**: Default to Iterator, use MutIterator for hot paths

5. **Convenience methods**: Should collections have ANY iteration methods?
   - Could keep ONE: `for_each` for simple side-effect iteration
   - Pro: Covers 80% case without Iterator conversion
   - Con: Breaks consistency

## Success Criteria

After migration, you should be able to:

1. ✅ Write iteration code once, use with any collection
2. ✅ Swap collection types without touching iteration logic  
3. ✅ Chain operations naturally with `|>`
4. ✅ Get good type inference (t-first helps)
5. ✅ Use rich iterator combinators (zip, enumerate, take, etc.)
6. ✅ Collect into any collection type at the end

## Implementation Checklist

- [ ] Design and implement full Iterator API
- [ ] Design and implement full MutIterator API
- [ ] Add `into_iter` to all Kernel.Collections modules
- [ ] Add `to_mut_iter` to all Kernel.Collections modules
- [ ] Add `into_iter` to all Std.Collections modules
- [ ] Update documentation with Iterator-centric examples
- [ ] Create migration guide
- [ ] Update all call sites (large task!)
- [ ] Deprecate/remove old collection iteration methods
- [ ] Add comprehensive tests

## Example: Full Iterator Module (proposed)

```ocaml
module Iterator : sig
  type 'a t
  
  (* Core *)
  val next : 'a t -> 'a option * 'a t
  val size : 'a t -> int
  
  (* Transformation *)
  val map : 'a t -> fn:('a -> 'b) -> 'b t
  val filter : 'a t -> fn:('a -> bool) -> 'a t
  val filter_map : 'a t -> fn:('a -> 'b option) -> 'b t
  val flat_map : 'a t -> fn:('a -> 'b t) -> 'b t
  
  (* Reduction *)
  val fold : 'a t -> init:'acc -> fn:('acc -> 'a -> 'acc) -> 'acc
  val reduce : 'a t -> fn:('a -> 'a -> 'a) -> 'a option
  val sum : int t -> int
  val product : int t -> int
  val count : 'a t -> int
  val min : 'a t -> compare:('a -> 'a -> int) -> 'a option
  val max : 'a t -> compare:('a -> 'a -> int) -> 'a option
  
  (* Search *)
  val find : 'a t -> fn:('a -> bool) -> 'a option
  val position : 'a t -> fn:('a -> bool) -> int option
  val any : 'a t -> fn:('a -> bool) -> bool
  val all : 'a t -> fn:('a -> bool) -> bool
  
  (* Combinators *)
  val take : 'a t -> int -> 'a t
  val drop : 'a t -> int -> 'a t
  val take_while : 'a t -> fn:('a -> bool) -> 'a t
  val drop_while : 'a t -> fn:('a -> bool) -> 'a t
  val skip : 'a t -> int -> 'a t
  val step_by : 'a t -> int -> 'a t
  val chain : 'a t -> 'a t -> 'a t
  val zip : 'a t -> 'b t -> ('a * 'b) t
  val unzip : ('a * 'b) t -> 'a t * 'b t
  val enumerate : 'a t -> (int * 'a) t
  val cycle : 'a t -> 'a t  (* infinite *)
  val repeat : 'a -> 'a t  (* infinite *)
  val range : int -> int -> int t
  
  (* Partition *)
  val partition : 'a t -> fn:('a -> bool) -> 'a t * 'a t
  val group_by : 'a t -> fn:('a -> 'k) -> ('k * 'a list) list
  
  (* Collectors - terminal operations *)
  val collect_list : 'a t -> 'a list
  val collect_vec : 'a t -> 'a Vector.t
  val collect_set : 'a t -> 'a HashSet.t
  val collect_map : ('k * 'v) t -> ('k, 'v) HashMap.t
  val to_list : 'a t -> 'a list  (* alias for collect_list *)
  
  (* Side effects *)
  val for_each : 'a t -> fn:('a -> unit) -> unit
  val inspect : 'a t -> fn:('a -> unit) -> 'a t  (* for debugging *)
end
```


---

## FINALIZED DESIGN DECISIONS

### ✅ Array/Stream Wrapping
**Decision**: Wrap both for consistency
- Provide `into_iter` / `to_mut_iter` 
- Keep underlying stdlib implementation
- Just add thin wrapper for iterator conversion

### ✅ Fold Signature
**Decision**: Element-first `fn:(item -> acc -> acc)`
```ocaml
val fold : 'a t -> init:'acc -> fn:('a -> 'acc -> 'acc) -> 'acc
```
- Matches current OCaml conventions
- Easier transition from existing code
- Still allows nice piping with named args

### ✅ No Backward Compatibility
**Decision**: Breaking changes, refactor as we go
- No deprecated aliases
- Clean break for better API
- Refactor codebase incrementally
- Document migration patterns clearly

### ✅ Naming: Use `~fn` everywhere
**Decision**: Short, simple, consistent
```ocaml
val map : 'a t -> fn:('a -> 'b) -> 'b t
val filter : 'a t -> fn:('a -> bool) -> 'a t
val fold : 'a t -> init:'acc -> fn:('a -> 'acc -> 'acc) -> 'acc
```
NOT `~predicate`, `~mapper`, `~folder` - just `~fn`

## Implementation Plan (Approved)

### Phase 1: Build Iterator Foundation (Week 1-2)
**Goal**: Rich Iterator API that collections can target

1. **Expand Iterator module** with full API:
   ```ocaml
   (* Transform *)
   val map : 'a t -> fn:('a -> 'b) -> 'b t
   val filter : 'a t -> fn:('a -> bool) -> 'a t
   val filter_map : 'a t -> fn:('a -> 'b option) -> 'b t
   val flat_map : 'a t -> fn:('a -> 'b t) -> 'b t
   
   (* Reduce *)
   val fold : 'a t -> init:'acc -> fn:('a -> 'acc -> 'acc) -> 'acc
   val reduce : 'a t -> fn:('a -> 'a -> 'a) -> 'a option
   val sum : int t -> int
   val count : 'a t -> int
   
   (* Search *)
   val find : 'a t -> fn:('a -> bool) -> 'a option
   val any : 'a t -> fn:('a -> bool) -> bool
   val all : 'a t -> fn:('a -> bool) -> bool
   
   (* Combinate *)
   val take : 'a t -> int -> 'a t
   val drop : 'a t -> int -> 'a t
   val zip : 'a t -> 'b t -> ('a * 'b) t
   val enumerate : 'a t -> (int * 'a) t
   val chain : 'a t -> 'a t -> 'a t
   
   (* Collect *)
   val collect_list : 'a t -> 'a list
   val collect_vec : 'a t -> 'a Vector.t
   val collect_set : 'a t -> 'a HashSet.t
   val collect_map : ('k * 'v) t -> ('k, 'v) HashMap.t
   
   (* Effects *)
   val for_each : 'a t -> fn:('a -> unit) -> unit
   ```

2. **Mirror in MutIterator** (mutable versions)

3. **Add comprehensive tests** for all combinators

### Phase 2: Collection Iterator Conversion (Week 3)
**Goal**: Every collection can convert to Iterator

Add to ALL collections:
```ocaml
val into_iter : 'a t -> 'a Iterator.t
val to_mut_iter : 'a t -> 'a MutIterator.t
```

Collections to update:
- [x] HashSet (already has `to_mut_iter`)
- [ ] Vector
- [ ] HashMap  
- [ ] List
- [ ] Array (wrap)
- [ ] Stream (wrap)
- [ ] Queue
- [ ] Deque
- [ ] Heap

### Phase 3: Codebase Migration (Week 4-6)
**Goal**: Update all call sites to use Iterator API

Pattern:
```ocaml
(* Before *)
let result = Vector.map (fun x -> x * 2) vec

(* After *)
let result = 
  vec 
  |> Vector.into_iter 
  |> Iterator.map ~fn:(fun x -> x * 2)
  |> Iterator.collect_vec
```

Strategy:
1. Start with low-usage collections (Queue, Deque, Heap)
2. Move to medium usage (HashMap, HashSet)
3. Tackle high usage (Vector, List) last
4. Update one package at a time
5. Keep build passing after each package

### Phase 4: Clean Up Collections (Week 7)
**Goal**: Remove now-redundant iteration methods

Remove from collections:
- `iter` (use `into_iter |> Iterator.for_each`)
- `map` (use `into_iter |> Iterator.map |> collect_*`)
- `filter` (use `into_iter |> Iterator.filter |> collect_*`)
- `fold` (use `into_iter |> Iterator.fold`)
- `find` (use `into_iter |> Iterator.find`)

Keep in collections:
- Construction: `create`, `of_list`
- Conversion: `to_list`, `into_iter`, `to_mut_iter`
- Queries: `length`, `is_empty`, `contains`
- Mutation: `push`, `pop`, `insert`, `remove`, `get`, `set`
- Set ops: `union`, `intersection`, `difference` (HashSet)

### Phase 5: Documentation (Week 8)
**Goal**: Clear examples and migration guide

1. Update all module docs with Iterator examples
2. Create migration guide with before/after
3. Add cookbook examples:
   - Chaining operations
   - Collecting to different types
   - Short-circuiting searches
   - Combining multiple collections

## Quick Reference Card

### Collection Operations (Keep These)
```ocaml
(* Construction *)
Vector.create ()
Vector.of_list [1; 2; 3]

(* Mutation *)
Vector.push vec 42
Vector.pop vec
Vector.get vec 0
Vector.set vec 0 value

(* Conversion *)
Vector.to_list vec
Vector.into_iter vec

(* Queries *)
Vector.length vec
Vector.is_empty vec
```

### Iterator Operations (Use These)
```ocaml
(* Transform *)
vec |> Vector.into_iter
    |> Iterator.map ~fn:(fun x -> x * 2)
    |> Iterator.filter ~fn:(fun x -> x > 0)
    |> Iterator.collect_vec

(* Reduce *)
vec |> Vector.into_iter
    |> Iterator.fold ~init:0 ~fn:(fun x acc -> acc + x)

(* Search *)
vec |> Vector.into_iter
    |> Iterator.find ~fn:(fun x -> x > 10)

(* Combine *)
vec1 |> Vector.into_iter
     |> Iterator.zip (Vector.into_iter vec2)
     |> Iterator.map ~fn:(fun (a, b) -> a + b)
     |> Iterator.collect_list
```

## Benefits Summary

✅ **Write once, run anywhere**: Same iteration code works with any collection
✅ **Better composition**: Chain operations naturally with `|>`
✅ **Type inference**: Collection type determined first, better errors
✅ **Flexibility**: Change collection type without rewriting iteration logic
✅ **Consistency**: One API for all iteration, not collection-specific
✅ **Rich combinators**: zip, enumerate, take, drop, chain, etc.
✅ **Efficient**: Short-circuiting with find, any, take
✅ **Self-documenting**: `~fn`, `~init`, `~key`, `~value` make intent clear

## Timeline

- **Week 1-2**: Build Iterator/MutIterator rich API
- **Week 3**: Add `into_iter` to all collections
- **Week 4-6**: Migrate codebase (biggest effort)
- **Week 7**: Remove redundant collection methods
- **Week 8**: Documentation and polish

**Total**: ~8 weeks for complete migration

---

**Status**: Design Finalized ✅
**Next Step**: Begin Phase 1 - Implement Iterator API

