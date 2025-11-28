# Collections Signature Standardization Plan

## Goals
1. Make collections ergonomic and type-inference friendly
2. Adopt t-first + named arguments pattern
3. Maintain consistency across all collection types
4. Improve IDE autocomplete and error messages

## Design Principles

### 1. **Container-First Pattern**
The collection/container should always be the first positional argument:
- Enables better type inference (OCaml infers left-to-right)
- Natural piping with `|>` operator
- Better error messages when types don't match

### 2. **Named Arguments for Functions**
All function/predicate arguments use labeled parameters:
- `~fn` for mapper functions
- `~predicate` for filters
- `~init` for initial accumulator values
- `~key` for map operations
- `~value` for map operations

### 3. **Consistent Naming**
- `map ~fn` - transform elements
- `filter ~predicate` - select elements
- `fold ~init ~fn` - reduce collection
- `iter ~fn` - side-effect iteration
- `find ~predicate` - locate element
- `exists ~predicate` - test any element
- `for_all ~predicate` - test all elements

## Current State Analysis

### Kernel.Collections Modules
1. **Array** - Re-exports stdlib (function-first, no named args)
2. **List** - Custom (mixed signatures)
3. **Stream** - (need to check)
4. **Vector** - Function-first currently
5. **HashMap** - Function-first currently
6. **HashSet** - (need to check)
7. **Queue** - Function-first currently

### Std.Collections Modules
1. **Deque** - Function-first currently
2. **Heap** - (need to check)

### Current Signatures (Examples)

**Vector (current):**
```ocaml
val iter : ('a -> unit) -> 'a t -> unit
val fold : ('a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
```

**HashMap (current):**
```ocaml
val iter : ('k -> 'v -> unit) -> ('k, 'v) t -> unit
val fold : ('k -> 'v -> 'acc -> 'acc) -> ('k, 'v) t -> 'acc -> 'acc
```

## Proposed Signatures

### Core Functions (all collections)

```ocaml
(* Constructors *)
val create : unit -> 'a t
val of_list : 'a list -> 'a t
val to_list : 'a t -> 'a list

(* Queries *)
val length : 'a t -> int
val is_empty : 'a t -> bool

(* Iteration *)
val iter : 'a t -> fn:('a -> unit) -> unit
val fold : 'a t -> init:'acc -> fn:('acc -> 'a -> 'acc) -> 'acc

(* Transformation *)
val map : 'a t -> fn:('a -> 'b) -> 'b t
val filter : 'a t -> predicate:('a -> bool) -> 'a t
val filter_map : 'a t -> fn:('a -> 'b option) -> 'b t

(* Search *)
val find : 'a t -> predicate:('a -> bool) -> 'a option
val exists : 'a t -> predicate:('a -> bool) -> bool
val for_all : 'a t -> predicate:('a -> bool) -> bool
```

### List-specific

```ocaml
val concat : 'a list list -> 'a list
val flatten : 'a list list -> 'a list
val rev : 'a list -> 'a list
val append : 'a list -> 'a list -> 'a list
val take : 'a list -> int -> 'a list
val drop : 'a list -> int -> 'a list
val zip : 'a list -> 'b list -> ('a * 'b) list
val unzip : ('a * 'b) list -> 'a list * 'b list
```

### Vector-specific

```ocaml
val push : 'a t -> 'a -> unit
val pop : 'a t -> 'a option
val get : 'a t -> int -> 'a option
val set : 'a t -> int -> 'a -> unit
val unsafe_get : 'a t -> int -> 'a
val unsafe_set : 'a t -> int -> 'a -> unit
```

### HashMap-specific

```ocaml
val insert : ('k, 'v) t -> key:'k -> value:'v -> 'v option
val get : ('k, 'v) t -> key:'k -> 'v option
val remove : ('k, 'v) t -> key:'k -> 'v option
val mem : ('k, 'v) t -> key:'k -> bool
val update : ('k, 'v) t -> key:'k -> fn:('v option -> 'v option) -> unit
val iter : ('k, 'v) t -> fn:(key:'k -> value:'v -> unit) -> unit
val fold : ('k, 'v) t -> init:'acc -> fn:('acc -> key:'k -> value:'v -> 'acc) -> 'acc
val map_values : ('k, 'v) t -> fn:('v -> 'w) -> ('k, 'w) t
```

### HashSet-specific

```ocaml
val add : 'a t -> 'a -> unit
val remove : 'a t -> 'a -> bool
val mem : 'a t -> 'a -> bool
val union : 'a t -> 'a t -> 'a t
val inter : 'a t -> 'a t -> 'a t
val diff : 'a t -> 'a t -> 'a t
```

### Queue-specific

```ocaml
val enqueue : 'a t -> 'a -> unit
val dequeue : 'a t -> 'a option
val peek : 'a t -> 'a option
```

### Deque-specific

```ocaml
val push_front : 'a t -> 'a -> unit
val push_back : 'a t -> 'a -> unit
val pop_front : 'a t -> 'a option
val pop_back : 'a t -> 'a option
val peek_front : 'a t -> 'a option
val peek_back : 'a t -> 'a option
```

## Migration Strategy

### Phase 1: Audit
- [ ] Create complete inventory of all function signatures across all modules
- [ ] Document current usage patterns in the codebase
- [ ] Identify breaking changes

### Phase 2: Design Review
- [ ] Review proposed signatures with team
- [ ] Finalize naming conventions
- [ ] Document rationale for each decision

### Phase 3: Implementation
1. Start with least-used modules (HashSet, Stream)
2. Update interfaces (.mli files)
3. Update implementations (.ml files)
4. Update tests
5. Move to more frequently used modules (Vector, List)

### Phase 4: Codebase Migration
- [ ] Update all usage sites to use new signatures
- [ ] This will be a large task - may need automated refactoring

### Phase 5: Documentation
- [ ] Update all docstrings with examples
- [ ] Create migration guide
- [ ] Update any tutorials/examples

## Open Questions

1. **Array module**: Should we wrap stdlib Array or keep as-is?
   - Pro wrapping: Consistency with other collections
   - Con wrapping: Users expect stdlib semantics

2. **Backward compatibility**: Do we provide deprecated aliases?
   - Could ease migration
   - But clutters API

3. **Fold direction**: `fn:('acc -> 'a -> 'acc)` vs `fn:('a -> 'acc -> 'acc)`?
   - Current: `fn:('a -> 'acc -> 'acc)` (element first)
   - Alternative: `fn:('acc -> 'a -> 'acc)` (accumulator first)
   - Accumulator-first is more common in other languages (Rust, etc.)

4. **Optional arguments**: When should we use `?` vs labeled?
   - Example: `?init` vs `init:`
   - Generally: required = labeled, optional = `?`

## Examples After Migration

### Before
```ocaml
let doubled = Vector.map (fun x -> x * 2) vec
let total = Vector.fold (fun acc x -> acc + x) vec 0
let evens = Vector.filter (fun x -> x mod 2 = 0) vec
```

### After
```ocaml
let doubled = Vector.map vec ~fn:(fun x -> x * 2)
let total = Vector.fold vec ~init:0 ~fn:(fun acc x -> acc + x)
let evens = Vector.filter vec ~predicate:(fun x -> x mod 2 = 0)

(* Or with piping *)
let result = 
  vec
  |> Vector.filter ~predicate:(fun x -> x > 0)
  |> Vector.map ~fn:(fun x -> x * 2)
  |> Vector.fold ~init:0 ~fn:(+)
```

## Benefits

1. **Better type inference**:
   ```ocaml
   (* Type of vec is inferred first, helps with polymorphic functions *)
   vec |> Vector.map ~fn:(fun x -> ...)
   ```

2. **Better error messages**:
   ```ocaml
   (* Old: confusing when function type is wrong *)
   Vector.map (fun x -> x + "str") vec
   (* Error: This expression has type string but an expression was expected of type int *)
   
   (* New: error points to the labeled argument *)
   Vector.map vec ~fn:(fun x -> x + "str")
   (* Error in ~fn argument: ... *)
   ```

3. **Self-documenting code**:
   ```ocaml
   HashMap.insert map ~key:"foo" ~value:42
   (* vs *)
   HashMap.insert map "foo" 42
   ```

4. **Flexible argument order** (for optional args):
   ```ocaml
   List.fold list ~fn:(+) ~init:0
   (* or *)
   List.fold list ~init:0 ~fn:(+)
   ```

## Complete Signature Inventory

### Kernel.Collections.Array
- Status: Re-exports `Stdlib.Array` (function-first, no named args)
- Action: Keep as-is or wrap? (TBD)

### Kernel.Collections.List
Current signatures (sampled):
```ocaml
val find_map : ('a -> 'b option) -> 'a list -> 'b option
val filter_map : ('a -> 'b option) -> 'a list -> 'b list
val is_empty : 'a list -> bool
```
- Status: Function-first, no named args
- Action: Update to t-first + named args

### Kernel.Collections.Stream  
- Status: Re-exports `Stdlib.Seq` (function-first, no named args)
- Action: Keep as-is or wrap? (TBD)

### Kernel.Collections.Vector
Current signatures:
```ocaml
val create : unit -> 'a t
val of_list : 'a list -> 'a t
val push : 'a t -> 'a -> unit
val pop : 'a t -> 'a option
val get : 'a t -> int -> 'a option
val is_empty : 'a t -> bool
val iter : ('a -> unit) -> 'a t -> unit
val fold : ('a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
val to_list : 'a t -> 'a list
```
- Status: Function-first, no named args
- Action: Update to t-first + named args

### Kernel.Collections.HashMap
Current signatures:
```ocaml
val create : unit -> ('k, 'v) t
val of_list : ('k * 'v) list -> ('k, 'v) t
val insert : ('k, 'v) t -> 'k -> 'v -> 'v option
val get : ('k, 'v) t -> 'k -> 'v option
val remove : ('k, 'v) t -> 'k -> 'v option
val is_empty : ('k, 'v) t -> bool
val iter : ('k -> 'v -> unit) -> ('k, 'v) t -> unit
val fold : ('k -> 'v -> 'acc -> 'acc) -> ('k, 'v) t -> 'acc -> 'acc
val to_list : ('k, 'v) t -> ('k * 'v) list
```
- Status: Mixed - mutation ops are t-first, but iter/fold are function-first
- Action: Make iter/fold t-first, add named args for keys/values

### Kernel.Collections.HashSet
Current signatures:
```ocaml
val create : unit -> 'a t
val of_list : 'a list -> 'a t
val insert : 'a t -> 'a -> bool
val remove : 'a t -> 'a -> bool
val contains : 'a t -> 'a -> bool
val is_empty : 'a t -> bool
val iter : 'a t -> fn:('a -> unit) -> unit
val fold : 'a t -> init:'acc -> fn:('acc -> 'a -> 'acc) -> 'acc
val to_list : 'a t -> 'a list
```
- Status: ✅ Already t-first + named args for iter/fold!
- Action: This is our model - use as reference

### Kernel.Collections.Queue
Current signatures:
```ocaml
val create : unit -> 'a t
val of_list : 'a list -> 'a t
val is_empty : 'a t -> bool
val iter : ('a -> unit) -> 'a t -> unit
val fold : ('a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
val to_list : 'a t -> 'a list
```
- Status: Function-first, no named args
- Action: Update to t-first + named args

### Std.Collections.Deque
Current signatures:
```ocaml
val create : unit -> 'a t
val of_list : 'a list -> 'a t
val push_front : 'a t -> 'a -> unit
val push_back : 'a t -> 'a -> unit
val pop_front : 'a t -> 'a option
val pop_back : 'a t -> 'a option
val is_empty : 'a t -> bool
val iter : ('a -> unit) -> 'a t -> unit
val fold : ('a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
val to_list : 'a t -> 'a list
```
- Status: Mixed - mutation ops are t-first, but iter/fold are function-first
- Action: Update iter/fold to t-first + named args

### Std.Collections.Heap
Current signatures:
```ocaml
val create : unit -> 'a t
val of_list : 'a list -> 'a t
val push : 'a t -> 'a -> unit
val pop : 'a t -> 'a option
val peek : 'a t -> 'a option
val is_empty : 'a t -> bool
val iter : ('a -> unit) -> 'a t -> unit
val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
```
- Status: Function-first, no named args
- Action: Update to t-first + named args

## Priority for Updates

### Phase 1 (Low Usage / Good Examples)
1. **HashSet** - Already done! ✅
2. **Stream** - Re-export, decide: keep as-is or wrap
3. **Array** - Re-export, decide: keep as-is or wrap

### Phase 2 (Medium Usage)
4. **Queue** - Simple, good next step
5. **Deque** - Similar to Queue
6. **Heap** - Has unique fold signature to consider

### Phase 3 (High Usage - Need careful migration)
7. **Vector** - Heavily used
8. **HashMap** - Heavily used
9. **List** - Most heavily used

## Implementation Checklist (per module)

For each module, we need to:
- [ ] Update .mli signature
- [ ] Update .ml implementation
- [ ] Update all call sites in codebase
- [ ] Add/update tests
- [ ] Update documentation examples

## Breaking Changes Summary

All function-passing operations will break:
- `iter fn collection` → `iter collection ~fn`
- `fold fn collection init` → `fold collection ~init ~fn`  
- `map fn collection` → `map collection ~fn`
- `filter predicate collection` → `filter collection ~predicate`

Mitigation: Could provide deprecated aliases with warnings for one release cycle.

## Next Steps

1. **Get approval on design** - Review this document
2. **Start with HashSet as template** - It's already correct!
3. **Update Queue next** - Simple, low usage
4. **Build tooling** - Script to help migrate call sites
5. **Tackle Vector/HashMap/List** - High usage, needs care

