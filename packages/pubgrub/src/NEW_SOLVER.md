# New Solver Architecture - Lessons Learned

## Current Status (NEW SOLVER)
- **Tests Passing**: 7/8 targeted tests (87.5%)
- **Previously Failing Tests**: 3/4 now pass (75% improvement)
- **Core Algorithm**: compute_pending architecture working well
- **Remaining Issue**: Version selection with conflicting dependency ranges

## Old Solver Status (FOR REFERENCE)
- **Tests Passing**: 117/121 (96.7%)
- **Core Algorithm**: Working well for most cases
- **Main Issue**: Pending list management after backtracking

## Problems with Current Architecture

### 1. **Pending List is Separate from Solution State**

**Problem**: The `pending` list is stored in `state` alongside `solution`, but they can become inconsistent after backtracking.

```ocaml
type state = {
  solution: Partial_solution.t;
  incompatibilities: (package, Incompatibility.t list) HashMap.t;
  pending: (package * version Ranges.t) list;  (* PROBLEM: Can be stale *)
}
```

**Why it fails**:
- When we choose a package from `pending`, we remove it
- When we backtrack and undo that decision, `pending` is still empty
- We can't easily reconstruct what should be pending without:
  - Tracking all the dependency relationships
  - Avoiding infinite loops from retrying failed choices
  - Ensuring learned incompatibilities are respected

**Example** (Test 120 - Double choices):
1. Choose `b@1.0.0` (level 2), remove from pending
2. Dependency: `d@1.0.0` (doesn't exist) → conflict
3. Backtrack to level 1, but pending is still empty
4. Say "solution found" even though `c` was never tried

### 2. **Implicit Dependency Graph**

**Problem**: Dependencies are only tracked implicitly through incompatibilities.

**Why it fails**:
- Hard to know "what packages are waiting to be decided?"
- After backtracking, we lose track of which packages still need decisions
- Can't easily tell if a package is a dependency of a decided package

**What we need**:
- Explicit tracking: "package A at level L has dependencies [B, C, D]"
- Easy query: "what packages are dependencies of decided packages but not yet decided?"

### 3. **Backtracking Only Modifies Solution, Not State**

**Problem**: `Partial_solution.backtrack` only removes assignments, doesn't restore pending.

```ocaml
let backtrack solution target_level =
  (* Removes assignments with level > target_level *)
  (* But doesn't tell us what should be pending now *)
```

**Why it fails**:
- Information loss: we removed package X at level L, but we don't remember:
  - That X was a dependency of Y
  - What range X needed to satisfy
  - That X should be reconsidered

### 4. **Unit Propagation Uses Mutable Buffers**

**Problem**: Complex control flow with refs and mutable state makes it hard to reason about.

```ocaml
let buffer = ref initial_packages in
let current_state = ref state in
(* ... lots of mutation ... *)
```

**Why it's problematic**:
- Hard to understand when/why packages are added to buffer
- Difficult to ensure we don't process the same package twice
- Makes debugging harder

## Lessons Learned

### ✅ What Works Well

1. **Version Selection Logic**: Choosing versions based on incompatibilities works
2. **Incompatibility Tracking**: HashMap of package → incompatibilities is efficient
3. **Conflict Resolution**: The backtracking level calculation is correct
4. **Relation Function**: Checking if incompatibilities are satisfied/contradicted works
5. **Decision Levels**: Tracking levels for decisions and derivations is essential
6. **Subset Checking**: Using `Ranges.subset_of` for positive terms was key insight

### ❌ What Needs Redesign

1. **Pending Management**: Should be computed from state, not stored separately
2. **State Representation**: Need explicit dependency tracking
3. **Backtracking**: Should return "what changed" to help rebuild pending
4. **Immutability**: More functional approach would make control flow clearer

## Proposed New Architecture

### 1. **Computed Pending List**

Instead of storing pending, **compute it on demand**:

```ocaml
(* Don't store pending in state *)
type state = {
  solution: Partial_solution.t;
  incompatibilities: (package, Incompatibility.t list) HashMap.t;
  dependencies: (package * version, (package * Ranges.t) list) HashMap.t;
  (* Track what each decided package depends on *)
}

(* Compute pending from current state *)
let compute_pending state : (package * Ranges.t) list =
  (* For each decided/constrained package: *)
  (*   For each of its dependencies: *)
  (*     If dependency is undecided, add to pending *)
```

**Benefits**:
- Always consistent with solution state
- No need to manually maintain pending
- Naturally handles backtracking (just recompute)
- Can't become stale

### 2. **Explicit Dependency Tracking**

Track dependencies separately from incompatibilities:

```ocaml
type dependency_info = {
  parent: package * version;
  required_range: Ranges.t;
  decision_level: int;
}

(* When we decide/derive a package, record its dependencies *)
let add_decision_with_deps state pkg ver deps =
  (* Add decision to solution *)
  (* Store: (pkg, ver) → deps *)
  (* For each dep, create incompatibility *)
```

**Benefits**:
- Easy to query: "what are the dependencies of decided packages?"
- Can reconstruct pending after backtracking
- Clear separation: dependencies vs learned incompatibilities

### 3. **Functional Backtracking**

Return information about what changed:

```ocaml
type backtrack_result = {
  new_solution: Partial_solution.t;
  removed_decisions: (package * version * decision_level) list;
  removed_derivations: (package * Ranges.t * decision_level) list;
}

let backtrack solution target_level : backtrack_result =
  (* Return not just new solution, but also what was removed *)
```

**Benefits**:
- Caller knows exactly what changed
- Can use this to update dependency tracking
- Can recompute pending from removed decisions

### 4. **Iterative Unit Propagation (Keep This)**

The current iterative approach with a buffer is actually good! But:
- Use immutable queue instead of mutable ref
- Return new state explicitly at each step
- Make the flow more functional

```ocaml
let rec process_buffer state buffer =
  match buffer with
  | [] -> Ok state
  | pkg :: rest ->
      match process_package state pkg with
      | Ok new_state -> process_buffer new_state rest
      | Error _ as err -> err
```

### 5. **Separate Concerns**

```ocaml
(* Clear separation of responsibilities *)

module DependencyGraph : sig
  type t
  val add_package_with_deps : t -> package -> version -> (package * Ranges.t) list -> t
  val get_pending : t -> Partial_solution.t -> (package * Ranges.t) list
  val backtrack : t -> decision_level -> t
end

module VersionSelection : sig
  val choose_version : 
    provider ->
    package ->
    Ranges.t ->
    Incompatibility.t list ->
    version option
end

module ConflictResolution : sig
  val resolve :
    state ->
    Incompatibility.t ->
    (state, failure) result
end
```

## Implementation Strategy

### Phase 1: Proof of Concept
1. Create `new_solver.ml` alongside existing `solver.ml`
2. Implement `compute_pending` function
3. Test with simple cases (first 10 tests)

### Phase 2: Dependency Tracking
1. Add `DependencyGraph` module
2. Track dependencies explicitly
3. Implement `get_pending` based on dependencies

### Phase 3: Backtracking
1. Modify backtracking to return change information
2. Use change info to update dependency graph
3. Recompute pending after backtracking

### Phase 4: Integration
1. Run full test suite
2. Compare with old solver
3. Switch to new solver if better

### Phase 5: Cleanup
1. Remove old solver if new one passes all tests
2. Simplify architecture
3. Document final design

## Key Insights for New Solver

### 1. **Pending is a View, Not State**

Think of pending as a **computed view** over the current solution state:
- "Which packages are mentioned in dependencies but not yet decided?"
- Always derivable from: decided packages + their dependencies + solution

### 2. **Backtracking is a Transformation**

Backtracking should be thought of as:
- Input: Current state + target level
- Output: New state + "what changed"
- The "what changed" info is crucial for incremental updates

### 3. **Dependencies ≠ Incompatibilities**

Separate these concerns:
- **Dependencies**: "A@1.0 needs B [1.0, 2.0)" - structural relationship
- **Incompatibilities**: "NOT A@1.0 OR B [1.0, 2.0)" - constraint to avoid
- Dependencies are input, incompatibilities are derived

### 4. **Level-Indexed Data Structures**

Consider indexing by decision level:
```ocaml
type state = {
  decisions_by_level: (decision_level, decision list) HashMap.t;
  (* Makes backtracking O(1) - just drop levels > target *)
}
```

### 5. **Immutable Core, Mutable Cache**

The core algorithm should be functional, but we can cache:
```ocaml
type state = {
  solution: Partial_solution.t;  (* Immutable *)
  incompatibilities: ... HashMap.t;  (* Immutable additions only *)
  pending_cache: (package * Ranges.t) list option ref;  (* Mutable cache *)
}

let get_pending state =
  match !(state.pending_cache) with
  | Some p -> p
  | None ->
      let p = compute_pending state in
      state.pending_cache := Some p;
      p
```

## Testing Strategy

### Test in Isolation
1. Test `compute_pending` with various solution states
2. Test `DependencyGraph.backtrack` separately
3. Test version selection without full solve loop

### Incremental Migration
1. Keep old solver working
2. Run both solvers on test suite
3. Debug differences
4. Switch when confident

### Add More Tests
Focus on backtracking scenarios:
```ocaml
Test.case "Backtrack and retry different version" (...)
Test.case "Multiple backtracks in sequence" (...)
Test.case "Backtrack to level 0 should fail" (...)
Test.case "Pending reconstructed after backtrack" (...)
```

## Success Criteria

The new solver should:
1. ✅ Pass all 121 tests (100%)
2. ✅ Be easier to understand and maintain
3. ✅ Have clear separation of concerns
4. ✅ Handle backtracking correctly every time
5. ✅ Never enter infinite loops
6. ✅ Maintain or improve performance

## References

- Current solver: `solver.ml` (117/121 tests passing)
- PubGrub paper: https://github.com/dart-lang/pub/blob/master/doc/solver.md
- Rust implementation: https://github.com/pubgrub-rs/pubgrub
- Key insight: "Pending is derived, not stored"

---

**Next Steps**: Create `new_solver.ml` starting with `compute_pending` function and test it thoroughly before building the full solver.
