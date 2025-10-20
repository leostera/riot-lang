# Incremental Type Checking Design

## The Problem

**Non-incremental (current):**
```ocaml
(* User edits one line in 10,000 line file *)
let x = 42  →  let x = 43

(* Type checker re-checks ENTIRE file *)
typecheck entire_file  (* ~100ms for large files! *)
```

**This is too slow for IDE/LSP!**

## The Goal

**Incremental:**
```ocaml
(* User edits one line *)
let x = 42  →  let x = 43

(* Type checker only re-checks affected scope *)
typecheck_incremental ~changed:(line 5, col 10-12)  (* < 5ms! *)
```

## Key Insights from Research

### 1. OCaml's Compiler (Non-Incremental)

OCaml's compiler is **NOT incremental**:
- Re-parses entire file on every change
- Re-type-checks entire file
- Fast enough for batch compilation (~100k LOC/sec)
- NOT fast enough for IDE (keystroke latency)

**Why OCaml doesn't do it:**
- Compiler optimized for correctness, not IDE speed
- Type checking is already very fast
- Incremental adds complexity

### 2. OCaml LSP (Incremental via Caching)

`ocaml-lsp-server` uses:
- **File-level caching**: Cache typed trees per file
- **Dependency tracking**: Re-check files that depend on changed file
- **Merlin's approach**: Fast re-parsing + localized re-typing

**Merlin's strategy:**
1. Parse entire file (fast: ~10ms)
2. Find smallest scope containing edit
3. Re-type-check only that scope
4. If scope types change, propagate upwards

### 3. Rust Analyzer (True Incremental)

`rust-analyzer` uses:
- **Salsa framework**: Fine-grained incremental computation
- **Query-based**: Each type query is cached
- **Demand-driven**: Only compute what's needed

```rust
// Salsa-style queries
fn type_of_expr(db: &Database, expr: ExprId) -> Type {
  // Automatically cached and invalidated
}

fn type_check_function(db: &Database, func: FuncId) -> Result {
  // Only re-runs if dependencies change
}
```

## Design Options for RAML

### Option 1: Scope-Based Incremental (Simplest)

**Approach:** Re-type-check only the changed function/let binding

```ocaml
type incremental_state = {
  (* Cache of typed definitions *)
  typed_bindings : (string, TypedTree.expression) Hashtbl.t;
  
  (* Environment at each top-level binding *)
  environments : (string, Environment.t) Hashtbl.t;
  
  (* Dependencies: what depends on what *)
  dependencies : (string, string list) Hashtbl.t;
}

let typecheck_incremental state ~source ~changes =
  (* 1. Parse entire file (fast) *)
  let structure = parse source in
  
  (* 2. Find changed bindings *)
  let changed_bindings = 
    find_bindings_containing changes structure 
  in
  
  (* 3. Invalidate cache for changed + dependencies *)
  let to_recheck = 
    changed_bindings @ 
    (flat_map (fun b -> get_dependencies b state) changed_bindings)
  in
  
  (* 4. Re-type-check only invalidated bindings *)
  List.iter (fun binding ->
    let env = get_environment_before binding state in
    let typed = check_binding env binding in
    cache_result state binding typed
  ) to_recheck
```

**Pros:**
- Simple to implement (~500 LOC)
- Good enough for most cases
- Works with current architecture

**Cons:**
- Still re-parses entire file
- Re-checks entire function even if change is localized
- No cross-file incrementality

### Option 2: Query-Based (Medium Complexity)

**Approach:** Make type checking query-driven with caching

```ocaml
module TypeQuery = struct
  type query =
    | TypeOfExpr of expr_id
    | TypeOfPattern of pattern_id
    | CheckFunction of func_id
    | CheckModule of module_id
  
  type cache = (query, result) Hashtbl.t
  
  let rec run cache query =
    match Hashtbl.find_opt cache query with
    | Some result -> result  (* Cache hit! *)
    | None ->
        let result = compute cache query in
        Hashtbl.add cache query result;
        result
  
  and compute cache = function
    | TypeOfExpr expr_id ->
        (* Recursively query sub-expressions *)
        let sub_types = 
          List.map (run cache % TypeOfExpr) (sub_exprs expr_id)
        in
        infer_type expr_id sub_types
    
    | CheckFunction func_id ->
        (* Query body type *)
        let body_type = run cache (TypeOfExpr (func_body func_id)) in
        check_function_type func_id body_type
end

(* On edit: invalidate affected queries *)
let invalidate_changed cache ~changes =
  (* Find queries that depend on changed spans *)
  let affected = find_affected_queries cache changes in
  List.iter (Hashtbl.remove cache) affected
```

**Pros:**
- Fine-grained caching
- Only recomputes affected queries
- Scales to large files

**Cons:**
- Requires query infrastructure
- More complex invalidation logic
- Need unique IDs for all AST nodes

### Option 3: Salsa-Style Framework (Complex)

**Approach:** Full incremental computation framework

```ocaml
module Salsa = struct
  type 'a query = {
    compute : database -> 'a;
    dependencies : database -> query list;
    mutable cached_value : 'a option;
    mutable revision : int;
  }
  
  type database = {
    revision : int ref;
    queries : (string, query) Hashtbl.t;
  }
  
  let run db query =
    if query.revision = !(db.revision) then
      Option.get query.cached_value  (* Cache hit *)
    else (
      (* Recompute *)
      let value = query.compute db in
      query.cached_value <- Some value;
      query.revision <- !(db.revision);
      value
    )
  
  let invalidate db spans =
    incr db.revision;  (* New revision invalidates everything *)
    (* Smart: track which queries depend on which spans *)
end
```

**Pros:**
- Professional-grade incrementality
- Scales to massive codebases
- Used by rust-analyzer successfully

**Cons:**
- Significant engineering effort (~2000 LOC)
- Requires rethinking entire architecture
- Overkill for current needs

## Recommendation: Start with Option 1

**Why:**
1. **Simple** - Works with current architecture
2. **Effective** - Handles 90% of IDE edits well
3. **Fast iteration** - Can upgrade to Option 2 later

**Implementation Plan:**

### Phase 1: Add Caching (This Week)

```ocaml
(* In checker.ml *)
type cache = {
  (* Cache typed top-level bindings *)
  bindings : (string * int, TypedTree.structure_item) Hashtbl.t;
    (* key = (name, source_hash) *)
  
  (* Environment snapshots *)
  envs : (string, Environment.t) Hashtbl.t;
}

let typecheck_with_cache cache source =
  let structure = parse source in
  
  (* Try to reuse cached bindings *)
  List.map (fun item ->
    let hash = hash_item item in
    match Hashtbl.find_opt cache.bindings (item.name, hash) with
    | Some cached -> cached  (* Reuse! *)
    | None ->
        let typed = check_item item in
        Hashtbl.add cache.bindings (item.name, hash) typed;
        typed
  ) structure
```

### Phase 2: Dependency Tracking (Next Week)

```ocaml
(* Track dependencies during type checking *)
type dependencies = {
  (* binding_name -> list of names it uses *)
  uses : (string, string list) Hashtbl.t;
  
  (* binding_name -> list of names that use it *)
  used_by : (string, string list) Hashtbl.t;
}

let collect_dependencies item =
  (* Walk AST and collect referenced names *)
  let used = ref [] in
  traverse_expr item.expr (function
    | ExprIdent name -> used := name :: !used
    | _ -> ()
  );
  !used

let invalidate_dependencies deps changed_names =
  (* Transitively find all affected bindings *)
  let rec collect acc = function
    | [] -> acc
    | name :: rest ->
        let users = Hashtbl.find_default deps.used_by name [] in
        collect (users @ acc) (users @ rest)
  in
  collect [] changed_names
```

### Phase 3: Span-Based Invalidation (Future)

```ocaml
(* More precise: only invalidate if actual type changed *)
type edit = {
  span : Syn.Ceibo.Span.t;
  old_text : string;
  new_text : string;
}

let invalidate_precise cache edit =
  (* 1. Find which binding contains edit span *)
  let affected_binding = find_containing_binding edit.span in
  
  (* 2. Quick check: did type actually change? *)
  let old_type = get_cached_type affected_binding cache in
  let new_type = recheck_binding affected_binding in
  
  (* 3. Only invalidate dependents if type changed *)
  if not (Types.equal old_type new_type) then
    invalidate_dependencies affected_binding
```

## Performance Targets

**Without incrementality:**
- Small files (< 100 LOC): 10ms ✅ (acceptable)
- Medium files (< 1000 LOC): 100ms ⚠️ (noticeable)
- Large files (> 5000 LOC): 500ms+ ❌ (unusable for IDE)

**With scope-based incrementality:**
- Any file, local edit: < 50ms ✅
- Any file, type signature change: 100-200ms ✅
- Any file, complete recheck: same as before

**With query-based incrementality:**
- Any file, local edit: < 20ms ✅
- Any file, type signature change: < 50ms ✅
- Large files with locality: < 10ms ✅

## LSP Integration

```ocaml
(* LSP server state *)
type lsp_state = {
  (* Per-file caches *)
  file_caches : (string, cache) Hashtbl.t;
  
  (* Global type environment *)
  global_env : Environment.t;
  
  (* Dependency graph across files *)
  file_deps : (string, string list) Hashtbl.t;
}

let handle_text_change lsp_state ~uri ~changes =
  (* 1. Get or create cache for file *)
  let cache = 
    Hashtbl.find_default lsp_state.file_caches uri (create_cache ())
  in
  
  (* 2. Incremental type check *)
  let result = typecheck_with_cache cache ~changes in
  
  (* 3. Return diagnostics *)
  let diagnostics = collect_diagnostics result in
  send_diagnostics_to_client ~uri diagnostics
```

## Testing Strategy

```ocaml
(* Test that incrementality gives same results *)
let test_incremental_correctness () =
  let source = {|
    let x = 42
    let y = x + 1
    let z = y * 2
  |} in
  
  (* Full check *)
  let full_result = typecheck source in
  
  (* Incremental check (edit x) *)
  let cache = create_cache () in
  let _ = typecheck_with_cache cache source in
  let source' = {|
    let x = 43
    let y = x + 1
    let z = y * 2
  |} in
  let incr_result = typecheck_with_cache cache source' in
  
  (* Should be equivalent *)
  assert (results_equal full_result incr_result)

(* Benchmark incrementality speedup *)
let bench_incremental () =
  let large_file = generate_program ~functions:1000 in
  
  (* Time: full recheck *)
  let t1 = time (fun () -> typecheck large_file) in
  
  (* Time: incremental after small edit *)
  let cache = create_cache () in
  let _ = typecheck_with_cache cache large_file in
  let edited = edit_line large_file 500 in
  let t2 = time (fun () -> typecheck_with_cache cache edited) in
  
  Log.info "Full: %dms, Incremental: %dms, Speedup: %.1fx"
    t1 t2 (float t1 /. float t2)
  (* Expected: 500ms, 20ms, 25x speedup *)
```

## API Design

```ocaml
(* Public API for incremental type checking *)
module IncrementalChecker : sig
  type t
  
  val create : unit -> t
  
  val typecheck :
    t ->
    source:string ->
    ?previous_version:string ->
    ?changes:edit list ->
    (typing_result * Diagnostic.collection, string) result
  
  val clear_cache : t -> unit
  
  val cache_stats : t -> {
    hits : int;
    misses : int;
    invalidations : int;
  }
end
```

## Next Steps

1. ✅ Complete diagnostic system (this PR)
2. ⏳ Add binding-level caching (Phase 1)
3. ⏳ Add dependency tracking (Phase 2)
4. ⏳ Benchmark and optimize
5. ⏳ Integrate with LSP server

The foundation is there - we just need to add caching! 🚀
