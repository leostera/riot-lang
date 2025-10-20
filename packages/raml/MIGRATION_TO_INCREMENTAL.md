# Migration Plan: Make Type Checker Only Incremental

## Current State

```
checker.ml  (monolithic, non-incremental)
    ↓
  typecheck : string -> result
    ↓
  Used by: main.ml, tests, build tools
```

## Target State

```
incrementalChecker.ml  (the ONLY type checker)
    ↓
  check : t -> source:string -> result
    ↓
  Used by: EVERYTHING
```

## Why This Is Better

1. **Single implementation** - No "incremental vs non-incremental" split
2. **Always fast** - Every tool gets incrementality for free
3. **LSP-ready** - Built for IDE use from day 1
4. **Simpler API** - One way to type-check, not two
5. **Better tested** - Only one code path to test

## Migration Steps

### Phase 1: Extract Core Logic (Today)

Move type checking logic from `checker.ml` to internal functions:

```ocaml
(* checker.ml - OLD *)
let typecheck source =
  let tokens = Syn.Lexer.tokenize source in
  let parse_result = Syn.Parser.parse ~source tokens in
  (* ... type checking ... *)

(* incrementalChecker.ml - NEW *)
module Internal = struct
  (* Pure functions, no state *)
  let check_structure_item env item =
    (* ... actual checking logic ... *)
  
  let infer_type env expr =
    (* ... type inference ... *)
end

(* Public API with caching *)
type t = {
  cache : cache_state;
  options : options;
}

let check checker ~source () =
  (* Parse *)
  let structure = parse source in
  
  (* Check each item incrementally *)
  List.map (check_item_incremental checker) structure
```

### Phase 2: Implement Caching (Today)

Add the incremental layer:

```ocaml
type cache_entry = {
  source_hash : int;
  typed_item : TypedTree.structure_item;
  env_before : Environment.t;
  env_after : Environment.t;
  dependencies : string list;
}

type cache_state = {
  entries : (string, cache_entry) Hashtbl.t;
  parse_cache : (string, UntypedTree.structure) Hashtbl.t;
}

let check_item_incremental checker item =
  let name = get_item_name item in
  let hash = hash_source_span item.span in
  
  match Hashtbl.find_opt checker.cache.entries name with
  | Some entry when entry.source_hash = hash ->
      (* Cache hit! *)
      Stats.incr checker.stats.items_cached;
      entry.typed_item
  
  | _ ->
      (* Cache miss - type check *)
      Stats.incr checker.stats.items_checked;
      let typed = Internal.check_structure_item env item in
      Hashtbl.replace checker.cache.entries name {
        source_hash = hash;
        typed_item = typed;
        env_before = env;
        env_after = new_env;
        dependencies = collect_deps typed;
      };
      typed
```

### Phase 3: Update All Callers (Today)

Replace all uses of `Checker.typecheck`:

```ocaml
(* OLD - main.ml *)
let handle_check file =
  match Checker.typecheck source with
  | Ok result -> ...
  | Error msg -> ...

(* NEW - main.ml *)
let handle_check file =
  let checker = IncrementalChecker.create () in
  match IncrementalChecker.check checker ~source () with
  | Ok { typed_tree; diagnostics; _ } -> ...
  | Error msg -> ...
```

### Phase 4: Add Query APIs (This Week)

Implement IDE support functions:

```ocaml
let get_type_at checker span =
  (* Walk typed tree to find expression at span *)
  match find_expr_at_span checker.typed_tree span with
  | Some expr -> Some expr.exp_type
  | None -> None

let get_completions checker span =
  (* Find environment at span *)
  match find_env_at_span checker span with
  | Some env -> Environment.all_names env
  | None -> []
```

### Phase 5: Multi-File Support (Next Week)

Extend to handle multiple files:

```ocaml
type multi_file_checker = {
  files : (file_id, t) Hashtbl.t;
  dependencies : (file_id, file_id list) Hashtbl.t;
  exports : (file_id, Environment.t) Hashtbl.t;
}

let check_file mfc ~file_id ~source () =
  (* 1. Get dependencies *)
  let deps = get_dependencies file_id mfc in
  
  (* 2. Load their exports *)
  let env = merge_exports deps mfc in
  
  (* 3. Check this file *)
  let checker = get_or_create_checker mfc file_id in
  check checker ~source ~initial_env:env ()
```

## Compatibility

### For Existing Code

Provide a simple wrapper:

```ocaml
(* checker.ml - compatibility shim *)
let typecheck source =
  (* Create one-shot checker *)
  let checker = IncrementalChecker.create () in
  match IncrementalChecker.check checker ~source () with
  | Ok { typed_tree; diagnostics; _ } ->
      (* Convert to old format *)
      Ok { tree = first_expr typed_tree; diagnostics = [] }
  | Error e -> Error e
```

### Migration Timeline

- **Day 1**: Extract core logic, add caching
- **Day 2**: Update main.ml and tests
- **Day 3**: Remove old checker.ml, everyone uses incremental
- **Week 2**: Add query APIs
- **Week 3**: Multi-file support

## Benefits Summary

| Feature | Old Checker | Incremental Checker |
|---------|-------------|---------------------|
| First check | 100ms | 100ms (same) |
| Re-check (no changes) | 100ms | 5ms (20x faster!) |
| Re-check (1 item changed) | 100ms | 10ms (10x faster!) |
| LSP "type at cursor" | Re-check file | Instant (cached) |
| LSP completions | Re-check file | Instant (cached) |
| Multi-file project | N * 100ms | Smart invalidation |

## Testing Strategy

```ocaml
(* Test: Incrementality is transparent *)
let test_incremental_equals_full () =
  let source = "let x = 42\nlet y = x + 1" in
  
  (* Old way *)
  let old_result = Checker.typecheck source in
  
  (* New way *)
  let checker = IncrementalChecker.create () in
  let new_result = IncrementalChecker.check checker ~source () in
  
  assert (results_equivalent old_result new_result)

(* Test: Caching works *)
let test_cache_reuse () =
  let checker = IncrementalChecker.create () in
  
  (* First check *)
  let result1 = IncrementalChecker.check checker ~source:"let x = 42" () in
  assert (result1.cache_stats.items_cached = 0);
  assert (result1.cache_stats.items_checked = 1);
  
  (* Second check (no change) *)
  let result2 = IncrementalChecker.check checker ~source:"let x = 42" () in
  assert (result2.cache_stats.items_cached = 1);
  assert (result2.cache_stats.items_checked = 0);
  
  (* Third check (change) *)
  let result3 = IncrementalChecker.check checker ~source:"let x = 43" () in
  assert (result3.cache_stats.items_cached = 0);
  assert (result3.cache_stats.items_checked = 1)
```

## Decision: Do It!

**Arguments for:**
- ✅ Simpler architecture (one checker, not two)
- ✅ Always fast (no performance cliff)
- ✅ LSP/IDE ready from day 1
- ✅ Future-proof (scales to large projects)
- ✅ Not much more code (~500 LOC for caching layer)

**Arguments against:**
- ⚠️ Need to migrate existing code (minimal - ~10 call sites)
- ⚠️ Slightly more complex (but worth it)

**Verdict: YES - Make it incremental-only!**

The benefits far outweigh the costs. Let's build it right the first time! 🚀
