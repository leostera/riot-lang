# tusk-executor

Parallel build execution engine for tusk.

## Status: 🚧 Refactoring to use Std.WorkerPool

This package is being refactored to:
1. Use `Std.WorkerPool.SimpleWorkerPool` instead of custom pool implementation
2. Work with the new `tusk-planner` Action Graph architecture
3. Remove dynamic dependency requeuing (now handled by static topological sort)

## Current Architecture (Legacy - To Be Replaced)

```
Build_server
├── spawns N Build_workers via custom Worker_pool
├── maintains Build_queue with dynamic requeuing
├── dispatches tasks dynamically
└── handles RequeueWithDependencies messages

Build_worker
├── receives Task from server
├── creates Sandbox
├── calls Build_planner
├── executes Actions
├── MAY send RequeueWithDependencies if deps missing
└── reports results
```

**Problem:** Dynamic dependency discovery at build time is complex and error-prone.

## New Architecture (With tusk-planner + Std.WorkerPool)

```
Planner.plan_node
├── Produces Action Graph (topologically sorted)
├── All dependencies resolved statically
└── Returns list of Action_nodes in execution order

Execute with SimpleWorkerPool
├── SimpleWorkerPool.run ~concurrency:N
├── Tasks: Action_node list (already in topo order)
├── Worker function: execute_action_node
└── Returns: (int * result) list (ordered)
```

**Benefits:**
- ✅ No dynamic requeuing needed
- ✅ No complex worker pool state management
- ✅ Use battle-tested `Std.WorkerPool`
- ✅ Simpler error handling
- ✅ Cache-aware via Merkle hashing

## Refactoring Plan

### Phase 1: Simplify Execution Model ✅ (Design)

Replace:
```ocaml
(* OLD: Dynamic dependency discovery + requeuing *)
let rec build_loop () =
  match receive () with
  | WorkerReady worker ->
      (match Build_queue.next build_queue with
       | Some node -> Worker_pool.send_task worker task
       | None -> ())
  | RequeueWithDependencies { node; deps } ->
      (* Discovered missing deps at runtime! *)
      Build_queue.requeue_with_deps build_queue node ~deps
  | TaskCompleted { node; artifact } ->
      Build_queue.mark_as_completed build_queue node ~artifact
```

With:
```ocaml
(* NEW: Static dependencies + readiness tracking *)
let build_package ~package ~toolchain ~workspace ~concurrency ~store =
  (* 1. Plan: get action graph with known dependencies *)
  match Planner.plan_node { package; toolchain; workspace; ... } with
  | Planned { action_graph; outputs; _ } ->
      (* 2. Initialize readiness tracking *)
      let dep_count = compute_dependency_counts action_graph in
      let ready = Queue.create () in
      
      (* Find nodes with no dependencies - ready immediately *)
      let nodes = Action_graph.nodes action_graph in
      List.iter (fun node ->
        if List.length node.deps = 0 then Queue.add node ready
      ) nodes;
      
      (* 3. Start worker pool *)
      let pool = DynamicWorkerPool.start ~concurrency ~owner:(self ())
        ~worker_fn:(fun ~owner ~task:node ->
          (* Execute with caching *)
          let hash = Action_graph.hash_action_node action_graph node in
          let result = match Store.get store hash with
            | Some artifact ->
                Store.promote store artifact ~target_dir:sandbox_dir;
                { status = Cached; artifact }
            | None ->
                execute_actions node.value.actions;
                verify_outputs node.value.outs;
                let artifact = Store.save store ~hash ~outs:node.value.outs in
                { status = Built; artifact }
          in
          send owner (TaskCompleted { node; result })
        ) ()
      in
      
      (* 4. Dispatch loop with readiness tracking *)
      let rec dispatch_loop () =
        match receive () with
        | DynamicWorkerPool.WorkerReady worker ->
            (match Queue.take ready with
             | None -> dispatch_loop ()
             | Some node ->
                 DynamicWorkerPool.send_task pool worker node;
                 dispatch_loop ())
        
        | TaskCompleted { node; result } ->
            (* Update dependents *)
            let dependents = find_nodes_depending_on action_graph node.id in
            List.iter (fun dep ->
              let count = HashMap.get dep_count dep.id |> Option.unwrap in
              let new_count = count - 1 in
              if new_count = 0 then
                Queue.add dep ready  (* Now ready! *)
              else
                HashMap.insert dep_count dep.id new_count
            ) dependents;
            
            (* Check if all done *)
            if all_nodes_completed () then () else dispatch_loop ()
      in
      
      dispatch_loop ();
      Ok ()
  | Cycle { cycle } -> Error (CyclicDependency cycle)
  | Error msg -> Error (PlanningFailed msg)
```

### Phase 2: Update Types

Remove:
- `worker_pool_types.ml` - Use `Std.WorkerPool` types
- `worker_pool.ml` - Use `Std.WorkerPool.SimpleWorkerPool`
- `build_queue.ml` - Not needed with static topo sort
- `RequeueWithDependencies` message - Not needed

Keep:
- `build_worker.ml` - Refactor to simple action executor
- `build_server.ml` - Simplify to coordinator

### Phase 3: Integration

Wire up with tusk-planner:
```ocaml
open Tusk_planner

let execute_build ~workspace ~toolchain ~packages ~concurrency ~store =
  List.map (fun package ->
    let sandbox_dir = create_sandbox package in
    build_package ~package ~toolchain ~workspace ~concurrency ~store ~sandbox_dir
  ) packages
```

## Dependencies

**Current (doesn't build):**
- Missing: `Core`, `Model`, `Workspace`, `Tusk_protocol`

**After refactor:**
- ✅ `std` - For WorkerPool, Path, etc.
- ✅ `miniriot` - For process primitives
- ✅ `tusk-model` - For Package, Workspace, Toolchain types
- ✅ `tusk-planner` - For Action_graph, Planner
- ✅ `tusk-store` - For artifact caching
- ✅ `tusk-ocaml` - For Ocamlc invocation (from action execution)

**No circular dependencies** - tusk-executor is the final package in the chain.

## Why DynamicWorkerPool (Not SimpleWorkerPool)?

**Key Insight:** We need **dependency-aware parallelism**, not just parallel map.

Given this graph:
```
    A
   / \
  B   C
   \ /
    D
```

**Goal:** Execute B and C in parallel (independent), then D (after both).

**SimpleWorkerPool:** Would execute all 4 in parallel, violating dependencies ❌

**DynamicWorkerPool:** Track readiness, dispatch when dependencies satisfied ✅

### Execution with Readiness Tracking

```ocaml
(* Track ready nodes - those with all dependencies satisfied *)
let ready = Queue.create () in
let dep_count = compute_dependency_counts action_graph in

let rec dispatch_loop () =
  match receive () with
  | WorkerReady worker ->
      (match Queue.take ready with
       | None -> dispatch_loop ()  (* No ready work *)
       | Some node ->
           DynamicWorkerPool.send_task pool worker node;
           dispatch_loop ())
  
  | TaskCompleted { node } ->
      (* Find nodes that depended on this one *)
      let dependents = find_dependents graph node in
      
      (* Decrement their counts, enqueue if now ready *)
      List.iter (fun dep ->
        let new_count = decrement_count dep_count dep in
        if new_count = 0 then Queue.add dep ready
      ) dependents;
      
      dispatch_loop ()
```

**Timeline:**
- T0: A ready → dispatch A
- T1: A done → B,C ready → dispatch both in parallel ✅
- T2: B,C done → D ready → dispatch D
- T3: D done → complete!

**Benefits:**
- ✅ Maximum parallelism (B and C execute simultaneously)
- ✅ Respects dependencies (D waits for B and C)
- ✅ Dynamic task assignment based on readiness
- ✅ No wasted worker time

## Key Insight: Static vs Dynamic Dependencies

**Old approach:**
```
Start building A → Discover it needs B → Requeue A → Build B → Retry A
```
Problems: Complex state, race conditions, error-prone

**New approach:**
```
Planner.plan_node → [B, A] (topo sorted) → Build B → Build A
```
Benefits: Simple, predictable, cache-friendly

## Next Steps

1. ✅ Document refactoring plan (this file)
2. ⏳ Create `executor.ml` with SimpleWorkerPool-based implementation
3. ⏳ Update `build_worker.ml` to execute Action.t list
4. ⏳ Remove deprecated worker pool code
5. ⏳ Add integration tests
6. ⏳ Wire up with main tusk binary

## See Also

- `packages/tusk-planner/ARCHITECTURE_REFACTOR.md` - Action graph design
- `packages/tusk-planner/CACHING.md` - Merkle graph caching
- `packages/std/src/worker_pool/` - Std.WorkerPool implementation
