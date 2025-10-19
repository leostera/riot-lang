# Parallel Execution Strategy

## The Problem

Given an action graph with dependencies:

```
    A
   / \
  B   C
   \ /
    D
```

**Goal:** Execute B and C in parallel (they don't depend on each other), then execute D after both complete.

**NOT:** Execute sequentially [A, B, C, D] - that wastes parallelism!

## Why SimpleWorkerPool Doesn't Work

`SimpleWorkerPool.run` executes tasks in the order provided:
```ocaml
let tasks = [A; B; C; D] in  (* Topologically sorted *)
SimpleWorkerPool.run ~concurrency:8 ~tasks ~fn:execute ()
(* Result: All 4 might run in parallel, violating dependencies! *)
```

Even though they're topo-sorted, SimpleWorkerPool doesn't respect dependencies - it just processes the list in parallel.

## Solution: DynamicWorkerPool + Readiness Tracking

We need to:
1. **Track which nodes are ready** (all dependencies satisfied)
2. **Dispatch ready nodes** to available workers
3. **Update readiness** as nodes complete
4. **Maximize parallelism** by dispatching multiple ready nodes simultaneously

```ocaml
type state = {
  action_graph: Action_graph.t;
  pending: Action_node.t HashMap.t;        (* Not ready yet *)
  ready: Action_node.t Queue.t;            (* Ready to execute *)
  in_progress: Action_node.t HashMap.t;    (* Currently executing *)
  completed: Action_node.t HashSet.t;      (* Finished *)
  dep_count: (Node_id.t, int) HashMap.t;   (* Remaining deps per node *)
}
```

### Algorithm

```ocaml
(* Initialize *)
let init_state action_graph =
  let all_nodes = Action_graph.nodes action_graph in
  let dep_count = HashMap.create () in
  let ready = Queue.create () in
  let pending = HashMap.create () in
  
  List.iter (fun node ->
    let count = List.length node.deps in
    if count = 0 then
      Queue.add node ready  (* No deps = ready immediately *)
    else (
      HashMap.insert dep_count node.id count;
      HashMap.insert pending node.id node
    )
  ) all_nodes;
  
  { action_graph; pending; ready; in_progress = HashMap.create ();
    completed = HashSet.create (); dep_count }

(* Main execution loop *)
let rec execute_loop state pool =
  match receive () with
  | DynamicWorkerPool.WorkerReady worker ->
      (match Queue.take state.ready with
       | None -> 
           (* No ready work, keep worker idle *)
           execute_loop state pool
       | Some node ->
           (* Dispatch ready node *)
           HashMap.insert state.in_progress node.id node;
           DynamicWorkerPool.send_task pool worker node;
           execute_loop state pool)
  
  | TaskCompleted { node; result } ->
      (* Mark complete *)
      HashMap.remove state.in_progress node.id;
      HashSet.insert state.completed node.id;
      
      (* Find nodes that depended on this one *)
      let graph = Action_graph.graph state.action_graph in
      let dependents = find_nodes_depending_on graph node.id in
      
      (* Decrement their dependency counts *)
      List.iter (fun dep_node ->
        match HashMap.get state.dep_count dep_node.id with
        | Some count ->
            let new_count = count - 1 in
            if new_count = 0 then (
              (* All dependencies satisfied! *)
              HashMap.remove state.pending dep_node.id;
              HashMap.remove state.dep_count dep_node.id;
              Queue.add dep_node state.ready
            ) else
              HashMap.insert state.dep_count dep_node.id new_count
        | None -> ()
      ) dependents;
      
      (* Check if done *)
      if HashSet.size state.completed = total_nodes then
        ()  (* All done! *)
      else
        execute_loop state pool
```

### Example Execution Timeline

```
Graph:     A
          / \
         B   C
          \ /
           D

Time 0: Ready=[A], Pending=[B,C,D]
  - Worker 1 gets A

Time 1: A completes, Ready=[B,C], Pending=[D]
  - Worker 1 gets B
  - Worker 2 gets C  ← PARALLEL!

Time 2: B and C both complete, Ready=[D], Pending=[]
  - Worker 1 gets D

Time 3: D completes, Ready=[], Pending=[], Completed=[A,B,C,D]
  - Done!
```

## So: DynamicWorkerPool IS the right choice

```ocaml
open Std.WorkerPool.DynamicWorkerPool

let execute_action_graph ~action_graph ~concurrency ~store =
  let pool = start 
    ~concurrency 
    ~owner:(self ())
    ~worker_fn:(fun ~owner ~task:node ->
      let hash = Action_graph.hash_action_node action_graph node in
      let result = match Store.get store hash with
        | Some artifact ->
            Store.promote store artifact;
            { status = Cached; artifact }
        | None ->
            execute_actions node.value.actions;
            verify_outputs node.value.outs;
            let artifact = Store.save store ~hash ~outs:node.value.outs in
            { status = Built; artifact }
      in
      send owner (TaskCompleted { node; result })
    )
    ()
  in
  
  let state = init_state action_graph in
  execute_loop state pool
```

## Key Difference from Old Implementation

**Old:** Dynamic dependency discovery + requeuing
```ocaml
| RequeueWithDependencies { node; deps } ->
    (* Discovered missing deps at runtime - requeue *)
```

**New:** Static dependencies + readiness tracking
```ocaml
| TaskCompleted { node; result } ->
    (* Decrement dependency counters, enqueue newly ready nodes *)
```

**Benefits:**
- ✅ Dependencies known upfront (from topo sort)
- ✅ No requeuing logic needed
- ✅ Maximum parallelism (all ready nodes dispatched)
- ✅ Simpler state (no "busy" tracking, just ready/pending/done)
- ✅ Cache-aware (hash before execution)

## Still Need to Remove from Old Implementation

- ✅ Custom worker pool → Use `Std.WorkerPool.DynamicWorkerPool`
- ✅ `RequeueWithDependencies` → Use readiness tracking
- ✅ `Build_queue.requeue_with_deps` → Use dependency counters
- ❌ Dynamic task assignment → Keep this! It's how we get parallelism

## Complexity Comparison

**Old approach complexity:**
- Worker pool: ~200 LOC
- Requeuing logic: ~100 LOC  
- Build queue: ~150 LOC
- **Total: ~450 LOC**

**New approach complexity:**
- Use Std.WorkerPool: 0 LOC (already exists)
- Readiness tracking: ~50 LOC
- Execution loop: ~50 LOC
- **Total: ~100 LOC**

**Reduction: 78% less code!**
