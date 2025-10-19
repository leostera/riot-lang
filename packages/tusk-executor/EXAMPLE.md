# Execution Example

## Scenario: Building a Package with Dependencies

```ocaml
(* Package structure *)
src/
  a.mli
  a.ml
  b.ml
  c.ml   (* depends on a, b *)
```

## Step 1: Planning

```ocaml
let input = Planner.{
  package = { name = "mylib"; ... };
  toolchain;
  workspace;
  source_dir = Path.v "src";
  sandbox_dir = Path.v "_build/sandbox";
} in

match Planner.plan_node input with
| Planned { action_graph; outputs; _ } -> ...
```

**Action Graph Created:**

```
Node 1: a.mli → [CompileInterface src/a.mli]
  deps: []
  outs: [a.cmi, a.cmti]
  srcs: [src/a.mli]

Node 2: a.ml → [CompileImplementation src/a.ml]
  deps: [1]  (* needs a.cmi from a.mli *)
  outs: [a.cmx, a.cmt]
  srcs: [src/a.ml]

Node 3: b.ml → [CompileImplementation src/b.ml]
  deps: []
  outs: [b.cmx, b.cmi, b.cmt]
  srcs: [src/b.ml]

Node 4: c.ml → [CompileImplementation src/c.ml -open A -open B]
  deps: [2, 3]  (* needs a.cmx and b.cmx *)
  outs: [c.cmx, c.cmi, c.cmt]
  srcs: [src/c.ml]

Node 5: mylib.cmxa → [CreateLibrary mylib.cmxa [a.cmx, b.cmx, c.cmx]]
  deps: [2, 3, 4]
  outs: [mylib.cmxa, mylib.a]
  srcs: []
```

## Step 2: Initialize Execution State

```ocaml
(* Compute dependency counts *)
let dep_count = HashMap.create () in
HashMap.insert dep_count node1.id 0;  (* No deps *)
HashMap.insert dep_count node2.id 1;  (* Needs node1 *)
HashMap.insert dep_count node3.id 0;  (* No deps *)
HashMap.insert dep_count node4.id 2;  (* Needs node2 + node3 *)
HashMap.insert dep_count node5.id 3;  (* Needs node2 + node3 + node4 *)

(* Find initially ready nodes *)
let ready = Queue.create () in
Queue.add node1 ready;  (* a.mli - no deps *)
Queue.add node3 ready;  (* b.ml - no deps *)
```

**State:**
```
Ready:    [node1 (a.mli), node3 (b.ml)]
Pending:  [node2 (a.ml), node4 (c.ml), node5 (library)]
```

## Step 3: Execution Timeline

### T=0: Initial Dispatch

Two workers become ready:

```ocaml
(* Worker 1 becomes ready *)
| DynamicWorkerPool.WorkerReady w1 ->
    let node1 = Queue.take ready in  (* a.mli *)
    DynamicWorkerPool.send_task pool w1 node1

(* Worker 2 becomes ready *)
| DynamicWorkerPool.WorkerReady w2 ->
    let node3 = Queue.take ready in  (* b.ml *)
    DynamicWorkerPool.send_task pool w2 node3
```

**Workers:**
- Worker 1: Compiling `a.mli` 
- Worker 2: Compiling `b.ml`

**State:**
```
Ready:    []
In Progress: [node1 (a.mli), node3 (b.ml)]
Pending:  [node2 (a.ml), node4 (c.ml), node5 (library)]
```

### T=1: a.mli Completes

```ocaml
| TaskCompleted { node = node1; result } ->
    (* Find dependents of node1 *)
    let dependents = [node2] in  (* a.ml depends on a.mli *)
    
    (* Update dependency counts *)
    List.iter (fun dep ->
      let count = HashMap.get dep_count dep.id in  (* node2: 1 *)
      let new_count = count - 1 in  (* 1 - 1 = 0 *)
      if new_count = 0 then
        Queue.add dep ready  (* a.ml is now ready! *)
    ) dependents
```

**State:**
```
Ready:    [node2 (a.ml)]
In Progress: [node3 (b.ml)]
Completed: [node1 (a.mli)]
Pending:  [node4 (c.ml), node5 (library)]
```

### T=2: Worker 1 Ready, Dispatch a.ml

```ocaml
| DynamicWorkerPool.WorkerReady w1 ->
    let node2 = Queue.take ready in  (* a.ml *)
    DynamicWorkerPool.send_task pool w1 node2
```

**Workers:**
- Worker 1: Compiling `a.ml`
- Worker 2: Still compiling `b.ml`

### T=3: b.ml Completes

```ocaml
| TaskCompleted { node = node3; result } ->
    (* Find dependents of node3 *)
    let dependents = [node4, node5] in  (* c.ml and library *)
    
    (* node4 (c.ml): 2 → 1 (still waiting for a.ml) *)
    HashMap.insert dep_count node4.id 1;
    
    (* node5 (library): 3 → 2 (still waiting for a.ml and c.ml) *)
    HashMap.insert dep_count node5.id 2
```

**State:**
```
Ready:    []
In Progress: [node2 (a.ml)]
Completed: [node1 (a.mli), node3 (b.ml)]
Pending:  [node4 (c.ml), node5 (library)]
  - node4: 1 dep remaining (a.ml)
  - node5: 2 deps remaining (a.ml, c.ml)
```

### T=4: a.ml Completes

```ocaml
| TaskCompleted { node = node2; result } ->
    let dependents = [node4, node5] in
    
    (* node4 (c.ml): 1 → 0 = READY! *)
    Queue.add node4 ready;
    
    (* node5 (library): 2 → 1 (still waiting for c.ml) *)
    HashMap.insert dep_count node5.id 1
```

**State:**
```
Ready:    [node4 (c.ml)]
In Progress: []
Completed: [node1, node2, node3]
Pending:  [node5 (library)]
  - node5: 1 dep remaining (c.ml)
```

### T=5: Dispatch c.ml

```ocaml
| DynamicWorkerPool.WorkerReady w1 ->
    let node4 = Queue.take ready in  (* c.ml *)
    DynamicWorkerPool.send_task pool w1 node4
```

**Workers:**
- Worker 1: Compiling `c.ml`
- Worker 2: Idle

### T=6: c.ml Completes

```ocaml
| TaskCompleted { node = node4; result } ->
    let dependents = [node5] in
    
    (* node5 (library): 1 → 0 = READY! *)
    Queue.add node5 ready
```

**State:**
```
Ready:    [node5 (library)]
Completed: [node1, node2, node3, node4]
```

### T=7: Create Library

```ocaml
| DynamicWorkerPool.WorkerReady w2 ->
    let node5 = Queue.take ready in
    DynamicWorkerPool.send_task pool w2 node5
```

### T=8: Library Completes - DONE!

```ocaml
| TaskCompleted { node = node5; result } ->
    (* All nodes completed! *)
```

## Parallelism Achieved

**Sequential execution:** T = T(a.mli) + T(a.ml) + T(b.ml) + T(c.ml) + T(library)

**Parallel execution:** 
- T=0-1: a.mli and b.ml **in parallel**
- T=2-4: a.ml executes
- T=5-6: c.ml executes  
- T=7-8: library executes

**Speedup:** If all compilations take ~1 second:
- Sequential: 5 seconds
- Parallel: 3 seconds (40% faster!)

With more independent modules, speedup increases dramatically.

## With Caching

```ocaml
(* Worker function with cache checks *)
~worker_fn:(fun ~owner ~task:node ->
  (* 1. Compute hash *)
  let hash = Action_graph.hash_action_node action_graph node in
  
  (* 2. Check cache *)
  match Store.get store hash with
  | Some artifact ->
      Log.info "Cache hit for %s" (describe_node node);
      Store.promote store artifact ~target_dir:sandbox_dir;
      send owner (TaskCompleted { 
        node; 
        result = { status = Cached; artifact; duration_ms = 0 }
      })
  
  | None ->
      Log.info "Building %s" (describe_node node);
      let start = Time.Instant.now () in
      
      (* 3. Execute actions *)
      execute_actions node.value.actions;
      
      (* 4. Verify outputs exist *)
      List.iter (fun out ->
        if not (Fs.exists out) then
          panic (format "Expected output not created: %s" (Path.to_string out))
      ) node.value.outs;
      
      (* 5. Save to cache *)
      let artifact = Store.save store ~package:pkg.name ~hash 
        ~sandbox_dir ~outs:node.value.outs
        |> Result.expect ~msg:"Failed to save artifact"
      in
      
      let duration_ms = 
        Time.Instant.duration_since ~earlier:start (Time.Instant.now ())
        |> Time.Duration.to_millis
      in
      
      send owner (TaskCompleted { 
        node; 
        result = { status = Built; artifact; duration_ms }
      })
)
```

**Second build (all cached):**
- T=0: All 5 nodes dispatched immediately (no dependencies in cache)
- T=0.1: All 5 complete from cache
- **Total: 0.1 seconds** (50x faster!)
