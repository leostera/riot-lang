# Action Graph Caching

## Concept

Each `action_node` is a self-contained build unit with:
- **Actions**: What to execute (CompileInterface, CompileImplementation, etc.)
- **Outputs**: What files will be produced
- **Hash**: Content-based fingerprint of the node

## Caching Workflow

```ocaml
(* 1. Plan the build *)
let action_graph, all_outputs = Action_graph.from_module_graph module_graph in

(* 2. For each action node in topological order *)
let sorted_nodes = Action_graph.topo_sort action_graph in

List.iter (fun (node : action_node G.node) ->
  (* 3. Compute hash of this specific node *)
  let node_hash = Action_graph.hash_action_node node.value in
  
  (* 4. Check cache *)
  match Store.get store node_hash with
  | Some artifact ->
      (* Cache hit! Copy outputs from cache *)
      Store.restore_artifact artifact ~dest:sandbox_dir;
      Log.info "Restored from cache: %s" (Crypto.hash_to_string node_hash)
      
  | None ->
      (* Cache miss - execute actions *)
      List.iter (fun action ->
        execute_action action ~cwd:sandbox_dir
      ) node.value.actions;
      
      (* Verify outputs were created *)
      List.iter (fun output ->
        if not (Fs.exists (Path.(sandbox_dir / output))) then
          panic (format "Expected output not created: %s" (Path.to_string output))
      ) node.value.outputs;
      
      (* Store in cache *)
      Store.save store node_hash 
        ~sandbox_dir 
        ~outputs:node.value.outputs
        
) sorted_nodes
```

## Benefits

### 1. Fine-Grained Caching
Each module is cached independently. Changing one module only invalidates that module's cache, not the entire package.

### 2. Incremental Builds
```
Initial build:
  Module A [miss] → compile → cache
  Module B [miss] → compile → cache  
  Module C [miss] → compile → cache

Edit Module B:
  Module A [HIT!] → restore from cache
  Module B [miss] → recompile → update cache
  Module C [HIT!] → restore from cache
```

### 3. Cross-Machine Sharing
The hash is content-based (not path-based), so:
- Same source → same hash
- Can share cache between machines
- Can share cache between CI builds

### 4. Step-by-Step Verification
After each action node executes, we immediately verify its declared outputs exist. No waiting until the end to discover missing files.

## Hash Computation

The hash includes:
- **Action types and parameters**
  - Source files
  - Compiler flags
  - Include paths
  - Output locations
- **Action order** (WriteFile before CompileImplementation)
- **Expected outputs**

What it DOESN'T include:
- Timestamps
- Absolute paths (uses relative paths)
- User/machine identifiers

## Example

```ocaml
(* Two different builds of the same source *)

(* Build 1: /home/alice/project *)
let node1 = {
  actions = [
    CompileImplementation {
      source = Path.v "foo.ml";
      output = Path.v "foo.cmx";
      includes = [Path.v "."];
      flags = [Open "Std"];
    }
  ];
  outputs = [Path.v "foo.cmx"; Path.v "foo.cmi"; Path.v "foo.cmt"];
}

(* Build 2: /home/bob/project (different machine, same code) *)
let node2 = {
  actions = [
    CompileImplementation {
      source = Path.v "foo.ml";  (* Same relative path *)
      output = Path.v "foo.cmx";
      includes = [Path.v "."];
      flags = [Open "Std"];
    }
  ];
  outputs = [Path.v "foo.cmx"; Path.v "foo.cmi"; Path.v "foo.cmt"];
}

(* Same hash! Can share cache *)
assert (Action_graph.hash_action_node node1 = Action_graph.hash_action_node node2)
```

## Store Integration

The `tusk-store` package provides the cache backend:

```ocaml
module Store : sig
  type t
  
  val get : t -> Crypto.hash -> Artifact.t option
  (** Retrieve cached artifact by action node hash *)
  
  val save : t -> Crypto.hash -> sandbox_dir:Path.t -> outputs:Path.t list 
    -> (Artifact.t, error) result
  (** Save action node outputs to cache *)
  
  val restore_artifact : Artifact.t -> dest:Path.t -> unit
  (** Copy cached outputs to destination *)
end
```

## Performance

**Without caching:**
```
Module A: 2.5s compile
Module B: 1.8s compile  
Module C: 3.2s compile
Total: 7.5s
```

**With caching (no changes):**
```
Module A: 0.05s restore
Module B: 0.03s restore
Module C: 0.04s restore
Total: 0.12s (60x faster!)
```

**With caching (Module B changed):**
```
Module A: 0.05s restore
Module B: 1.8s compile
Module C: 0.04s restore
Total: 1.89s (4x faster!)
```
