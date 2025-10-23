# Coordinator Architecture

The coordinator orchestrates workspace-wide builds using a two-level planning strategy.

## Key Insight: Fast Workspace Planning + Lazy Package Planning

### 1. Workspace-Level Planning (FAST)
- **What**: Read `tusk.toml` files, extract dependencies
- **Cost**: O(packages) - just file reads
- **When**: Every build
- **Output**: Topologically sorted package list

### 2. Package-Level Planning (EXPENSIVE)  
- **What**: Build module graph + action graph
- **Cost**: O(modules × dependencies) - file scanning, graph construction
- **When**: ONLY on cache miss
- **Output**: Action graph ready for execution

## Build Flow

```ocaml
(* 1. FAST: Plan workspace *)
plan_workspace ~workspace ~target
  → Ok { packages = [pkg1; pkg2; pkg3]; ... }

(* 2. For each package in topological order: *)
for package in packages do
  
  (* 2a. Compute content hash from sources *)
  let hash = compute_package_hash ~package in
  
  (* 2b. CHECK CACHE FIRST - avoid expensive planning! *)
  match Store.get store hash with
  | Some artifact →
      (* CACHE HIT: Skip planning entirely, just promote *)
      Store.promote store hash ~target_dir;
      return `Cached
      
  | None →
      (* CACHE MISS: Now do expensive package planning *)
      plan_package ~workspace ~toolchain ~package
        → Ok { module_graph; action_graph }
      
      (* Execute actions in parallel *)
      Executor.execute ~action_graph ~toolchain ~concurrency
      
      (* Store results for future cache hits *)
      Store.save store ~package ~hash ~outs
      Store.promote store hash ~target_dir
      return `Built
done
```

## Performance Characteristics

| Operation | Cost | Frequency |
|-----------|------|-----------|
| Workspace planning | O(packages) | Every build |
| Package hash | O(source files) | Per package |
| Cache check | O(1) | Per package |
| Package planning | O(modules²) | **Only on cache miss** |
| Action execution | O(actions) | Only on cache miss |

## Cache Strategy

**Package-level caching** (not action-level):
- Hash includes ALL source files in package
- Single cache entry per package
- Invalidates entire package on any source change
- Simple, correct, fast cache checks

**Why not action-level caching?**
- Would require expensive planning just to compute action hashes
- Defeats purpose of avoiding planning on cache hit
- Package-level is sufficient for most workflows

## Integration with Store

The Store provides content-addressable storage:

```ocaml
(* Check if hash exists *)
Store.exists store hash → bool

(* Get artifact metadata *)
Store.get store hash → Artifact.t option

(* Save build outputs *)
Store.save store ~package ~hash ~sandbox_dir ~outs → Artifact.t

(* Promote from cache to target *)
Store.promote store hash ~target_dir → (unit, error) result
```

## Example: Rebuilding After Source Change

```
Workspace: [std, tusk-model, tusk-planner]

Build 1 (cold cache):
  1. plan_workspace → [std, tusk-model, tusk-planner]
  2. std: hash → cache miss → plan + build + store (100ms planning)
  3. tusk-model: hash → cache miss → plan + build + store (50ms planning)
  4. tusk-planner: hash → cache miss → plan + build + store (80ms planning)
  Total: 230ms planning + execution

Build 2 (no changes):
  1. plan_workspace → [std, tusk-model, tusk-planner]
  2. std: hash → cache hit → promote (0ms planning!)
  3. tusk-model: hash → cache hit → promote (0ms planning!)
  4. tusk-planner: hash → cache hit → promote (0ms planning!)
  Total: 0ms planning, just file copies

Build 3 (edit tusk-model/src/package.ml):
  1. plan_workspace → [std, tusk-model, tusk-planner]
  2. std: hash → cache hit → promote (0ms planning!)
  3. tusk-model: hash → cache miss → plan + build + store (50ms planning)
  4. tusk-planner: hash → cache hit → promote (0ms planning!)
  Total: 50ms planning for changed package only
```

## Future Enhancements

1. **Parallel package builds**: Independent packages can build concurrently
2. **Incremental hashing**: Watch mode with inotify for instant cache checks
3. **Distributed cache**: Share artifacts across machines/CI
4. **Action-level caching**: For very large packages with stable subgraphs
