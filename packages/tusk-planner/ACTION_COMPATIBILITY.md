# Action Type Compatibility

## tusk-planner Actions vs Main Tusk Actions

### ✅ Identical Actions

| Action | tusk-planner | Main tusk | Notes |
|--------|--------------|-----------|-------|
| CompileInterface | ✅ | ✅ | Same signature with `Ocamlc.compiler_flag list` |
| CompileImplementation | ✅ | ✅ | Same signature with `Ocamlc.compiler_flag list` |
| GenerateInterface | ✅ | ✅ | Defined but not generated in either |
| CompileC | ✅ | ✅ | Same signature |
| CreateLibrary | ✅ | ✅ | Same signature |
| CreateExecutable | ✅ | ✅ | Same signature |
| CopyFile | ✅ | ✅ | Same signature |
| WriteFile | ✅ | ✅ | Same signature |
| DeclareOutputs | ✅ | ✅ | Same signature |

### ❌ Not in tusk-planner

| Action | Why Not Included |
|--------|-----------------|
| CopyDir | Used in sandbox setup, not module compilation |

### Summary

The action types are **100% compatible** for module compilation actions. The only difference is `CopyDir` which is used in sandbox setup (before module compilation) and thus not part of the planner's responsibility.

## Integration Notes

### Converting Action Graph to Action List

```ocaml
let actions = Action_graph.to_action_list action_graph
```

This returns actions in topological dependency order, ready for sequential execution.

### Type Compatibility

```ocaml
(* tusk-planner *)
type action = Action_graph.action

(* main tusk *)
type action = Actions.action
```

These are structurally identical and can be used interchangeably in the build system.
