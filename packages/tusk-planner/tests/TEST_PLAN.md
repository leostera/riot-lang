# Test Plan: Module Graph → Action Graph

## Test Progression: Simple → Complex

### ✅ Level 0: Already Exists
- **Empty package** - Just root node
- **Single module** - One .ml file

### 🎯 Level 1: Basic Scenarios

#### Test 1: Single Module with Interface
```
src/
  foo.mli
  foo.ml
```
**Expected:**
- 2 module nodes (MLI, ML)
- 2 action nodes (CompileInterface, CompileImplementation)
- Dependency: ML depends on MLI
- Outputs: foo.cmi, foo.cmti, foo.cmx, foo.cmt

#### Test 2: Two Modules, Linear Dependency
```
src/
  a.ml      (* no dependencies *)
  b.ml      (* open A *)
```
**Expected:**
- 2 module nodes
- 2 action nodes
- Dependency: B depends on A
- Library node aggregates both
- Outputs: a.cmx, a.cmi, a.cmt, b.cmx, b.cmi, b.cmt, lib.cmxa, lib.a

#### Test 3: Module with C Stub
```
src/
  math.ml
  math_stubs.c
```
**Expected:**
- 2 module nodes (ML, C)
- 2 action nodes (CompileImplementation, CompileC)
- Library includes both .cmx and .o
- Outputs: math.cmx, math_stubs.o, lib.cmxa, lib.a

### 🎯 Level 2: Intermediate Scenarios

#### Test 4: Diamond Dependency
```
src/
  base.ml
  left.ml   (* open Base *)
  right.ml  (* open Base *)
  top.ml    (* open Left; open Right *)
```
**Expected:**
- 4 module nodes
- Proper dependency wiring (Top → Left,Right → Base)
- Topological sort respects dependencies
- All modules in library

#### Test 5: Generated Module (Aliases)
```
src/
  foo.ml
  bar.ml
  MyLib__Aliases.ml-gen  (* generated *)
```
**Expected:**
- 3 module nodes (2 concrete, 1 generated)
- Generated node has WriteFile + CompileImplementation
- CompileImplementation depends on WriteFile
- Special flag: -no-alias-deps for aliases

#### Test 6: Mixed Interfaces
```
src/
  a.mli + a.ml
  b.ml (no interface)
  c.mli + c.ml
```
**Expected:**
- 5 module nodes (3 MLI, 3 ML but 1 has no MLI)
- Correct dependency wiring
- b.ml generates .cmi automatically

### 🎯 Level 3: Real-World Scenarios

#### Test 7: Library with Binary
```
src/
  lib.ml
  utils.ml
bin/
  main.ml  (* depends on lib *)
```
**Expected:**
- 3 module nodes
- 1 library node (lib.ml + utils.ml)
- 1 binary node (main.ml)
- Binary depends on library
- Binary gets correct link order

#### Test 8: Multiple Binaries
```
src/
  core.ml
bin/
  cli.ml
  server.ml
```
**Expected:**
- 3 module nodes
- 1 library node
- 2 binary nodes (both depend on library)
- Each binary independent

#### Test 9: Unix Dependency
```
src/
  file_ops.ml  (* uses Unix *)
```
**Dependencies:** unix
**Expected:**
- Includes: ["."; "+unix"]
- Libraries: ["unix.cmxa"; "mylib.cmxa"]

#### Test 10: Transitive Dependencies
```
Package A → Package B → Package C

Package A builds against B.cmxa and C.cmxa
```
**Expected:**
- Dependency list in correct topological order
- Link order: [C.cmxa; B.cmxa; A.cmxa]

### 🎯 Level 4: Edge Cases

#### Test 11: Circular Dependency (Error Case)
```
src/
  a.ml  (* open B *)
  b.ml  (* open A *)
```
**Expected:**
- `Cycle { cycle = ["A"; "B"; "A"] }`
- Error before action generation

#### Test 12: Empty Library
```
src/
  (no files)
```
**Expected:**
- Just root node
- No library node
- No actions
- Empty outputs

#### Test 13: Header-Only C Code
```
src/
  utils.h
  (no .c files)
```
**Expected:**
- H node (no actions)
- No library
- Empty outputs

#### Test 14: Complex Multi-Module
```
src/
  types.mli + types.ml
  utils.mli + utils.ml  (* open Types *)
  parser.ml             (* open Types; open Utils *)
  lexer.ml              (* open Types *)
  main.ml               (* open Parser; open Lexer *)
  stubs.c
```
**Expected:**
- 10 module nodes (5 ML, 3 MLI, 1 C, 1 Root)
- Complex dependency graph
- Correct topological sort
- Library with all .cmx + .o

## Test Structure

```ocaml
let test_scenario_name () =
  (* Setup *)
  let fixture_dir = Path.v "tests/fixtures/scenario_name" in
  
  (* Build *)
  let input = make_test_input fixture_dir in
  
  (* Execute *)
  match Planner.plan_node input with
  | Planned { module_graph; action_graph; outputs } ->
      (* Assertions *)
      assert_module_count module_graph expected_count;
      assert_action_count action_graph expected_count;
      assert_outputs outputs expected_outputs;
      assert_dependencies action_graph expected_deps;
      Ok ()
  | Cycle { cycle } -> 
      Error (format "Unexpected cycle: %s" (String.concat " -> " cycle))
  | Error msg ->
      Error msg
```

## Fixtures to Create

```
tests/fixtures/
  single-with-interface/
    src/foo.mli, foo.ml
  linear-dependency/
    src/a.ml, b.ml
  c-stubs/
    src/math.ml, math_stubs.c
  diamond-dependency/
    src/base.ml, left.ml, right.ml, top.ml
  generated-aliases/
    src/foo.ml, bar.ml, MyLib__Aliases.ml-gen
  mixed-interfaces/
    src/a.mli, a.ml, b.ml, c.mli, c.ml
  library-with-binary/
    src/lib.ml, utils.ml
    bin/main.ml
  multiple-binaries/
    src/core.ml
    bin/cli.ml, server.ml
  unix-dependency/
    src/file_ops.ml
    tusk.toml (with unix dep)
  circular-dependency/
    src/a.ml, b.ml (mutual recursion)
  empty-library/
    src/ (empty)
  header-only/
    src/utils.h
  complex-multi-module/
    src/types.mli, types.ml, utils.mli, utils.ml, 
        parser.ml, lexer.ml, main.ml, stubs.c
```

## Helper Functions Needed

```ocaml
val assert_module_count : Module_node.t G.t -> int -> (unit, string) result
val assert_action_count : Action_graph.t -> int -> (unit, string) result
val assert_outputs : Path.t list -> Path.t list -> (unit, string) result
val assert_has_dependency : Action_graph.t -> string -> string -> (unit, string) result
val assert_topological_order : Action_graph.t -> string list -> (unit, string) result
```

## Metrics

- **Total tests:** ~15
- **Coverage:** All module kinds, all action types, all dependency patterns
- **Progression:** Simple → Complex
- **Error cases:** Cycles, empty packages
