# PubGrub - Version Solving for OCaml

A straightforward OCaml implementation of the [PubGrub](https://github.com/dart-lang/pub/blob/master/doc/solver.md) version solving algorithm, based on the Rust implementation [pubgrub-rs](https://github.com/pubgrub-rs/pubgrub).

## What is PubGrub?

PubGrub efficiently finds sets of packages and versions that satisfy all dependency constraints. When no solution exists, it provides clear, human-readable explanations.

## Features

- ✅ **Modular Design**: Clean separation of concerns across modules
- ✅ **Semantic Versioning**: Uses `Std.Version` for full SemVer 2.0 support
- ✅ **Version Ranges**: Express constraints like `>=1.0.0`, `<2.0.0`
- ✅ **Simple API**: No functors, no complex types - just straightforward OCaml
- ✅ **Offline Provider**: Test dependencies in-memory
- 🚧 **Conflict Resolution**: Basic solver implemented, advanced features coming

## Quick Start

```ocaml
open Std
open Pubgrub

let v major minor patch = make_version ~major ~minor ~patch

let () =
  let provider = create_offline () in
  
  (* Define packages and dependencies *)
  add_package provider "root" (v 1 0 0) [
    ("foo", full);
    ("bar", higher_than (v 2 0 0));
  ];
  
  add_package provider "foo" (v 1 0 0) [];
  add_package provider "bar" (v 2 5 0) [];
  
  (* Solve! *)
  match solve (to_provider provider) "root" (v 1 0 0) with
  | Ok (Solver.Success solution) ->
      List.iter (fun (pkg, ver) ->
        println "%s@%s" pkg (version_to_string ver)
      ) solution
  | Ok (Solver.Failure conflict) ->
      println "No solution: %s" (explain_conflict conflict)
  | Error err ->
      println "Error: %s" err
```

## Module Structure

```
src/
├── main.ml                  # Test binary
├── Pubgrub.ml/.mli         # Main module, re-exports everything
├── ranges.ml/.mli          # Version range operations
├── term.ml/.mli            # Package + version constraints  
├── provider.ml/.mli        # Dependency provider interface
├── incompatibility.ml/.mli # Conflict tracking
├── partial_solution.ml/.mli # Solution state
├── solver.ml/.mli          # Core algorithm
└── report.ml/.mli          # Error reporting
```

## Testing

Run the built-in tests:

```bash
tusk run pubgrub
```

Output:
```
PubGrub Solver Tests

=== Test: Simple dependency resolution ===
✓ Solution found with 2 packages:
  • root@1.0.0
  • foo@2.0.0

=== Test: Transitive dependencies ===
✓ Solution found with 3 packages:
  • bar@1.0.0
  • root@1.0.0
  • foo@1.0.0

✅ All tests completed
```

## Core Types

### Version

Uses `Std.Version` for full SemVer 2.0 support:

```ocaml
let v = make_version ~major:1 ~minor:2 ~patch:3

(* Parse from string *)
match version_of_string "1.2.3" with
| Ok v -> println "%s" (version_to_string v)
| Error err -> println "Parse error"
```

### Version Ranges

```ocaml
(* Any version *)
let any = full

(* No version *)
let none = empty

(* Exactly one version *)
let exact = singleton (v 1 0 0)

(* Greater than or equal *)
let gte = higher_than (v 1 0 0)

(* Strictly greater than *)
let gt = strictly_higher_than (v 1 0 0)

(* Less than *)
let lt = strictly_lower_than (v 2 0 0)

(* Between versions (inclusive start, exclusive end) *)
let range = between (v 1 0 0) (v 2 0 0)
```

### Dependency Provider

```ocaml
type 'error Provider.t = {
  choose_version : package -> version_ranges -> (version option, 'error) result;
  get_dependencies : package -> version -> (dependencies, 'error) result;
}

(* Offline provider for testing *)
let provider = create_offline ()
add_package provider "my-pkg" (v 1 0 0) [
  ("dep1", higher_than (v 2 0 0));
  ("dep2", between (v 1 0 0) (v 3 0 0));
]
```

## Algorithm Overview

The solver implements a simplified version of PubGrub:

1. **Decision Making**: Pick a package and version to add to the solution
2. **Unit Propagation**: Derive consequences from current decisions
3. **Conflict Detection**: Identify when constraints can't be satisfied
4. **Backtracking**: Revert decisions when conflicts occur

Current implementation is a straightforward breadth-first search that:
- Chooses the highest version for each package
- Adds dependencies to the work queue
- Detects when no version is available

## Status

**Alpha** - Core functionality works but advanced features are still in development.

### What's Working ✅

- Version parsing and comparison (via Std.Version)
- Version range operations (union, intersection, complement)
- Offline dependency provider
- Basic solver algorithm
- Transitive dependency resolution
- Version constraints
- Diamond dependencies

### TODO 🚧

- Advanced conflict resolution and backtracking
- Incompatibility learning
- Human-readable error reporting with derivation trees
- Performance optimizations
- Comprehensive test suite

## Design Philosophy

**Keep it simple and practical:**

- No functors, no complex module types
- Plain OCaml types and functions
- Uses `Std` library consistently (no `Stdlib`/`Unix`/`Sys`)
- Modular structure with clear separation of concerns
- Focus on getting it working, then optimize

## Architecture

See [DESIGN.md](./DESIGN.md) for detailed design documentation.

## References

- [PubGrub Algorithm](https://github.com/dart-lang/pub/blob/master/doc/solver.md)
- [Introductory Blog Post](https://medium.com/@nex3/pubgrub-2fb6470504f)
- [Rust Implementation](https://github.com/pubgrub-rs/pubgrub)
