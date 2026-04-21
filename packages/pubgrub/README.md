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
- ✅ **Conflict Resolution**: Full PubGrub algorithm with prior cause derivation
- ✅ **Intelligent Backtracking**: Decision-level tracking with proper backtracking
- ✅ **Human-Readable Errors**: Derivation tree building and conflict explanation

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
├── Pubgrub.ml/.mli          # Main module, re-exports everything
├── ranges.ml/.mli           # Version range operations
├── term.ml/.mli             # Package + version constraints
├── provider.ml/.mli         # Dependency provider interface
├── incompatibility.ml/.mli  # Conflict tracking
├── partial_solution.ml/.mli # Solution state and satisfier search
├── solver.ml/.mli           # Core solver loop
├── trace.ml/.mli            # Structured debugging surface
└── report.ml/.mli           # Conflict explanation
```

## Testing

Run the built-in tests:

```bash
riot test -p pubgrub
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

The solver follows the core PubGrub loop:

1. **Decision Making**: Pick a package and version to add to the solution
2. **Unit Propagation**: Derive consequences from current decisions
3. **Conflict Detection**: Identify when constraints can't be satisfied
4. **Conflict Resolution**: Learn a prior cause when a conflict is satisfied
5. **Backtracking**: Revert decisions to the appropriate decision level

The implementation keeps structured traces in `Pubgrub.Trace` and produces
human-readable conflict reports through `Pubgrub.Report`.

## Status

**Beta** - core solving, tracing, and explanation are implemented; deeper
performance work and API tightening are still in progress.

### What's Working ✅

- **Full PubGrub Algorithm**: Complete implementation following the Rust reference
- **Version Management**: Parsing, comparison, and SemVer support via Std.Version
- **Version Ranges**: Union, intersection, complement, and containment operations
- **Conflict Resolution**: Prior cause derivation and intelligent backtracking
- **Decision Levels**: Proper tracking for efficient backtracking
- **Unit Propagation**: Automatic constraint propagation during solving
- **Derivation Trees**: Build trees showing why conflicts occur
- **Error Reporting**: Human-readable explanations of solving failures
- **Offline Provider**: In-memory dependency provider for testing
- **Comprehensive Tests**: smoke and reference coverage across simple,
  transitive, conflict-heavy, and generated dependency graphs

### TODO 🚧

- **Resolution Statistics**: Track package conflict counts to prioritize decisions for faster solving
- **Performance Optimizations**:
  - Small vector/map optimizations for common cases
  - Arena allocation for incompatibilities
  - Conflict-based heuristics for decision ordering
- **Advanced Provider Features**:
  - Caching dependency provider
  - Richer offline provider API
- **Enhanced Error Reporting**:
  - Report collapsing and cleanup for better readability
  - Shared incompatibility highlighting

## Design Philosophy

**Keep it simple and practical:**

- No functors, no complex module types
- Plain OCaml types and functions
- Uses `Std` library consistently (no `Stdlib`/`Unix`/`Sys`)
- Modular structure with clear separation of concerns
- Focus on getting it working, then optimize

## Architecture

See [DESIGN.md](./DESIGN.md) for package structure, solver flow, and key
invariants.

## References

- [PubGrub Algorithm](https://github.com/dart-lang/pub/blob/master/doc/solver.md)
- [Introductory Blog Post](https://medium.com/@nex3/pubgrub-2fb6470504f)
- [Rust Implementation](https://github.com/pubgrub-rs/pubgrub)
