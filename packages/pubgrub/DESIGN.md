# PubGrub OCaml Port Design

## Overview

This is a straightforward OCaml port of the PubGrub version solving algorithm from the Rust implementation at [pubgrub-rs/pubgrub](https://github.com/pubgrub-rs/pubgrub). PubGrub was originally designed by Natalie Weizenbaum for the Dart package manager and provides fast version resolution with clear, human-readable error messages.

## What is PubGrub?

PubGrub efficiently finds sets of packages and versions that satisfy all dependency constraints. When no solution exists, it provides clear explanations like:

```
Because dropdown >=2.0.0 depends on icons >=2.0.0 and
  root depends on icons <2.0.0, dropdown >=2.0.0 is forbidden.

And because menu >=1.1.0 depends on dropdown >=2.0.0,
  menu >=1.1.0 is forbidden.

So, because root depends on both menu >=1.0.0 and intl >=5.0.0,
  version solving failed.
```

### Algorithm Foundation

- Uses unit propagation and conflict-driven clause learning
- Maintains a partial solution that's incrementally refined
- Tracks incompatibilities to avoid repeating failed paths
- Backtracks intelligently when conflicts are found

## Simple OCaml Design

**Philosophy**: Keep it simple, straightforward, and practical. No functors, no abstract module types, just plain OCaml types and functions.

### 1. Version - Just a Record

**Purpose**: Represent semantic versions (major.minor.patch)

```ocaml
type version = {
  major : int;
  minor : int;
  patch : int;
}

val compare_version : version -> version -> int
val version_of_string : string -> (version, string) result
val version_to_string : version -> string
val bump_major : version -> version
val bump_minor : version -> version
val bump_patch : version -> version
```

Simple, straightforward. No module types needed.

### 2. Package - Just a String

**Purpose**: Identify packages

We'll just use `string` for package names. Simple and works for 99% of cases. If you need something fancier, wrap it yourself.

```ocaml
type package = string
```

### 3. Ranges - Version Sets

**Purpose**: Represent sets of versions like `>=1.0.0, <2.0.0`

```ocaml
type 'v bound = 
  | Unbounded 
  | Included of 'v 
  | Excluded of 'v

type 'v range = 'v bound * 'v bound

type 'v ranges = 'v range list

val empty : 'v ranges
val full : 'v ranges
val singleton : 'v -> 'v ranges
val higher_than : 'v -> 'v ranges
val lower_than : 'v -> 'v ranges
val between : 'v -> 'v -> 'v ranges

val complement : 'v ranges -> 'v ranges
val intersection : 'v ranges -> 'v ranges -> 'v ranges
val union : 'v ranges -> 'v ranges -> 'v ranges
val contains : 'v ranges -> 'v -> bool
```

Just a list of ranges. No fancy types, no private constructors.

### 4. Term - Package + Version Constraint

**Purpose**: A single constraint like "package@>=1.0.0" or "NOT package@<2.0.0"

```ocaml
type term = {
  package : string;
  ranges : version ranges;
  positive : bool;
}
```

Dead simple. `positive = true` means "must be in range", `positive = false` means "must NOT be in range".

### 5. Dependencies - What a Package Needs

**Purpose**: List dependencies for a specific package@version

```ocaml
type dependencies =
  | Available of (string * version ranges) list
  | Unavailable of string
```

Either we have the dependency list, or it's unavailable (with a reason).

### 6. DependencyProvider - How to Get Info

**Purpose**: Interface for querying package information

```ocaml
type 'error provider = {
  choose_version : string -> version ranges -> (version option, 'error) result;
  get_dependencies : string -> version -> (dependencies, 'error) result;
}
```

Just a record with two functions. Easy to create, easy to use.

For testing, we provide:

```ocaml
type offline_provider

val create_offline : unit -> offline_provider
val add_package : offline_provider -> string -> version -> (string * version ranges) list -> unit
val to_provider : offline_provider -> string provider
```

### 7. Incompatibility - A Conflict

**Purpose**: Track why certain combinations don't work

```ocaml
type incompatibility = {
  terms : term list;
  cause : cause;
}

and cause =
  | Root
  | Dependency of string * version
  | Conflict of incompatibility * incompatibility
```

An incompatibility says "these terms can't all be true at once". The cause explains why.

### 8. Solver - The Main Algorithm

**Purpose**: Find a solution or explain why there isn't one

```ocaml
type solution = (string * version) list

type solve_result =
  | Success of solution
  | Failure of incompatibility

val solve : 'error provider -> string -> version -> (solve_result, 'error) result
```

Usage:
```ocaml
let provider = create_my_provider () in
match solve provider "my-package" {major=1; minor=0; patch=0} with
| Ok (Success solution) -> 
    List.iter (fun (pkg, ver) -> 
      Printf.printf "%s@%s\n" pkg (version_to_string ver)
    ) solution
| Ok (Failure conflict) ->
    Printf.printf "No solution: %s\n" (explain_conflict conflict)
| Error err ->
    Printf.printf "Provider error: %s\n" err
```

## Data Structure Choices

### Rust → OCaml (Keep It Simple!)

| Rust | OCaml | Why |
|------|-------|-----|
| `Result<T, E>` | `('t, 'e) result` | Standard OCaml result |
| `Option<T>` | `'t option` | Standard OCaml option |
| `HashMap<K, V>` | `('k, 'v) HashMap.t` | From Std.Collections |
| `BTreeSet<T>` | `'t HashSet.t` | From Std.Collections |
| `Vec<T>` | `'t list` | Plain old lists work great |
| `SmallVec<[T; 1]>` | `'t list` | Lists are fine |
| String | `string` | Just use strings |

## Standard Library Usage

**CRITICAL**: Never use OCaml's `Stdlib`, `Unix`, or `Sys` modules. Always use `Std`:

```ocaml
open Std

let map = Collections.HashMap.create ()
let set = Collections.HashSet.create ()

let result = do_something ()
  |> Result.map process
  |> Result.expect ~msg:"Failed"

Log.debug "Processing package %s" pkg_name;
```

## Module Organization (Simple!)

```
packages/pubgrub/src/
├── Pubgrub.ml       # Main module with everything
└── Pubgrub.mli      # Public interface
```

That's it. One file with all the code. When it gets too big, we'll split it. But start simple.

Contents:
1. Version types and functions
2. Ranges types and functions  
3. Term, dependencies, incompatibility types
4. Provider record type
5. Offline provider
6. Solver functions
7. Error reporting

## Implementation Order

1. ✅ Package structure
2. Version type and parsing
3. Ranges implementation (the tricky part)
4. Terms and dependencies
5. Offline provider (for testing)
6. Incompatibility tracking
7. Solver algorithm
8. Error reporting
9. Examples

## Simple Example Usage

```ocaml
open Std
open Pubgrub

let () =
  let provider = create_offline () in
  
  add_package provider "root" {major=1;minor=0;patch=0} [
    ("menu", full);
    ("icons", full);
  ];
  
  add_package provider "menu" {major=1;minor=0;patch=0} [
    ("dropdown", full);
  ];
  
  add_package provider "dropdown" {major=1;minor=0;patch=0} [
    ("icons", higher_than {major=2;minor=0;patch=0});
  ];
  
  add_package provider "icons" {major=1;minor=0;patch=0} [];
  add_package provider "icons" {major=2;minor=0;patch=0} [];
  
  match solve (to_provider provider) "root" {major=1;minor=0;patch=0} with
  | Ok (Success solution) ->
      List.iter (fun (pkg, ver) ->
        Log.info "%s@%s" pkg (version_to_string ver)
      ) solution
  | Ok (Failure conflict) ->
      Log.error "No solution:\n%s" (explain_conflict conflict)
  | Error err ->
      Log.error "Error: %s" err
```

Clean. Simple. Works.

## References

- [PubGrub Algorithm](https://github.com/dart-lang/pub/blob/master/doc/solver.md)
- [Rust Implementation](https://github.com/pubgrub-rs/pubgrub)
