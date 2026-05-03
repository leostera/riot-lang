(**
   Difference computation for data structures.

   This module provides a protocol for computing differences between values of
   the same type, with deep comparison support for nested structures.

   ## Overview

   The Diff module defines:
   - {!change} - Type representing additions, removals, and changes
   - {!diff} - Diff result with path information for nested structures
   - {!Diffable} - Protocol for types that can be diffed
   - Helper functions for filtering and querying diff results

   ## Quick Start

   ```ocaml open Std

   let o1 = Data.Json.obj
   [ ("user", Data.Json.obj [ ("name", Data.Json.string "Alice"); ("age",
    Data.Json.int 30) ]) ]

   let o2 = Data.Json.obj
   [ ("user", Data.Json.obj [ ("name", Data.Json.string "Alice"); ("age",
    Data.Json.int 31) ]) ]

   let diff = Data.Json.diff o1 o2 (*
   [{ path = ["user"; "age"]; change = Changed (Int 30, Int 31) }] *)

   let has_changes = Diff.has_changes diff (* true *)

   let changes = Diff.changes diff (*
   [{ path = ["user"; "age"]; change = Changed (Int 30, Int 31) }] *) ```

   ## Path Tracking

   Each diff result includes a path showing where in the nested structure the
   difference occurred:

   - Empty list `[]` for top-level differences
   - Object field names for object differences: `["user"; "name"]`
   - Array indices as strings for array differences: `["users"; "0"; "name"]`
*)
type path_component =
  | Key of string
  | Index of int
type path = path_component list
type 'value kind =
  | Added of 'value
  | Removed of 'value
  | Changed of 'value * 'value
type 'value change = {
  path: path;
  kind: 'value kind;
}
type 'value diff =
  | Equal
  | Diff of 'value change list

val has_changes: 'a change list -> bool

val additions: 'a change list -> 'a change list

val removals: 'a change list -> 'a change list

val changes: 'a change list -> 'a change list

val at_path: path -> 'a change list -> 'a change list
