open Std
open Std.Collections

(**
   Module Scanner - Recursively scans directories for source files

   This module provides type-safe directory scanning that: 1. Tags files by
   type (ML, MLI, C, H) instead of using string extensions 2. Sorts entries for
   deterministic compilation order 3. Returns paths relative to the source
   directory

   The sorting order ensures:
   - .mli files come before .ml files (interfaces must be compiled first)
   - Directories come last (allows processing files before descending) 
*)
type entry =
  | ML of string * Path.t
  | MLI of string * Path.t
  | C of string * Path.t
  | H of string * Path.t
  | Other of string * Path.t * string
  | Dir of string * Path.t * entry list

(**
   Compare entries for sorting.

   Sort order: 1. MLI files first (must compile interfaces before
   implementations) 2. ML files second 3. C and H files 4. Other files 5.
   Directories last

   This ensures proper OCaml compilation order and allows processing all files
   in a directory before descending into subdirectories. 
*)
let compare_entries = fun e1 e2 ->
  let get_name = function
    | ML (n, _) | MLI (n, _) | C (n, _) | H (n, _) | Other (n, _, _) | Dir (n, _, _) -> n
  in
  let get_priority = function
    | MLI _ -> 0
    | ML _ -> 1
    | C _ -> 2
    | H _ -> 3
    | Other _ -> 4
    | Dir _ -> 5
  in
  match get_priority e1, get_priority e2 with
  | p1, p2 when p1 != p2 -> Int.compare p1 p2
  | _ -> String.compare (get_name e1) (get_name e2)

(**
   Recursively scan a directory and build a hierarchical entry list.

   Parameters:
   - from_dir: Absolute path to scan (e.g., /abs/path/project/src)
   - rel_path: Relative path for entries (e.g., src)

   The function maintains two paths:
   - from_dir: Used for filesystem operations (absolute)
   - rel_path: Stored in entries (relative to package root)

   This allows the rest of the build system to work with relative paths while
   filesystem operations use absolute paths.

   Returns entries sorted by type (MLI, ML, C, H, Other, Dir). 
*)
let rec scan_directory = fun ~from_dir ~rel_path ->
  match Fs.read_dir from_dir with
  | Error _ -> []
  | Ok iter ->
      let entries = Std.Iter.MutIterator.to_list iter in
      let scanned = List.map entries ~fn:(
        fun entry ->
          let source_path = Path.(from_dir / entry) in
          let entry_rel_path = Path.(rel_path / entry) in
          let name = Path.basename entry in
          match Fs.is_dir source_path with
          | Ok true ->
              let children = scan_directory ~from_dir:source_path ~rel_path:entry_rel_path in
              [
                Dir (name, entry_rel_path, children);
              ]
          | Ok false -> (
            match Path.extension source_path with
            | Some ".ml" ->
                [
                  ML (name, entry_rel_path);
                ]
            | Some ".mli" ->
                [
                  MLI (name, entry_rel_path);
                ]
            | Some ".c" ->
                [
                  C (name, entry_rel_path);
                ]
            | Some ".h" ->
                [
                  H (name, entry_rel_path);
                ]
            | Some ext ->
                [
                  Other (name, entry_rel_path, ext);
                ]
            | None ->
                [
                  Other (name, entry_rel_path, "");
                ]
          )
          | Error _ -> []
      ) |> List.concat in List.sort scanned ~compare:compare_entries

(**
   Scan a source directory relative to project root.

   Example: root = /home/user/project source_dir = src

   This scans /home/user/project/src and returns entries with paths like
   "src/main.ml", "src/utils/helper.ml" (relative to project root). 
*)
let scan = fun ~root ~source_dir ->
  let dir = Path.(root / source_dir) in scan_directory ~from_dir:dir ~rel_path:source_dir
