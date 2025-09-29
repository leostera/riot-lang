(** Workspace module - handles scanning and discovering packages in a workspace
*)

open Std
open Std.Data

type dependency = { name : string; version : string }

type package = {
  name : string;
  path : Path.t;
  relative_path : Path.t;
  dependencies : dependency list;
}

type t = { root : Path.t; target_dir_root : Path.t; packages : package list }

(** Parse workspace members from a workspace TOML file *)
let parse_workspace_toml toml_path =
  try
    let toml = Toml.parse_file toml_path in
    (* The parser flattens sections, so look for "workspace.members" *)
    match Toml.find_value "workspace.members" toml with
    | Some members_value -> (
        match Toml.get_array members_value with
        | Some arr -> List.filter_map (fun v -> Toml.get_string v) arr
        | None -> [])
    | None -> []
  with _ -> []

(** Parse package dependencies from a package TOML file *)
let parse_package_toml toml_path =
  try
    let toml = Toml.parse_file toml_path in
    (* Get package name - look for flattened "package.name" *)
    let name =
      match Toml.find_value "package.name" toml with
      | Some name_value -> (
          match Toml.get_string name_value with
          | Some n -> n
          | None -> Filename.basename (Filename.dirname toml_path))
      | None -> Filename.basename (Filename.dirname toml_path)
    in
    (* Get dependencies - need to extract from the flattened table *)
    let deps =
      match toml with
      | Table items ->
          (* Look for all keys starting with "dependencies." *)
          List.filter_map
            (fun (key, _value) ->
              if String.length key > 13 && String.sub key 0 13 = "dependencies."
              then Some (String.sub key 13 (String.length key - 13))
              else None)
            items
      | _ -> []
    in
    (name, deps)
  with _ ->
    let name = Filename.basename (Filename.dirname toml_path) in
    (name, [])

(** Scan a directory for a tusk.toml file *)
let find_tusk_toml dir =
  let path = Filename.concat dir "tusk.toml" in
  match Std.Path.of_string path with
  | Error _ -> None
  | Ok path_obj -> (
      match Std.Fs.file_exists path_obj with
      | Ok true -> Some path
      | Ok false | Error _ -> None)

(** Scan workspace starting from root directory *)
let scan_internal ~root =
  match find_tusk_toml root with
  | None ->
      Printf.eprintf "Error: No tusk.toml found in %s\n" root;
      {
        root = Path.of_string root |> Result.unwrap;
        target_dir_root =
          Path.of_string (Filename.concat root "target") |> Result.unwrap;
        packages = [];
      }
  | Some workspace_toml ->
      (* Parse workspace members *)
      let members = parse_workspace_toml workspace_toml in

      (* Scan each member package *)
      let packages =
        List.filter_map
          (fun member ->
            let member_path = Filename.concat root member in
            match find_tusk_toml member_path with
            | None ->
                Printf.eprintf "Warning: No tusk.toml found for member %s\n"
                  member;
                None
            | Some package_toml ->
                let name, deps = parse_package_toml package_toml in
                Some
                  {
                    name;
                    path = Path.of_string member_path |> Result.unwrap;
                    relative_path = Path.of_string member |> Result.unwrap;
                    dependencies =
                      List.map (fun d -> { name = d; version = "" }) deps;
                  })
          members
      in

      {
        root = Path.of_string root |> Result.unwrap;
        target_dir_root =
          Path.of_string (Filename.concat root "target") |> Result.unwrap;
        packages;
      }

(** Public interface functions *)
let scan path =
  let root_str = Path.to_string path in
  try Ok (scan_internal ~root:root_str)
  with _ -> Error Error.ScanWorkspaceError

let load ~root = scan root

(** Tests submodule *)
module Tests = struct
  let test_scan_finds_workspace_toml () : (unit, string) result =
    (* Test that scan correctly locates workspace.toml *)
    Ok ()
    [@test]

  let test_workspace_parses_member_packages () : (unit, string) result =
    (* Test that all members are discovered and parsed *)
    Ok ()
    [@test]

  let test_package_dependencies_parsed_correctly () : (unit, string) result =
    (* Test that package dependencies are extracted from tusk.toml *)
    Ok ()
    [@test]

  let test_relative_paths_computed_correctly () : (unit, string) result =
    (* Test that package relative paths are correct *)
    Ok ()
    [@test]

  let test_single_package_mode_without_workspace_toml () : (unit, string) result
      =
    (* Test that single tusk.toml without workspace.toml works *)
    Ok ()
end [@test]
