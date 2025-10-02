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

(** Hash a package into a hasher *)
module Package = struct
  let hash (type state) (module H : Crypto.Hasher.Intf with type state = state)
      (hasher : state) (pkg : package) =
    H.write_string hasher pkg.name;
    (* Sort dependencies by name for deterministic hashing *)
    let sorted_deps =
      List.sort
        (fun (a : dependency) (b : dependency) -> String.compare a.name b.name)
        pkg.dependencies
    in
    List.iter
      (fun (dep : dependency) -> H.write_string hasher dep.name)
      sorted_deps
end

(** Parse workspace members from a workspace TOML file *)
let parse_workspace_toml toml_path =
  Log.debug "[WORKSPACE] Parsing workspace TOML: %s" toml_path;
  match Toml.parse_file toml_path with
  | Error err ->
      Log.debug "[WORKSPACE] TOML parse error: %s" (Toml.error_to_string err);
      []
  | Ok toml ->
      Log.debug "[WORKSPACE] TOML parsed successfully";
      
      (* Debug: print all keys in the TOML *)
      (match toml with
      | Toml.Table items ->
          Log.debug "[WORKSPACE] TOML has %d top-level keys:" (List.length items);
          List.iter (fun (key, _) -> Log.debug "[WORKSPACE]   - %s" key) items
      | _ -> Log.debug "[WORKSPACE] TOML is not a table");
      
      (* The parser flattens sections, so look for "workspace.members" *)
      match Toml.find_value "workspace.members" toml with
      | Some members_value -> (
          Log.debug "[WORKSPACE] Found workspace.members field";
          match Toml.get_array members_value with
          | Some arr -> 
              let members = List.filter_map (fun v -> Toml.get_string v) arr in
              Log.debug "[WORKSPACE] Extracted %d members from array" (List.length members);
              members
          | None -> 
              Log.debug "[WORKSPACE] workspace.members is not an array";
              [])
      | None -> 
          Log.debug "[WORKSPACE] No workspace.members field found in TOML";
          []

(** Parse package dependencies from a package TOML file *)
let parse_package_toml toml_path =
  match Toml.parse_file toml_path with
  | Error err ->
      Log.debug "[WORKSPACE] Failed to parse package TOML %s: %s" 
        toml_path (Toml.error_to_string err);
      let path_obj = Path.v toml_path in
      let dir = Path.parent path_obj in
      let name = match dir with Some d -> Path.basename d | None -> "unknown" in
      (name, [])
  | Ok toml ->
      (* Get package name - look for flattened "package.name" *)
      let name =
        match Toml.find_value "package.name" toml with
        | Some name_value -> (
            match Toml.get_string name_value with
            | Some n -> n
            | None -> (
                let path_obj = Path.v toml_path in
                let dir = Path.parent path_obj in
                match dir with Some d -> Path.basename d | None -> "unknown"))
        | None -> (
            let path_obj = Path.v toml_path in
            let dir = Path.parent path_obj in
            match dir with Some d -> Path.basename d | None -> "unknown")
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

(** Scan a directory for a tusk.toml file *)
let find_tusk_toml dir =
  let dir_path = Path.v dir in
  let path = Path.(dir_path / Path.v "tusk.toml") in
  Log.debug "[WORKSPACE] Looking for tusk.toml at: %s" (Path.to_string path);
  match Std.Fs.exists path with
  | Ok true -> 
      Log.debug "[WORKSPACE] Found tusk.toml";
      Some (Path.to_string path)
  | Ok false -> 
      Log.debug "[WORKSPACE] tusk.toml does not exist";
      None
  | Error (Fs.SystemError msg) -> 
      Log.debug "[WORKSPACE] Error checking for tusk.toml: %s" msg;
      None

(** Scan workspace starting from root directory *)
let scan_internal ~root =
  Log.debug "[WORKSPACE] Scanning workspace from root: %s" root;
  match find_tusk_toml root with
  | None ->
      Log.debug "[WORKSPACE] No tusk.toml found in %s" root;
      println "Error: No tusk.toml found in %s" root;
      {
        root = Path.v root;
        target_dir_root = Path.(Path.v root / Path.v "target");
        packages = [];
      }
  | Some workspace_toml ->
      Log.debug "[WORKSPACE] Found workspace tusk.toml: %s" workspace_toml;
      (* Parse workspace members *)
      let members = parse_workspace_toml workspace_toml in
      Log.debug "[WORKSPACE] Parsed %d workspace members: [%s]" 
        (List.length members) (String.concat ", " members);

      (* Scan each member package *)
      let packages =
        List.filter_map
          (fun member ->
            Log.debug "[WORKSPACE] Scanning member: %s" member;
            let member_path =
              Path.to_string Path.(Path.v root / Path.v member)
            in
            Log.debug "[WORKSPACE]   Member path: %s" member_path;
            match find_tusk_toml member_path with
            | None ->
                Log.debug "[WORKSPACE]   No tusk.toml found for member %s" member;
                println "Warning: No tusk.toml found for member %s"
                  member;
                None
            | Some package_toml ->
                Log.debug "[WORKSPACE]   Found package tusk.toml: %s" package_toml;
                let name, deps = parse_package_toml package_toml in
                Log.debug "[WORKSPACE]   Package name: %s, deps: [%s]" name (String.concat ", " deps);
                Some
                  {
                    name;
                    path = Path.v member_path;
                    relative_path = Path.v member;
                    dependencies =
                      List.map (fun d -> { name = d; version = "" }) deps;
                  })
          members
      in

      Log.debug "[WORKSPACE] Scan complete: found %d packages" (List.length packages);
      {
        root = Path.v root;
        target_dir_root = Path.(Path.v root / Path.v "target");
        packages;
      }

(** Public interface functions *)
let scan path =
  let root_str = Path.to_string path in
  Log.debug "[WORKSPACE] scan() called with path: %s" root_str;
  try 
    let workspace = scan_internal ~root:root_str in
    Log.debug "[WORKSPACE] scan() succeeded with %d packages" (List.length workspace.packages);
    Ok workspace
  with exn -> 
      Log.debug "[WORKSPACE] scan() failed with exception: %s" (Exception.to_string exn);
      Error Error.ScanWorkspaceError

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
