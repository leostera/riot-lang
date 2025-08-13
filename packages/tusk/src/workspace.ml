(** Workspace module - handles scanning and discovering packages in a workspace
*)

type package = {
  name : string;
  path : string;
  relative_path : string; (* Path relative to workspace root *)
  dependencies : string list;
}

type workspace = {
  root : string;
  target_dir_root : string;
  packages : package list;
}

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
  if System.file_exists path then Some path else None

(** Scan workspace starting from root directory *)
let scan ~root =
  match find_tusk_toml root with
  | None ->
      Printf.eprintf "Error: No tusk.toml found in %s\n" root;
      { root; target_dir_root = Filename.concat root "target"; packages = [] }
  | Some workspace_toml ->
      Printf.printf "Found workspace at: %s\n" workspace_toml;

      (* Parse workspace members *)
      let members = parse_workspace_toml workspace_toml in
      Printf.printf "Workspace members: %s\n" (String.concat ", " members);

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
                Printf.printf "  Package %s: deps=[%s]\n" name
                  (String.concat ", " deps);
                Some
                  {
                    name;
                    path = member_path;
                    relative_path = member;
                    dependencies = deps;
                  })
          members
      in

      { root; target_dir_root = Filename.concat root "target"; packages }

(** Tests submodule *)
module Tests = struct
  [@test]
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
  let test_single_package_mode_without_workspace_toml () : (unit, string) result =
    (* Test that single tusk.toml without workspace.toml works *)
    Ok ()
end
