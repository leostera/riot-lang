open Std
open Model

(** Workspace manager - handles scanning and caching workspace state *)

type cached_workspace = {
  workspace : Workspace.t;
  root : string;
  last_scanned : Datetime.t;
}

let workspace_cache = ref None
let cache_ttl = 30.0

let is_within_workspace current_dir cached_root =
  String.starts_with ~prefix:cached_root current_dir

let is_cache_valid cached =
  let now = Datetime.now () in
  let elapsed =
    Datetime.to_timestamp now -. Datetime.to_timestamp cached.last_scanned
  in
  elapsed < cache_ttl

let clear_cache () = workspace_cache := None

let get_cached_root () =
  match !workspace_cache with Some cached -> Some cached.root | None -> None

(** Finding workspace TOML files *)

module Find = struct
  let tusk_toml_in_dir (dir : Path.t) : Path.t option =
    let path = Path.(dir / Path.v "tusk.toml") in
    Log.debug "[WORKSPACE] Looking for tusk.toml at: %s" (Path.to_string path);
    match Fs.exists path with
    | Ok true ->
        Log.debug "[WORKSPACE] Found tusk.toml";
        Some path
    | Ok false ->
        Log.debug "[WORKSPACE] tusk.toml does not exist";
        None
    | Error (Fs.SystemError msg) ->
        Log.debug "[WORKSPACE] Error checking for tusk.toml: %s" msg;
        None

  let is_workspace_toml (toml_path : Path.t) : bool =
    Log.debug "[WORKSPACE] Checking if %s is a workspace root"
      (Path.to_string toml_path);
    match Fs.read_to_string toml_path with
    | Error _ ->
        Log.debug "[WORKSPACE] Failed to read %s" (Path.to_string toml_path);
        false
    | Ok content -> (
        match Data.Toml.parse content with
        | Ok (Data.Toml.Table items) ->
            let has_workspace = List.assoc_opt "workspace" items <> None in
            Log.debug "[WORKSPACE] %s %s a workspace root"
              (Path.to_string toml_path)
              (if has_workspace then "IS" else "IS NOT");
            has_workspace
        | _ ->
            Log.debug "[WORKSPACE] Failed to parse %s"
              (Path.to_string toml_path);
            false)

  let rec workspace_root (start_dir : Path.t) : Path.t option =
    Log.debug "[WORKSPACE] Searching for workspace root from: %s"
      (Path.to_string start_dir);
    let tusk_toml = Path.(start_dir / Path.v "tusk.toml") in
    match Fs.exists tusk_toml with
    | Ok true ->
        if is_workspace_toml tusk_toml then (
          Log.debug "[WORKSPACE] Found workspace root at: %s"
            (Path.to_string start_dir);
          Some start_dir)
        else (
          Log.debug "[WORKSPACE] Found package toml, walking up...";
          match Path.parent start_dir with
          | Some parent when parent <> start_dir -> workspace_root parent
          | _ ->
              Log.debug "[WORKSPACE] Reached filesystem root";
              None)
    | Ok false | Error _ -> (
        Log.debug "[WORKSPACE] No tusk.toml here, walking up...";
        match Path.parent start_dir with
        | Some parent when parent <> start_dir -> workspace_root parent
        | _ ->
            Log.debug "[WORKSPACE] Reached filesystem root";
            None)
end

(** Consolidating workspace + packages *)

module Consolidate = struct
  let load_member_package (workspace_root : Path.t) (member : string)
      ~(workspace_deps : Package.dependency list) : Package.t option =
    Log.debug "[WORKSPACE] Loading member: %s" member;
    let member_path = Path.(workspace_root / Path.v member) in
    let toml_path = Path.(member_path / Path.v "tusk.toml") in
    match Fs.exists toml_path with
    | Ok true -> (
        match Fs.read_to_string toml_path with
        | Error _ ->
            Log.error "[WORKSPACE] Failed to read %s" (Path.to_string toml_path);
            None
        | Ok content -> (
            match Data.Toml.parse content with
            | Error err ->
                Log.error "[WORKSPACE] Failed to parse %s: %s"
                  (Path.to_string toml_path)
                  (Data.Toml.error_to_string err);
                None
            | Ok toml -> (
                let relative_path = Path.v member in
                match
                  Package.from_toml toml ~workspace_deps ~path:member_path
                    ~relative_path
                with
                | Ok pkg ->
                    Log.debug "[WORKSPACE] Loaded package: %s" pkg.name;
                    Some pkg
                | Error msg ->
                    Log.error "[WORKSPACE] Failed to parse package %s: %s"
                      member msg;
                    None)))
    | _ ->
        Log.error "[WORKSPACE] No tusk.toml found for member: %s" member;
        None

  let rec load_external_package (workspace_root : Path.t)
      (dep : Package.dependency) ~(seen : string list ref) : Package.t list =
    match dep.source with
    | Workspace -> []
    | Path dep_path ->
        if List.mem dep.name !seen then []
        else (
          seen := dep.name :: !seen;
          Log.debug "[WORKSPACE] Loading external package: %s from %s" dep.name
            (Path.to_string dep_path);
          let abs_path = Path.(workspace_root / dep_path) in
          let toml_path = Path.(abs_path / Path.v "tusk.toml") in
          match Fs.exists toml_path with
          | Ok true -> (
              match Fs.read_to_string toml_path with
              | Error _ ->
                  Log.error "[WORKSPACE] Failed to read external package: %s"
                    dep.name;
                  []
              | Ok content -> (
                  match Data.Toml.parse content with
                  | Error err ->
                      Log.error
                        "[WORKSPACE] Failed to parse external package %s: %s"
                        dep.name
                        (Data.Toml.error_to_string err);
                      []
                  | Ok toml -> (
                      let rel_path =
                        let abs_str = Path.to_string abs_path in
                        let root_str = Path.to_string workspace_root in
                        if String.starts_with ~prefix:root_str abs_str then
                          String.sub abs_str
                            (String.length root_str + 1)
                            (String.length abs_str - String.length root_str - 1)
                        else abs_str
                      in
                      let relative_path = Path.v rel_path in
                      match
                        Package.from_toml toml ~workspace_deps:[] ~path:abs_path
                          ~relative_path
                      with
                      | Ok pkg ->
                          Log.debug
                            "[WORKSPACE] Loaded external package: %s at %s"
                            pkg.name (Path.to_string abs_path);
                          (* Load transitive dependencies - their paths are relative to this package's directory *)
                          let transitive_deps =
                            List.map
                              (fun (dep : Package.dependency) ->
                                match dep.source with
                                | Workspace -> dep
                                | Path rel_path ->
                                    (* Resolve relative to this package's directory, then relative to workspace root *)
                                    let resolved_path =
                                      Path.(abs_path / rel_path)
                                    in
                                    { dep with source = Path resolved_path })
                              pkg.dependencies
                          in
                          let transitive =
                            List.concat_map
                              (load_external_package workspace_root ~seen)
                              transitive_deps
                          in
                          pkg :: transitive
                      | Error msg ->
                          Log.error
                            "[WORKSPACE] Failed to parse external package: %s"
                            msg;
                          [])))
          | _ ->
              Log.error "[WORKSPACE] External package not found: %s at %s"
                dep.name (Path.to_string dep_path);
              [])

  let build (workspace_root : Path.t) (workspace_toml : Workspace.manifest) :
      Workspace.t =
    Log.debug "[WORKSPACE] Building workspace with %d members"
      (List.length workspace_toml.members);
    Log.debug "[WORKSPACE] Workspace has %d dependencies"
      (List.length workspace_toml.dependencies);

    (* Load member packages *)
    let member_packages =
      List.filter_map
        (fun member ->
          load_member_package workspace_root member
            ~workspace_deps:workspace_toml.dependencies)
        workspace_toml.members
    in

    Log.debug "[WORKSPACE] Loaded %d member packages"
      (List.length member_packages);

    (* Load external packages *)
    let seen = ref (List.map (fun (p : Package.t) -> p.name) member_packages) in
    let external_packages =
      List.concat_map
        (fun (pkg : Package.t) ->
          List.concat_map
            (load_external_package workspace_root ~seen)
            pkg.dependencies)
        member_packages
    in

    Log.debug "[WORKSPACE] Loaded %d external packages"
      (List.length external_packages);

    let all_packages = member_packages @ external_packages in
    Log.debug "[WORKSPACE] Total packages: %d" (List.length all_packages);

    Workspace.make ~root:workspace_root ~packages:all_packages
end

(** Public API *)

let scan (path : Path.t) : (Workspace.t, Error.t) result =
  Log.debug "[WORKSPACE] Scanning from: %s" (Path.to_string path);
  try
    match Find.workspace_root path with
    | None ->
        Log.debug "[WORKSPACE] No workspace root found";
        Error Error.ScanWorkspaceError
    | Some workspace_root -> (
        Log.debug "[WORKSPACE] Found workspace root: %s"
          (Path.to_string workspace_root);
        let toml_path = Path.(workspace_root / Path.v "tusk.toml") in
        match Fs.read_to_string toml_path with
        | Error _ ->
            Log.error "[WORKSPACE] Failed to read workspace TOML";
            Error Error.ScanWorkspaceError
        | Ok content -> (
            match Data.Toml.parse content with
            | Error err ->
                Log.error "[WORKSPACE] Failed to parse workspace TOML: %s"
                  (Data.Toml.error_to_string err);
                Error Error.ScanWorkspaceError
            | Ok toml -> (
                match Workspace.manifest_from_toml toml with
                | Error msg ->
                    Log.error "[WORKSPACE] Failed to parse workspace TOML: %s"
                      msg;
                    Error Error.ScanWorkspaceError
                | Ok workspace_toml ->
                    let workspace =
                      Consolidate.build workspace_root workspace_toml
                    in
                    Log.debug "[WORKSPACE] Loaded %d packages"
                      (List.length workspace.packages);
                    Ok workspace)))
  with exn ->
    Log.error "[WORKSPACE] Scan failed: %s" (Exception.to_string exn);
    Error Error.ScanWorkspaceError

let load ~root = scan root
