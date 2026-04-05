open Std
open Std.Collections
open Std.Sync
open Std.Sync.Cell

let riot_toml = Path.v "riot.toml"

type load_error =
  | PackageNotFound of {
      dependant: string option;  (* None for workspace-level deps *)
      package: string;
      path: string;
    }
  | PackageTomlReadFailed of { package: string; path: string }
  | PackageTomlParseFailed of { package: string; path: string }
  | PackageFromTomlFailed of { package: string; path: string; error: string }

type t = {
  workspace_roots: (string, Path.t option) HashMap.t;
  manifests: (string, (Data.Toml.value, string) result) HashMap.t;
  scans: (string, ((Workspace.t * load_error list), string) result) HashMap.t;
}

let create = fun () ->
  { workspace_roots = HashMap.create (); manifests = HashMap.create (); scans = HashMap.create () }

let path_key = fun path -> Path.to_string path

let load_riot_toml = fun t manifest_path ->
  let key = path_key manifest_path in
  match HashMap.get t.manifests key with
  | Some result -> result
  | None ->
      let result =
        match Fs.read_to_string manifest_path with
        | Error err -> Error ("failed to read manifest '"
        ^ Path.to_string manifest_path
        ^ "': "
        ^ IO.error_message err)
        | Ok content -> (
            match Data.Toml.parse content with
            | Error err -> Error ("failed to parse manifest '"
            ^ Path.to_string manifest_path
            ^ "': "
            ^ Data.Toml.error_to_string err)
            | Ok toml -> Ok toml
          )
      in
      let _ = HashMap.insert t.manifests key result in
      result

let rec find_workspace_root: t -> Path.t -> Path.t option = fun t start_dir ->
  let key = path_key start_dir in
  match HashMap.get t.workspace_roots key with
  | Some root -> root
  | None ->
      let riot_toml = Path.(start_dir / riot_toml) in
      let result =
        match Fs.exists riot_toml with
        | Ok true -> (
            match load_riot_toml t riot_toml with
            | Ok (Data.Toml.Table items) ->
                let has_workspace = List.assoc_opt "workspace" items != None in
                if has_workspace then
                  Some start_dir
                else
                  (
                    match Path.parent start_dir with
                    | Some parent when parent != start_dir -> find_workspace_root t parent
                    | _ -> None
                  )
            | Ok _
            | Error _ -> None
          )
        | Ok false
        | Error _ -> (
            match Path.parent start_dir with
            | Some parent when parent != start_dir -> find_workspace_root t parent
            | _ -> None
          )
      in
      let _ = HashMap.insert t.workspace_roots key result in
      result

let load_member_package:
  t ->
  Path.t ->
  string ->
  workspace_deps:Package.dependency list ->
  workspace_dev_deps:Package.dependency list ->
  workspace_build_deps:Package.dependency list ->
  (Package.t option * load_error list) = fun t workspace_root member ~workspace_deps ~workspace_dev_deps ~workspace_build_deps ->
  let member_path = Path.(workspace_root / Path.v member) in
  let toml_path = Path.(member_path / riot_toml) in
  let member_name = Path.basename member_path in
  match Fs.exists toml_path with
  | Ok true -> (
      match load_riot_toml t toml_path with
      | Error err when String.starts_with ~prefix:"failed to read" err ->
          (
            None,
            [ PackageTomlReadFailed { package = member_name; path = Path.to_string member_path } ]
          )
      | Error _ ->
          (
            None,
            [ PackageTomlParseFailed { package = member_name; path = Path.to_string member_path } ]
          )
      | Ok toml -> (
          let relative_path = Path.v member in
          match Package.from_toml
            toml
            ~workspace_deps
            ~workspace_dev_deps
            ~workspace_build_deps
            ~path:member_path
            ~relative_path with
          | Ok pkg -> (Some pkg, [])
          | Error error -> (
            None,
            [
              PackageFromTomlFailed {
                package = member_name;
                path = Path.to_string member_path;
                error
              }
            ]
          )
        )
    )
  | _ -> (
    None,
    [
      PackageNotFound { dependant = None; package = member_name; path = Path.to_string member_path }
    ]
  )

let resolve_dependency_root = fun ~declared_from dep_path ->
  if Path.is_absolute dep_path then
    dep_path
  else
    Path.(declared_from / dep_path)

let dependency_has_external_fallback = fun (dep: Package.dependency) ->
  Option.is_some dep.source.source_locator || Option.is_some dep.source.version

let rec load_external_package:
  t ->
  Path.t ->
  declared_from:Path.t ->
  Package.dependency ->
  seen:string list Cell.t ->
  workspace_deps:Package.dependency list ->
  workspace_dev_deps:Package.dependency list ->
  workspace_build_deps:Package.dependency list ->
  dependant:string option ->
  (Package.t list * load_error list) = fun t workspace_root ~declared_from dep ~seen ~workspace_deps ~workspace_dev_deps ~workspace_build_deps ~dependant ->
  match dep.source with
  | { workspace=true; _ } ->
      ([], [])
  | { builtin=true; _ } ->
      ([], [])
  | { path=None; _ } ->
      ([], [])
  | { path=Some dep_path; _ } ->
      if List.mem dep.name !seen then
        ([], [])
      else (
        let abs_path = resolve_dependency_root ~declared_from dep_path in
        let toml_path = Path.(abs_path / riot_toml) in
        let path_str = Path.to_string dep_path in
        match Fs.exists toml_path with
        | Ok false when dependency_has_external_fallback dep ->
            ([], [])
        | Ok true -> (
            seen := dep.name :: !seen;
            match load_riot_toml t toml_path with
            | Error err when String.starts_with ~prefix:"failed to read" err ->
                ([], [ PackageTomlReadFailed { package = dep.name; path = path_str } ])
            | Error _ ->
                ([], [ PackageTomlParseFailed { package = dep.name; path = path_str } ])
            | Ok toml -> (
                let rel_path =
                  let abs_str = Path.to_string abs_path in
                  let root_str = Path.to_string workspace_root in
                  if String.starts_with ~prefix:root_str abs_str then
                    String.sub
                      abs_str
                      (String.length root_str + 1)
                      (String.length abs_str - String.length root_str - 1)
                  else
                    abs_str
                in
                let relative_path = Path.v rel_path in
                match Package.from_toml
                  toml
                  ~workspace_deps
                  ~workspace_dev_deps
                  ~workspace_build_deps
                  ~path:abs_path
                  ~relative_path with
                | Ok pkg ->
                    let transitive_results = List.map
                      (load_external_package
                        t
                        workspace_root
                        ~declared_from:abs_path
                        ~seen
                        ~workspace_deps
                        ~workspace_dev_deps
                        ~workspace_build_deps
                        ~dependant:(Some pkg.name))
                      (Package.all_dependencies pkg) in
                    let transitive_pkgs = List.concat_map fst transitive_results in
                    let transitive_errs = List.concat_map snd transitive_results in
                    (pkg :: transitive_pkgs, transitive_errs)
                | Error error -> (
                  [],
                  [ PackageFromTomlFailed { package = dep.name; path = path_str; error } ]
                )
              )
          )
        | _ ->
            seen := dep.name :: !seen;
            ([], [ PackageNotFound { dependant; package = dep.name; path = path_str } ])
      )

let build_workspace: t -> Path.t -> Workspace.manifest -> (Workspace.t * load_error list) = fun t workspace_root workspace_manifest ->
  let member_results =
    List.map
      (fun member ->
        load_member_package
          t
          workspace_root
          (Path.to_string member)
          ~workspace_deps:workspace_manifest.dependencies
          ~workspace_dev_deps:workspace_manifest.dev_dependencies
          ~workspace_build_deps:workspace_manifest.build_dependencies)
      workspace_manifest.members
  in
  let member_packages = List.filter_map fst member_results in
  let member_errors = List.concat_map snd member_results in
  let seen =
    Cell.create (List.map (fun (p: Package.t) -> p.name) member_packages)
  in
  (* Load workspace-level dependencies first *)
  let workspace_results = List.map
    (load_external_package
      t
      workspace_root
      ~declared_from:workspace_root
      ~seen
      ~workspace_deps:workspace_manifest.dependencies
      ~workspace_dev_deps:workspace_manifest.dev_dependencies
      ~workspace_build_deps:workspace_manifest.build_dependencies
      ~dependant:None)
    (workspace_manifest.dependencies @ workspace_manifest.dev_dependencies @ workspace_manifest.build_dependencies) in
  let workspace_packages = List.concat_map fst workspace_results in
  let workspace_errors = List.concat_map snd workspace_results in
  (* Then load any additional dependencies from member packages *)
  let external_results =
    List.concat_map
      (fun (pkg: Package.t) ->
        List.map
          (load_external_package
            t
            workspace_root
            ~declared_from:pkg.path
            ~seen
            ~workspace_deps:workspace_manifest.dependencies
            ~workspace_dev_deps:workspace_manifest.dev_dependencies
            ~workspace_build_deps:workspace_manifest.build_dependencies
            ~dependant:(Some pkg.name))
          (Package.all_dependencies pkg))
      member_packages
  in
  let external_packages = List.concat_map fst external_results in
  let external_errors = List.concat_map snd external_results in
  let all_packages = member_packages @ workspace_packages @ external_packages in
  let all_errors = member_errors @ workspace_errors @ external_errors in
  (
    Workspace.make
      ~root:workspace_root
      ~packages:all_packages
      ~dependencies:workspace_manifest.dependencies
      ~dev_dependencies:workspace_manifest.dev_dependencies
      ~build_dependencies:workspace_manifest.build_dependencies
      ~profile_overrides:workspace_manifest.profile_overrides
      ?target_dir:workspace_manifest.target_dir
      (),
    all_errors
  )

let scan: t -> Path.t -> ((Workspace.t * load_error list), string) result = fun t path ->
  try
    match find_workspace_root t path with
    | None -> Error "No workspace root found"
    | Some workspace_root ->
        let key = path_key workspace_root in
        match HashMap.get t.scans key with
        | Some result -> result
        | None ->
            let toml_path = Path.(workspace_root / riot_toml) in
            let result =
              match load_riot_toml t toml_path with
              | Error err when String.starts_with ~prefix:"failed to read" err ->
                  Error "Failed to read workspace TOML"
              | Error err ->
                  Error ("Failed to parse workspace TOML: " ^ err)
              | Ok toml -> (
                  match Workspace.of_toml toml with
                  | Error msg -> Error ("Failed to parse workspace manifest: " ^ msg)
                  | Ok workspace_manifest ->
                      let (workspace, errors) = build_workspace t workspace_root workspace_manifest in
                      Ok (workspace, errors)
                )
            in
            let _ = HashMap.insert t.scans key result in
            result
  with
  | exn -> Error ("Scan failed: " ^ Exception.to_string exn)

let load = fun t ~root -> scan t root

let load_error_to_string = function
  | PackageNotFound { dependant; package; path } ->
      let dep_str =
        match dependant with
        | None -> "workspace"
        | Some name -> "package '" ^ name ^ "'"
      in
      dep_str ^ ": could not find riot.toml for '" ^ package ^ "' at path " ^ path
  | PackageTomlReadFailed { package; path } ->
      "package '" ^ package ^ "': failed to read riot.toml at path " ^ path
  | PackageTomlParseFailed { package; path } ->
      "package '" ^ package ^ "': failed to parse riot.toml at path " ^ path
  | PackageFromTomlFailed { package; path; error } ->
      "package '" ^ package ^ "': failed to load from riot.toml at path " ^ path ^ ": " ^ error
