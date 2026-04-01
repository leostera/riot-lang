open Std
open Std.Collections
open Std.Sync
open Std.Sync.Cell

let tusk_toml = Path.v "tusk.toml"

type load_error =
  | PackageNotFound of {
      dependant: string option;  (* None for workspace-level deps *)
      package: string;
      path: string;
    }
  | PackageTomlReadFailed of { package: string; path: string }
  | PackageTomlParseFailed of { package: string; path: string }
  | PackageFromTomlFailed of { package: string; path: string }

let rec find_workspace_root : Path.t -> Path.t option = fun start_dir ->
  let tusk_toml = Path.(start_dir / tusk_toml) in
  match Fs.exists tusk_toml with
  | Ok true -> (
      match Fs.read_to_string tusk_toml with
      | Error _ -> None
      | Ok content -> (
          match Data.Toml.parse content with
          | Ok (Data.Toml.Table items) -> (
              let has_workspace = List.assoc_opt "workspace" items != None in
              if has_workspace then
                Some start_dir
              else
                match Path.parent start_dir with
                | Some parent when parent != start_dir -> find_workspace_root parent
                | _ -> None
            )
          | _ -> None
        )
    )
  | Ok false
  | Error _ -> (
      match Path.parent start_dir with
      | Some parent when parent != start_dir -> find_workspace_root parent
      | _ -> None
    )

let load_member_package : Path.t ->
string ->
workspace_deps:Package.dependency list ->
workspace_dev_deps:Package.dependency list ->
workspace_build_deps:Package.dependency list ->
Package.t option = fun workspace_root member ~workspace_deps ~workspace_dev_deps ~workspace_build_deps ->
  let member_path = Path.(workspace_root / Path.v member) in
  let toml_path = Path.(member_path / tusk_toml) in
  match Fs.exists toml_path with
  | Ok true -> (
      match Fs.read_to_string toml_path with
      | Error _ -> None
      | Ok content -> (
          match Data.Toml.parse content with
          | Error _ -> None
          | Ok toml -> (
              let relative_path = Path.v member in
              match Package.from_toml
                toml
                ~workspace_deps
                ~workspace_dev_deps
                ~workspace_build_deps
                ~path:member_path
                ~relative_path with
              | Ok pkg -> Some pkg
              | Error _ -> None
            )
        )
    )
  | _ -> None

let rec load_external_package : Path.t ->
Package.dependency ->
seen:string list Cell.t ->
workspace_deps:Package.dependency list ->
workspace_dev_deps:Package.dependency list ->
workspace_build_deps:Package.dependency list ->
dependant:string option ->
(Package.t list * load_error list) = fun workspace_root dep ~seen ~workspace_deps ~workspace_dev_deps ~workspace_build_deps ~dependant ->
  match dep.source with
  | Package.Workspace -> ([], [])
  | Package.Registry _ -> ([], [])
  | Package.Path dep_path ->
      if List.mem dep.name !seen then
        ([], [])
      else (
        seen := dep.name :: !seen;
        let abs_path = Path.(workspace_root / dep_path) in
        let toml_path = Path.(abs_path / tusk_toml) in
        let path_str = Path.to_string dep_path in
        match Fs.exists toml_path with
        | Ok true -> (
            match Fs.read_to_string toml_path with
            | Error _ -> ([], [ PackageTomlReadFailed { package = dep.name; path = path_str } ])
            | Ok content -> (
                match Data.Toml.parse content with
                | Error _ -> ([], [ PackageTomlParseFailed { package = dep.name; path = path_str } ])
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
                        let transitive_deps =
                          List.map
                            (fun (dep: Package.dependency) ->
                              match dep.source with
                              | Package.Workspace -> dep
                              | Package.Registry _ -> dep
                              | Package.Path rel_path ->
                                  let resolved_path = Path.(abs_path / rel_path) in
                                  { dep with source = Package.Path resolved_path })
                            (Package.all_dependencies pkg)
                        in
                        let transitive_results = List.map
                          (load_external_package
                            workspace_root
                            ~seen
                            ~workspace_deps
                            ~workspace_dev_deps
                            ~workspace_build_deps
                            ~dependant:(Some pkg.name))
                          transitive_deps in
                        let transitive_pkgs = List.concat_map fst transitive_results in
                        let transitive_errs = List.concat_map snd transitive_results in
                        (pkg :: transitive_pkgs, transitive_errs)
                    | Error _ -> (
                      [],
                      [ PackageFromTomlFailed { package = dep.name; path = path_str } ]
                    )
                  )
              )
          )
        | _ -> ([], [ PackageNotFound { dependant; package = dep.name; path = path_str } ])
      )

let build_workspace : Path.t -> Workspace.manifest -> (Workspace.t * load_error list) = fun workspace_root workspace_manifest ->
  let member_packages =
    List.filter_map
      (fun member ->
        load_member_package
          workspace_root
          (Path.to_string member)
          ~workspace_deps:workspace_manifest.dependencies
          ~workspace_dev_deps:workspace_manifest.dev_dependencies
          ~workspace_build_deps:workspace_manifest.build_dependencies)
      workspace_manifest.members
  in
  let seen =
    Cell.create (List.map (fun (p: Package.t) -> p.name) member_packages)
  in
  (* Load workspace-level dependencies first *)
  let workspace_results = List.map
    (load_external_package
      workspace_root
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
            workspace_root
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
  let all_errors = workspace_errors @ external_errors in
  (
    Workspace.make
      ~root:workspace_root
      ~packages:all_packages
      ~profile_overrides:workspace_manifest.profile_overrides
      ?target_dir:workspace_manifest.target_dir
      (),
    all_errors
  )

let scan : Path.t -> ((Workspace.t * load_error list), string) result = fun path ->
  try
    match find_workspace_root path with
    | None -> Error "No workspace root found"
    | Some workspace_root -> (
        let toml_path = Path.(workspace_root / tusk_toml) in
        match Fs.read_to_string toml_path with
        | Error _ -> Error "Failed to read workspace TOML"
        | Ok content -> (
            match Data.Toml.parse content with
            | Error err -> Error ("Failed to parse workspace TOML: " ^ Data.Toml.error_to_string err)
            | Ok toml -> (
                match Workspace.of_toml toml with
                | Error msg -> Error ("Failed to parse workspace manifest: " ^ msg)
                | Ok workspace_manifest ->
                    let (workspace, errors) = build_workspace workspace_root workspace_manifest in
                    Ok (workspace, errors)
              )
          )
      )
  with
  | exn -> Error ("Scan failed: " ^ Exception.to_string exn)

let load = fun ~root -> scan root

let load_error_to_string = function
  | PackageNotFound { dependant; package; path } ->
      let dep_str =
        match dependant with
        | None -> "workspace"
        | Some name -> "package '" ^ name ^ "'"
      in
      dep_str ^ ": could not find tusk.toml for '" ^ package ^ "' at path " ^ path
  | PackageTomlReadFailed { package; path } ->
      "package '" ^ package ^ "': failed to read tusk.toml at path " ^ path
  | PackageTomlParseFailed { package; path } ->
      "package '" ^ package ^ "': failed to parse tusk.toml at path " ^ path
  | PackageFromTomlFailed { package; path } ->
      "package '" ^ package ^ "': failed to load from tusk.toml at path " ^ path
