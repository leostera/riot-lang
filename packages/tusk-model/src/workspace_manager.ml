open Std

let tusk_toml = Path.v "tusk.toml"

let rec find_workspace_root (start_dir : Path.t) : Path.t option =
  let tusk_toml = Path.(start_dir / tusk_toml) in
  match Fs.exists tusk_toml with
  | Ok true -> (
      match Fs.read_to_string tusk_toml with
      | Error _ -> None
      | Ok content -> (
          match Data.Toml.parse content with
          | Ok (Data.Toml.Table items) -> (
              let has_workspace = List.assoc_opt "workspace" items <> None in
              if has_workspace then Some start_dir
              else
                match Path.parent start_dir with
                | Some parent when parent <> start_dir ->
                    find_workspace_root parent
                | _ -> None)
          | _ -> None))
  | Ok false | Error _ -> (
      match Path.parent start_dir with
      | Some parent when parent <> start_dir -> find_workspace_root parent
      | _ -> None)

let load_member_package (workspace_root : Path.t) (member : string)
    ~(workspace_deps : Package.dependency list) : Package.t option =
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
              match
                Package.from_toml toml ~workspace_deps ~path:member_path
                  ~relative_path
              with
              | Ok pkg -> Some pkg
              | Error _ -> None)))
  | _ -> None

let rec load_external_package (workspace_root : Path.t)
    (dep : Package.dependency) ~(seen : string list ref) : Package.t list =
  match dep.source with
  | Package.Workspace -> []
  | Package.Path dep_path ->
      if List.mem dep.name !seen then []
      else (
        seen := dep.name :: !seen;
        let abs_path = Path.(workspace_root / dep_path) in
        let toml_path = Path.(abs_path / tusk_toml) in
        match Fs.exists toml_path with
        | Ok true -> (
            match Fs.read_to_string toml_path with
            | Error _ -> []
            | Ok content -> (
                match Data.Toml.parse content with
                | Error _ -> []
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
                        let transitive_deps =
                          List.map
                            (fun (dep : Package.dependency) ->
                              match dep.source with
                              | Package.Workspace -> dep
                              | Package.Path rel_path ->
                                  let resolved_path =
                                    Path.(abs_path / rel_path)
                                  in
                                  {
                                    dep with
                                    source = Package.Path resolved_path;
                                  })
                            pkg.dependencies
                        in
                        let transitive =
                          List.concat_map
                            (load_external_package workspace_root ~seen)
                            transitive_deps
                        in
                        pkg :: transitive
                    | Error _ -> [])))
        | _ -> [])

let build_workspace (workspace_root : Path.t)
    (workspace_manifest : Workspace.manifest) : Workspace.t =
  let member_packages =
    List.filter_map
      (fun member ->
        load_member_package workspace_root (Path.to_string member)
          ~workspace_deps:workspace_manifest.dependencies)
      workspace_manifest.members
  in

  let seen = ref (List.map (fun (p : Package.t) -> p.name) member_packages) in
  let external_packages =
    List.concat_map
      (fun (pkg : Package.t) ->
        List.concat_map
          (load_external_package workspace_root ~seen)
          pkg.dependencies)
      member_packages
  in

  let all_packages = member_packages @ external_packages in
  Workspace.make ~root:workspace_root ~packages:all_packages

let scan (path : Path.t) : (Workspace.t, string) result =
  try
    match find_workspace_root path with
    | None -> Error "No workspace root found"
    | Some workspace_root -> (
        let toml_path = Path.(workspace_root / tusk_toml) in
        match Fs.read_to_string toml_path with
        | Error _ -> Error "Failed to read workspace TOML"
        | Ok content -> (
            match Data.Toml.parse content with
            | Error err ->
                Error
                  (format "Failed to parse workspace TOML: %s"
                     (Data.Toml.error_to_string err))
            | Ok toml -> (
                match Workspace.manifest_from_toml toml with
                | Error msg ->
                    Error (format "Failed to parse workspace manifest: %s" msg)
                | Ok workspace_manifest ->
                    let workspace =
                      build_workspace workspace_root workspace_manifest
                    in
                    Ok workspace)))
  with exn -> Error (format "Scan failed: %s" (Exception.to_string exn))

let load ~root = scan root
