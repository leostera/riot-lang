open Std
open Std.Collections
open Std.Sync
open Std.Sync.Cell

let riot_toml = Path.v "riot.toml"

type manifest_load_error =
  | ManifestReadFailed of {
      path: Path.t;
      error: IO.error;
    }
  | ManifestParseFailed of {
      path: Path.t;
      error: Data.Toml.error;
    }

type scan_error =
  | WorkspaceTomlLoadFailed of {
      path: Path.t;
      error: manifest_load_error;
    }
  | WorkspaceManifestDecodeFailed of {
      path: Path.t;
      error: Workspace_manifest.error;
    }
  | PackageTomlLoadFailed of {
      path: Path.t;
      error: manifest_load_error;
    }
  | PackageManifestDecodeFailed of {
      path: Path.t;
      error: Package_manifest.error;
    }
  | NoWorkspaceRootFound
  | ScanException of { message: string }

type load_error =
  | PackageNotFound of {
      dependant: string option;
      (* None for workspace-level deps *)
      package: string;
      path: string;
    }
  | PackageTomlReadFailed of { package: string; path: string }
  | PackageTomlParseFailed of { package: string; path: string }
  | PackageFromTomlFailed of {
      package: string;
      path: string;
      error: Package_manifest.error;
    }

type t = {
  workspace_roots: (string, Path.t option) HashMap.t;
  scans: (string, (Workspace_manifest.t * load_error list, scan_error) result) HashMap.t;
}

let create = fun () -> { workspace_roots = HashMap.create (); scans = HashMap.create () }

let clear_cache = fun t ->
  HashMap.clear t.workspace_roots;
  HashMap.clear t.scans

let path_key = fun path -> Path.to_string path

let elapsed_us_since = fun started_at ->
  Time.Instant.elapsed started_at
  |> Time.Duration.to_micros

let model_trace_enabled = fun () ->
  match Env.get Env.String ~var:"RIOT_MODEL_TRACE" with
  | Some ("1" | "true" | "yes") -> true
  | _ -> false

let trace_workspace_manager = fun message ->
  if model_trace_enabled () then
    eprintln ("riot-model workspace " ^ message)

let manifest_load_error_message = fun __tmp1 ->
  match __tmp1 with
  | ManifestReadFailed { path; error } ->
      "failed to read manifest '" ^ Path.to_string path ^ "': " ^ IO.error_message error
  | ManifestParseFailed { path; error } ->
      "failed to parse manifest '" ^ Path.to_string path ^ "': " ^ Data.Toml.error_to_string error

let scan_error_message = fun __tmp1 ->
  match __tmp1 with
  | WorkspaceTomlLoadFailed { error; _ }
  | PackageTomlLoadFailed { error; _ } -> manifest_load_error_message error
  | WorkspaceManifestDecodeFailed { path; error } ->
      "failed to parse workspace manifest '"
      ^ Path.to_string path
      ^ "': "
      ^ Workspace_manifest.error_message error
  | PackageManifestDecodeFailed { path; error } ->
      "failed to parse package manifest '"
      ^ Path.to_string path
      ^ "': "
      ^ Package_manifest.error_message error
  | NoWorkspaceRootFound -> "no workspace root found"
  | ScanException { message } -> "scan failed: " ^ message

let load_riot_toml = fun t manifest_path ->
  let _ = t in
  match Fs.read_to_string manifest_path with
  | Error err -> Error (ManifestReadFailed { path = manifest_path; error = err })
  | Ok content -> (
      match Data.Toml.parse content with
      | Error err -> Error (ManifestParseFailed { path = manifest_path; error = err })
      | Ok toml -> Ok toml
    )

let rec find_workspace_root: t -> Path.t -> Path.t option = fun t start_dir ->
  let key = path_key start_dir in
  match HashMap.get t.workspace_roots ~key with
  | Some root -> root
  | None ->
      let riot_toml = Path.(start_dir / riot_toml) in
      let result =
        match Fs.exists riot_toml with
        | Ok true -> (
            match load_riot_toml t riot_toml with
            | Ok (Data.Toml.Table items) ->
                let has_workspace = Fields.get "workspace" items != None in
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
      let _ = HashMap.insert t.workspace_roots ~key ~value:result in
      result

let rec find_scan_roots:
  t ->
  Path.t ->
  package_root:Path.t option ->
  (Path.t option * Path.t option) = fun t start_dir ~package_root ->
  let manifest_path = Path.(start_dir / riot_toml) in
  let next package_root =
    match Path.parent start_dir with
    | Some parent when parent != start_dir -> find_scan_roots t parent ~package_root
    | _ -> (None, package_root)
  in
  match Fs.exists manifest_path with
  | Ok true -> (
      match load_riot_toml t manifest_path with
      | Ok (Data.Toml.Table items) ->
          let package_root =
            if Option.is_none package_root && Fields.get "package" items != None then
              Some start_dir
            else
              package_root
          in
          if Fields.get "workspace" items != None then
            (Some start_dir, package_root)
          else
            next package_root
      | Ok _
      | Error _ -> next package_root
    )
  | Ok false
  | Error _ -> next package_root

let load_member_package:
  t ->
  Path.t ->
  string ->
  workspace_deps:Package.dependency list ->
  workspace_dev_deps:Package.dependency list ->
  workspace_build_deps:Package.dependency list ->
  (Package_manifest.t option * load_error list) = fun
  t workspace_root member ~workspace_deps ~workspace_dev_deps ~workspace_build_deps ->
  let member_path = Path.(workspace_root / Path.v member) in
  let toml_path = Path.(member_path / riot_toml) in
  let member_name = Path.basename member_path in
  match Fs.exists toml_path with
  | Ok true -> (
      match load_riot_toml t toml_path with
      | Error (ManifestReadFailed _) -> (
        None,
        [ PackageTomlReadFailed { package = member_name; path = Path.to_string member_path } ]
      )
      | Error (ManifestParseFailed _) -> (
        None,
        [ PackageTomlParseFailed { package = member_name; path = Path.to_string member_path } ]
      )
      | Ok toml -> (
          let relative_path = Path.v member in
          match Package_manifest.from_toml
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
                error;
              };
            ]
          )
        )
    )
  | _ -> (
    None,
    [
      PackageNotFound {
        dependant = None;
        package = member_name;
        path = Path.to_string member_path;
      };
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
  seen:Package_name.t list Cell.t ->
  workspace_deps:Package.dependency list ->
  workspace_dev_deps:Package.dependency list ->
  workspace_build_deps:Package.dependency list ->
  dependant:string option ->
  (Package_manifest.t list * load_error list) = fun
  t
  workspace_root
  ~declared_from
  dep
  ~seen
  ~workspace_deps
  ~workspace_dev_deps
  ~workspace_build_deps
  ~dependant ->
  match dep.source with
  | { workspace = true; _ } -> ([], [])
  | { builtin = true; _ } -> ([], [])
  | { path = None; _ } -> ([], [])
  | { path = Some dep_path; _ } ->
      if List.contains !seen ~value:dep.name then
        ([], [])
      else
        (
          let abs_path = resolve_dependency_root ~declared_from dep_path in
          let toml_path = Path.(abs_path / riot_toml) in
          let path_str = Path.to_string dep_path in
          match Fs.exists toml_path with
          | Ok false when dependency_has_external_fallback dep -> ([], [])
          | Ok true -> (
              seen := dep.name :: !seen;
              match load_riot_toml t toml_path with
              | Error (ManifestReadFailed _) -> (
                [],
                [
                  PackageTomlReadFailed {
                    package = Package_name.to_string dep.name;
                    path = path_str;
                  };
                ]
              )
              | Error (ManifestParseFailed _) -> (
                [],
                [
                  PackageTomlParseFailed {
                    package = Package_name.to_string dep.name;
                    path = path_str;
                  };
                ]
              )
              | Ok toml -> (
                  let rel_path =
                    let abs_str = Path.to_string abs_path in
                    let root_str = Path.to_string workspace_root in
                    if String.starts_with ~prefix:root_str abs_str then
                      String.sub
                        abs_str
                        ~offset:(String.length root_str + 1)
                        ~len:(String.length abs_str - String.length root_str - 1)
                    else
                      abs_str
                  in
                  let relative_path = Path.v rel_path in
                  match Package_manifest.from_toml
                    toml
                    ~workspace_deps
                    ~workspace_dev_deps
                    ~workspace_build_deps
                    ~path:abs_path
                    ~relative_path with
                  | Ok pkg ->
                      let pkg_name = Package_name.to_string pkg.name in
                      let transitive_results =
                        List.map
                          (Package_manifest.all_dependencies pkg)
                          ~fn:(load_external_package
                            t
                            workspace_root
                            ~declared_from:abs_path
                            ~seen
                            ~workspace_deps
                            ~workspace_dev_deps
                            ~workspace_build_deps
                            ~dependant:(Some pkg_name))
                      in
                      let transitive_pkgs =
                        transitive_results
                        |> List.map ~fn:(fun (packages, _) -> packages)
                        |> List.concat
                      in
                      let transitive_errs =
                        transitive_results
                        |> List.map ~fn:(fun (_, errors) -> errors)
                        |> List.concat
                      in
                      (pkg :: transitive_pkgs, transitive_errs)
                  | Error error -> (
                    [],
                    [
                      PackageFromTomlFailed {
                        package = Package_name.to_string dep.name;
                        path = path_str;
                        error;
                      };
                    ]
                  )
                )
            )
          | _ ->
              seen := dep.name :: !seen;
              (
                [],
                [
                  PackageNotFound {
                    dependant;
                    package = Package_name.to_string dep.name;
                    path = path_str;
                  };
                ]
              )
        )

let build_workspace:
  t ->
  Path.t ->
  Workspace_manifest.manifest ->
  (Workspace_manifest.t * load_error list) = fun t workspace_root workspace_manifest ->
  let started_at = Time.Instant.now () in
  let member_started_at = Time.Instant.now () in
  let member_results =
    List.map
      workspace_manifest.members
      ~fn:(fun member ->
        load_member_package
          t
          workspace_root
          (Path.to_string member)
          ~workspace_deps:workspace_manifest.dependencies
          ~workspace_dev_deps:workspace_manifest.dev_dependencies
          ~workspace_build_deps:workspace_manifest.build_dependencies)
  in
  let () =
    trace_workspace_manager
      ("member-results-us=" ^ Int.to_string (elapsed_us_since member_started_at))
  in
  let member_packages = List.filter_map member_results ~fn:(fun (pkg, _) -> pkg) in
  let member_errors =
    member_results
    |> List.map ~fn:(fun (_, errors) -> errors)
    |> List.concat
  in
  let seen = Cell.create (List.map member_packages ~fn:(fun (p: Package_manifest.t) -> p.name)) in
  (* Load workspace-level dependencies first *)
  let workspace_deps_started_at = Time.Instant.now () in
  let workspace_results =
    List.map
      ((workspace_manifest.dependencies @ workspace_manifest.dev_dependencies)
      @ workspace_manifest.build_dependencies)
      ~fn:(load_external_package
        t
        workspace_root
        ~declared_from:workspace_root
        ~seen
        ~workspace_deps:workspace_manifest.dependencies
        ~workspace_dev_deps:workspace_manifest.dev_dependencies
        ~workspace_build_deps:workspace_manifest.build_dependencies
        ~dependant:None)
  in
  let () =
    trace_workspace_manager
      ("workspace-deps-us=" ^ Int.to_string (elapsed_us_since workspace_deps_started_at))
  in
  let workspace_packages =
    workspace_results
    |> List.map ~fn:(fun (packages, _) -> packages)
    |> List.concat
  in
  let workspace_errors =
    workspace_results
    |> List.map ~fn:(fun (_, errors) -> errors)
    |> List.concat
  in
  (* Then load any additional dependencies from member packages *)
  let external_started_at = Time.Instant.now () in
  let external_results =
    member_packages
    |> List.map
      ~fn:(fun (pkg: Package_manifest.t) ->
        let pkg_name = Package_name.to_string pkg.name in
        List.map
          (Package_manifest.all_dependencies pkg)
          ~fn:(load_external_package
            t
            workspace_root
            ~declared_from:pkg.path
            ~seen
            ~workspace_deps:workspace_manifest.dependencies
            ~workspace_dev_deps:workspace_manifest.dev_dependencies
            ~workspace_build_deps:workspace_manifest.build_dependencies
            ~dependant:(Some pkg_name)))
    |> List.concat
  in
  let () =
    trace_workspace_manager
      ("member-external-deps-us=" ^ Int.to_string (elapsed_us_since external_started_at))
  in
  let external_packages =
    external_results
    |> List.map ~fn:(fun (packages, _) -> packages)
    |> List.concat
  in
  let external_errors =
    external_results
    |> List.map ~fn:(fun (_, errors) -> errors)
    |> List.concat
  in
  let all_packages = (member_packages @ workspace_packages) @ external_packages in
  let all_errors = (member_errors @ workspace_errors) @ external_errors in
  let () =
    trace_workspace_manager
      ("build-workspace-total-us="
      ^ Int.to_string (elapsed_us_since started_at)
      ^ " members="
      ^ Int.to_string (List.length member_packages)
      ^ " externals="
      ^ Int.to_string (List.length workspace_packages + List.length external_packages)
      ^ " errors="
      ^ Int.to_string (List.length all_errors))
  in
  (
    Workspace_manifest.make
      ?name:workspace_manifest.name
      ~root:workspace_root
      ~packages:all_packages
      ~source_ignore_patterns:workspace_manifest.source_ignore_patterns
      ~dependencies:workspace_manifest.dependencies
      ~dev_dependencies:workspace_manifest.dev_dependencies
      ~build_dependencies:workspace_manifest.build_dependencies
      ~profile_overrides:workspace_manifest.profile_overrides
      ?target_dir:workspace_manifest.target_dir
      (),
    all_errors
  )

let build_single_package_workspace:
  t ->
  Path.t ->
  (Workspace_manifest.t * load_error list, scan_error) result = fun t package_root ->
  let manifest_path = Path.(package_root / riot_toml) in
  match load_riot_toml t manifest_path with
  | Error err -> Error (PackageTomlLoadFailed { path = manifest_path; error = err })
  | Ok toml -> (
      match Package_manifest.from_toml
        toml
        ~workspace_deps:[]
        ~workspace_dev_deps:[]
        ~workspace_build_deps:[]
        ~path:package_root
        ~relative_path:(Path.v ".") with
      | Error err -> Error (PackageManifestDecodeFailed { path = manifest_path; error = err })
      | Ok package ->
          let package_name = Package_name.to_string package.name in
          let seen = Cell.create [ package.name ] in
          let external_results =
            List.map
              (Package_manifest.all_dependencies package)
              ~fn:(load_external_package
                t
                package_root
                ~declared_from:package_root
                ~seen
                ~workspace_deps:[]
                ~workspace_dev_deps:[]
                ~workspace_build_deps:[]
                ~dependant:(Some package_name))
          in
          let external_packages =
            external_results
            |> List.map ~fn:(fun (packages, _) -> packages)
            |> List.concat
          in
          let external_errors =
            external_results
            |> List.map ~fn:(fun (_, errors) -> errors)
            |> List.concat
          in
          Ok (
            Workspace_manifest.make ~root:package_root ~packages:(package :: external_packages) (),
            external_errors
          )
    )

let scan: t -> Path.t -> (Workspace_manifest.t * load_error list, scan_error) result = fun t path ->
  try
    let started_at = Time.Instant.now () in
    let find_roots_started_at = Time.Instant.now () in
    let roots = find_scan_roots t path ~package_root:None in
    let () =
      trace_workspace_manager
        ("find-scan-roots-us=" ^ Int.to_string (elapsed_us_since find_roots_started_at))
    in
    match roots with
    | (Some workspace_root, _) ->
        let key = path_key workspace_root in
        (
          match HashMap.get t.scans ~key with
          | Some result -> result
          | None ->
              let toml_path = Path.(workspace_root / riot_toml) in
              let result =
                match load_riot_toml t toml_path with
                | Error err -> Error (WorkspaceTomlLoadFailed { path = toml_path; error = err })
                | Ok toml -> (
                    let workspace_of_toml_started_at = Time.Instant.now () in
                    match Workspace_manifest.from_toml toml with
                    | Error err ->
                        Error (WorkspaceManifestDecodeFailed { path = toml_path; error = err })
                    | Ok workspace_manifest ->
                        let () =
                          trace_workspace_manager
                            ("workspace-of-toml-us="
                            ^ Int.to_string (elapsed_us_since workspace_of_toml_started_at))
                        in
                        let (workspace, errors) =
                          build_workspace t workspace_root workspace_manifest
                        in
                        Ok (workspace, errors)
                  )
              in
              let _ = HashMap.insert t.scans ~key ~value:result in
              let () =
                trace_workspace_manager
                  ("scan-total-us=" ^ Int.to_string (elapsed_us_since started_at))
              in
              result
        )
    | (None, Some package_root) ->
        let key = path_key package_root in
        (
          match HashMap.get t.scans ~key with
          | Some result -> result
          | None ->
              let result = build_single_package_workspace t package_root in
              let _ = HashMap.insert t.scans ~key ~value:result in
              let () =
                trace_workspace_manager
                  ("scan-total-us=" ^ Int.to_string (elapsed_us_since started_at))
              in
              result
        )
    | (None, None) -> Error NoWorkspaceRootFound
  with
  | exn -> Error (ScanException { message = Exception.to_string exn })

let load = fun t ~root -> scan t root

let load_error_to_string = fun __tmp1 ->
  match __tmp1 with
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
      "package '"
      ^ package
      ^ "': failed to load from riot.toml at path "
      ^ path
      ^ ": "
      ^ Package_manifest.error_message error
