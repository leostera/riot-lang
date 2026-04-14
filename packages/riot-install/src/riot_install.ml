open Std

type destination =
  | Local
  | Global

type external_spec =
  | Source of {
      spec: Riot_deps.Git_dependency.spec;
      update: bool;
    }
  | Registry of Riot_deps.Registry_package_spec.t

type request =
  | Workspace of {
      workspace: Riot_model.Workspace.t;
      package_name: string option;
      binary_name: string;
      destination: destination;
    }
  | External of {
      spec: external_spec;
      binary_name: string;
    }

type install_event =
  | Build of Riot_build.Event.t
  | InstallingBinary of { package: string; binary: string }
  | PromotedBinary of { binary: string; destination: Path.t; mode: destination }
  | InstalledBinary of { binary: string; duration_ms: int; destination: Path.t; mode: destination }

type install_error =
  | BinaryNotFound of { binary_name: string }
  | BinaryNotFoundInPackage of { package_name: string; binary_name: string }
  | BuildFailed of Riot_build.error
  | ArtifactNotFound of { package_name: string; binary_name: string; reason: string }
  | PromotionFailed of { binary_name: string; destination: Path.t; mode: destination; reason: string }
  | ExternalTargetLoadFailed of { target: string; reason: string }

let ( let* ) value fn = Result.and_then value ~fn

let no_event: install_event -> unit = fun _ -> ()

let destination_name = function
  | Local -> "local"
  | Global -> "global"

let install_error_message = function
  | BinaryNotFound { binary_name } ->
      "binary '" ^ binary_name ^ "' not found in workspace"
  | BinaryNotFoundInPackage { package_name; binary_name } ->
      "binary '" ^ binary_name ^ "' not found in package '" ^ package_name ^ "'"
  | BuildFailed err -> Riot_build.error_message err
  | ArtifactNotFound { package_name; binary_name; reason } ->
      "binary '" ^ binary_name ^ "' was not produced by package '" ^ package_name ^ "': " ^ reason
  | PromotionFailed { binary_name; destination; mode; reason } ->
      "failed to promote "
      ^ binary_name
      ^ " to "
      ^ Path.to_string destination
      ^ " ("
      ^ destination_name mode
      ^ ")"
      ^ ": "
      ^ reason
  | ExternalTargetLoadFailed { target; reason } ->
      "failed to load external target '" ^ target ^ "': " ^ reason

let path_json = fun path -> Data.Json.String (Path.to_string path)

let install_event_to_json = function
  | Build event -> Riot_build.Event.to_json event
  | InstallingBinary { package; binary } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "InstallingBinary");
        ("package", Data.Json.String package);
        ("binary", Data.Json.String binary);
      ])
  | PromotedBinary { binary; destination; mode } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "PromotedBinary");
        ("binary", Data.Json.String binary);
        ("destination", path_json destination);
        ("mode", Data.Json.String (destination_name mode));
      ])
  | InstalledBinary { binary; duration_ms; destination; mode } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "InstalledBinary");
        ("binary", Data.Json.String binary);
        ("duration_ms", Data.Json.Int duration_ms);
        ("destination", path_json destination);
        ("mode", Data.Json.String (destination_name mode));
      ])

let realized_runnable_packages = fun ?package_filter (workspace: Riot_model.Workspace.t) ->
  Riot_model.Workspace.realize_packages ~intent:Riot_model.Package.Run workspace
  |> List.filter ~fn:Riot_model.Package.is_workspace_member
  |> List.filter ~fn:(fun (pkg: Riot_model.Package.t) ->
      match package_filter with
      | None -> true
      | Some package_name -> String.equal package_name pkg.name)

let resolve_binary = fun ~(workspace: Riot_model.Workspace.t) ~package_name ~binary_name ->
  let packages = realized_runnable_packages ?package_filter:package_name workspace in
  match package_name with
  | Some expected_package -> (
      match
        List.find packages ~fn:(fun (pkg: Riot_model.Package.t) ->
          String.equal pkg.name expected_package
          && List.any pkg.binaries ~fn:(fun (bin: Riot_model.Package.binary) ->
              String.equal bin.name binary_name))
      with
      | Some pkg -> Ok pkg.name
      | None -> Error (BinaryNotFoundInPackage { package_name = expected_package; binary_name })
    )
  | None -> (
      match
        List.find packages ~fn:(fun (pkg: Riot_model.Package.t) ->
          List.any pkg.binaries ~fn:(fun (bin: Riot_model.Package.binary) ->
            String.equal bin.name binary_name))
      with
      | Some pkg -> Ok pkg.name
      | None -> Error (BinaryNotFound { binary_name })
    )

let make_pm_event = fun ~session_id kind ->
  Riot_model.Event.create ~session_id ~level:Riot_model.Event.Info kind

let emit_pm_build_event = fun ~session_id ~on_event kind ->
  on_event (Build (Riot_build.Event.Pm (make_pm_event ~session_id kind)))

let load_source_workspace = fun ~on_event ~source_spec ~update ->
  let session_id = Riot_model.Session_id.make () in
  Riot_deps.load_source_workspace_from_spec
    ~emit:(emit_pm_build_event ~session_id ~on_event)
    ~update
    ~spec:source_spec
    ()
  |> Result.map_err ~fn:(fun err ->
      ExternalTargetLoadFailed {
        target = Riot_deps.Git_dependency.to_string source_spec;
        reason = Riot_deps.package_error_message err;
      })

let load_registry_workspace = fun ~on_event ~package_spec ->
  let session_id = Riot_model.Session_id.make () in
  Riot_deps.load_registry_workspace_from_spec
    ~emit:(emit_pm_build_event ~session_id ~on_event)
    ~spec:package_spec
    ()
  |> Result.map_err ~fn:(fun err ->
      ExternalTargetLoadFailed {
        target = Riot_deps.Registry_package_spec.to_string package_spec;
        reason = Riot_deps.package_error_message err;
      })

let find_built_binary_path = fun
  ~(store: Riot_store.Store.t)
  ~(output: Riot_build.Output.t)
  ~package_name
  ~binary_name
  ->
  match
    Riot_build.Output.find_package output package_name
    |> Option.and_then ~fn:(fun package_output ->
        Riot_build.Output.find_export package_output binary_name)
  with
  | None ->
      Error (ArtifactNotFound {
        package_name;
        binary_name;
        reason = "binary '" ^ binary_name ^ "' was not produced by build output";
      })
  | Some export_entry -> (
      match Riot_store.Store.export_source_path store export_entry with
      | Some path -> Ok path
      | None ->
          Error (ArtifactNotFound {
            package_name;
            binary_name;
            reason = "binary '" ^ binary_name ^ "' resolved to an invalid absolute export path";
          })
    )

let install_temp_path = fun dst ->
  let dir = Path.dirname dst in
  let name = Path.basename dst in
  let pid = Process.id () in
  let nonce = Random.bits () |> Result.expect ~msg:"failed to generate install nonce" in
  Path.(dir / Path.v ("." ^ name ^ ".install-" ^ Int32.to_string pid ^ "-" ^ Int.to_string nonce))

let cleanup_temp_file = fun path ->
  match Fs.remove_file path with
  | Ok () -> ()
  | Error _ -> ()

let install_binary_atomically = fun ~src ~dst ~permissions ->
  let temp_path = install_temp_path dst in
  match Fs.copy ~src ~dst:temp_path with
  | Error err -> Error err
  | Ok () -> (
      match Fs.set_permissions temp_path permissions with
      | Error err ->
          cleanup_temp_file temp_path;
          Error err
      | Ok () -> (
          match Fs.rename ~src:temp_path ~dst with
          | Ok () -> Ok ()
          | Error err ->
              cleanup_temp_file temp_path;
              Error err
        )
    )

let promote_binary = fun ~on_event ~src ~dst ~binary ~mode ->
  match install_binary_atomically ~src ~dst ~permissions:Fs.Permissions.executable with
  | Ok () ->
      on_event (PromotedBinary { binary; destination = dst; mode });
      Ok ()
  | Error reason ->
      Error (PromotionFailed {
        binary_name = binary;
        destination = dst;
        mode;
        reason = IO.error_message reason;
      })

let build_request = fun ~workspace ~package_name ->
  Riot_build.Request.make
    ~workspace:(Riot_build.Prepared_workspace.of_workspace workspace)
    ~packages:[ package_name ]
    ~targets:Riot_model.Target.Host
    ~scope:Riot_build.Request.Runtime
    ~profile:Riot_model.Profile.debug
    ()

let destination_path = fun ~(workspace:Riot_model.Workspace.t) ~binary_name mode ->
  match mode with
  | Local -> Path.(workspace.root / Path.v binary_name)
  | Global -> Path.(Riot_model.Riot_dirs.dot_riot / Path.v "bin" / Path.v binary_name)

let ensure_destination_parent = fun path ->
  match Path.parent path with
  | Some parent -> Fs.create_dir_all parent
  | None -> Ok ()

let install_workspace = fun ~on_event ~workspace ~package_name ~binary_name ~destination ->
  let started_at = Time.Instant.now () in
  let* package_name =
    resolve_binary
      ~workspace
      ~package_name
      ~binary_name
  in
  on_event (InstallingBinary { package = package_name; binary = binary_name });
  let* output =
    Riot_build.build
      ~on_event:(fun event -> on_event (Build event))
      (build_request ~workspace ~package_name)
    |> Result.map_err ~fn:(fun err -> BuildFailed err)
  in
  let store =
    Riot_store.Store.create_for_lane
      ~workspace
      ~profile:"debug"
      ~target:(Riot_model.Riot_dirs.host_target ())
  in
  let* binary_path =
    find_built_binary_path
      ~store
      ~output
      ~package_name
      ~binary_name
  in
  let destination_path = destination_path ~workspace ~binary_name destination in
  let* () =
    ensure_destination_parent destination_path
    |> Result.map_err ~fn:(fun reason ->
        PromotionFailed {
          binary_name;
          destination = destination_path;
          mode = destination;
          reason = IO.error_message reason;
        })
  in
  let* () =
    promote_binary
      ~on_event
      ~src:binary_path
      ~dst:destination_path
      ~binary:binary_name
      ~mode:destination
  in
  let duration =
    Time.Instant.duration_since
      ~earlier:started_at
      (Time.Instant.now ())
  in
  on_event (InstalledBinary {
    binary = binary_name;
    duration_ms = Time.Duration.to_millis duration;
    destination = destination_path;
    mode = destination;
  });
  Ok ()

let install_external = fun ~on_event ~spec ~binary_name ->
  let* loaded =
    match spec with
    | Source { spec; update } ->
        load_source_workspace
          ~on_event
          ~source_spec:spec
          ~update
    | Registry spec ->
        load_registry_workspace
          ~on_event
          ~package_spec:spec
  in
  install_workspace
    ~on_event
    ~workspace:loaded.workspace
    ~package_name:(Some loaded.package_name)
    ~binary_name
    ~destination:Global

let install = fun ?(on_event = no_event) (request: request) ->
  match request with
  | Workspace { workspace; package_name; binary_name; destination } ->
      install_workspace
        ~on_event
        ~workspace
        ~package_name
        ~binary_name
        ~destination
  | External { spec; binary_name } ->
      install_external
        ~on_event
        ~spec
        ~binary_name
