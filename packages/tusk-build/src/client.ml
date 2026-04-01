open Std
open Tusk_model

type t = {
  server_pid: Pid.t;
  workspace_root: Path.t;
}

type build_stats = {
  duration_ms: int;
  packages_built: int;
  packages_failed: int;
  total_modules: int;
  cache_hits: int;
  cache_misses: int;
}

type error =
  | StartupFailed of { error: Internal_server.error }
  | PackageNotFound of { package_name: string; available_packages: string list }
  | PackagesNotFound of { package_names: string list; available_packages: string list }
  | BuildFailed of { errors: Tusk_executor.Package_builder.build_result list }
  | PlanningFailed of { reason: string }
  | CycleDetected of { cycle_nodes: string list }
  | BuildAlreadyRunning of { lock_path: Path.t }
  | UnexpectedEvent of { reason: string }

type streaming_event =
  | BuildStarted of Session_id.t
  | BuildEvent of Telemetry.event
  | BuildCompleted of {
      session_id: Session_id.t;
      completed_at: Datetime.t;
      stats: build_stats;
      results: Tusk_executor.Package_builder.build_result list
    }
  | BuildFailed of {
      session_id: Session_id.t;
      failed_at: Datetime.t;
      stats: build_stats;
      built: Tusk_executor.Package_builder.build_result list;
      errors: Tusk_executor.Package_builder.build_result list
    }
  | PlanningFailed of { session_id: Session_id.t; failed_at: Datetime.t; reason: string }
  | CycleDetected of { session_id: Session_id.t; detected_at: Datetime.t; cycle_nodes: string list }

type build_target =
  BuildPackage of string
  | BuildPackages of string list
  | BuildAll

type build_scope =
  Runtime
  | Dev

let no_emit : Tusk_model.Event.kind -> unit = fun _ -> ()

let error_message = function
  | StartupFailed { error } ->
      Internal_server.error_message error
  | PackageNotFound { package_name; _ } ->
      "Package '" ^ package_name ^ "' not found"
  | PackagesNotFound { package_names; _ } ->
      "Packages not found: " ^ String.concat ", " package_names
  | BuildFailed { errors } ->
      let render_error (result: Tusk_executor.Package_builder.build_result) =
        match result.status with
        | Tusk_executor.Package_builder.Failed err -> result.package.name
        ^ ": "
        ^ Tusk_executor.Package_builder.package_error_to_string err
        | Tusk_executor.Package_builder.Skipped { reason } -> result.package.name
        ^ ": skipped ("
        ^ reason
        ^ ")"
        | Tusk_executor.Package_builder.Built _
        | Tusk_executor.Package_builder.Cached _ -> result.package.name ^ ": build failed"
      in
      (
        match errors with
        | [] -> "build failed"
        | [ result ] -> render_error result
        | results -> "build failed:\n" ^ String.concat "\n" (List.map render_error results)
      )
  | PlanningFailed { reason } ->
      "planning failed: " ^ reason
  | CycleDetected { cycle_nodes } ->
      "cyclic dependency detected: " ^ String.concat " -> " cycle_nodes
  | BuildAlreadyRunning { lock_path } ->
      "another tusk build is already running (" ^ Path.to_string lock_path ^ ")"
  | UnexpectedEvent { reason } ->
      reason

let connect_local = fun ?(emit = no_emit) ~workspace () ->
  match Internal_server.start_local ~emit ~workspace ~config:Server_config.default () with
  | Ok server_pid -> Ok { server_pid; workspace_root = workspace.root }
  | Error err -> Error (StartupFailed { error = err })

let close = fun _t -> ()

let send_request = fun t request -> send t.server_pid (Protocol.ServerRequest request)

let receive_response = fun ~selector -> receive ~selector ()

let scan_workspace = fun t ~current_dir ->
  send_request t (Protocol.ScanWorkspace { client_pid = self (); current_dir });
  let selector msg =
    match msg with
    | Protocol.ServerResponse Protocol.WorkspaceScanned -> `select (Ok ())
    | _ -> `skip
  in
  receive_response ~selector

module BuildLock = struct
  type nonrec t = {
    path: Path.t;
    file: Fs.File.t;
  }

  let retry_interval = Time.Duration.from_millis 500

  let path = fun ~workspace_root ~profile ~target ->
    Tusk_model.Tusk_dirs.build_lock_path_with_target ~workspace_root ~profile ~target

  let release = fun t ->
    let _ = Fs.File.unlock t.file in
    let _ = Fs.File.close t.file in
    ()

  let lock_failure = fun action path ->
    Failure ("Failed to " ^ action ^ " build lock file at " ^ Path.to_string path)

  let rec retry = fun ?(announced = false) t ->
    if not announced then
      eprintln "build lock is taken, waiting...";
    sleep retry_interval;
    match Fs.File.try_lock_exclusive t.file with
    | Ok true ->
        Ok t
    | Ok false ->
        retry ~announced:true t
    | Error _ ->
        release t;
        raise (lock_failure "lock" t.path)

  let wait = fun ~workspace_root ~profile ~target ->
    let build_dir = Tusk_model.Tusk_dirs.target_dir ~workspace_root ~profile ~target in
    let _ = Fs.create_dir_all build_dir |> Result.expect ~msg:"Failed to create build directory" in
    let path = path ~workspace_root ~profile ~target in
    let file =
      match Fs.File.open_write path with
      | Ok file -> file
      | Error _ -> raise (lock_failure "open" path)
    in
    let t = { path; file } in
    match Fs.File.try_lock_exclusive file with
    | Ok true ->
        Ok t
    | Ok false ->
        retry t
    | Error _ ->
        release t;
        raise (lock_failure "lock" path)

  let acquire = fun ~workspace_root ~profile ~target fn ->
    match wait ~workspace_root ~profile ~target with
    | Error err -> Error err
    | Ok t ->
        try
          let result = fn () in
          release t;
          result
        with
        | exn ->
            release t;
            raise exn
end

let convert_build_stats : Protocol.BuildStats.t -> build_stats = fun stats ->
  {
    duration_ms = int_of_float (Protocol.BuildStats.get_build_duration stats *. 1000.0);
    packages_built = Protocol.BuildStats.get_packages_built stats;
    packages_failed = Protocol.BuildStats.get_packages_failed stats;
    total_modules = Protocol.BuildStats.get_total_modules stats;
    cache_hits = Protocol.BuildStats.get_cache_hits stats;
    cache_misses = Protocol.BuildStats.get_cache_misses stats;
  }

let same_session = fun left right -> Session_id.to_string left = Session_id.to_string right

let rec handle_streaming_events = fun t session_id callback ->
  let selector msg =
    match msg with
    | Protocol.ServerResponse (Protocol.BuildEvent { session_id=event_session_id; event }) -> `select (`BuildEvent (
      event_session_id,
      event
    ))
    | Protocol.ServerResponse (Protocol.BuildCompleted {
      session_id=event_session_id;
      completed_at;
      stats;
      results
    }) -> `select (`BuildCompleted (event_session_id, completed_at, stats, results))
    | Protocol.ServerResponse (Protocol.BuildFailed {
      session_id=event_session_id;
      failed_at;
      stats;
      built;
      errors
    }) -> `select (`BuildFailed (event_session_id, failed_at, stats, built, errors))
    | Protocol.ServerResponse (Protocol.PlanningFailed {
      session_id=event_session_id;
      failed_at;
      reason
    }) -> `select (`PlanningFailed (event_session_id, failed_at, reason))
    | Protocol.ServerResponse (Protocol.CycleDetected {
      session_id=event_session_id;
      detected_at;
      cycle_nodes
    }) -> `select (`CycleDetected (event_session_id, detected_at, cycle_nodes))
    | Protocol.ServerResponse (Protocol.PackageNotFound {
      session_id=event_session_id;
      package_name;
      available_packages
    }) -> `select (`PackageNotFound (event_session_id, package_name, available_packages))
    | Protocol.ServerResponse (Protocol.PackagesNotFound {
      session_id=event_session_id;
      package_names;
      available_packages
    }) -> `select (`PackagesNotFound (event_session_id, package_names, available_packages))
    | _ -> `skip
  in
  match receive_response ~selector with
  | `BuildEvent (event_session_id, event) ->
      if same_session session_id event_session_id then
        callback (BuildEvent event);
      handle_streaming_events t session_id callback
  | `BuildCompleted (event_session_id, completed_at, stats, results) ->
      if same_session session_id event_session_id then
        let final_event = BuildCompleted {
          session_id = event_session_id;
          completed_at;
          stats = convert_build_stats stats;
          results
        } in
        callback final_event;
        Ok final_event
      else
        handle_streaming_events t session_id callback
  | `BuildFailed (event_session_id, failed_at, stats, built, errors) ->
      if same_session session_id event_session_id then
        let final_event = BuildFailed {
          session_id = event_session_id;
          failed_at;
          stats = convert_build_stats stats;
          built;
          errors;
        }
        in
        callback final_event;
        Error ((BuildFailed { errors }): error)
      else
        handle_streaming_events t session_id callback
  | `PlanningFailed (event_session_id, failed_at, reason) ->
      if same_session session_id event_session_id then
        let final_event = PlanningFailed { session_id = event_session_id; failed_at; reason } in
        callback final_event;
        Error ((PlanningFailed { reason }): error)
      else
        handle_streaming_events t session_id callback
  | `CycleDetected (event_session_id, detected_at, cycle_nodes) ->
      if same_session session_id event_session_id then
        let final_event = CycleDetected { session_id = event_session_id; detected_at; cycle_nodes } in
        callback final_event;
        Error ((CycleDetected { cycle_nodes }): error)
      else
        handle_streaming_events t session_id callback
  | `PackageNotFound (event_session_id, package_name, available_packages) ->
      if same_session session_id event_session_id then
        Error (PackageNotFound { package_name; available_packages })
      else
        handle_streaming_events t session_id callback
  | `PackagesNotFound (event_session_id, package_names, available_packages) ->
      if same_session session_id event_session_id then
        Error (PackagesNotFound { package_names; available_packages })
      else
        handle_streaming_events t session_id callback

let build_streaming = fun t target ?(scope = Runtime) ?(profile = "debug") ?target_arch callback ->
  let lock_target =
    match target_arch with
    | Some target -> target
    | None -> Tusk_model.Tusk_dirs.host_target ()
  in
  BuildLock.acquire ~workspace_root:t.workspace_root ~profile ~target:lock_target
    (fun () ->
      let request_target =
        match target with
        | BuildPackage package -> Protocol.Package package
        | BuildPackages packages -> Protocol.Packages packages
        | BuildAll -> Protocol.All
      in
      let session_id = Session_id.make () in
      send_request t
        (
          Protocol.Build {
            client_pid = self ();
            target = request_target;
            scope =
              (
                match scope with
                | Runtime -> Protocol.Runtime
                | Dev -> Protocol.Dev
              );
            profile;
            target_arch;
            session_id;
          }
        );
      let selector msg =
        match msg with
        | Protocol.ServerResponse (Protocol.BuildStarted {
          session_id=started_session_id;
          started_at=_
        }) when same_session session_id started_session_id -> `select (Ok started_session_id)
        | Protocol.ServerResponse (Protocol.PackageNotFound {
          session_id=event_session_id;
          package_name;
          available_packages
        }) when same_session session_id event_session_id -> `select (Error (PackageNotFound {
          package_name;
          available_packages
        }))
        | Protocol.ServerResponse (Protocol.PackagesNotFound {
          session_id=event_session_id;
          package_names;
          available_packages
        }) when same_session session_id event_session_id -> `select (Error (PackagesNotFound {
          package_names;
          available_packages
        }))
        | _ -> `skip
      in
      match receive_response ~selector with
      | Ok started_session_id ->
          callback (BuildStarted started_session_id);
          handle_streaming_events t started_session_id callback
      | Error err -> Error err)

let find_executable = fun t name ->
  send_request t (Protocol.FindExecutable { client_pid = self (); name });
  let selector msg =
    match msg with
    | Protocol.ServerResponse (Protocol.ExecutableFound { package; binary }) -> `select (Ok (Some (
      package,
      binary
    )))
    | Protocol.ServerResponse Protocol.ExecutableNotFound -> `select (Ok None)
    | _ -> `skip
  in
  receive_response ~selector

let find_artifact = fun t ~package ~kind ~name ->
  send_request t (Protocol.FindArtifact { client_pid = self (); package; kind; name });
  let selector msg =
    match msg with
    | Protocol.ServerResponse (Protocol.ArtifactFound { path }) -> `select (Ok (Path.to_string path))
    | Protocol.ServerResponse (Protocol.ArtifactNotFound { error }) -> `select (Error error)
    | _ -> `skip
  in
  receive_response ~selector

let new_package = fun t ~path ~name ~is_library ->
  let path =
    match Path.of_string path with
    | Ok path -> path
    | Error _ -> Path.v path
  in
  send_request t (Protocol.NewPackage { client_pid = self (); path; name; is_library });
  let selector msg =
    match msg with
    | Protocol.ServerResponse (Protocol.PackageCreated { path; name }) -> `select (Ok (path, name))
    | Protocol.ServerResponse (Protocol.PackageCreationError { error }) -> `select (Error error)
    | _ -> `skip
  in
  receive_response ~selector
