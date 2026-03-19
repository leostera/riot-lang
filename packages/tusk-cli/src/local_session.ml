open Std
open Tusk_model
open Tusk_protocol
open Tusk_server

type t = { server_pid : Pid.t }

type error =
  | PackageNotFound of {
      package_name : string;
      available_packages : string list;
    }
  | UnexpectedEvent of { event : WireProtocol.response; reason : string }

type streaming_event =
  | BuildStarted of Session_id.t
  | BuildEvent of Telemetry.event
  | BuildCompleted of {
      session_id : Session_id.t;
      completed_at : Datetime.t;
      stats : WireProtocol.build_stats;
      results : WireProtocol.build_result list;
    }
  | BuildFailed of {
      session_id : Session_id.t;
      failed_at : Datetime.t;
      stats : WireProtocol.build_stats;
      built : WireProtocol.build_result list;
      errors : WireProtocol.build_result list;
    }
  | PlanningFailed of {
      session_id : Session_id.t;
      failed_at : Datetime.t;
      reason : string;
    }
  | CycleDetected of {
      session_id : Session_id.t;
      detected_at : Datetime.t;
      cycle_nodes : string list;
    }

type build_target = BuildPackage of string | BuildAll

let connect_local ~workspace =
  match Tusk_server.start_local ~workspace ~config:Tusk_server.Server_config.default with
  | Ok server_pid -> Ok { server_pid }
  | Error exn -> Error (Exception.to_string exn)

let close _t = ()

let send_request t request =
  send t.server_pid (Protocol.ServerRequest request)

let receive_response ~selector = receive ~selector ()

let convert_build_stats (stats : Protocol.BuildStats.t) :
    WireProtocol.build_stats =
  {
    WireProtocol.duration_ms =
      int_of_float (Protocol.BuildStats.get_build_duration stats *. 1000.0);
    packages_built = Protocol.BuildStats.get_packages_built stats;
    packages_failed = Protocol.BuildStats.get_packages_failed stats;
    total_modules = Protocol.BuildStats.get_total_modules stats;
    cache_hits = Protocol.BuildStats.get_cache_hits stats;
    cache_misses = Protocol.BuildStats.get_cache_misses stats;
  }

let convert_build_result (result : Tusk_executor.Package_builder.build_result) :
    WireProtocol.build_result =
  let status : WireProtocol.build_status =
    match result.status with
    | Tusk_executor.Package_builder.Cached artifact -> WireProtocol.Cached artifact
    | Tusk_executor.Package_builder.Built artifact -> WireProtocol.Built artifact
    | Tusk_executor.Package_builder.Failed err ->
        let wire_err : WireProtocol.package_error =
          match err with
          | Tusk_executor.Package_builder.PlanningFailed e ->
              WireProtocol.PlanningFailed e
          | Tusk_executor.Package_builder.ExecutionFailed { message } ->
              WireProtocol.ExecutionFailed { message }
          | Tusk_executor.Package_builder.ActionExecutionFailed { message } ->
              WireProtocol.ActionExecutionFailed { message }
          | Tusk_executor.Package_builder.ActionOutputsNotCreated { missing } ->
              WireProtocol.ActionOutputsNotCreated { missing }
          | Tusk_executor.Package_builder.ActionDependenciesFailed { failed } ->
              WireProtocol.ActionDependenciesFailed { failed }
        in
        WireProtocol.Failed wire_err
  in
  { WireProtocol.package = result.package; status; duration = result.duration }

let same_session left right =
  Session_id.to_string left = Session_id.to_string right

let rec handle_streaming_events t session_id callback =
  let selector msg =
    match msg with
    | Protocol.ServerResponse
        (Protocol.BuildEvent { session_id = event_session_id; event }) ->
        `select (`BuildEvent (event_session_id, event))
    | Protocol.ServerResponse
        (Protocol.BuildCompleted
          { session_id = event_session_id; completed_at; stats; results }) ->
        `select (`BuildCompleted (event_session_id, completed_at, stats, results))
    | Protocol.ServerResponse
        (Protocol.BuildFailed
          { session_id = event_session_id; failed_at; stats; built; errors }) ->
        `select
          (`BuildFailed (event_session_id, failed_at, stats, built, errors))
    | Protocol.ServerResponse
        (Protocol.PlanningFailed
          { session_id = event_session_id; failed_at; reason }) ->
        `select (`PlanningFailed (event_session_id, failed_at, reason))
    | Protocol.ServerResponse
        (Protocol.CycleDetected
          { session_id = event_session_id; detected_at; cycle_nodes }) ->
        `select (`CycleDetected (event_session_id, detected_at, cycle_nodes))
    | Protocol.ServerResponse
        (Protocol.PackageNotFound
          { session_id = event_session_id; package_name; available_packages }) ->
        `select
          (`PackageNotFound (event_session_id, package_name, available_packages))
    | _ -> `skip
  in
  match receive_response ~selector with
  | `BuildEvent (event_session_id, event) ->
      if same_session session_id event_session_id then callback (BuildEvent event);
      handle_streaming_events t session_id callback
  | `BuildCompleted (event_session_id, completed_at, stats, results) ->
      if same_session session_id event_session_id then
        let final_event =
          BuildCompleted
            {
              session_id = event_session_id;
              completed_at;
              stats = convert_build_stats stats;
              results = List.map convert_build_result results;
            }
        in
        callback final_event;
        Ok final_event
      else handle_streaming_events t session_id callback
  | `BuildFailed (event_session_id, failed_at, stats, built, errors) ->
      if same_session session_id event_session_id then
        let final_event =
          BuildFailed
            {
              session_id = event_session_id;
              failed_at;
              stats = convert_build_stats stats;
              built = List.map convert_build_result built;
              errors = List.map convert_build_result errors;
            }
        in
        callback final_event;
        Ok final_event
      else handle_streaming_events t session_id callback
  | `PlanningFailed (event_session_id, failed_at, reason) ->
      if same_session session_id event_session_id then
        let final_event =
          PlanningFailed { session_id = event_session_id; failed_at; reason }
        in
        callback final_event;
        Ok final_event
      else handle_streaming_events t session_id callback
  | `CycleDetected (event_session_id, detected_at, cycle_nodes) ->
      if same_session session_id event_session_id then
        let final_event =
          CycleDetected { session_id = event_session_id; detected_at; cycle_nodes }
        in
        callback final_event;
        Ok final_event
      else handle_streaming_events t session_id callback
  | `PackageNotFound (event_session_id, package_name, available_packages) ->
      if same_session session_id event_session_id then
        Error (PackageNotFound { package_name; available_packages })
      else handle_streaming_events t session_id callback

let build_streaming t target ?target_arch callback =
  let target =
    match target with
    | BuildPackage package -> Protocol.Package package
    | BuildAll -> Protocol.All
  in
  let session_id = Session_id.make () in
  send_request t
    (Protocol.Build { client_pid = self (); target; target_arch; session_id });
  let selector msg =
    match msg with
    | Protocol.ServerResponse
        (Protocol.BuildStarted
          { session_id = started_session_id; started_at = _ })
      when same_session session_id started_session_id ->
        `select (Ok started_session_id)
    | Protocol.ServerResponse
        (Protocol.PackageNotFound
          { session_id = event_session_id; package_name; available_packages })
      when same_session session_id event_session_id ->
        `select (Error (PackageNotFound { package_name; available_packages }))
    | _ -> `skip
  in
  match receive_response ~selector with
  | Ok started_session_id ->
      callback (BuildStarted started_session_id);
      handle_streaming_events t started_session_id callback
  | Error err -> Error err

let find_executable t name =
  send_request t (Protocol.FindExecutable { client_pid = self (); name });
  let selector msg =
    match msg with
    | Protocol.ServerResponse (Protocol.ExecutableFound { package; binary }) ->
        `select (Ok (Some (package, binary)))
    | Protocol.ServerResponse Protocol.ExecutableNotFound -> `select (Ok None)
    | _ -> `skip
  in
  receive_response ~selector

let find_artifact t ~package ~kind ~name =
  send_request t
    (Protocol.FindArtifact { client_pid = self (); package; kind; name });
  let selector msg =
    match msg with
    | Protocol.ServerResponse (Protocol.ArtifactFound { path }) ->
        `select (Ok (Path.to_string path))
    | Protocol.ServerResponse (Protocol.ArtifactNotFound { error }) ->
        `select (Error error)
    | _ -> `skip
  in
  receive_response ~selector

let new_package t ~path ~name ~is_library =
  let path =
    match Path.of_string path with
    | Ok path -> path
    | Error _ -> Path.v path
  in
  send_request t
    (Protocol.NewPackage { client_pid = self (); path; name; is_library });
  let selector msg =
    match msg with
    | Protocol.ServerResponse (Protocol.PackageCreated { path; name }) ->
        `select (Ok (path, name))
    | Protocol.ServerResponse (Protocol.PackageCreationError { error }) ->
        `select (Error error)
    | _ -> `skip
  in
  receive_response ~selector
