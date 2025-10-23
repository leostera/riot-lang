open Std
open Miniriot
open Tusk_model

type t = { server_pid : Pid.t }
type build_request = BuildAll | BuildPackage of string

type build_event =
  | Started of { session_id : string; started_at : Time.Instant.t }
  | PackageStarted of { package : string }
  | PackageCompleted of {
      package : string;
      status : [ `built | `cached | `failed ];
      duration_ms : int;
    }
  | Completed of {
      session_id : string;
      completed_at : Time.Instant.t;
      total_duration_ms : int;
      cached_count : int;
      built_count : int;
      failed_count : int;
    }
  | Failed of { session_id : string; error : string }
  | CycleDetected of { cycle : string list }

type server_config = {
  workspace : Workspace.t;
  toolchain : Tusk_toolchain.t;
  store : Tusk_store.Store.t;
  concurrency : int;
}

type Message.t +=
  | Build of { request : build_request; client : Pid.t }
  | Shutdown

type Message.t += BuildResponse of (unit, string) result

let build_worker config request client =
  let target =
    match request with
    | BuildAll -> Tusk_planner.Workspace_planner.All
    | BuildPackage name -> Tusk_planner.Workspace_planner.Package name
  in
  let result =
    Tusk_executor.Coordinator.build_workspace ~workspace:config.workspace
      ~toolchain:config.toolchain ~store:config.store ~target
      ~concurrency:config.concurrency
  in
  match result with
  | Ok _workspace_result ->
      send client (BuildResponse (Ok ()));
      Ok ()
  | Error err ->
      let error_msg =
        match err with
        | Tusk_planner.Workspace_planner.PackageNotFound { name; available } ->
            format "Package '%s' not found. Available: %s" name
              (String.concat ", " available)
        | Tusk_planner.Workspace_planner.CycleDetected { cycle } ->
            format "Cycle detected: %s" (String.concat " -> " cycle)
      in
      send client (BuildResponse (Error error_msg));
      Ok ()

let server_loop config =
  let rec loop () =
    let selector msg =
      match msg with
      | Build _ -> `select msg
      | Shutdown -> `select msg
      | _ -> `skip
    in
    match receive ~selector () with
    | Build { request; client } ->
        let _worker = spawn (fun () -> build_worker config request client) in
        loop ()
    | Shutdown -> Ok ()
    | _ -> loop ()
  in
  loop ()

let start ~workspace ~toolchain ~store ~concurrency =
  let config = { workspace; toolchain; store; concurrency } in
  let server_pid = spawn (fun () -> server_loop config) in
  { server_pid }

let shutdown server = send server.server_pid Shutdown

let build server request ~on_event =
  let session_id = format "build-%d" (Random.int 1000000) in
  let started_at = Time.Instant.now () in
  on_event (Started { session_id; started_at });

  send server.server_pid (Build { request; client = self () });

  let selector msg =
    match msg with BuildResponse _ -> `select msg | _ -> `skip
  in
  match receive ~selector () with
  | BuildResponse result -> (
      let completed_at = Time.Instant.now () in
      match result with
      | Ok () ->
          on_event
            (Completed
               {
                 session_id;
                 completed_at;
                 total_duration_ms = 0;
                 cached_count = 0;
                 built_count = 0;
                 failed_count = 0;
               });
          Ok ()
      | Error err ->
          on_event (Failed { session_id; error = err });
          Error err)
  | _ -> Error "Unexpected response from server"
