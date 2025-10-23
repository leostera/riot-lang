open Std
open Miniriot
open Tusk_model
module Protocol = Protocol
module Server_manager = Server_manager

type t = { server_pid : Pid.t }
type build_request = Protocol.target

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
  let planner_target =
    match request with
    | Protocol.All -> Tusk_planner.Workspace_planner.All
    | Protocol.Package name -> Tusk_planner.Workspace_planner.Package name
  in
  let result =
    Tusk_executor.Coordinator.build_workspace ~workspace:config.workspace
      ~toolchain:config.toolchain ~store:config.store ~target:planner_target
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

type server_state = {
  workspace : Workspace.t;
  toolchain : Tusk_toolchain.t;
  store : Tusk_store.Store.t;
  concurrency : int;
}

let rec loop state =
  let selector msg =
    match msg with Protocol.ServerRequest req -> `select req | _ -> `skip
  in
  Log.trace "Server loop waiting for message...";
  match receive ~selector () with
  | Protocol.Ping { client_pid } ->
      Log.debug "Server loop received: Ping";
      send client_pid (Protocol.ServerResponse Protocol.Pong);
      loop state
  | Protocol.GetWorkspaceConfig { client_pid } ->
      Log.debug "Server loop received: GetWorkspaceConfig";
      send client_pid
        (Protocol.ServerResponse
           (Protocol.WorkspaceConfig
              { workspace = state.workspace; toolchain = state.toolchain }));
      loop state
  | Protocol.GetBuildGraph { client_pid } ->
      Log.debug "Server loop received: GetBuildGraph";
      send client_pid
        (Protocol.ServerResponse
           (Protocol.BuildGraph { nodes = state.workspace.packages }));
      loop state
  | Protocol.GetPackageInfo { client_pid; package_name } ->
      Log.debug "Server loop received: GetPackageInfo(%s)" package_name;
      let package_opt =
        List.find_opt
          (fun (pkg : Package.t) -> pkg.name = package_name)
          state.workspace.packages
      in
      (match package_opt with
      | None ->
          send client_pid
            (Protocol.ServerResponse
               (Protocol.PackageInfo
                  {
                    package =
                      {
                        name = package_name;
                        path = Path.of_string "" |> Result.unwrap;
                        relative_path = Path.of_string "" |> Result.unwrap;
                        dependencies = [];
                        binaries = [];
                        library = None;
                        test_library = None;
                        test_modules = [];
                      };
                    sources = [];
                    dependencies = [];
                  }))
      | Some package ->
          send client_pid
            (Protocol.ServerResponse
               (Protocol.PackageInfo
                  { package; sources = []; dependencies = [] })));
      loop state
  | Protocol.FindExecutable { client_pid; name } ->
      Log.debug "Server loop received: FindExecutable(%s)" name;
      let found =
        List.find_map
          (fun (pkg : Package.t) ->
            List.find_opt
              (fun (bin : Package.binary) -> bin.name = name)
              pkg.binaries
            |> Option.map (fun _ -> pkg))
          state.workspace.packages
      in
      (match found with
      | Some pkg ->
          send client_pid
            (Protocol.ServerResponse
               (Protocol.ExecutableFound { package = pkg.name; binary = name }))
      | None ->
          send client_pid (Protocol.ServerResponse Protocol.ExecutableNotFound));
      loop state
  | Protocol.FindArtifact { client_pid; package; kind; name } ->
      Log.debug "Server loop received: FindArtifact(%s, %s, %s)" package kind
        name;
      let path =
        Path.(
          state.workspace.root / Path.v "target" / Path.v "debug" / Path.v "out"
          / Path.v "packages" / Path.v package / Path.v name)
      in
      let response =
        match Fs.exists path with
        | Ok true -> Protocol.ServerResponse (Protocol.ArtifactFound { path })
        | Ok false | Error _ ->
            Protocol.ServerResponse
              (Protocol.ArtifactNotFound
                 {
                   error =
                     format "Artifact '%s' not found in package '%s'" name
                       package;
                 })
      in
      send client_pid response;
      loop state
  | Protocol.Build { client_pid; target; session_id } ->
      Log.debug "Server loop received: Build";
      send client_pid
        (Protocol.ServerResponse
           (Protocol.BuildStarted { session_id; started_at = Datetime.now () }));
      spawn (fun () ->
          let planner_target =
            match target with
            | Protocol.All -> Tusk_planner.Workspace_planner.All
            | Protocol.Package name ->
                Tusk_planner.Workspace_planner.Package name
          in
          let result =
            Tusk_executor.Coordinator.build_workspace ~workspace:state.workspace
              ~toolchain:state.toolchain ~store:state.store
              ~target:planner_target ~concurrency:state.concurrency
          in
          match result with
          | Ok _workspace_result ->
              send client_pid
                (Protocol.ServerResponse
                   (Protocol.BuildCompleted
                      {
                        session_id;
                        completed_At = Datetime.now ();
                        stats = Protocol.BuildStats.make ();
                      }));
              Ok ()
          | Error err ->
              let error_msg =
                match err with
                | Tusk_planner.Workspace_planner.PackageNotFound
                    { name; available } ->
                    send client_pid
                      (Protocol.ServerResponse
                         (Protocol.PackageNotFound
                            {
                              session_id;
                              package_name = name;
                              available_packages = available;
                            }));
                    format "Package '%s' not found" name
                | Tusk_planner.Workspace_planner.CycleDetected { cycle } ->
                    send client_pid
                      (Protocol.ServerResponse
                         (Protocol.CycleDetected
                            {
                              session_id;
                              cycle_nodes = cycle;
                              detected_at = Datetime.now ();
                            }));
                    format "Cycle detected: %s" (String.concat " -> " cycle)
              in
              Log.error "Build failed: %s" error_msg;
              Ok ())
      |> ignore;
      loop state
  | Protocol.ScanWorkspace { client_pid; current_dir } ->
      Log.debug "Server loop received: ScanWorkspace";
      let workspace =
        Workspace_manager.scan current_dir
        |> Result.expect ~msg:"tusk_server: workspace scan failed"
      in
      let new_state = { state with workspace } in
      send client_pid
        (Protocol.ServerResponse
           (Protocol.BuildCompleted
              {
                session_id = Session_id.make ();
                completed_At = Datetime.now ();
                stats = Protocol.BuildStats.make ();
              }));
      loop new_state
  | Protocol.FormatFile { client_pid; file_path; check_only } ->
      Log.debug "Server loop received: FormatFile";
      let ocamlformat = Tusk_toolchain.ocamlformat state.toolchain in
      let response =
        match
          Tusk_toolchain.Ocamlformat.format_file ocamlformat ~file_path
            ~check_only
        with
        | Tusk_toolchain.Ocamlformat.Formatted { code; changed } ->
            Protocol.FormatResult { formatted_code = code; changed }
        | Tusk_toolchain.Ocamlformat.Error err ->
            Protocol.FormatError { error = err }
      in
      send client_pid (Protocol.ServerResponse response);
      loop state
  | Protocol.FormatCode { client_pid; code; file_path } ->
      Log.debug "Server loop received: FormatCode";
      let ocamlformat = Tusk_toolchain.ocamlformat state.toolchain in
      let response =
        match
          Tusk_toolchain.Ocamlformat.format_code ocamlformat ~code ~file_path
        with
        | Tusk_toolchain.Ocamlformat.Formatted { code; changed } ->
            Protocol.FormatResult { formatted_code = code; changed }
        | Tusk_toolchain.Ocamlformat.Error err ->
            Protocol.FormatError { error = err }
      in
      send client_pid (Protocol.ServerResponse response);
      loop state
  | Protocol.FormatAll { client_pid; mode = _ } ->
      Log.debug "Server loop received: FormatAll";
      send client_pid
        (Protocol.ServerResponse
           (Protocol.FormatError
              { error = "FormatAll not yet implemented with worker pool" }));
      loop state
  | Protocol.NewPackage { client_pid; path; name; is_library } -> (
      Log.debug "Server loop received: NewPackage";
      let src_dir = Path.(path / Path.v "src") in
      match Fs.create_dir_all src_dir with
      | Error _ ->
          send client_pid
            (Protocol.ServerResponse
               (Protocol.PackageCreationError
                  { error = "Failed to create src directory" }));
          loop state
      | Ok () -> (
          let module_name =
            String.split_on_char '-' name
            |> List.map String.capitalize_ascii
            |> String.concat ""
          in
          let main_ml = Path.(src_dir / Path.v (module_name ^ ".ml")) in
          let ml_content =
            if is_library then
              format "open Std\n\n(** Main module for %s library *)\n" name
            else "open Std\n\nlet () = println \"Hello, World!\"\n"
          in
          let package_toml = Path.(path / Path.v "tusk.toml") in
          let toml_content =
            format
              "[package]\n\
               name = \"%s\"\n\
               version = \"0.1.0\"\n\n\
               [dependencies]\n\
               std = \"*\"\n\n\
               %s\n"
              name
              (if is_library then ""
               else format "[package.bin]\nmain = \"%s\"" name)
          in
          match
            (Fs.write ml_content main_ml, Fs.write toml_content package_toml)
          with
          | Ok (), Ok () ->
              let updated_workspace =
                Workspace_manager.scan state.workspace.root
                |> Result.expect ~msg:"Failed to rescan workspace"
              in
              let updated_state =
                { state with workspace = updated_workspace }
              in
              send client_pid
                (Protocol.ServerResponse
                   (Protocol.PackageCreated { path = Path.to_string path; name }));
              loop updated_state
          | _ ->
              send client_pid
                (Protocol.ServerResponse
                   (Protocol.PackageCreationError
                      { error = "Failed to write package files" }));
              loop state))
  | _ ->
      Log.warn "Received unknown request, ignoring";
      loop state

let write_daemon_files ~workspace ~port =
  let home =
    match Env.home_dir () with
    | Some h -> h
    | None -> failwith "Failed to get home directory"
  in
  let project_id = Workspace.project_id workspace in
  let daemon_path =
    Path.(home / Path.v ".tusk" / Path.v "projects" / Path.v project_id)
  in

  let _ =
    Fs.create_dir_all daemon_path
    |> Result.expect ~msg:"Failed to create daemon dir"
  in

  let pid = Kernel.System.OsProcess.current_pid () in
  let pid_file = Path.(daemon_path / Path.v "server.pid") in
  let port_file = Path.(daemon_path / Path.v "server.port") in

  let _ = Fs.write (string_of_int pid) pid_file in
  let _ = Fs.write (string_of_int port) port_file in
  Log.debug "Wrote daemon files: pid=%d, port=%d" pid port

let start_tcp_server ~server ~port =
  spawn (fun () ->
      let addr = Net.Addr.tcp Net.Addr.loopback port in
      Log.debug "TCP server binding to 127.0.0.1:%d" port;
      let handler ~req stream =
        Log.debug "TCP server received connection";
        Log.debug "Request: %s" req;
        let reply msg =
          let bytes = Bytes.of_string (msg ^ "\n") in
          match
            Net.TcpStream.write stream bytes ~pos:0 ~len:(Bytes.length bytes) ()
          with
          | Ok _bytes_written -> ()
          | Error e ->
              Log.error "Failed to write to stream: %s"
                (match e with
                | `Closed -> "closed"
                | `Connection_refused -> "connection refused"
                | `System_error msg -> msg);
              failwith "network write failed"
        in
        spawn (fun () ->
            send server
              (Protocol.ServerRequest (Protocol.Ping { client_pid = self () }));
            let selector msg =
              match msg with
              | Protocol.ServerResponse Protocol.Pong -> `select ()
              | _ -> `skip
            in
            match receive ~selector () with
            | () ->
                reply "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":\"pong\"}";
                Ok ()
            | exception _ ->
                reply
                  "{\"jsonrpc\":\"2.0\",\"id\":1,\"error\":{\"code\":-32603,\"message\":\"Internal \
                   error\"}}";
                Ok ())
        |> ignore;
        Log.debug "Handler finished"
      in
      Log.debug "Starting TCP listener...";
      let _ = Net.TcpServer.listen addr ~handler in
      Log.info "TCP server listening successfully";
      Ok ())

let start_with_listener () =
  try
    Log.set_level Log.Debug;
    Log.info "Starting Tusk server with listener";
    let current_dir =
      Env.current_dir ()
      |> Result.expect ~msg:"tusk_server: could not get current dir"
    in
    Log.debug "Got current directory: %s" (Path.to_string current_dir);

    let server_pid = self () in
    Log.trace "Server PID: %s" (Pid.to_string server_pid);

    Log.info "Scanning workspace...";
    let workspace =
      Workspace_manager.scan current_dir
      |> Result.expect ~msg:"tusk_server: workspace scan failed"
    in
    Log.info "Workspace scanned successfully: %d packages found"
      (List.length workspace.packages);

    let port = Workspace.server_port workspace in
    Log.debug "Using workspace-specific port: %d" port;

    Log.info "Loading toolchains...";
    let toolchain_config = Toolchain_config.from_workspace workspace in
    let toolchain =
      Tusk_toolchain.init ~config:toolchain_config
      |> Result.expect ~msg:"tusk_server: toolchain loading failed"
    in
    Log.info "Toolchain ready";

    Log.info "Initializing store...";
    let store = Tusk_store.Store.create ~workspace in
    Log.info "Store initialized";

    Log.info "Starting TCP server on port %d..." port;
    let _ = start_tcp_server ~server:server_pid ~port in
    Log.info "TCP server started successfully";

    write_daemon_files ~workspace ~port;

    let state =
      {
        workspace;
        toolchain;
        store;
        concurrency = System.available_parallelism;
      }
    in

    Log.info "Tusk server entering main loop";
    loop state
  with exn ->
    Log.error "Server initialization failed: %s" (Exception.to_string exn);
    Error exn
