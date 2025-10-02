(** Build server - Miniriot process that orchestrates builds *)

open Std
open Miniriot
open Model
open Core
open Tusk_protocol
open Ocaml
open Executor
module Log = Std.Log

type t = Pid.t
(** Server handle is just a PID *)

type state = {
  active_build_graph : Build_graph.t;
      (* Graph for current build (full or filtered) *)
  build_graph : Build_graph.t; (* Full workspace build graph *)
  build_queue : Build_queue.t; (* Two-queue system for dependency ordering *)
  build_results : Build_results.t;
  build_start_time : float option; (* Start time for build stats *)
  toolchain : Toolchains.toolchain; (* Current toolchain *)
  workspace : Workspace.t;
  workers : int; (* Number of workers to use *)
}
(** Server state *)

(** this is the main server loop where we'll handle all the incoming requests
    from clients, be it the CLI, MCP, LSP, or direct RPC communication.

    None of these handlers can really block the loop, so we gotta handle and
    dispatch, except restart and shutdown *)
let rec loop state =
  let selector msg =
    match msg with ServerRequest req -> `select req | _ -> `skip
  in

  Log.trace "Server loop waiting for message...";
  match receive ~selector () with
  | Ping { client_pid } ->
      Log.debug "Server loop received: Ping";
      handle_ping state client_pid
  | Build { client_pid; target; session_id } ->
      Log.debug "Server loop received: Build";
      handle_build state client_pid target session_id
  | ScanWorkspace { client_pid; current_dir } ->
      Log.debug "Server loop received: ScanWorkspace";
      handle_scan_workspace state client_pid current_dir
  | GetWorkspaceConfig { client_pid } ->
      Log.debug "Server loop received: GetWorkspaceConfig";
      handle_get_workspace_config state client_pid
  | GetPackageInfo { client_pid; package_name } ->
      Log.debug "Server loop received: GetPackageInfo";
      handle_get_package_info state client_pid package_name
  | GetBuildGraph { client_pid } ->
      Log.debug "Server loop received: GetBuildGraph";
      handle_get_build_graph state client_pid
  | FormatFile { client_pid; file_path; check_only } ->
      Log.debug "Server loop received: FormatFile";
      handle_format_file state client_pid file_path check_only
  | FormatCode { client_pid; code; file_path } ->
      Log.debug "Server loop received: FormatCode";
      handle_format_code state client_pid code file_path
  | FormatAll { client_pid; mode } ->
      Log.debug "Server loop received: FormatAll";
      handle_format_all state client_pid mode
  | NewPackage { client_pid; path; name; is_library } ->
      Log.debug "Server loop received: NewPackage";
      handle_new_package state client_pid path name is_library

(** Handler for the ping message. *)
and handle_ping state client_pid =
  Log.debug "handle_ping: Received Ping from %s" (Pid.to_string client_pid);
  Log.debug "handle_ping: Sending Pong response";
  send client_pid (ServerResponse Pong);
  Log.debug "handle_ping: Pong sent, continuing loop";
  loop state

(** Handler for the scan workspace message. *)
and handle_scan_workspace state client_pid current_dir =
  (* Rescan the workspace and update state *)
  let workspace =
    Workspace_manager.scan current_dir
    |> Result.expect ~msg:"tusk_server: operation failed"
  in
  let build_graph = Build_graph.create workspace state.toolchain in
  let new_state = { state with workspace; build_graph } in
  (* Send build completed to signal scan is done *)
  (* For scan workspace, we don't have a build session *)
  send client_pid
    (ServerResponse
       (BuildCompleted
          {
            session_id = Session_id.make ();
            completed_At = Datetime.now ();
            stats = Tusk_protocol.BuildStats.make ();
          }));
  loop new_state

(** Handler for getting workspace configuration. *)
and handle_get_workspace_config state client_pid =
  Log.debug "Server: Received GetWorkspaceConfig from %s"
    (Pid.to_string client_pid);
  (* Send the current workspace and toolchain information *)
  send client_pid
    (ServerResponse
       (WorkspaceConfig
          { workspace = state.workspace; toolchain = state.toolchain }));
  loop state

(** Handler for getting package information. *)
and handle_get_package_info state client_pid package_name =
  Log.debug "Server: Received GetPackageInfo for %s from %s" package_name
    (Pid.to_string client_pid);

  (* Find the package in the workspace *)
  let package_opt =
    List.find_opt
      (fun (pkg : Workspace.package) -> pkg.name = package_name)
      state.workspace.packages
  in

  match package_opt with
  | None ->
      (* Package not found *)
      Log.debug "Server: Package %s not found" package_name;
      send client_pid
        (ServerResponse
           (PackageInfo
              {
                package =
                  {
                    name = package_name;
                    path = Path.of_string "" |> Result.unwrap;
                    relative_path = Path.of_string "" |> Result.unwrap;
                    dependencies = [];
                  };
                sources = [];
                dependencies = [];
              }));
      loop state
  | Some package -> (
      (* Find the build node for this package *)
      let node_opt = Build_graph.find_node state.build_graph package_name in

      match node_opt with
      | None ->
          (* No build node, return package without sources *)
          send client_pid
            (ServerResponse
               (PackageInfo { package; sources = []; dependencies = [] }));
          loop state
      | Some node ->
          (* Return package with sources and dependencies *)
          (* Resolve dependency IDs to actual nodes for the client *)
          let dep_nodes =
            List.map
              (Build_graph.get_node state.build_graph)
              node.Build_node.deps
          in
          send client_pid
            (ServerResponse
               (PackageInfo
                  {
                    package;
                    sources =
                      List.map (fun s -> s.Build_node.file) node.Build_node.srcs;
                    dependencies = dep_nodes;
                  }));
          loop state)

(** Handler for getting the build graph. *)
and handle_get_build_graph state client_pid =
  Log.debug "Server: Received GetBuildGraph from %s" (Pid.to_string client_pid);

  (* Get all nodes from the build graph using topological sort *)
  let nodes =
    try Build_graph.topological_sort state.build_graph
    with Build_graph.Cycle_detected cycle_nodes ->
      (* Just return empty list if there's a cycle - the client can detect this separately *)
      []
  in

  (* Send the build graph *)
  send client_pid (ServerResponse (BuildGraph { nodes }));
  loop state

and handle_format_file state client_pid file_path check_only =
  Log.debug "Server: Received FormatFile from %s for %s (check_only=%b)"
    (Pid.to_string client_pid) (Path.to_string file_path) check_only;

  let response =
    match
      Ocamlformat.format_file ~toolchain:state.toolchain ~file_path ~check_only
    with
    | Formatted { code; changed } ->
        FormatResult { formatted_code = code; changed }
    | Error error -> FormatError { error }
  in
  send client_pid (ServerResponse response);
  loop state

and handle_format_code state client_pid code file_path =
  Log.debug "Server: Received FormatCode from %s" (Pid.to_string client_pid);

  let response =
    match
      Ocamlformat.format_code ~toolchain:state.toolchain ~code ~file_path
    with
    | Formatted { code; changed } ->
        FormatResult { formatted_code = code; changed }
    | Error error -> FormatError { error }
  in
  send client_pid (ServerResponse response);
  loop state

and handle_format_all state client_pid mode =
  Log.debug "Server: Received FormatAll from %s (mode=%s)"
    (Pid.to_string client_pid)
    (match mode with `check -> "check" | `write -> "write");

  (* TODO: Implement using GenericWorkerPool for concurrent formatting *)
  send client_pid
    (ServerResponse
       (FormatError { error = "FormatAll not yet implemented with worker pool" }));
  loop state

and handle_new_package state client_pid path name is_library =
  Log.debug "Server: Received NewPackage from %s for %s at %s"
    (Pid.to_string client_pid) name (Path.to_string path);

  let src_dir = Path.(path / Path.v "src") in

  (* Create directories *)
  let _ =
    Fs.create_dir_all src_dir |> Result.expect ~msg:"Failed to create src dir"
  in

  (* Create main module files *)
  let main_ml = Path.(src_dir / Path.v (name ^ ".ml")) in
  let main_mli = Path.(src_dir / Path.v (name ^ ".mli")) in

  let ml_content =
    if is_library then "(** Main module for " ^ name ^ " library *)\n"
    else
      "(** Main entry point for " ^ name
      ^ " *)\n\nlet () = print_endline \"Hello from " ^ name ^ "\"\n"
  in

  let mli_content =
    if is_library then
      "(** " ^ String.capitalize_ascii name ^ " library interface *)\n"
    else "(** " ^ String.capitalize_ascii name ^ " executable interface *)\n"
  in

  (* Write module files *)
  let _ = Fs.write ml_content main_ml in
  let _ = Fs.write mli_content main_mli in

  (* Create package tusk.toml *)
  let package_toml = Path.(path / Path.v "tusk.toml") in
  let toml_content =
    format
      "[package]\n\
       name = \"%s\"\n\
       version = \"0.1.0\"\n\n\
       [dependencies]\n\
       std = \"*\"\n\
       # Add dependencies here\n\n\
       %s\n"
      name
      (if is_library then "" else "[package.bin]\nmain = \"" ^ name ^ "\"")
  in
  let _ = Fs.write toml_content package_toml in

  (* Send success response *)
  send client_pid
    (ServerResponse (PackageCreated { path = Path.to_string path; name }));
  loop state

(** Handler for the build message. *)
and handle_build state client_pid target session_id =
  Log.debug "Server: handle_build called for target: %s"
    (match target with All -> "All" | Package p -> format "Package(%s)" p);
  Build_server.start ~workspace:state.workspace ~toolchain:state.toolchain
    ~workers:state.workers ~session_id ~client_pid ~target;
  loop state

let start_tcp_server ~server ~port =
  spawn @@ fun () ->
  let addr = Net.Addr.tcp Net.Addr.loopback port in
  Log.debug "TCP server binding to 127.0.0.1:%d" port;
  let jsonrpc_server = Tusk_jsonrpc.Server.create server in
  let handler ~req stream =
    Log.debug "TCP server received connection";
    Log.debug "Request: %s" req;
    let reply msg =
      Log.debug "reply() called with: %s" msg;
      let bytes = Bytes.of_string (msg ^ "\n") in
      Log.debug "Writing %d bytes to stream" (Bytes.length bytes);
      match
        Net.TcpStream.write stream bytes ~pos:0 ~len:(Bytes.length bytes) ()
      with
      | Ok bytes_written ->
          Log.debug "Successfully wrote %d bytes" bytes_written;
          ()
      | Error e ->
          Log.error "Failed to write to stream: %s"
            (match e with `Closed -> "closed" | `System_error msg -> msg);
          failwith "network write failed"
    in
    Log.debug "Calling Jsonrpc.Server.handle_message";
    Jsonrpc.Server.handle_message jsonrpc_server reply req;
    Log.debug "Handler finished"
  in
  Log.debug "Starting TCP listener...";
  let _ = Net.TcpServer.listen addr ~handler in
  Log.info "TCP server listening successfully";
  Ok ()

(** Write daemon files so RPC clients can find us *)
let write_daemon_files ~workspace ~port =
  let home =
    match Env.home_dir () with
    | Some h -> h
    | None -> failwith "Failed to get home directory"
  in
  let root_str = Path.to_string workspace.Workspace.root in
  let project_id = format "%08x" (Hashtbl.hash root_str) in
  let daemon_path =
    Path.(home / Path.v ".tusk" / Path.v "daemons" / Path.v project_id)
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

(** Main server loop *)
let init ~current_dir ~workers ~port =
  Log.info "Tusk server initializing...";
  Log.debug "Current dir: %s" (Path.to_string current_dir);
  Log.debug "Workers: %d, Port: %d" workers port;

  let server_pid = self () in
  Log.trace "Server PID: %s" (Pid.to_string server_pid);

  Log.info "Scanning workspace...";
  let workspace =
    Workspace_manager.scan current_dir
    |> Result.expect ~msg:"tusk_server: operation failed"
  in
  Log.info "Workspace scanned successfully: %d packages found"
    (List.length workspace.packages);

  Log.info "Loading toolchains...";
  let toolchain = Toolchains.ready_toolchains workspace in
  Log.info "Toolchain ready: %s"
    (Path.to_string (Toolchains.get_toolchain_path toolchain));

  Log.info "Building dependency graph...";
  let build_graph = Build_graph.create workspace toolchain in
  Log.info "Build graph created with %d nodes" (Build_graph.size build_graph);

  let build_results = Build_results.create () in
  let build_queue = Build_queue.create build_results in

  Log.info "Starting TCP server on port %d..." port;
  let _ = start_tcp_server ~server:server_pid ~port in
  Log.info "TCP server started successfully";

  write_daemon_files ~workspace ~port;

  let state =
    {
      workspace;
      toolchain;
      build_graph;
      active_build_graph = build_graph;
      build_results;
      build_queue;
      build_start_time = None;
      workers;
    }
  in

  Log.info "Tusk server entering main loop";
  loop state

(** Start the server with TCP listener for RPC. This function makes the current
    process _become_ the Tusk server and spin up a sepaarate riot process for
    the listening in to tcp requests *)
let start () =
  spawn (fun () ->
      let current_dir =
        Env.current_dir ()
        |> Result.expect ~msg:"tusk_server: could not get current dir"
      in
      let workers = Std.System.available_parallelism in
      let port = 9753 in
      init ~current_dir ~workers ~port)

(** Start with listener - makes current process become the server *)
let start_with_listener () =
  try
    Log.set_level Log.Debug;
    Log.info "Starting Tusk server with listener";
    let current_dir =
      Env.current_dir ()
      |> Result.expect ~msg:"tusk_server: could not get current dir"
    in
    Log.debug "Got current directory: %s" (Path.to_string current_dir);
    let workers = Std.System.available_parallelism in
    let port = 9753 in
    Log.debug "Configuration: workers=%d, port=%d" workers port;
    init ~current_dir ~workers ~port
  with
  | Failure msg ->
      Log.error "Server initialization failed: %s" msg;
      raise (Failure msg)
  | exn ->
      Log.error "Server initialization failed with exception: %s"
        (Exception.to_string exn);
      raise exn

(** Scan workspace *)
let scan_workspace server =
  send server
    (ServerRequest
       (ScanWorkspace
          {
            client_pid = self ();
            current_dir =
              Env.current_dir ()
              |> Result.expect ~msg:"tusk_server: operation failed";
          }));
  let selector msg =
    match msg with
    | ServerResponse (BuildCompleted _) -> `select ()
    | _ -> `skip
  in
  match receive ~selector () with
  | () ->
      (* TODO: Return actual workspace from server response *)
      let workspace =
        Workspace_manager.scan
          (Env.current_dir ()
          |> Result.expect ~msg:"tusk_server: operation failed")
        |> Result.expect ~msg:"tusk_server: operation failed"
      in
      Ok workspace
  | exception _ -> Error Error.ScanWorkspaceError

(** Shutdown server *)
let shutdown server =
  (* TODO: Implement proper shutdown *)
  Ok ()

(** Build all packages *)
let build_all server =
  send server
    (ServerRequest
       (Build
          {
            client_pid = self ();
            target = All;
            session_id = Session_id.make ();
          }));
  let selector msg =
    match msg with
    | ServerResponse (BuildCompleted _) -> `select ()
    | _ -> `skip
  in
  match receive ~selector () with
  | () -> Ok (Build_results.create ())
  | exception _ -> Error Error.ScanWorkspaceError

(** Build specific package *)
let build_package ~name =
  (* For now, we'll need to start a server if not already running *)
  let server = start () in
  send server
    (ServerRequest
       (Build
          {
            client_pid = self ();
            target = Package name;
            session_id = Session_id.make ();
          }));
  let selector msg =
    match msg with
    | ServerResponse (BuildCompleted _) -> `select ()
    | _ -> `skip
  in
  match receive ~selector () with
  | () -> Ok (Build_results.create ())
  | exception _ -> Error Error.ScanWorkspaceError
