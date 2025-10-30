(** Internal server - Main server loop for handling requests *)

open Std
open Miniriot
open Tusk_model

(** Internal messages for server-to-server communication *)
type Message.t += UpdatePackageGraph of Tusk_planner.Package_graph.t

type server_state = {
  workspace : Workspace.t;
  toolchain : Tusk_toolchain.t;
  store : Tusk_store.Store.t;
  concurrency : int;
  package_graph : Tusk_planner.Package_graph.t;
}

(** Main server loop - handle all incoming requests *)
let rec loop state =
  let selector msg =
    match msg with
    | Protocol.ServerRequest req -> `select (`Request req)
    | UpdatePackageGraph pg -> `select (`UpdateGraph pg)
    | _ -> `skip
  in

  Log.info "[INTERNAL_SERVER] Server loop ready, awaiting next request...";
  match receive ~selector () with
  | `UpdateGraph package_graph ->
      Log.info "[INTERNAL_SERVER] Received updated package graph from build worker";
      loop { state with package_graph }
  | `Request (Protocol.Ping { client_pid }) ->
      Log.debug "Server loop received: Ping";
      handle_ping state client_pid
  | `Request (Protocol.Build { client_pid; target; session_id }) ->
      Log.debug "Server loop received: Build";
      handle_build state client_pid target session_id
  | `Request (Protocol.ScanWorkspace { client_pid; current_dir }) ->
      Log.debug "Server loop received: ScanWorkspace";
      handle_scan_workspace state client_pid current_dir
  | `Request (Protocol.GetWorkspaceConfig { client_pid }) ->
      Log.debug "Server loop received: GetWorkspaceConfig";
      handle_get_workspace_config state client_pid
  | `Request (Protocol.GetPackageInfo { client_pid; package_name }) ->
      Log.debug "Server loop received: GetPackageInfo";
      handle_get_package_info state client_pid package_name
  | `Request (Protocol.GetPackageGraph { client_pid }) ->
      Log.debug "Server loop received: GetPackageGraph";
      handle_get_package_graph state client_pid
  | `Request (Protocol.FindExecutable { client_pid; name }) ->
      Log.debug "Server loop received: FindExecutable(%s)" name;
      handle_find_executable state client_pid name
  | `Request (Protocol.FindArtifact { client_pid; package; kind; name }) ->
      Log.debug
        "Server loop received: FindArtifact(package=%s, kind=%s, name=%s)"
        package kind name;
      handle_find_artifact state client_pid package kind name
  | `Request (Protocol.FormatFile { client_pid; file_path; check_only }) ->
      Log.debug "Server loop received: FormatFile";
      handle_format_file state client_pid file_path check_only
  | `Request (Protocol.FormatCode { client_pid; code; file_path }) ->
      Log.debug "Server loop received: FormatCode";
      handle_format_code state client_pid code file_path
  | `Request (Protocol.FormatAll { client_pid; mode }) ->
      Log.debug "Server loop received: FormatAll";
      handle_format_all state client_pid mode
  | `Request (Protocol.NewPackage { client_pid; path; name; is_library }) ->
      Log.debug "Server loop received: NewPackage";
      handle_new_package state client_pid path name is_library

(** Handler for ping message *)
and handle_ping state client_pid =
  Log.debug "handle_ping: Received Ping from %s" (Pid.to_string client_pid);
  send client_pid (Protocol.ServerResponse Protocol.Pong);
  Log.debug "handle_ping: Pong sent, continuing loop";
  loop state

(** Handler for scan workspace message *)
and handle_scan_workspace state client_pid current_dir =
  let workspace =
    Workspace_manager.scan current_dir
    |> Result.expect ~msg:"tusk_server: workspace scan failed"
  in
  let package_graph = Tusk_planner.Package_graph.create workspace in
  let new_state = { state with workspace; package_graph } in
  send client_pid
    (Protocol.ServerResponse
       (Protocol.BuildCompleted
          {
            session_id = Session_id.make ();
            completed_at = Datetime.now ();
            stats = Protocol.BuildStats.make ();
            results = [];
          }));
  loop new_state

(** Handler for getting workspace configuration *)
and handle_get_workspace_config state client_pid =
  Log.debug "Server: Received GetWorkspaceConfig from %s"
    (Pid.to_string client_pid);
  send client_pid
    (Protocol.ServerResponse
       (Protocol.WorkspaceConfig
          { workspace = state.workspace; toolchain = state.toolchain }));
  loop state

(** Handler for getting package information *)
and handle_get_package_info state client_pid package_name =
  Log.debug "Server: Received GetPackageInfo for %s from %s" package_name
    (Pid.to_string client_pid);

  let package_opt =
    List.find_opt
      (fun (pkg : Package.t) -> pkg.name = package_name)
      state.workspace.packages
  in

  (match package_opt with
  | None ->
      Log.debug "Server: Package %s not found" package_name;
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
                    sources = { src = []; native = []; tests = [] };
                  };
                sources = [];
                dependencies = [];
              }))
  | Some package ->
      let dep_nodes =
        Tusk_planner.Package_graph.get_dependencies state.package_graph package
      in
      let dependencies =
        List.map Tusk_planner.Package_graph.get_package dep_nodes
      in
      let all_sources =
        List.concat
          [ package.sources.src; package.sources.native; package.sources.tests ]
      in
      send client_pid
        (Protocol.ServerResponse
           (Protocol.PackageInfo
              { package; sources = all_sources; dependencies })));
  loop state

(** Handler for getting the package graph *)
and handle_get_package_graph state client_pid =
  Log.debug "Server: Received GetPackageGraph from %s" (Pid.to_string client_pid);
  let sorted_packages =
    Tusk_planner.Package_graph.(
      topological_sort state.package_graph |> List.map get_package)
  in
  send client_pid
    (Protocol.ServerResponse (Protocol.PackageGraph { nodes = sorted_packages }));
  loop state

and handle_find_executable state client_pid name =
  Log.debug "Server: handle_find_executable %s" name;
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

and handle_find_artifact state client_pid package kind name =
  Log.info "Server: handle_find_artifact package=%s kind=%s name=%s" package
    kind name;
  
  (* Find the package in the workspace *)
  let pkg_opt =
    List.find_opt
      (fun (p : Package.t) -> p.name = package)
      state.workspace.packages
  in
  let response =
    match pkg_opt with
    | None ->
        Log.info "Server: Package '%s' not found in workspace" package;
        Protocol.ServerResponse
          (Protocol.ArtifactNotFound
             { error = format "Package '%s' not found" package })
    | Some pkg -> (
        (* Look up package node in the graph to get its hash *)
        let package_node_opt =
          Tusk_planner.Package_graph.get_package_node state.package_graph pkg
        in
        match package_node_opt with
        | None ->
            Log.info "Server: Package '%s' not found in package graph" package;
            Protocol.ServerResponse
              (Protocol.ArtifactNotFound
                 { error = format "Package '%s' not in build graph" package })
        | Some package_node -> (
            let hash_opt =
              Tusk_planner.Package_graph.get_hash package_node
            in
            match hash_opt with
            | None ->
                Log.info "Server: Package '%s' has no hash (not built)" package;
                Protocol.ServerResponse
                  (Protocol.ArtifactNotFound
                     { error = format "Package '%s' has not been built" package })
            | Some hash -> (
                (* Get the artifact from the store using package hash *)
                match Tusk_store.Store.get state.store hash with
                | None ->
                    Log.info "Server: No artifact found for package '%s' hash %s"
                      package (Std.Crypto.Digest.hex hash);
                    Protocol.ServerResponse
                      (Protocol.ArtifactNotFound
                         { error =
                             format "Package '%s' has not been built" package
                         })
                | Some artifact ->
                    (* Find the binary file in the artifact *)
                    let binary_path =
                      List.find_opt
                        (fun file_path ->
                          let basename = Path.basename file_path in
                          basename = name)
                        artifact.Tusk_store.Artifact.files
                    in
                    (match binary_path with
                    | Some file_path ->
                        let artifact_dir =
                          Tusk_store.Store.get_artifact_dir state.store artifact
                        in
                        let full_path = Path.(artifact_dir / file_path) in
                        Log.info "Server: Artifact found at %s"
                          (Path.to_string full_path);
                        Protocol.ServerResponse
                          (Protocol.ArtifactFound { path = full_path })
                    | None ->
                        Log.info
                          "Server: Artifact '%s' not found in package '%s' files"
                          name package;
                        Protocol.ServerResponse
                          (Protocol.ArtifactNotFound
                             {
                               error =
                                 format "Artifact '%s' not found in package '%s'"
                                   name package;
                             })))))
  in
  Log.debug "Server: Sending response";
  send client_pid response;
  Log.debug "Server: Response sent, continuing loop";
  loop state

and handle_format_file state client_pid file_path check_only =
  Log.debug "Server: Received FormatFile from %s for %s (check_only=%b)"
    (Pid.to_string client_pid) (Path.to_string file_path) check_only;

  let ocamlformat = Tusk_toolchain.ocamlformat state.toolchain in
  let response =
    match
      Tusk_toolchain.Ocamlformat.format_file ocamlformat ~file_path ~check_only
    with
    | Tusk_toolchain.Ocamlformat.Formatted { code; changed } ->
        Protocol.FormatResult { formatted_code = code; changed }
    | Tusk_toolchain.Ocamlformat.Error err ->
        Protocol.FormatError { error = err }
  in
  send client_pid (Protocol.ServerResponse response);
  loop state

and handle_format_code state client_pid code file_path =
  Log.debug "Server: Received FormatCode from %s" (Pid.to_string client_pid);

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

and handle_format_all state client_pid mode =
  Log.debug "Server: Received FormatAll from %s (mode=%s)"
    (Pid.to_string client_pid)
    (match mode with `check -> "check" | `write -> "write");

  send client_pid
    (Protocol.ServerResponse
       (Protocol.FormatError
          { error = "FormatAll not yet implemented with worker pool" }));
  loop state

and handle_new_package state client_pid path name is_library =
  Log.debug "Server: Received NewPackage from %s for %s at %s"
    (Pid.to_string client_pid) name (Path.to_string path);

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
      let main_mli = Path.(src_dir / Path.v (module_name ^ ".mli")) in

      let ml_content =
        if is_library then
          "open Std\n\n(** Main module for " ^ name ^ " library *)\n"
        else "open Std\n\nlet () = println \"Hello, World!\"\n"
      in

      let mli_content =
        if is_library then Some ("(** " ^ name ^ " library interface *)\n")
        else None
      in

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

      let write_mli =
        match mli_content with
        | None -> Ok ()
        | Some content -> Fs.write content main_mli
      in

      match
        ( Fs.write ml_content main_ml,
          Fs.write toml_content package_toml,
          write_mli )
      with
      | Ok (), Ok (), Ok () ->
          Log.debug "Server: Rescanning workspace after package creation";
          let updated_workspace =
            Workspace_manager.scan state.workspace.root
            |> Result.expect
                 ~msg:"Failed to rescan workspace after package creation"
          in
          Log.debug "Server: Workspace rescanned, found %d packages"
            (List.length updated_workspace.packages);

          let updated_package_graph =
            Tusk_planner.Package_graph.create updated_workspace
          in
          let updated_state =
            {
              state with
              workspace = updated_workspace;
              package_graph = updated_package_graph;
            }
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
          loop state)

(** Handler for build message - spawns worker and continues loop immediately *)
and handle_build state client_pid target session_id =
  Log.debug "Server: handle_build called for target: %s"
    (match target with
    | Protocol.All -> "All"
    | Protocol.Package p -> format "Package(%s)" p);

  let server_pid = self () in
  Build_server.start ~workspace:state.workspace ~toolchain:state.toolchain
    ~store:state.store ~concurrency:state.concurrency ~session_id ~client_pid
    ~server_pid ~target;

  Log.info "[INTERNAL_SERVER] Build worker spawned, continuing server loop";
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

    Log.info "Ensuring Tusk directories exist...";
    let _ = Tusk_model.Tusk_dirs.ensure_created () in

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

    write_daemon_files ~workspace ~port;

    let package_graph = Tusk_planner.Package_graph.create workspace in
    let state =
      {
        workspace;
        toolchain;
        store;
        concurrency = System.available_parallelism;
        package_graph;
      }
    in

    Log.info "Starting JSON-RPC server on port %d..." port;
    let _ = Jsonrpc_server.start_tcp_server ~server:server_pid ~port in
    Log.info "JSON-RPC server started successfully";

    Log.info "Tusk server entering main loop";
    loop state
  with exn ->
    Log.error "Server initialization failed: %s" (Exception.to_string exn);
    Error exn
