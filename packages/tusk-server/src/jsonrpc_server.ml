open Std

open Tusk_model

(** JSON-RPC bridge between external clients and internal tusk server.

    This module provides:
    - WireProtocol: External JSON-RPC types
    - Server: Handlers that translate WireProtocol ↔ Protocol messages
    - TCP server setup *)

module WireProtocol = Tusk_protocol.WireProtocol

module Server = struct
  

  type ctx = { server_pid : Pid.t }

  (** Convert internal BuildStats to WireProtocol build_stats *)
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

  (** Convert internal build_result to WireProtocol build_result *)
  let convert_build_result (result : Tusk_executor.Package_builder.build_result)
      : WireProtocol.build_result =
    (* WireProtocol types are aliases, cast through constructors *)
    let status : WireProtocol.build_status =
      match result.status with
      | Tusk_executor.Package_builder.Cached artifact -> WireProtocol.Cached artifact
      | Tusk_executor.Package_builder.Built artifact -> WireProtocol.Built artifact
      | Tusk_executor.Package_builder.Failed err ->
          let wire_err : WireProtocol.package_error =
            match err with
            | Tusk_executor.Package_builder.PlanningFailed e -> WireProtocol.PlanningFailed e
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
    {
      WireProtocol.package = result.package;
      status;
      duration = result.duration;
    }

  let handle_ping ctx reply _request =
    Log.info "[JSONRPC] >>> handle_ping called (client_pid=%s)"
      (Pid.to_string (self ()));
    send ctx.server_pid
      (Protocol.ServerRequest (Protocol.Ping { client_pid = self () }));
    Log.debug "[JSONRPC]     Sent Ping to server, awaiting Pong...";
    let selector msg =
      match msg with
      | Protocol.ServerResponse Protocol.Pong -> `select ()
      | _ -> `skip
    in
    match receive ~selector () with
    | () ->
        Log.info "[JSONRPC] <<< handle_ping responding with Pong";
        reply WireProtocol.Pong
    | exception e ->
        Log.error "[JSONRPC] handle_ping exception: %s" (Printexc.to_string e)

  let handle_get_workspace_config ctx reply _request =
    Log.info "[JSONRPC] >>> handle_get_workspace_config called";
    send ctx.server_pid
      (Protocol.ServerRequest
         (Protocol.GetWorkspaceConfig { client_pid = self () }));
    let selector msg =
      match msg with
      | Protocol.ServerResponse (Protocol.WorkspaceConfig _) -> `select msg
      | _ -> `skip
    in
    match receive ~selector () with
    | Protocol.ServerResponse (Protocol.WorkspaceConfig { workspace; toolchain })
      ->
        Log.info "[JSONRPC] <<< WorkspaceConfig received";
        (* Get toolchain info from workspace config *)
        let toolchain_config = Tusk_model.Toolchain_config.from_workspace workspace in
        let toolchain_path = Tusk_model.Tusk_dirs.toolchains_dir toolchain_config in
        let wire_config : WireProtocol.workspace_config =
          {
            workspace_root = Path.to_string workspace.root;
            target_dir = Path.to_string workspace.target_dir_root;
            toolchain = toolchain_config.version;
            toolchain_path = Path.to_string toolchain_path;
            packages =
              List.map
                (fun (pkg : Package.t) ->
                  {
                    WireProtocol.name = pkg.name;
                    path = Path.to_string pkg.path;
                    dependencies = List.map (fun (dep : Package.dependency) -> dep.name) pkg.dependencies;
                  })
                workspace.packages;
            total_packages = List.length workspace.packages;
          }
        in
        reply (WireProtocol.WorkspaceConfig wire_config)
    | _ ->
        Log.error "[JSONRPC] Unexpected response for GetWorkspaceConfig";
        reply (WireProtocol.Error "Unexpected response")

  let handle_get_package_info ctx reply request =
    match request with
    | WireProtocol.GetPackageInfo package_name ->
        Log.info "[JSONRPC] >>> handle_get_package_info called (package=%s)"
          package_name;
        send ctx.server_pid
          (Protocol.ServerRequest
             (Protocol.GetPackageInfo { client_pid = self (); package_name }));
        let selector msg =
          match msg with
          | Protocol.ServerResponse (Protocol.PackageInfo _) -> `select msg
          | _ -> `skip
        in
        (match receive ~selector () with
        | Protocol.ServerResponse (Protocol.PackageInfo { package; sources }) ->
            Log.info "[JSONRPC] <<< PackageInfo received";
            let dep_names = List.map (fun (dep : Package.dependency) -> dep.name) package.dependencies in
            let wire_info : WireProtocol.package_detail =
              {
                package =
                  {
                    WireProtocol.name = package.name;
                    path = Path.to_string package.path;
                    dependencies = dep_names;
                  };
                sources = List.map Path.to_string sources;
                dependency_names = dep_names;
              }
            in
            reply (WireProtocol.PackageInfo wire_info)
        | _ ->
            Log.error "[JSONRPC] Unexpected response for GetPackageInfo";
            reply (WireProtocol.Error "Unexpected response"))
    | _ ->
        Log.error "[JSONRPC] handle_get_package_info called with wrong request type"

  let handle_get_package_graph ctx reply _request =
    Log.info "[JSONRPC] >>> handle_get_package_graph called";
    send ctx.server_pid
      (Protocol.ServerRequest (Protocol.GetPackageGraph { client_pid = self () }));
    let selector msg =
      match msg with
      | Protocol.ServerResponse (Protocol.PackageGraph _) -> `select msg
      | _ -> `skip
    in
    match receive ~selector () with
    | Protocol.ServerResponse (Protocol.PackageGraph { nodes }) ->
        Log.info "[JSONRPC] <<< PackageGraph received (%d nodes)" (List.length nodes);
        let wire_nodes =
          List.map
            (fun (pkg : Package.t) ->
              let deps = List.map (fun (dep : Package.dependency) -> dep.name) pkg.dependencies in
              {
                WireProtocol.package_name = pkg.name;
                src_dir = Path.to_string pkg.path;
                out_dir = ""; (* TODO: compute output dir *)
                status = "pending";
                deps;
              })
            nodes
        in
        reply (WireProtocol.PackageGraph { nodes = wire_nodes })
    | _ ->
        Log.error "[JSONRPC] Unexpected response for GetPackageGraph";
        reply (WireProtocol.Error "Unexpected response")

  let handle_find_executable ctx reply request =
    match request with
    | WireProtocol.FindExecutable name ->
        Log.info "[JSONRPC] >>> handle_find_executable called (name=%s)" name;
        send ctx.server_pid
          (Protocol.ServerRequest
             (Protocol.FindExecutable { client_pid = self (); name }));
        Log.debug "[JSONRPC]     Awaiting ExecutableFound/ExecutableNotFound...";
        let selector msg =
          match msg with
          | Protocol.ServerResponse (Protocol.ExecutableFound _)
          | Protocol.ServerResponse Protocol.ExecutableNotFound ->
              `select msg
          | _ -> `skip
        in
        (match receive ~selector () with
        | Protocol.ServerResponse (Protocol.ExecutableFound { package; binary })
          ->
            Log.info "[JSONRPC] <<< ExecutableFound: package=%s binary=%s" package
              binary;
            reply (WireProtocol.ExecutableFound { package; binary })
        | Protocol.ServerResponse Protocol.ExecutableNotFound ->
            Log.info "[JSONRPC] <<< ExecutableNotFound";
            reply WireProtocol.ExecutableNotFound
        | _ ->
            Log.error "[JSONRPC] Unexpected response for FindExecutable";
            reply (WireProtocol.Error "Unexpected response"))
    | _ ->
        Log.error "[JSONRPC] handle_find_executable called with wrong request type"

  let handle_find_artifact ctx reply request =
    match request with
    | WireProtocol.FindArtifact { package; kind; name } ->
        Log.info "[JSONRPC] >>> handle_find_artifact called (package=%s, name=%s)"
          package name;
        send ctx.server_pid
          (Protocol.ServerRequest
             (Protocol.FindArtifact
                { client_pid = self (); package; kind; name }));
        let selector msg =
          match msg with
          | Protocol.ServerResponse (Protocol.ArtifactFound _)
          | Protocol.ServerResponse (Protocol.ArtifactNotFound _) ->
              `select msg
          | _ -> `skip
        in
        (match receive ~selector () with
        | Protocol.ServerResponse (Protocol.ArtifactFound { path }) ->
            Log.info "[JSONRPC] <<< ArtifactFound: %s" (Path.to_string path);
            reply (WireProtocol.ArtifactFound { path = Path.to_string path })
        | Protocol.ServerResponse (Protocol.ArtifactNotFound { error }) ->
            Log.info "[JSONRPC] <<< ArtifactNotFound: %s" error;
            reply (WireProtocol.ArtifactNotFound { error })
        | _ ->
            Log.error "[JSONRPC] Unexpected response for FindArtifact";
            reply (WireProtocol.Error "Unexpected response"))
    | _ ->
        Log.error "[JSONRPC] handle_find_artifact called with wrong request type"

  let handle_format_file ctx reply request =
    match request with
    | WireProtocol.FormatFile { file_path; check_only } ->
        Log.info "[JSONRPC] >>> handle_format_file called (file=%s)" file_path;
        let file_path_obj = Path.of_string file_path |> Result.expect ~msg:"Invalid file path" in
        send ctx.server_pid
          (Protocol.ServerRequest
             (Protocol.FormatFile
                { client_pid = self (); file_path = file_path_obj; check_only }));
        let selector msg =
          match msg with
          | Protocol.ServerResponse (Protocol.FormatResult _)
          | Protocol.ServerResponse (Protocol.FormatError _) ->
              `select msg
          | _ -> `skip
        in
        (match receive ~selector () with
        | Protocol.ServerResponse
            (Protocol.FormatResult { formatted_code; changed }) ->
            Log.info "[JSONRPC] <<< FormatResult (changed=%b)" changed;
            reply (WireProtocol.FormatResult { formatted_code; changed })
        | Protocol.ServerResponse (Protocol.FormatError { error }) ->
            Log.info "[JSONRPC] <<< FormatError: %s" error;
            reply (WireProtocol.FormatError { error })
        | _ ->
            Log.error "[JSONRPC] Unexpected response for FormatFile";
            reply (WireProtocol.Error "Unexpected response"))
    | _ ->
        Log.error "[JSONRPC] handle_format_file called with wrong request type"

  let handle_format_code ctx reply request =
    match request with
    | WireProtocol.FormatCode { code; file_path } ->
        Log.info "[JSONRPC] >>> handle_format_code called";
        let file_path_obj = Option.map (fun p -> Path.of_string p |> Result.expect ~msg:"Invalid file path") file_path in
        send ctx.server_pid
          (Protocol.ServerRequest
             (Protocol.FormatCode { client_pid = self (); code; file_path = file_path_obj }));
        let selector msg =
          match msg with
          | Protocol.ServerResponse (Protocol.FormatResult _)
          | Protocol.ServerResponse (Protocol.FormatError _) ->
              `select msg
          | _ -> `skip
        in
        (match receive ~selector () with
        | Protocol.ServerResponse
            (Protocol.FormatResult { formatted_code; changed }) ->
            Log.info "[JSONRPC] <<< FormatResult (changed=%b)" changed;
            reply (WireProtocol.FormatResult { formatted_code; changed })
        | Protocol.ServerResponse (Protocol.FormatError { error }) ->
            Log.info "[JSONRPC] <<< FormatError: %s" error;
            reply (WireProtocol.FormatError { error })
        | _ ->
            Log.error "[JSONRPC] Unexpected response for FormatCode";
            reply (WireProtocol.Error "Unexpected response"))
    | _ ->
        Log.error "[JSONRPC] handle_format_code called with wrong request type"

  let handle_format_all ctx reply request =
    match request with
    | WireProtocol.FormatAll { mode } ->
        Log.info "[JSONRPC] >>> handle_format_all called";
        send ctx.server_pid
          (Protocol.ServerRequest
             (Protocol.FormatAll { client_pid = self (); mode }));
        let selector msg =
          match msg with
          | Protocol.ServerResponse (Protocol.FormatAllResult _) -> `select msg
          | _ -> `skip
        in
        (match receive ~selector () with
        | Protocol.ServerResponse
            (Protocol.FormatAllResult { files_formatted; files_failed; errors })
          ->
            Log.info "[JSONRPC] <<< FormatAllResult (formatted=%d, failed=%d)"
              files_formatted files_failed;
            reply
              (WireProtocol.FormatAllResult
                 { files_formatted; files_failed; errors })
        | _ ->
            Log.error "[JSONRPC] Unexpected response for FormatAll";
            reply (WireProtocol.Error "Unexpected response"))
    | _ ->
        Log.error "[JSONRPC] handle_format_all called with wrong request type"

  let handle_new_package ctx reply request =
    match request with
    | WireProtocol.NewPackage { path; name; is_library } ->
        Log.info "[JSONRPC] >>> handle_new_package called (name=%s)" name;
        let path_obj = Path.of_string path |> Result.expect ~msg:"Invalid path" in
        send ctx.server_pid
          (Protocol.ServerRequest
             (Protocol.NewPackage
                { client_pid = self (); path = path_obj; name; is_library }));
        let selector msg =
          match msg with
          | Protocol.ServerResponse (Protocol.PackageCreated _)
          | Protocol.ServerResponse (Protocol.PackageCreationError _) ->
              `select msg
          | _ -> `skip
        in
        (match receive ~selector () with
        | Protocol.ServerResponse (Protocol.PackageCreated { path; name }) ->
            Log.info "[JSONRPC] <<< PackageCreated: %s" name;
            reply (WireProtocol.PackageCreated { path; name })
        | Protocol.ServerResponse (Protocol.PackageCreationError { error }) ->
            Log.info "[JSONRPC] <<< PackageCreationError: %s" error;
            reply (WireProtocol.PackageCreationError { error })
        | _ ->
            Log.error "[JSONRPC] Unexpected response for NewPackage";
            reply (WireProtocol.Error "Unexpected response"))
    | _ ->
        Log.error "[JSONRPC] handle_new_package called with wrong request type"

  let handle_build ctx reply request =
    let target_str =
      match request with
      | WireProtocol.BuildPackage pkg -> format "BuildPackage(%s)" pkg
      | WireProtocol.BuildAll -> "BuildAll"
      | _ -> "Unknown"
    in
    Log.info "[JSONRPC] >>> handle_build called (client_pid=%s, target=%s)"
      (Pid.to_string (self ()))
      target_str;
    let session_id = Session_id.make () in

    (* Convert WireProtocol request to internal Protocol *)
    let target =
      match request with
      | WireProtocol.BuildPackage pkg -> Protocol.Package pkg
      | WireProtocol.BuildAll -> Protocol.All
      | _ -> Protocol.All
    in

    (* Send build request to internal server *)
    Log.debug "[JSONRPC]     Sending Build request to server (session_id=%s)..."
      (Session_id.to_string session_id);
    send ctx.server_pid
      (Protocol.ServerRequest
         (Protocol.Build { client_pid = self (); target; session_id }));

    (* Wait for BuildStarted response *)
    Log.debug "[JSONRPC]     Awaiting BuildStarted response...";
    let selector msg =
      match msg with
      | Protocol.ServerResponse (Protocol.BuildStarted _) -> `select msg
      | _ -> `skip
    in
    match receive ~selector () with
    | Protocol.ServerResponse (Protocol.BuildStarted { session_id; started_at })
      ->
        Log.info "[JSONRPC] <<< Received BuildStarted, replying to client...";
        (* Send BuildStarted to client *)
        reply (WireProtocol.BuildStarted { session_id; started_at });

        (* Now stream events until build completes *)
        Log.debug "[JSONRPC]     Entering event_loop to stream build events...";
        let rec event_loop () =
          let selector msg =
            match msg with
            | Protocol.ServerResponse _ -> `select msg
            | _ -> `skip
          in
          match receive ~selector () with
          | Protocol.ServerResponse (Protocol.BuildEvent { session_id; event })
            ->
              Log.debug
                "[JSONRPC] <<< Received BuildEvent, forwarding to client";
              reply (WireProtocol.BuildEvent { session_id; event });
              event_loop ()
          | Protocol.ServerResponse
              (Protocol.BuildCompleted
                 { session_id; completed_at; stats; results }) ->
              let wire_results = List.map convert_build_result results in
              Log.info "[JSONRPC] <<< Build completed: %d packages"
                (List.length results);
              reply
                (WireProtocol.BuildComplete
                   {
                     session_id;
                     completed_at;
                     stats = convert_build_stats stats;
                     results = wire_results;
                   });
              ()
          | Protocol.ServerResponse
              (Protocol.BuildFailed
                 { session_id; failed_at; stats; built; errors }) ->
              let wire_built = List.map convert_build_result built in
              let wire_errors = List.map convert_build_result errors in
              let failed_package_names =
                List.map
                  (fun (r : Tusk_executor.Package_builder.build_result) ->
                    r.package.name)
                  errors
              in
              Log.warn "[JSONRPC] <<< Build failed: %d packages failed: %s"
                (List.length errors)
                (String.concat ", " failed_package_names);
              (* Log detailed error information *)
              List.iter
                (fun (r : Tusk_executor.Package_builder.build_result) ->
                  match r.status with
                  | Tusk_executor.Package_builder.Failed err ->
                      let error_msg =
                        Tusk_executor.Package_builder.package_error_to_string
                          err
                      in
                      Log.error "[JSONRPC] Package %s failed: %s" r.package.name
                        error_msg
                  | _ -> ())
                errors;
              reply
                (WireProtocol.BuildFailed
                   {
                     session_id;
                     failed_at;
                     stats = convert_build_stats stats;
                     built = wire_built;
                     errors = wire_errors;
                   });
              ()
          | Protocol.ServerResponse
              (Protocol.PlanningFailed { session_id; failed_at; reason }) ->
              Log.warn "[JSONRPC] <<< Planning failed: %s" reason;
              reply (WireProtocol.PlanningFailed { session_id; failed_at; reason });
              ()
          | Protocol.ServerResponse
              (Protocol.PackageNotFound
                 { session_id; package_name; available_packages }) ->
              Log.warn "[JSONRPC] <<< Package not found: %s" package_name;
              reply
                (WireProtocol.PackageNotFound
                   { session_id; package_name; available_packages });
              ()
          | Protocol.ServerResponse
              (Protocol.CycleDetected { session_id; cycle_nodes; detected_at })
            ->
              Log.warn "[JSONRPC] <<< Cycle detected in build graph";
              reply
                (WireProtocol.CycleDetected
                   { session_id; detected_at; cycle_nodes });
              ()
          | msg ->
              Log.debug
                "[JSONRPC]     Received other message in event_loop, \
                 continuing...";
              event_loop ()
        in
        event_loop ()
    | msg ->
        Log.error
          "[JSONRPC] handle_build: unexpected response (not BuildStarted)"

  (** Create JSON-RPC server with handlers *)
  let create server_pid =
    let ctx = { server_pid } in
    let methods =
      Jsonrpc.Server.
        [
          {
            method_ = Tusk_protocol.method_ping;
            fn =
              (fun reply request ->
                match request with
                | WireProtocol.Ping -> handle_ping ctx reply request
                | _ -> ());
          };
          {
            method_ = Tusk_protocol.method_get_workspace_config;
            fn =
              (fun reply request ->
                match request with
                | WireProtocol.GetWorkspaceConfig ->
                    handle_get_workspace_config ctx reply request
                | _ -> ());
          };
          {
            method_ = Tusk_protocol.method_get_package_info;
            fn =
              (fun reply request ->
                match request with
                | WireProtocol.GetPackageInfo _ ->
                    handle_get_package_info ctx reply request
                | _ -> ());
          };
          {
            method_ = Tusk_protocol.method_get_package_graph;
            fn =
              (fun reply request ->
                match request with
                | WireProtocol.GetPackageGraph ->
                    handle_get_package_graph ctx reply request
                | _ -> ());
          };
          {
            method_ = Tusk_protocol.method_find_executable;
            fn =
              (fun reply request ->
                match request with
                | WireProtocol.FindExecutable _ ->
                    handle_find_executable ctx reply request
                | _ -> ());
          };
          {
            method_ = Tusk_protocol.method_find_artifact;
            fn =
              (fun reply request ->
                match request with
                | WireProtocol.FindArtifact _ ->
                    handle_find_artifact ctx reply request
                | _ -> ());
          };
          {
            method_ = Tusk_protocol.method_format_file;
            fn =
              (fun reply request ->
                match request with
                | WireProtocol.FormatFile _ -> handle_format_file ctx reply request
                | _ -> ());
          };
          {
            method_ = Tusk_protocol.method_format_code;
            fn =
              (fun reply request ->
                match request with
                | WireProtocol.FormatCode _ -> handle_format_code ctx reply request
                | _ -> ());
          };
          {
            method_ = Tusk_protocol.method_format_all;
            fn =
              (fun reply request ->
                match request with
                | WireProtocol.FormatAll _ -> handle_format_all ctx reply request
                | _ -> ());
          };
          {
            method_ = Tusk_protocol.method_new_package;
            fn =
              (fun reply request ->
                match request with
                | WireProtocol.NewPackage _ -> handle_new_package ctx reply request
                | _ -> ());
          };
          {
            method_ = Tusk_protocol.method_build_package;
            fn =
              (fun reply request ->
                match request with
                | WireProtocol.BuildPackage _ -> handle_build ctx reply request
                | _ -> ());
          };
          {
            method_ = Tusk_protocol.method_build_all;
            fn =
              (fun reply request ->
                match request with
                | WireProtocol.BuildAll -> handle_build ctx reply request
                | _ -> ());
          };
        ]
    in
    Jsonrpc.Server.create ~protocol:(module WireProtocol) ~methods
end

(** Start TCP server that bridges JSON-RPC to internal server *)
let start_tcp_server ~server ~port =
  spawn (fun () ->
      let addr = Net.Addr.tcp Net.Addr.loopback port in
      Log.debug "TCP server binding to 127.0.0.1:%d" port;

      (* Create JSON-RPC server *)
      let jsonrpc_server = Server.create server in

      (* TCP handler - runs in spawned process from TcpServer *)
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
                | `System_error msg -> msg)
        in
        Log.debug "Calling Jsonrpc.Server.handle_message";
        Jsonrpc.Server.handle_message jsonrpc_server reply req;
        Log.info "[JSONRPC] Handler finished, connection handler returning"
      in
      Log.debug "Starting TCP listener...";
      let _ = Net.TcpServer.listen addr ~handler in
      Log.info "TCP server listening successfully";
      Ok ())
