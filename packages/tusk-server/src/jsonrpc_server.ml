open Std
open Miniriot
open Tusk_model

(** JSON-RPC bridge between external clients and internal tusk server.

    This module provides:
    - WireProtocol: External JSON-RPC types
    - Server: Handlers that translate WireProtocol ↔ Protocol messages
    - TCP server setup *)

module WireProtocol = Tusk_protocol.WireProtocol

module Server = struct
  open Miniriot

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
    let status =
      match result.status with
      | Tusk_executor.Package_builder.Cached artifact ->
          WireProtocol.Cached artifact
      | Tusk_executor.Package_builder.Built artifact ->
          WireProtocol.Built artifact
      | Tusk_executor.Package_builder.Failed err ->
          let wire_err =
            match err with
            | Tusk_executor.Package_builder.PlanningFailed planning_err ->
                WireProtocol.PlanningFailed planning_err
            | Tusk_executor.Package_builder.ExecutionFailed { message } ->
                WireProtocol.ExecutionFailed { message }
            | Tusk_executor.Package_builder.ActionFailed action_err ->
                WireProtocol.ActionFailed action_err
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
