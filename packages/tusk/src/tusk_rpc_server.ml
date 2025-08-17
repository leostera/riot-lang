(** Tusk RPC Server - JSON-RPC server for handling tusk commands *)

open Miniriot

type ctx = { server_pid : Pid.t }

(** Convert log_event to structured JSON with event_data *)
let log_event_to_json log_event =
  let timestamp = Log.format_timestamp (Log.get_timestamp_ms ()) in
  let level = match Log.event_level log_event with
    | Error -> "error" | Warn -> "warn" | Info -> "info" 
    | Debug -> "debug" | Trace -> "trace" in
  let event_name = Log.event_name log_event in
  let message = Log.event_message log_event in
  
  (* Extract structured data based on log event type *)
  let event_data = match log_event with
    | Log.BuildStarted { packages; total_modules; workers } ->
        Json.Object [
          ("packages", Json.Array (List.map (fun p -> Json.String p) packages));
          ("total_modules", Json.Int total_modules);
          ("workers", Json.Int workers);
        ]
    | Log.PackageComplete res ->
        Json.Object [
          ("package_name", Json.String res.package);
          ("success", Json.Bool res.success);
          ("duration_ms", Json.Int res.duration_ms);
          ("modules_compiled", Json.Int res.modules_compiled);
          ("cache_hits", Json.Int res.cache_hits);
          ("cache_misses", Json.Int res.cache_misses);
        ]
    | Log.CacheHit { package; hash } -> 
        Json.Object [
          ("package_name", Json.String package);
          ("hash", Json.String hash);
        ]
    | Log.CacheMiss { package; hash } -> 
        Json.Object [
          ("package_name", Json.String package);
          ("hash", Json.String hash);
        ]
    | Log.HashComputed { package; hash } ->
        Json.Object [
          ("package_name", Json.String package);
          ("hash", Json.String hash);
        ]
    | Log.WorkerAssigned { worker_id; package } ->
        Json.Object [
          ("worker_id", Json.String (Worker_id.to_string worker_id));
          ("package_name", Json.String package);
        ]
    | Log.QueuePackage { package; queue_type } ->
        let typ = match queue_type with `Ready -> "ready" | `Waiting -> "waiting" in
        Json.Object [
          ("package_name", Json.String package);
          ("queue_type", Json.String typ);
        ]
    | Log.Info msg | Log.Debug msg | Log.Warn msg | Log.Error msg ->
        Json.Object [("message", Json.String msg)]
    | _ ->
        (* For other events, just include the message as data *)
        Json.Object [("message", Json.String message)]
  in
  
  Json.Object [
    ("type", Json.String "build_event");
    ("timestamp", Json.String timestamp);
    ("level", Json.String level);
    ("event", Json.String event_name);
    ("message", Json.String message);
    ("event_data", event_data);
  ]

let handle_build ctx reply params =
  let package =
    match params with
    | Jsonrpc.Named params -> (
        match List.assoc_opt "package" params with
        | Some (Json.String pkg) -> Some pkg
        | _ -> None)
    | _ -> None
  in
  (* Send build request to internal server *)
  let request =
    match package with Some pkg -> Rpc.BuildPackage pkg | None -> Rpc.BuildAll
  in
  send ctx.server_pid (Rpc.ClientRequest (self (), request));

  (* Wait for response *)
  let selector = function
    | Rpc.ServerResponse response -> `select response
    | _ -> `skip
  in
  match receive ~selector () with
  | Rpc.BuildStarted { session_id } ->
      (* Send BuildStarted response with session_id *)
      let start_response =
        Jsonrpc.make_response
          ~result:
            (Json.Object
               [
                 ("type", Json.String "build_started");
                 ("session_id", Json.String (Session_id.to_string session_id));
               ])
          ~id:Jsonrpc.Null ()
      in
      reply start_response;

      (* Now enter receive loop for build events *)
      let rec receive_events () =
        match receive ~selector () with
        | Rpc.BuildEvent { session_id = _; log_event } ->
            (* Convert log event to structured JSON *)
            let event_json = log_event_to_json log_event in
            let event_response =
              Jsonrpc.make_response
                ~result:event_json
                ~id:Jsonrpc.Null ()
            in
            reply event_response;
            receive_events () (* Continue receiving events *)
        | Rpc.Success ->
            (* Build completed successfully *)
            let response =
              Jsonrpc.make_response
                ~result:
                  (Json.Object
                     [
                       ("type", Json.String "build_completed");
                       ("status", Json.String "success");
                     ])
                ~id:Jsonrpc.Null ()
            in
            reply response
        | Rpc.Error msg ->
            (* Build failed *)
            let response =
              Jsonrpc.make_response
                ~result:
                  (Json.Object
                     [
                       ("type", Json.String "build_completed");
                       ("status", Json.String "error");
                       ("message", Json.String msg);
                     ])
                ~id:Jsonrpc.Null ()
            in
            reply response
        | _ -> receive_events () (* Ignore other messages and continue *)
      in
      receive_events ()
  | _ ->
      let error =
        Jsonrpc.
          { code = InternalError; message = "Unexpected response"; data = None }
      in
      let response = Jsonrpc.make_response ~error ~id:Jsonrpc.Null () in
      reply response

let handle_ping ctx reply params =
  (* Send internal message to server *)
  send ctx.server_pid (Rpc.ClientRequest (self (), Rpc.Ping));
  (* Wait for response *)
  let selector = function
    | Rpc.ServerResponse response -> `select response
    | _ -> `skip
  in
  match receive ~selector () with
  | Rpc.Pong ->
      let response =
        Jsonrpc.make_response ~result:(Json.String "pong") ~id:Jsonrpc.Null ()
      in
      reply response
  | _ ->
      let error =
        Jsonrpc.
          { code = InternalError; message = "Unexpected response"; data = None }
      in
      let response = Jsonrpc.make_response ~error ~id:Jsonrpc.Null () in
      reply response

let handle_shutdown ctx reply params =
  send ctx.server_pid (Rpc.ClientRequest (self (), Rpc.Shutdown));
  let response =
    Jsonrpc.make_response
      ~result:(Json.Object [ ("status", Json.String "shutting down") ])
      ~id:Jsonrpc.Null ()
  in
  reply response

let handle_workspace_config ctx reply params =
  send ctx.server_pid (Rpc.ClientRequest (self (), Rpc.GetWorkspaceConfig));
  let selector = function
    | Rpc.ServerResponse response -> `select response
    | _ -> `skip
  in
  match receive ~selector () with
  | Rpc.WorkspaceConfig config ->
      let result =
        Json.Object
          [
            ("workspace_root", Json.String config.workspace_root);
            ("toolchain", Json.String config.toolchain);
            ( "packages",
              Json.Array (List.map (fun p -> Json.String p) config.packages) );
          ]
      in
      let response = Jsonrpc.make_response ~result ~id:Jsonrpc.Null () in
      reply response
  | Rpc.Error msg ->
      let error = Jsonrpc.make_error ~code:InternalError ~message:msg () in
      let response = Jsonrpc.make_response ~error ~id:Jsonrpc.Null () in
      reply response
  | _ ->
      let error =
        Jsonrpc.make_error ~code:InternalError ~message:"Unexpected response" ()
      in
      let response = Jsonrpc.make_response ~error ~id:Jsonrpc.Null () in
      reply response

let handle_build_graph ctx reply params =
  send ctx.server_pid (Rpc.ClientRequest (self (), Rpc.GetBuildGraph));
  let selector = function
    | Rpc.ServerResponse response -> `select response
    | _ -> `skip
  in
  match receive ~selector () with
  | Rpc.BuildGraph graph ->
      let nodes_json =
        Json.Array
          (List.map
             (fun node ->
               Json.Object
                 [
                   ("package_name", Json.String node.Rpc.package_name);
                   ("src_dir", Json.String node.Rpc.src_dir);
                   ("out_dir", Json.String node.Rpc.out_dir);
                   ("status", Json.String node.Rpc.status);
                   ( "deps",
                     Json.Array
                       (List.map (fun d -> Json.String d) node.Rpc.deps) );
                 ])
             graph.nodes)
      in
      let result = Json.Object [ ("nodes", nodes_json) ] in
      let response = Jsonrpc.make_response ~result ~id:Jsonrpc.Null () in
      reply response
  | Rpc.Error msg ->
      let error = Jsonrpc.make_error ~code:InternalError ~message:msg () in
      let response = Jsonrpc.make_response ~error ~id:Jsonrpc.Null () in
      reply response
  | _ ->
      let error =
        Jsonrpc.make_error ~code:InternalError ~message:"Unexpected response" ()
      in
      let response = Jsonrpc.make_response ~error ~id:Jsonrpc.Null () in
      reply response

let handle_restart ctx reply params =
  send ctx.server_pid (Rpc.ClientRequest (self (), Rpc.Restart));
  let response =
    Jsonrpc.make_response
      ~result:(Json.Object [ ("status", Json.String "restarting") ])
      ~id:Jsonrpc.Null ()
  in
  reply response

(** Create a JSON-RPC server handler for the tusk server *)
let create server_pid =
  let ctx = { server_pid } in
  let methods =
    [
      (Tusk_jsonrpc.method_ping, handle_ping ctx);
      (Tusk_jsonrpc.method_build_package, handle_build ctx);
      (Tusk_jsonrpc.method_build_all, handle_build ctx);
      (Tusk_jsonrpc.method_get_workspace_config, handle_workspace_config ctx);
      (Tusk_jsonrpc.method_get_build_graph, handle_build_graph ctx);
      (Tusk_jsonrpc.method_restart, handle_restart ctx);
      (Tusk_jsonrpc.method_shutdown, handle_shutdown ctx);
    ]
  in
  Printf.eprintf "[RPC SERVER DEBUG] Registering methods:\n";
  List.iter
    (fun (method_name, _handler) -> Printf.eprintf "  - %s\n" method_name)
    methods;
  flush stderr;
  Jsonrpc.Server.create ~methods
