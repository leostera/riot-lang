(** Tusk RPC Server - JSON-RPC server for handling tusk commands *)

open Miniriot

type ctx = { server_pid : Pid.t }

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
      (* Build started, now wait for completion *)
      (match receive ~selector () with
      | Rpc.Success ->
          let response =
            Jsonrpc.make_response
              ~result:(Json.Object [ ("status", Json.String "success") ])
              ~id:Jsonrpc.Null ()
          in
          reply response
      | Rpc.Error msg ->
          let error =
            Jsonrpc.{ code = InternalError; message = msg; data = None }
          in
          let response = Jsonrpc.make_response ~error ~id:Jsonrpc.Null () in
          reply response
      | _ ->
          let error =
            Jsonrpc.
              { code = InternalError; message = "Unexpected response after BuildStarted"; data = None }
          in
          let response = Jsonrpc.make_response ~error ~id:Jsonrpc.Null () in
          reply response)
  | Rpc.Success ->
      let response =
        Jsonrpc.make_response
          ~result:(Json.Object [ ("status", Json.String "success") ])
          ~id:Jsonrpc.Null ()
      in
      reply response
  | Rpc.Error msg ->
      let error =
        Jsonrpc.{ code = InternalError; message = msg; data = None }
      in
      let response = Jsonrpc.make_response ~error ~id:Jsonrpc.Null () in
      reply response
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
      let result = Json.Object [
        ("workspace_root", Json.String config.workspace_root);
        ("toolchain", Json.String config.toolchain);
        ("packages", Json.Array (List.map (fun p -> Json.String p) config.packages))
      ] in
      let response = Jsonrpc.make_response ~result ~id:Jsonrpc.Null () in
      reply response
  | Rpc.Error msg ->
      let error = Jsonrpc.make_error ~code:InternalError ~message:msg () in
      let response = Jsonrpc.make_response ~error ~id:Jsonrpc.Null () in
      reply response
  | _ ->
      let error = Jsonrpc.make_error ~code:InternalError ~message:"Unexpected response" () in
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
      let nodes_json = Json.Array (List.map (fun node ->
        Json.Object [
          ("package_name", Json.String node.Rpc.package_name);
          ("src_dir", Json.String node.Rpc.src_dir);
          ("out_dir", Json.String node.Rpc.out_dir);
          ("status", Json.String node.Rpc.status);
          ("deps", Json.Array (List.map (fun d -> Json.String d) node.Rpc.deps))
        ]
      ) graph.nodes) in
      let result = Json.Object [("nodes", nodes_json)] in
      let response = Jsonrpc.make_response ~result ~id:Jsonrpc.Null () in
      reply response
  | Rpc.Error msg ->
      let error = Jsonrpc.make_error ~code:InternalError ~message:msg () in
      let response = Jsonrpc.make_response ~error ~id:Jsonrpc.Null () in
      reply response
  | _ ->
      let error = Jsonrpc.make_error ~code:InternalError ~message:"Unexpected response" () in
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
