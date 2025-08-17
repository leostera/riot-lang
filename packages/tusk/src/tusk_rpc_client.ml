(** Tusk RPC Client - High-level client that wraps JSON-RPC *)

open Miniriot

(** Client type *)
type t = { client : Jsonrpc.Client.t; transport : Net.TcpClient.t }

(** High-level tusk types *)
type build_request = BuildPackage of string | BuildAll

(** Streaming build event *)
type streaming_event =
  | BuildStarted of Session_id.t
  | BuildEvent of Json.t
  | BuildFinished of (unit, string) result

(** Create a new Tusk RPC client *)
let create () =
  (* Get current workspace and daemon info *)
  let workspace = Workspace.scan ~root:(System.getcwd ()) in
  match Server_manager.daemon workspace with
  | None ->
      failwith "Server is not running for this project"
  | Some daemon_info ->
      let port = daemon_info.port in
    
    (* Create TCP transport using Net.TcpClient *)
    match Net.TcpClient.connect ~host:"127.0.0.1" ~port with
    | Ok transport ->
        let client = Jsonrpc.Client.create (module Net.TcpClient) transport in
        { client; transport }
    | Error e ->
        failwith "Failed to connect to server"

(** Close the client *)
let close t = 
  Jsonrpc.Client.close t.client;
  Net.TcpClient.close t.transport

(** Ping the server *)
let ping t =
  match Jsonrpc.Client.call t.client ~method_:Tusk_jsonrpc.method_ping ~params:Jsonrpc.NoParams () with
  | Ok _ -> Ok ()
  | Error e -> Error (Printf.sprintf "Error %d: %s" (Jsonrpc.error_code_to_int e.code) e.message)

(** Get workspace configuration *)
let get_workspace_config t =
  match Jsonrpc.Client.call t.client 
    ~method_:Tusk_jsonrpc.method_get_workspace_config ~params:Jsonrpc.NoParams () with
  | Ok json -> (
      match json with
      | Json.Object fields ->
          let workspace_root = 
            match List.assoc_opt "workspace_root" fields with
            | Some (Json.String s) -> s | _ -> ""
          in
          let toolchain = 
            match List.assoc_opt "toolchain" fields with
            | Some (Json.String s) -> s | _ -> ""
          in
          let packages = 
            match List.assoc_opt "packages" fields with
            | Some (Json.Array arr) ->
                List.filter_map (function Json.String s -> Some s | _ -> None) arr
            | _ -> []
          in
          Ok Rpc.{ workspace_root; toolchain; packages }
      | _ -> Error "Invalid workspace config response format")
  | Error e -> Error (Printf.sprintf "Error %d: %s" (Jsonrpc.error_code_to_int e.code) e.message)

(** Get build graph *)
let get_build_graph t =
  match Jsonrpc.Client.call t.client 
    ~method_:Tusk_jsonrpc.method_get_build_graph ~params:Jsonrpc.NoParams () with
  | Ok json -> (
      match json with
      | Json.Object fields ->
          let nodes = 
            match List.assoc_opt "nodes" fields with
            | Some (Json.Array node_array) ->
                List.filter_map (function
                  | Json.Object node_fields ->
                      let package_name = 
                        match List.assoc_opt "package_name" node_fields with
                        | Some (Json.String s) -> s | _ -> ""
                      in
                      let src_dir = 
                        match List.assoc_opt "src_dir" node_fields with
                        | Some (Json.String s) -> s | _ -> ""
                      in
                      let out_dir = 
                        match List.assoc_opt "out_dir" node_fields with
                        | Some (Json.String s) -> s | _ -> ""
                      in
                      let status = 
                        match List.assoc_opt "status" node_fields with
                        | Some (Json.String s) -> s | _ -> ""
                      in
                      let deps = 
                        match List.assoc_opt "deps" node_fields with
                        | Some (Json.Array arr) ->
                            List.filter_map (function Json.String s -> Some s | _ -> None) arr
                        | _ -> []
                      in
                      Some Rpc.{ package_name; src_dir; out_dir; status; deps }
                  | _ -> None
                ) node_array
            | _ -> []
          in
          Ok Rpc.{ nodes }
      | _ -> Error "Invalid build graph response format")
  | Error e -> Error (Printf.sprintf "Error %d: %s" (Jsonrpc.error_code_to_int e.code) e.message)

(** Build with streaming support *)
let build_streaming t request callback =
  let method_name, params =
    match request with
    | BuildPackage pkg ->
        (Tusk_jsonrpc.method_build_package, Tusk_jsonrpc.build_package_params pkg)
    | BuildAll -> (Tusk_jsonrpc.method_build_all, Jsonrpc.NoParams)
  in

  (* Send the build request *)
  match Jsonrpc.Client.call t.client ~method_:method_name ~params () with
  | Ok json -> (
      (* Try to parse the response as BuildStarted or direct Success *)
      match json with
      | Json.Object fields -> (
          match List.assoc_opt "session_id" fields with
          | Some (Json.String session_id_str) ->
              (* This is a BuildStarted response *)
              let session_id = Session_id.of_string session_id_str in
              callback (BuildStarted session_id);
              (* For now, assume builds complete immediately and return success *)
              Ok (BuildFinished (Ok ()))
          | None ->
              (* Check if this is a Success/Error response *)
              (match List.assoc_opt "status" fields with
              | Some (Json.String "success") ->
                  Ok (BuildFinished (Ok ()))
              | Some (Json.String "error") ->
                  let msg = match List.assoc_opt "message" fields with
                    | Some (Json.String s) -> s
                    | _ -> "Unknown error"
                  in
                  Ok (BuildFinished (Error msg))
              | _ ->
                  (* Unknown response format *)
                  Error "Unexpected response format"))
      | _ ->
          (* Non-object response *)
          Error "Unexpected response format")
  | Error e ->
      Error (Printf.sprintf "Build request failed: Error %d: %s"
             (Jsonrpc.error_code_to_int e.code) e.message)

(** Shutdown the server *)
let shutdown t =
  match Jsonrpc.Client.call t.client ~method_:Tusk_jsonrpc.method_shutdown ~params:Jsonrpc.NoParams () with
  | Ok _ -> Ok ()
  | Error e -> Error (Printf.sprintf "Error %d: %s" (Jsonrpc.error_code_to_int e.code) e.message)

(** Restart the server *)
let restart t =
  match Jsonrpc.Client.call t.client ~method_:Tusk_jsonrpc.method_restart ~params:Jsonrpc.NoParams () with
  | Ok _ -> Ok ()
  | Error e -> Error (Printf.sprintf "Error %d: %s" (Jsonrpc.error_code_to_int e.code) e.message)