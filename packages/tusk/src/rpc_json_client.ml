(** Tusk-specific JSON-RPC client *)

open Miniriot

(** Tusk RPC protocol implementation *)
module TuskProtocol = struct
  type request = Rpc.request
  type response = Rpc.response
  
  let serialize_request req = 
    Json.to_string (Rpc.request_to_json req)
  
  let serialize_response res = 
    Json.to_string (Rpc.response_to_json res)
  
  let deserialize_response str =
    match Json.of_string str with
    | Error e -> Error (Printf.sprintf "JSON parse error: %s" e)
    | Ok json -> Rpc.response_of_json json
  
  let deserialize_request str =
    match Json.of_string str with
    | Error e -> Error (Printf.sprintf "JSON parse error: %s" e)
    | Ok json -> Rpc.request_of_json json
  
  let is_streaming_response = function
    | Rpc.BuildStarted _ | Rpc.LogOutput _ -> true
    | _ -> false
  
  let is_final_response = function
    | Rpc.Success | Rpc.Error _ -> true
    | _ -> false
end

(** Helper to get server connection info *)
let get_server_port () =
  (* Get daemon directory for this project *)
  let home = System.get_home () in
  let cwd = System.getcwd () in
  let hash = Hasher.hash_string cwd in
  let project_id = Hasher.to_string hash in
  let daemon_path = Printf.sprintf "%s/.tusk/daemons/%s" home project_id in
  let port_file = Printf.sprintf "%s/server.port" daemon_path in
  
  (* Check if port file exists *)
  if not (Sys.file_exists port_file) then
    Error "Server is not running for this project"
  else
    (* Read the port *)
    try
      let ic = open_in port_file in
      let port_str = input_line ic in
      close_in ic;
      Ok (int_of_string port_str)
    with
    | e -> Error (Printf.sprintf "Failed to read port file: %s" (Printexc.to_string e))

(** Connect to the tusk server *)
let connect () =
  match get_server_port () with
  | Error e -> Error e
  | Ok port ->
      match Jsonrpc_client.TcpTransport.connect ~host:"127.0.0.1" ~port with
      | Error e -> Error e
      | Ok transport ->
          Ok (Jsonrpc_client.create 
            ~transport:(module Jsonrpc_client.TcpTransport)
            ~protocol:(module TuskProtocol)
            transport)

(** Send a request and get response *)
let call request =
  match connect () with
  | Error msg -> Error msg
  | Ok client ->
      let result = Jsonrpc_client.call client request in
      Jsonrpc_client.close client;
      result

(** Send a build request and collect all log messages *)
let call_build request =
  match connect () with
  | Error msg -> Error msg
  | Ok client ->
      let result = 
        match Jsonrpc_client.call_streaming client request with
        | Error e -> Error e
        | Ok (final_response, all_responses) ->
            (* Extract session ID from first streaming response *)
            let session_id = 
              List.fold_left (fun acc resp ->
                match resp with
                | Rpc.BuildStarted { session_id } -> session_id
                | _ -> acc
              ) "" all_responses
            in
            
            (* Extract log messages *)
            let logs = List.filter_map (function
              | Rpc.LogOutput { message; _ } -> Some message
              | _ -> None
            ) all_responses in
            
            (* Return session, logs, and final response *)
            Ok (session_id, logs, Ok final_response)
      in
      Jsonrpc_client.close client;
      result

(** Convenience functions *)
let ping () =
  match call Rpc.Ping with
  | Ok Rpc.Pong -> Ok ()
  | Ok _ -> Error "Unexpected response to ping"
  | Error e -> Error e

let get_build_graph () =
  match call Rpc.GetBuildGraph with
  | Ok (Rpc.BuildGraph graph) -> Ok graph
  | Ok _ -> Error "Unexpected response to GetBuildGraph"
  | Error e -> Error e

let get_workspace_config () =
  match call Rpc.GetWorkspaceConfig with
  | Ok (Rpc.WorkspaceConfig config) -> Ok config
  | Ok _ -> Error "Unexpected response to GetWorkspaceConfig"
  | Error e -> Error e