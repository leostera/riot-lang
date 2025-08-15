(** Tusk-specific JSON-RPC client *)

open Miniriot

(** Tusk RPC protocol implementation *)
module TuskProtocol = struct
  type request = Rpc.request
  type response = Rpc.response
  
  let serialize_request req = 
    Rpc.request_to_json req
  
  let serialize_response res = 
    Rpc.response_to_json res
  
  let deserialize_response json =
    Rpc.response_of_json json
  
  let deserialize_request json =
    Rpc.request_of_json json
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
      (* Send the request *)
      match Jsonrpc_client.send client request with
      | Error e -> 
          Jsonrpc_client.close client;
          Error e
      | Ok () ->
          (* Collect all responses *)
          let rec collect_responses session_id logs =
            match Jsonrpc_client.receive client with
            | Error e -> 
                Jsonrpc_client.close client;
                Error e
            | Ok response ->
                match response with
                | Rpc.BuildStarted { session_id = sid } ->
                    (* First response with session ID *)
                    collect_responses sid logs
                | Rpc.LogOutput { message; _ } ->
                    (* Accumulate log message *)
                    collect_responses session_id (message :: logs)
                | Rpc.Success ->
                    (* Build succeeded *)
                    Jsonrpc_client.close client;
                    Ok (session_id, List.rev logs, Ok Rpc.Success)
                | Rpc.Error msg ->
                    (* Build failed *)
                    Jsonrpc_client.close client;
                    Ok (session_id, List.rev logs, Ok (Rpc.Error msg))
                | _ ->
                    (* Other response - keep reading *)
                    collect_responses session_id logs
          in
          collect_responses "" []

(** Send a build request and stream responses via callback *)
let call_build_streaming request callback =
  match connect () with
  | Error msg -> Error msg
  | Ok client ->
      (* Send the request *)
      match Jsonrpc_client.send client request with
      | Error e ->
          Jsonrpc_client.close client;
          Error e
      | Ok () ->
          (* Read responses and call callback for each *)
          let rec read_responses () =
            match Jsonrpc_client.receive client with
            | Error e ->
                Jsonrpc_client.close client;
                Error e
            | Ok response ->
                (* Call callback for this response *)
                callback response;
                (* Check if this is the final response *)
                match response with
                | Rpc.Success | Rpc.Error _ ->
                    (* Final response *)
                    Jsonrpc_client.close client;
                    Ok response
                | _ ->
                    (* Keep reading *)
                    read_responses ()
          in
          read_responses ()

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