(** JSON-RPC client for tusk server *)

open Miniriot

(** Send a JSON-RPC request and get response *)
let call request =
  match Rpc_client.connect () with
  | Error msg -> Error msg
  | Ok client -> (
      try
        (* Convert request to JSON string *)
        let json_request = Rpc_json.request_to_json request in
        let request_str = Json.to_string json_request in

        (* Send as a raw RPC command with JSON prefix *)
        match Rpc_client.send_raw client ("JSON:" ^ request_str) with
        | Error e ->
            Miniriot.Net.TcpStream.close client;
            Error e
        | Ok response -> (
            Miniriot.Net.TcpStream.close client;

            (* Parse response as JSON *)
            match Json.of_string response with
            | Error e ->
                Error (Printf.sprintf "Failed to parse JSON response: %s" e)
            | Ok json -> Rpc_json.response_of_json json)
      with e ->
        Miniriot.Net.TcpStream.close client;
        Error (Printf.sprintf "RPC call failed: %s" (Printexc.to_string e)))

(** Ping the server *)
let ping () =
  match call Rpc_json.Ping with
  | Ok Rpc_json.Pong -> Ok ()
  | Ok _ -> Error "Unexpected response to ping"
  | Error e -> Error e

(** Get the build graph *)
let get_build_graph () =
  match call Rpc_json.GetBuildGraph with
  | Ok (Rpc_json.BuildGraph graph) -> Ok graph
  | Ok _ -> Error "Unexpected response to GetBuildGraph"
  | Error e -> Error e

(** Get workspace configuration *)
let get_workspace_config () =
  match call Rpc_json.GetWorkspaceConfig with
  | Ok (Rpc_json.WorkspaceConfig config) -> Ok config
  | Ok _ -> Error "Unexpected response to GetWorkspaceConfig"
  | Error e -> Error e
