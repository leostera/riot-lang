open Std
open Std.Data

open Tusk_model
open Tusk_protocol

type t = {
  client : (WireProtocol.request, WireProtocol.response) Jsonrpc.Client.t;
  transport : Net.TcpClient.t;
}

type error =
  | JsonrpcError of Jsonrpc.error
  | PackageNotFound of {
      package_name : string;
      available_packages : string list;
    }
  | UnexpectedEvent of { event : WireProtocol.response; reason : string }

let jsonrpc_error_to_string = function
  | Jsonrpc.ParseError { parse_error; _ } ->
      "Parse error: " ^ parse_error ^ ""
  | Jsonrpc.InvalidRequest { reason; _ } -> "Invalid request: " ^ reason ^ ""
  | Jsonrpc.MethodNotFound { method_name } ->
      "Method not found: " ^ method_name ^ ""
  | Jsonrpc.InvalidParams { reason; _ } -> "Invalid params: " ^ reason ^ ""
  | Jsonrpc.InternalError { details; _ } -> "Internal error: " ^ details ^ ""
  | Jsonrpc.UnknownServerError { code; message; _ } ->
      "Server error " ^ Int.to_string code ^ ": " ^ message

(** Create a new Tusk RPC client *)
let create ~host ~port =
  (* Create TCP transport using Std.Net.TcpClient *)
  match Std.Net.TcpClient.connect ~host ~port with
  | Ok transport ->
      let client =
        Jsonrpc.Client.create
          ~transport:(module Std.Net.TcpClient)
          ~protocol:(module WireProtocol)
          transport
      in
      Ok { client; transport }
  | Error e ->
      let error_msg =
        match e with
        | Connection_refused -> "Connection refused"
        | Closed -> "Connection closed"
        | System_error io_err -> "System error: " ^ IO.error_message io_err
      in
      Error ("Failed to connect to server: " ^ error_msg)

(** Connect to a running tusk server by discovering workspace and daemon *)
let connect () =
  (* Find workspace root from current directory *)
  let cwd =
    Env.current_dir () |> Result.expect ~msg:"Failed to get current directory"
  in
  let workspace_root =
    Workspace_manager.find_workspace_root cwd
  in
  match workspace_root with
  | None -> 
      Error (String.concat "" [
        "Not in a tusk workspace (cwd: ";
        Path.to_string cwd;
        ")"
      ])
  | Some root ->
      (* Create workspace to get project_id *)
      let workspace = Workspace.make ~root ~packages:[] () in
      
      (* Use Tusk_dirs.project_dir to get daemon directory (in ~/.tusk/projects/<project_id>) *)
      let daemon_path = Tusk_dirs.project_dir workspace in
      let port_file = Path.(daemon_path / Path.v "server.port") in
      
      match Fs.read_to_string port_file with
      | Error _ -> 
          Error (String.concat "" [
            "No tusk server running (daemon port file not found at ";
            Path.to_string port_file;
            ")"
          ])
      | Ok port_content ->
          let port = int_of_string (String.trim port_content) in
          create ~host:"127.0.0.1" ~port

(** Close the client *)
let close t =
  (* Jsonrpc.Client.close already closes the transport *)
  Jsonrpc.Client.close t.client
