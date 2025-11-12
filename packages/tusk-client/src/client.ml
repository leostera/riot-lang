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
        | System_error msg -> "System error: " ^ msg ^ ""
      in
      Error ("Failed to connect to server: " ^ error_msg)

(** Close the client *)
let close t =
  (* Jsonrpc.Client.close already closes the transport *)
  Jsonrpc.Client.close t.client
