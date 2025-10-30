open Std
open Std.Data
open Miniriot
open Tusk_model
open Tusk_protocol
open Client
(** Get package graph *)
let get_package_graph t =
  match
    Jsonrpc.Client.call t.client ~method_:method_get_package_graph
      ~params:Jsonrpc.NoParams ()
  with
  | Ok (WireProtocol.PackageGraph graph) -> Ok graph
  | Ok _ -> Error "Invalid package graph response"
  | Error e ->
      Error (jsonrpc_error_to_string e)
