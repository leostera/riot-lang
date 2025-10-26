open Std
open Std.Data
open Miniriot
open Tusk_model
open Tusk_protocol
open Client
(** Get build graph *)
let get_build_graph t =
  match
    Jsonrpc.Client.call t.client ~method_:method_get_build_graph
      ~params:Jsonrpc.NoParams ()
  with
  | Ok (WireProtocol.BuildGraph graph) -> Ok graph
  | Ok _ -> Error "Invalid build graph response"
  | Error e ->
      Error (jsonrpc_error_to_string e)
