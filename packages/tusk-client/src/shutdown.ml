open Std
open Std.Data
open Miniriot
open Tusk_model
open Tusk_protocol
open Client
(** Shutdown the server *)
let shutdown t =
  match
    Jsonrpc.Client.call t.client ~method_:method_shutdown
      ~params:Jsonrpc.NoParams ()
  with
  | Ok _ -> Ok ()
  | Error e ->
      Error (jsonrpc_error_to_string e)
