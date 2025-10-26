open Std
open Std.Data
open Miniriot
open Tusk_model
open Tusk_protocol
open Client
(** Ping the server *)
let ping t =
  match
    Jsonrpc.Client.call t.client ~method_:method_ping ~params:Jsonrpc.NoParams
      ()
  with
  | Ok _ -> Ok ()
  | Error e -> Error (jsonrpc_error_to_string e)
