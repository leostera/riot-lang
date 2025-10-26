open Std
open Std.Data
open Miniriot
open Tusk_model
open Tusk_protocol
open Client
(** Find an executable by binary name *)
let find_executable t name =
  match
    Jsonrpc.Client.call t.client ~method_:method_find_executable
      ~params:(Jsonrpc.Named [ ("name", Json.String name) ])
      ()
  with
  | Ok (WireProtocol.ExecutableFound { package; binary }) ->
      Ok (Some (package, binary))
  | Ok WireProtocol.ExecutableNotFound -> Ok None
  | Ok _ -> Error "Invalid findExecutable response"
  | Error e ->
      Error (jsonrpc_error_to_string e)
