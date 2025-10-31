open Std
open Std.Data

open Tusk_model
open Tusk_protocol
open Client
(** Get package information *)
let get_package_info t package_name =
  match
    Jsonrpc.Client.call t.client ~method_:method_get_package_info
      ~params:(Jsonrpc.Named [ ("package", Json.String package_name) ])
      ()
  with
  | Ok (WireProtocol.PackageInfo detail) -> Ok detail
  | Ok _ -> Error "Invalid package info response"
  | Error e ->
      Error (jsonrpc_error_to_string e)
