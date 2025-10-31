open Std
open Std.Data

open Tusk_model
open Tusk_protocol
open Client
(** Get workspace configuration *)
let get_workspace_config t =
  match
    Jsonrpc.Client.call t.client ~method_:method_get_workspace_config
      ~params:Jsonrpc.NoParams ()
  with
  | Ok (WireProtocol.WorkspaceConfig config) -> Ok config
  | Ok _ -> Error "Invalid workspace config response"
  | Error e ->
      Error (jsonrpc_error_to_string e)
