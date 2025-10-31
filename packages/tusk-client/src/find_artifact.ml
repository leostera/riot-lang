open Std
open Std.Data

open Tusk_model
open Tusk_protocol
open Client
(** Find an artifact path *)
let find_artifact t ~package ~kind ~name =
  match
    Jsonrpc.Client.call t.client ~method_:method_find_artifact
      ~params:
        (Jsonrpc.Named
           [
             ("package", Json.String package);
             ("kind", Json.String kind);
             ("name", Json.String name);
           ])
      ()
  with
  | Ok (WireProtocol.ArtifactFound { path }) -> Ok path
  | Ok (WireProtocol.ArtifactNotFound { error }) -> Error error
  | Ok _ -> Error "Invalid findArtifact response"
  | Error e -> Error (jsonrpc_error_to_string e)
