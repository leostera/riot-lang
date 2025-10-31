open Std
open Std.Data

open Tusk_model
open Tusk_protocol
open Client

(** Create a new package *)
let new_package t ~path ~name ~is_library =
  match
    Jsonrpc.Client.call t.client ~method_:method_new_package
      ~params:
        (Jsonrpc.Named
           [
             ("path", Json.String path);
             ("name", Json.String name);
             ("is_library", Json.Bool is_library);
           ])
      ()
  with
  | Ok (WireProtocol.PackageCreated { path; name }) -> Ok (path, name)
  | Ok (WireProtocol.PackageCreationError { error }) -> Error error
  | Ok _ -> Error "Invalid package creation response"
  | Error e -> Error (jsonrpc_error_to_string e)

(** Create a new package in ./packages/ with dependencies *)
let create_package t ~name ~deps ~is_library =
  (* Create package in ./packages/<name> *)
  let path = format "packages/%s" name in
  match new_package t ~path ~name ~is_library with
  | Ok (created_path, created_name) ->
      (* TODO: Add dependencies to tusk.toml *)
      let files =
        [ format "%s/tusk.toml" created_path; format "%s/src" created_path ]
      in
      Ok (created_path, files)
  | Error e -> Error e
