open Std
open Std.Data
open Miniriot
open Tusk_model
open Tusk_protocol
open Client
(** Format all OCaml files in the workspace *)
let format_all t ~mode =
  match
    Jsonrpc.Client.call t.client ~method_:method_format_all
      ~params:
        (Jsonrpc.Named
           [
             ( "mode",
               Json.String
                 (match mode with `check -> "check" | `write -> "write") );
           ])
      ()
  with
  | Ok (WireProtocol.FormatAllResult { files_formatted; files_failed; errors })
    ->
      Ok (files_formatted, files_failed, errors)
  | Ok (WireProtocol.FormatError { error }) -> Error error
  | Ok _ -> Error "Invalid format all response"
  | Error err -> Error (jsonrpc_error_to_string err)
