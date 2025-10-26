open Std
open Std.Data
open Miniriot
open Tusk_model
open Tusk_protocol
open Client
(** Format a file with ocamlformat *)
let format_file t ~file_path ~check_only =
  match
    Jsonrpc.Client.call t.client ~method_:method_format_file
      ~params:
        (Jsonrpc.Named
           [
             ("file_path", Json.String file_path);
             ("check_only", Json.Bool check_only);
           ])
      ()
  with
  | Ok (WireProtocol.FormatResult { formatted_code; changed }) ->
      Ok (formatted_code, changed)
  | Ok (WireProtocol.FormatError { error }) -> Error error
  | Ok _ -> Error "Invalid format response"
  | Error e ->
      Error (jsonrpc_error_to_string e)
