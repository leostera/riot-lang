open Std
open Std.Data
open Miniriot
open Tusk_model
open Tusk_protocol
open Client
(** Format code string with ocamlformat *)
let format_code t ~code ~file_path =
  let params =
    match file_path with
    | Some path ->
        Jsonrpc.Named
          [ ("code", Json.String code); ("file_path", Json.String path) ]
    | None -> Jsonrpc.Named [ ("code", Json.String code) ]
  in
  match Jsonrpc.Client.call t.client ~method_:method_format_code ~params () with
  | Ok (WireProtocol.FormatResult { formatted_code; changed }) ->
      Ok (formatted_code, changed)
  | Ok (WireProtocol.FormatError { error }) -> Error error
  | Ok _ -> Error "Invalid format response"
  | Error e ->
      Error (jsonrpc_error_to_string e)
