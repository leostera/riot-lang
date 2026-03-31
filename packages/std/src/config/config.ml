open Global
open Collections
module Spec = Spec
module Loader = Loader
module Validator = Validator
module Provider = Provider
module Server = Server

type error =
  | NotFound of {
      app : string;
    }
  | ValidationError of {
      app : string;
      errors : string list;
    }
  | ParseError of {
      path : string;
      message : string;
    }
  | FileNotFound of {
      path : string;
    }

let error_to_string =
  function
  | NotFound { app } ->
      "Config section [" ^ app ^ "] not found in config file"
  | ValidationError { app; errors } ->
      let errs = String.concat ", " errors in
      "Validation errors for app '" ^ app ^ "': " ^ errs
  | ParseError { path; message } ->
      "Parse error in " ^ path ^ ": " ^ message
  | FileNotFound { path } ->
      "Config file not found: " ^ path

module type ConfigSpec = sig
  val spec : Spec.t

  type t
  val get : Spec.value -> (t, error) result
end

let config_server : Server.t option Sync.Cell.t = cell None

let ensure_loaded = fun () ->
  match !config_server with
  | Some server -> server
  | None ->
      (* Auto-load on first access! *)
      let server = Server.init ~provider:(Provider.env ()) in
      config_server := Some server;
      server

let load = fun ?(provider = Provider.env ()) () -> config_server := Some (Server.init ~provider)

let load_from_env = fun () -> load ()

let load_string = fun str -> load ~provider:(Provider.static str) ()

let load_file = fun path -> load ~provider:(Provider.file path) ()

let get (type a) ((module M : ConfigSpec with type t = a)) : (a, error) result =
  let app_name = Spec.app_name M.spec in
  let server = ensure_loaded () in
  match Server.get server ~app:app_name with
  | None -> Error (NotFound {app = app_name})
  | Some value -> M.get value

let reload = fun ?provider () ->
  let server = ensure_loaded () in
  let new_server = Server.reload ?provider server in
  config_server := Some new_server;
  Ok ()

let patch = fun ~app updates ->
  let server = ensure_loaded () in
  match Server.patch server ~app ~updates with
  | Error err -> Error (Server.error_to_string err)
  | Ok _new_server ->
      (* Patch mutates the HashMap in place, so server is already updated *)
      Ok ()

(* Value extraction helpers - panic on type mismatch or missing keys *)

let get_string = fun (value:Spec.value) key ->
  match value with
  | Spec.Map kvs -> (
      match List.assoc_opt key kvs with
      | Some (Spec.String s) -> s
      | Some _ -> panic ("Config key '" ^ key ^ "' is not a string")
      | None -> panic ("Config key '" ^ key ^ "' not found")
    )
  | _ -> panic "Expected Map value"

let get_char = fun (value:Spec.value) key ->
  match value with
  | Spec.Map kvs -> (
      match List.assoc_opt key kvs with
      | Some (Spec.Char c) -> c
      | Some _ -> panic ("Config key '" ^ key ^ "' is not a char")
      | None -> panic ("Config key '" ^ key ^ "' not found")
    )
  | _ -> panic "Expected Map value"

let get_int = fun (value:Spec.value) key ->
  match value with
  | Spec.Map kvs -> (
      match List.assoc_opt key kvs with
      | Some (Spec.Int i) -> i
      | Some _ -> panic ("Config key '" ^ key ^ "' is not an int")
      | None -> panic ("Config key '" ^ key ^ "' not found")
    )
  | _ -> panic "Expected Map value"

let get_int32 = fun (value:Spec.value) key ->
  match value with
  | Spec.Map kvs -> (
      match List.assoc_opt key kvs with
      | Some (Spec.Int32 i) -> i
      | Some _ -> panic ("Config key '" ^ key ^ "' is not an int32")
      | None -> panic ("Config key '" ^ key ^ "' not found")
    )
  | _ -> panic "Expected Map value"

let get_int64 = fun (value:Spec.value) key ->
  match value with
  | Spec.Map kvs -> (
      match List.assoc_opt key kvs with
      | Some (Spec.Int64 i) -> i
      | Some _ -> panic ("Config key '" ^ key ^ "' is not an int64")
      | None -> panic ("Config key '" ^ key ^ "' not found")
    )
  | _ -> panic "Expected Map value"

let get_bool = fun (value:Spec.value) key ->
  match value with
  | Spec.Map kvs -> (
      match List.assoc_opt key kvs with
      | Some (Spec.Bool b) -> b
      | Some _ -> panic ("Config key '" ^ key ^ "' is not a bool")
      | None -> panic ("Config key '" ^ key ^ "' not found")
    )
  | _ -> panic "Expected Map value"

let get_float = fun (value:Spec.value) key ->
  match value with
  | Spec.Map kvs -> (
      match List.assoc_opt key kvs with
      | Some (Spec.Float f) -> f
      | Some _ -> panic ("Config key '" ^ key ^ "' is not a float")
      | None -> panic ("Config key '" ^ key ^ "' not found")
    )
  | _ -> panic "Expected Map value"

let get_uri = fun (value:Spec.value) key ->
  match value with
  | Spec.Map kvs -> (
      match List.assoc_opt key kvs with
      | Some (Spec.Uri uri) -> uri
      | Some _ -> panic ("Config key '" ^ key ^ "' is not a URI")
      | None -> panic ("Config key '" ^ key ^ "' not found")
    )
  | _ -> panic "Expected Map value"

let get_datetime = fun (value:Spec.value) key ->
  match value with
  | Spec.Map kvs -> (
      match List.assoc_opt key kvs with
      | Some (Spec.Datetime dt) -> dt
      | Some _ -> panic ("Config key '" ^ key ^ "' is not a datetime")
      | None -> panic ("Config key '" ^ key ^ "' not found")
    )
  | _ -> panic "Expected Map value"

let get_path = fun (value:Spec.value) key ->
  match value with
  | Spec.Map kvs -> (
      match List.assoc_opt key kvs with
      | Some (Spec.Path p) -> p
      | Some _ -> panic ("Config key '" ^ key ^ "' is not a path")
      | None -> panic ("Config key '" ^ key ^ "' not found")
    )
  | _ -> panic "Expected Map value"

let get_uuid = fun (value:Spec.value) key ->
  match value with
  | Spec.Map kvs -> (
      match List.assoc_opt key kvs with
      | Some (Spec.Uuid uuid) -> uuid
      | Some _ -> panic ("Config key '" ^ key ^ "' is not a UUID")
      | None -> panic ("Config key '" ^ key ^ "' not found")
    )
  | _ -> panic "Expected Map value"

let get_list = fun (value:Spec.value) key ->
  match value with
  | Spec.Map kvs -> (
      match List.assoc_opt key kvs with
      | Some (Spec.List items) -> items
      | Some _ -> panic ("Config key '" ^ key ^ "' is not a list")
      | None -> panic ("Config key '" ^ key ^ "' not found")
    )
  | _ -> panic "Expected Map value"

let get_discriminated_union = fun (value:Spec.value) key ->
  match value with
  | Spec.Map kvs -> (
      match List.assoc_opt key kvs with
      | Some (Spec.DiscriminatedUnion { discriminant; variant; fields }) -> (
        discriminant,
        variant,
        fields
      )
      | Some _ -> panic ("Config key '" ^ key ^ "' is not a discriminated union")
      | None -> panic ("Config key '" ^ key ^ "' not found")
    )
  | _ -> panic "Expected Map value"

let get_map = fun (value:Spec.value) key ->
  match value with
  | Spec.Map kvs -> (
      match List.assoc_opt key kvs with
      | Some (Spec.Map _ as m) -> m
      | Some _ -> panic ("Config key '" ^ key ^ "' is not a map")
      | None -> panic ("Config key '" ^ key ^ "' not found")
    )
  | _ -> panic "Expected Map value"

let as_string = fun (value:Spec.value) ->
  match value with
  | Spec.String s -> s
  | _ -> panic "Expected String value"

let as_char = fun (value:Spec.value) ->
  match value with
  | Spec.Char c -> c
  | _ -> panic "Expected Char value"

let as_int = fun (value:Spec.value) ->
  match value with
  | Spec.Int i -> i
  | _ -> panic "Expected Int value"

let as_int32 = fun (value:Spec.value) ->
  match value with
  | Spec.Int32 i -> i
  | _ -> panic "Expected Int32 value"

let as_int64 = fun (value:Spec.value) ->
  match value with
  | Spec.Int64 i -> i
  | _ -> panic "Expected Int64 value"

let as_bool = fun (value:Spec.value) ->
  match value with
  | Spec.Bool b -> b
  | _ -> panic "Expected Bool value"

let as_float = fun (value:Spec.value) ->
  match value with
  | Spec.Float f -> f
  | _ -> panic "Expected Float value"

let as_uri = fun (value:Spec.value) ->
  match value with
  | Spec.Uri uri -> uri
  | _ -> panic "Expected Uri value"

let as_datetime = fun (value:Spec.value) ->
  match value with
  | Spec.Datetime dt -> dt
  | _ -> panic "Expected Datetime value"

let as_path = fun (value:Spec.value) ->
  match value with
  | Spec.Path p -> p
  | _ -> panic "Expected Path value"

let as_uuid = fun (value:Spec.value) ->
  match value with
  | Spec.Uuid uuid -> uuid
  | _ -> panic "Expected Uuid value"

let as_list = fun (value:Spec.value) ->
  match value with
  | Spec.List items -> items
  | _ -> panic "Expected List value"

let as_discriminated_union = fun (value:Spec.value) ->
  match value with
  | Spec.DiscriminatedUnion { discriminant; variant; fields } -> (discriminant, variant, fields)
  | _ -> panic "Expected DiscriminatedUnion value"

let as_map = fun (value:Spec.value) ->
  match value with
  | Spec.Map kvs -> kvs
  | _ -> panic "Expected Map value"
