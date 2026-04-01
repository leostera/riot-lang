open Std
open Std.Data
open Std.Collections

let version = "2.0"

type id =
  String of string
  | Number of int
  | Null

type params =
  | Positional of Json.t list
  | Named of (string * Json.t) list
  | NoParams

type prerequest = {
  method_: string;
  params: params;
}

type request = {
  jsonrpc: string;
  method_: string;
  params: params;
  id: id option;
}

type error =
  | ParseError of { raw_input: string; parse_error: string }
  | InvalidRequest of { request_json: Json.t; reason: string }
  | MethodNotFound of { method_name: string }
  | InvalidParams of { method_name: string; params: params; reason: string }
  | InternalError of { context: string; details: string }
  | UnknownServerError of { code: int; message: string; data: Json.t option }

type 'res response = {
  jsonrpc: string;
  result: 'res;
  id: id;
}

type batch_request = request list

type 'res batch_response = 'res response list
(** Convert ID to JSON *)
let id_to_json = function
  | String s -> Json.String s
  | Number n -> Json.Int n
  | Null -> Json.Null
(** Convert JSON to ID *)
let id_of_json = function
  | Json.String s -> Ok (String s)
  | Json.Int n -> Ok (Number n)
  | Json.Null -> Ok Null
  | _ -> Error "Invalid ID type"
(** Convert params to JSON *)
let params_to_json = function
  | Positional lst -> Json.Array lst
  | Named pairs -> Json.Object pairs
  | NoParams -> Json.Null
(** Convert JSON to params *)
let params_of_json = function
  | Json.Array lst -> Ok (Positional lst)
  | Json.Object pairs -> Ok (Named pairs)
  | Json.Null -> Ok NoParams
  | _ -> Error "Invalid params type"
(** Convert request to JSON *)
let request_to_json = fun (req: request) ->
  let fields = [ ("jsonrpc", Json.String req.jsonrpc); ("method", Json.String req.method_); ] in
  let fields =
    match req.params with
    | NoParams -> fields
    | params -> ("params", params_to_json params) :: fields
  in
  let fields =
    match req.id with
    | None -> fields
    | Some id -> ("id", id_to_json id) :: fields
  in
  Json.Object fields
(** Convert JSON to request *)
let request_of_json = fun json ->
  match json with
  | Json.Object fields -> (
      match (List.assoc_opt "jsonrpc" fields, List.assoc_opt "method" fields) with
      | Some (Json.String "2.0"), Some (Json.String method_) -> (
          let params =
            match List.assoc_opt "params" fields with
            | None -> Ok NoParams
            | Some p -> params_of_json p
          in
          let id_result =
            match List.assoc_opt "id" fields with
            | None -> Ok None
            | Some j -> (
                match id_of_json j with
                | Ok parsed_id -> Ok (Some parsed_id)
                | Error e -> Error e
              )
          in
          match (params, id_result) with
          | Ok params, Ok id -> Ok { jsonrpc = "2.0"; method_; params; id }
          | Error e, _ -> Error e
          | _, Error e -> Error e
        )
      | Some (Json.String v), _ ->
          Error ("Invalid JSON-RPC version: " ^ v ^ " (expected 2.0)")
      | _ ->
          Error "Invalid request: missing jsonrpc or method field"
    )
  | _ -> Error "Request must be an object"
(** Note: response_to_json removed - needs to be protocol-specific due to
    parameterized type *)

(** Note: response_of_json removed - needs to be protocol-specific due to
    parameterized type *)

(** Helper to make a request *)
let request = fun ~method_ ?params ?id () ->
  { jsonrpc = version; method_; params = Option.unwrap_or params ~default:NoParams; id }
(** Create a successful response with result *)
let result = fun res ~id -> { jsonrpc = version; result = Ok res; id }

let ok = fun ?(id = Null) res -> { jsonrpc = version; result = res; id }
(** Helper to make a notification (request without id) *)
let notification = fun ~method_ ?params () ->
  { jsonrpc = version; method_; params = Option.unwrap_or params ~default:NoParams; id = None }
(** Check if a request is a notification *)
let is_notification = fun (req: request) ->
  match req.id with
  | None -> true
  | Some _ -> false

(* ApplicationProtocol module type *)

module type ApplicationProtocol = sig
  type request
  type response
  val response_to_json: response -> Json.t

  val response_of_json: Json.t -> (response, Json.t) result

  val request_to_params: request -> prerequest

  val request_of_params: string -> params -> (request, Json.t) result
end
