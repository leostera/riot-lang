let version = "2.0"

type id = String of string | Number of int | Null

type params =
  | Positional of Json.t list
  | Named of (string * Json.t) list
  | NoParams

type request = {
  jsonrpc : string;
  method_ : string;
  params : params;
  id : id option;
}

type error_code =
  | ParseError
  | InvalidRequest
  | MethodNotFound
  | InvalidParams
  | InternalError
  | ServerError of int
  | ApplicationError of int

type error = { code : error_code; message : string; data : Json.t option }

type response = {
  jsonrpc : string;
  result : Json.t option;
  error : error option;
  id : id;
}

type batch_request = request list
type batch_response = response list

(** Convert error code to integer *)
let error_code_to_int = function
  | ParseError -> -32700
  | InvalidRequest -> -32600
  | MethodNotFound -> -32601
  | InvalidParams -> -32602
  | InternalError -> -32603
  | ServerError n -> n (* Should be -32000 to -32099 *)
  | ApplicationError n -> n

(** Convert integer to error code *)
let int_to_error_code = function
  | -32700 -> ParseError
  | -32600 -> InvalidRequest
  | -32601 -> MethodNotFound
  | -32602 -> InvalidParams
  | -32603 -> InternalError
  | n when n >= -32099 && n <= -32000 -> ServerError n
  | n -> ApplicationError n

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

(** Convert error to JSON *)
let error_to_json err =
  let fields =
    [
      ("code", Json.Int (error_code_to_int err.code));
      ("message", Json.String err.message);
    ]
  in
  let fields =
    match err.data with Some d -> ("data", d) :: fields | None -> fields
  in
  Json.Object fields

(** Convert JSON to error *)
let error_of_json json =
  match json with
  | Json.Object fields -> (
      match (List.assoc_opt "code" fields, List.assoc_opt "message" fields) with
      | Some (Json.Int code), Some (Json.String message) ->
          let data = List.assoc_opt "data" fields in
          Ok { code = int_to_error_code code; message; data }
      | _ -> Error "Invalid error object: missing or invalid code/message")
  | _ -> Error "Error must be an object"

(** Convert request to JSON *)
let request_to_json (req : request) =
  let fields =
    [
      ("jsonrpc", Json.String req.jsonrpc); ("method", Json.String req.method_);
    ]
  in
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
let request_of_json json =
  match json with
  | Json.Object fields -> (
      match
        (List.assoc_opt "jsonrpc" fields, List.assoc_opt "method" fields)
      with
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
                | Error e -> Error e)
          in
          match (params, id_result) with
          | Ok params, Ok id -> Ok { jsonrpc = "2.0"; method_; params; id }
          | Error e, _ -> Error e
          | _, Error e -> Error e)
      | Some (Json.String v), _ ->
          Error (Printf.sprintf "Invalid JSON-RPC version: %s (expected 2.0)" v)
      | _ -> Error "Invalid request: missing jsonrpc or method field")
  | _ -> Error "Request must be an object"

(** Convert response to JSON *)
let response_to_json (resp : response) =
  let fields =
    [ ("jsonrpc", Json.String resp.jsonrpc); ("id", id_to_json resp.id) ]
  in
  let fields =
    match (resp.result, resp.error) with
    | Some result, None -> ("result", result) :: fields
    | None, Some error -> ("error", error_to_json error) :: fields
    | _ -> fields (* Invalid state but we'll encode it anyway *)
  in
  Json.Object fields

(** Convert JSON to response *)
let response_of_json json =
  match json with
  | Json.Object fields -> (
      match (List.assoc_opt "jsonrpc" fields, List.assoc_opt "id" fields) with
      | Some (Json.String "2.0"), Some id_json -> (
          match id_of_json id_json with
          | Ok id -> (
              let result = List.assoc_opt "result" fields in
              let error =
                match List.assoc_opt "error" fields with
                | None -> Ok None
                | Some e -> (
                    match error_of_json e with
                    | Ok err -> Ok (Some err)
                    | Error msg -> Error msg)
              in
              match error with
              | Ok error -> Ok { jsonrpc = "2.0"; result; error; id }
              | Error e -> Error e)
          | Error e -> Error e)
      | Some (Json.String v), _ ->
          Error (Printf.sprintf "Invalid JSON-RPC version: %s (expected 2.0)" v)
      | _ -> Error "Invalid response: missing jsonrpc or id field")
  | _ -> Error "Response must be an object"

(** Helper to make a request *)
let make_request ~method_ ?params ?id () =
  {
    jsonrpc = version;
    method_;
    params = Option.value params ~default:NoParams;
    id;
  }

(** Helper to make a response *)
let make_response ?result ?error ~id () =
  { jsonrpc = version; result; error; id }

(** Helper to make an error *)
let make_error ~code ~message ?data () = { code; message; data }

(** Helper to make a notification (request without id) *)
let make_notification ~method_ ?params () =
  {
    jsonrpc = version;
    method_;
    params = Option.value params ~default:NoParams;
    id = None;
  }

(** Check if a request is a notification *)
let is_notification (req : request) =
  match req.id with None -> true | Some _ -> false
