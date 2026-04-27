open Std

type parser =
  | Urlencoded
  | Json
  | Multipart

type config = {
  parsers: parser list;
  max_body_size: int;
}

type parse_error =
  | BodyTooLarge of { size: int; max_size: int }
  | InvalidContentType of string
  | InvalidJson of Std.Data.Json.error
  | JsonRootNotObject of string
  | MissingMultipartBoundary

let default_config = fun () -> {
  parsers = [ Urlencoded; Json ];
  max_body_size = 10 * 1_024 * 1_024;
}

let rec json_kind = function
  | Std.Data.Json.Null -> "null"
  | Bool _ -> "bool"
  | Int _ -> "int"
  | Float _ -> "float"
  | String _ -> "string"
  | Array _ -> "array"
  | Object _ -> "object"
  | Embed json -> json_kind json

let parse_error_to_string = function
  | BodyTooLarge { size; max_size } ->
      "Request body is too large ("
      ^ Int.to_string size
      ^ " bytes, maximum is "
      ^ Int.to_string max_size
      ^ " bytes)"
  | InvalidContentType content_type -> "Invalid Content-Type header: " ^ content_type
  | InvalidJson error -> "Invalid JSON request body: " ^ Std.Data.Json.error_to_string error
  | JsonRootNotObject kind -> "Expected JSON request body to be an object, got " ^ kind
  | MissingMultipartBoundary -> "multipart/form-data request is missing a boundary parameter"

(** Parse application/x-www-form-urlencoded body using Net.Uri.Query.parse *)
let parse_urlencoded = fun body -> Net.Uri.Query.parse body

let parse_json_result = fun body ->
  match Std.Data.Json.of_string body with
  | Error error -> Error (InvalidJson error)
  | Ok (Std.Data.Json.Object fields) ->
      Ok (
        List.filter_map
          ~fn:(fun ((k, v)) ->
            match v with
            | Std.Data.Json.String s -> Some (k, s)
            | Std.Data.Json.Int i -> Some (k, Int.to_string i)
            | Std.Data.Json.Float f -> Some (k, Float.to_string f)
            | Std.Data.Json.Bool b -> Some (k, Bool.to_string b)
            | Std.Data.Json.Null -> Some (k, "")
            | _ -> None)
          fields
      )
  | Ok json -> Error (JsonRootNotObject (json_kind json))

(** Parse multipart/form-data - TODO: use Mime library *)
let parse_multipart = fun ~boundary:_ _body ->
  (* TODO: Implement proper multipart parsing with Mime library *)
  (* For now, return empty list *)
  []

let strip_quotes = fun value ->
  let len = String.length value in
  if
    len >= 2
    && String.get_unchecked value ~at:0 = '"'
    && String.get_unchecked value ~at:(len - 1) = '"'
  then
    String.sub value ~offset:1 ~len:(len - 2)
  else
    value

let parse_body = fun config ~content_type ~body ->
  if String.length body > config.max_body_size then
    Error (BodyTooLarge { size = String.length body; max_size = config.max_body_size })
  else
    match Net.Http.Header.Value.parse_content_type content_type with
    | Error `InvalidContentType -> Error (InvalidContentType content_type)
    | Ok (media_type, params) ->
        let media_type = String.lowercase_ascii media_type in
        if
          String.equal media_type "application/x-www-form-urlencoded"
          && List.contains config.parsers ~value:Urlencoded
        then
          Ok (parse_urlencoded body)
        else if
          String.equal media_type "application/json" && List.contains config.parsers ~value:Json
        then
          parse_json_result body
        else if
          String.equal media_type "multipart/form-data"
          && List.contains config.parsers ~value:Multipart
        then
          match Std.Collections.Proplist.get params ~key:"boundary" with
          | Some boundary -> Ok (parse_multipart ~boundary:(strip_quotes boundary) body)
          | None -> Error MissingMultipartBoundary
        else
          Ok []

let respond_with_error = fun error conn ->
  let status =
    match error with
    | BodyTooLarge _ -> Net.Http.Status.PayloadTooLarge
    | InvalidContentType _
    | InvalidJson _
    | JsonRootNotObject _
    | MissingMultipartBoundary -> Net.Http.Status.BadRequest
  in
  conn
  |> Conn.with_status status
  |> Conn.with_header "Content-Type" "text/plain; charset=utf-8"
  |> Conn.with_body (parse_error_to_string error)
  |> Conn.send

let handle = fun config conn ->
  match Net.Http.Header.get (Conn.headers conn) "content-type" with
  | None -> conn
  | Some content_type -> (
      match parse_body config ~content_type ~body:(Conn.body conn) with
      | Ok body_params -> Conn.set_body_params body_params conn
      | Error error -> respond_with_error error conn
    )

let make = fun ?(config = default_config ()) () ->
  fun ~conn ~next ->
    let conn = handle config conn in
    next conn

module For_testing = struct
  let parse_body = parse_body
end
