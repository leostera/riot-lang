open Std

type parser =
  | Urlencoded
  | Json
  | Multipart

type config = {
  parsers: parser list;
  max_body_size: int;
}

type json_root_kind =
  | JsonNull
  | JsonBool
  | JsonInt
  | JsonFloat
  | JsonString
  | JsonArray
  | JsonObject

type parse_error =
  | BodyTooLarge of { size: int; max_size: int }
  | InvalidContentType of { value: string }
  | InvalidJson of Std.Data.Json.error
  | JsonRootNotObject of json_root_kind
  | MissingMultipartBoundary
  | UnsupportedMultipart of { boundary: string }

type parsed_body = {
  body_params: (string * string) list;
  json: Std.Data.Json.t option;
}

let parsed_json_key: Std.Data.Json.t Conn.assign_key = Conn.assign_key ()

let parsed_json = fun conn -> Conn.get_assign parsed_json_key conn

let default_config = fun () -> {
  parsers = [ Urlencoded; Json ];
  max_body_size = 10 * 1_024 * 1_024;
}

let rec json_root_kind = fun __tmp1 ->
  match __tmp1 with
  | Std.Data.Json.Null -> JsonNull
  | Bool _ -> JsonBool
  | Int _ -> JsonInt
  | Float _ -> JsonFloat
  | String _ -> JsonString
  | Array _ -> JsonArray
  | Object _ -> JsonObject
  | Embed json -> json_root_kind json

let json_root_kind_to_string = fun __tmp1 ->
  match __tmp1 with
  | JsonNull -> "null"
  | JsonBool -> "bool"
  | JsonInt -> "int"
  | JsonFloat -> "float"
  | JsonString -> "string"
  | JsonArray -> "array"
  | JsonObject -> "object"

let parse_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | BodyTooLarge { size; max_size } ->
      "Request body is too large ("
      ^ Int.to_string size
      ^ " bytes, maximum is "
      ^ Int.to_string max_size
      ^ " bytes)"
  | InvalidContentType { value } -> "Invalid Content-Type header: " ^ value
  | InvalidJson error -> "Invalid JSON request body: " ^ Std.Data.Json.error_to_string error
  | JsonRootNotObject kind ->
      "Expected JSON request body to be an object, got " ^ json_root_kind_to_string kind
  | MissingMultipartBoundary -> "multipart/form-data request is missing a boundary parameter"
  | UnsupportedMultipart { boundary } ->
      "multipart/form-data request parsing is not supported yet (boundary: " ^ boundary ^ ")"

let parse_urlencoded = fun body -> Net.Uri.Query.parse body

let parse_json = fun body ->
  match Std.Data.Json.from_string body with
  | Error error -> Error (InvalidJson error)
  | Ok (Std.Data.Json.Object fields) ->
      let json = Std.Data.Json.Object fields in
      let body_params =
        List.filter_map
          ~fn:(fun (k, v) ->
            match v with
            | Std.Data.Json.String s -> Some (k, s)
            | Std.Data.Json.Int i -> Some (k, Int.to_string i)
            | Std.Data.Json.Float f -> Some (k, Float.to_string f)
            | Std.Data.Json.Bool b -> Some (k, Bool.to_string b)
            | Std.Data.Json.Null -> Some (k, "")
            | _ -> None)
          fields
      in
      Ok { body_params; json = Some json }
  | Ok json -> Error (JsonRootNotObject (json_root_kind json))

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

let parse_content_type = fun content_type ->
  match Net.Http.Header.Value.parse_content_type content_type with
  | Error Net.Http.Header.Value.InvalidContentType ->
      Error (InvalidContentType { value = content_type })
  | Ok parsed -> Ok parsed

let parse_body_full = fun config ~content_type ~body ->
  if String.length body > config.max_body_size then
    Error (BodyTooLarge { size = String.length body; max_size = config.max_body_size })
  else
    match parse_content_type content_type with
    | Error error -> Error error
    | Ok (media_type, params) ->
        let media_type = String.lowercase_ascii media_type in
        if
          String.equal media_type "application/x-www-form-urlencoded"
          && List.contains config.parsers ~value:Urlencoded
        then
          Ok { body_params = parse_urlencoded body; json = None }
        else if
          String.equal media_type "application/json" && List.contains config.parsers ~value:Json
        then
          parse_json body
        else if
          String.equal media_type "multipart/form-data"
          && List.contains config.parsers ~value:Multipart
        then
          match Std.Collections.Proplist.get params ~key:"boundary" with
          | Some boundary -> Error (UnsupportedMultipart { boundary = strip_quotes boundary })
          | None -> Error MissingMultipartBoundary
        else
          Ok { body_params = []; json = None }

let parse_body = fun config ~content_type ~body ->
  match parse_body_full config ~content_type ~body with
  | Ok parsed -> Ok parsed.body_params
  | Error error -> Error error

let respond_with_error = fun error conn ->
  let status =
    match error with
    | BodyTooLarge _ -> Net.Http.Status.PayloadTooLarge
    | InvalidContentType _
    | InvalidJson _
    | JsonRootNotObject _
    | MissingMultipartBoundary -> Net.Http.Status.BadRequest
    | UnsupportedMultipart _ -> Net.Http.Status.UnsupportedMediaType
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
      match parse_body_full config ~content_type ~body:(Conn.body conn) with
      | Ok parsed ->
          let conn =
            match parsed.json with
            | Some json -> Conn.assign parsed_json_key json conn
            | None -> conn
          in
          Conn.set_body_params parsed.body_params conn
      | Error error -> respond_with_error error conn
    )

let make = fun ?(config = default_config ()) () ->
  fun ~conn ~next ->
    let conn = handle config conn in
    next conn
