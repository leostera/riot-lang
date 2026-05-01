open Std

(** {1 Type Matching} *)

(**
   Check if content type matches pattern.

   Supports:
   - Exact match: "application/json" = "application/json"
   - Type wildcard: "text/*" matches "text/plain", "text/html", etc.
   - Full wildcard: "*/*" matches anything
*)
let matches_pattern = fun ~pattern ~content_type ->
  match (pattern, content_type) with
  | ("*/*", _) -> true
  | (pat, ct) -> (
      (* Split type/subtype *)
      match (String.split_on_char '/' pat, String.split_on_char '/' ct) with
      | ([ type1; "*" ], [ type2; _ ]) -> String.equal type1 type2
      | ([ type1; sub1 ], [ type2; sub2 ]) -> String.equal type1 type2 && String.equal sub1 sub2
      | _ -> false
    )

(** {1 Accept Header Parsing} *)

type accept_entry = { media_type: string; quality: float }

type quality_parse_error =
  | MissingQualityValue
  | InvalidQualityValue of { value: string }
  | MalformedQualityParameter of { parameter: string }

type accept_parse_error =
  | EmptyMediaType
  | InvalidQuality of quality_parse_error
  | QualityOutOfRange of float

type accept_rejection =
  | MalformedAcceptHeader of {
      value: string;
      error: accept_parse_error;
    }
  | UnsupportedAcceptHeader of { value: string }

type content_type_rejection =
  | MissingContentType
  | InvalidContentType of { value: string }
  | UnsupportedContentType of { value: string }

type validation_error =
  | AcceptRejected of accept_rejection
  | ContentTypeRejected of content_type_rejection

(**
   Parse quality value from parameter string.

   Example: "q=0.8" -> Some 0.8
*)
let parse_quality = fun param ->
  let trimmed = String.trim param in
  match String.split_on_char '=' trimmed with
  | [ key; value ] when String.equal (String.lowercase_ascii (String.trim key)) "q" -> (
      let value = String.trim value in
      match Float.parse value with
      | None -> Error (InvalidQuality (InvalidQualityValue { value }))
      | Some quality when Order.is_lt (Float.compare quality 0.0)
      || Order.is_gt (Float.compare quality 1.0) -> Error (QualityOutOfRange quality)
      | Some quality -> Ok (Some quality)
    )
  | [ key ] when String.equal (String.lowercase_ascii (String.trim key)) "q" ->
      Error (InvalidQuality MissingQualityValue)
  | key :: _ when String.equal (String.lowercase_ascii (String.trim key)) "q" ->
      Error (InvalidQuality (MalformedQualityParameter { parameter = trimmed }))
  | _ -> Ok None

(**
   Parse single Accept header entry with quality value.

   Examples:
   - "application/json" -> { media_type = "application/json"; quality = 1.0 }
   - "text/html;q=0.9" -> { media_type = "text/html"; quality = 0.9 }
*)
let parse_accept_entry = fun entry ->
  match String.split_on_char ';' entry with
  | [] -> Ok { media_type = "*/*"; quality = 1.0 }
  | media_type :: params ->
      let media_type = String.trim media_type in
      if String.length media_type = 0 then
        Error EmptyMediaType
      else
        let rec parse_params = fun quality params ->
          match params with
          | [] -> Ok { media_type; quality }
          | param :: rest -> (
              match parse_quality param with
              | Error error -> Error error
              | Ok None -> parse_params quality rest
              | Ok (Some quality) -> parse_params quality rest
            )
        in
        parse_params 1.0 params

(**
   Parse full Accept header.

   Returns list sorted by quality (highest first).
*)
let parse_accept = fun header ->
  let entries =
    String.split_on_char ',' header
    |> List.map ~fn:String.trim
    |> List.filter ~fn:(fun s -> String.length s > 0)
  in
  let rec parse_entries = fun acc entries ->
    match entries with
    | [] -> Ok (List.sort acc ~compare:(fun a b -> Float.compare b.quality a.quality))
    | entry :: rest -> (
        match parse_accept_entry entry with
        | Ok parsed -> parse_entries (parsed :: acc) rest
        | Error error -> Error error
      )
  in
  parse_entries [] entries

let parse_accept_or_empty = fun header ->
  match parse_accept header with
  | Ok entries -> entries
  | Error _ -> []

let accept_header_matches = fun ~types accept ->
  match parse_accept accept with
  | Error error -> Error error
  | Ok entries ->
      Ok (List.exists
        (fun entry ->
          Order.is_gt (Float.compare entry.quality 0.0)
          && List.exists
            (fun content_type -> matches_pattern ~pattern:entry.media_type ~content_type)
            types)
        entries)

let quality_parse_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | MissingQualityValue -> "missing q value"
  | InvalidQualityValue { value } -> "invalid q value: " ^ value
  | MalformedQualityParameter { parameter } -> "malformed q parameter: " ^ parameter

let accept_parse_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | EmptyMediaType -> "empty media type"
  | InvalidQuality error -> quality_parse_error_to_string error
  | QualityOutOfRange quality -> "q value is outside 0.0..1.0: " ^ Float.to_string quality

let accept_rejection_to_string = fun __tmp1 ->
  match __tmp1 with
  | MalformedAcceptHeader { value; error } ->
      "Malformed Accept header: " ^ value ^ " (" ^ accept_parse_error_to_string error ^ ")"
  | UnsupportedAcceptHeader { value } -> "No supported response media type matches Accept: " ^ value

let content_type_rejection_to_string = fun __tmp1 ->
  match __tmp1 with
  | MissingContentType -> "Request body is missing a Content-Type header"
  | InvalidContentType { value } -> "Invalid Content-Type header: " ^ value
  | UnsupportedContentType { value } -> "Unsupported request Content-Type: " ^ value

let validation_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | AcceptRejected rejection -> accept_rejection_to_string rejection
  | ContentTypeRejected rejection -> content_type_rejection_to_string rejection

(** {1 Content-Type Parsing} *)

(**
   Extract base content type, stripping parameters.

   Examples:
   - "application/json" -> Some "application/json"
   - "application/json; charset=utf-8" -> Some "application/json"
   - "multipart/form-data; boundary=..." -> Some "multipart/form-data"
*)
let get_base_content_type = fun ct ->
  match String.split_on_char ';' ct with
  | [] -> None
  | base :: _ ->
      let trimmed = String.trim base in
      if String.length trimmed = 0 then
        None
      else
        Some trimmed

(** {1 Configuration} *)

type config = {
  types: string list;
  check_accept: bool;
  check_content_type: bool;
  on_reject: (Conn.t -> string option -> Conn.t) option;
}

let default_config = {
  types = [ "*/*" ];
  check_accept = true;
  check_content_type = true;
  on_reject = None;
}

(** {1 HTTP Responses} *)

(** Send 406 Not Acceptable response *)
let reject_not_acceptable = fun conn config rejection ->
  let received =
    match rejection with
    | MalformedAcceptHeader { value; _ }
    | UnsupportedAcceptHeader { value } -> Some value
  in
  match config.on_reject with
  | Some handler -> handler conn received
  | None ->
      Conn.respond conn ~status:NotAcceptable ~body:(accept_rejection_to_string rejection)
      |> Conn.halt

(** Send 415 Unsupported Media Type response *)
let reject_unsupported_media_type = fun conn config rejection ->
  let received =
    match rejection with
    | MissingContentType -> None
    | InvalidContentType { value }
    | UnsupportedContentType { value } -> Some value
  in
  match config.on_reject with
  | Some handler -> handler conn received
  | None ->
      Conn.respond
        conn
        ~status:UnsupportedMediaType
        ~body:(content_type_rejection_to_string rejection)
      |> Conn.halt

(** {1 Validation Logic} *)

(** Check if Accept header matches any accepted types *)
let check_accept_header_result = fun conn config ->
  let headers = Conn.headers conn in
  match Net.Http.Header.get headers "accept" with
  | None -> Ok ()
  | Some accept -> (
      match accept_header_matches ~types:config.types accept with
      | Error error -> Error (MalformedAcceptHeader { value = accept; error })
      | Ok true -> Ok ()
      | Ok false -> Error (UnsupportedAcceptHeader { value = accept })
    )

let check_accept_header = fun conn config ->
  match check_accept_header_result conn config with
  | Ok () -> (true, None)
  | Error (MalformedAcceptHeader { value; _ })
  | Error (UnsupportedAcceptHeader { value }) -> (false, Some value)

(** Check if Content-Type header matches any accepted types *)
let check_content_type_header_result = fun conn config ->
  let headers = Conn.headers conn in
  match Net.Http.Header.get headers "content-type" with
  | None -> Error MissingContentType
  | Some ct -> (
      match get_base_content_type ct with
      | Some base ->
          if
            List.exists (fun pattern -> matches_pattern ~pattern ~content_type:base) config.types
          then
            Ok ()
          else
            Error (UnsupportedContentType { value = ct })
      | None -> Error (InvalidContentType { value = ct })
    )

(** Check if request declares a body. *)
let request_declares_body = fun ~method_ ~headers ->
  match method_ with
  | Net.Http.Method.Post
  | Put
  | Patch ->
      if Net.Http.Header.has headers "transfer-encoding" then
        true
      else
        (
          match Net.Http.Header.get headers "content-length" with
          | Some value -> (
              match Int.parse (String.trim value) with
              | Some len -> len > 0
              | None -> true
            )
          | None -> false
        )
  | _ -> false

let has_declared_request_body = fun conn ->
  request_declares_body
    ~method_:(Conn.method_ conn)
    ~headers:(Conn.headers conn)

let validate = fun conn config ->
  let has_body = has_declared_request_body conn in
  if config.check_accept then
    match check_accept_header_result conn config with
    | Error rejection -> Error (AcceptRejected rejection)
    | Ok () ->
        if config.check_content_type && has_body then
          match check_content_type_header_result conn config with
          | Error rejection -> Error (ContentTypeRejected rejection)
          | Ok () -> Ok ()
        else
          Ok ()
  else if config.check_content_type && has_body then
    match check_content_type_header_result conn config with
    | Error rejection -> Error (ContentTypeRejected rejection)
    | Ok () -> Ok ()
  else
    Ok ()

(** {1 Middleware} *)

let make = fun config ->
  fun ~conn ~next ->
    match validate conn config with
    | Ok () -> next conn
    | Error (AcceptRejected rejection) -> reject_not_acceptable conn config rejection
    | Error (ContentTypeRejected rejection) -> reject_unsupported_media_type conn config rejection

let middleware = fun ?config:(cfg = default_config) types ->
  (* If types list is provided, override config.types *)
  let cfg' =
    if List.is_empty types then
      cfg
    else
      { cfg with types }
  in
  make cfg'
