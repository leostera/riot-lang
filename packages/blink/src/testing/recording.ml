open Std

module Error = Recorder_error
module H = Client
module Json = Data.Json
module Vector = Collections.Vector

type mode = Record_mode.t =
  | RecordOnce
  | ReplayOnly
  | RecordAll
  | NewEpisodes

type request_fingerprint = Error.request_fingerprint = {
  method_: string;
  url: string;
  body_sha256: string option;
}

type body = { sha256: string; bytes: string }

type stored_request = {
  method_: string;
  url: string;
  headers: (string * string) list;
  body: body option;
}

type stored_response = {
  status: int;
  headers: (string * string) list;
  body: body;
}

type interaction = {
  request: stored_request;
  response: stored_response;
}

type t = {
  version: int;
  name: string;
  record_mode: mode;
  interactions: interaction Vector.t;
}

let sha256 = fun bytes ->
  Crypto.(Sha256.hash_string bytes
  |> Digest.hex)

let char_between = fun char lower upper ->
  Char.compare char lower != Order.LT && Char.compare char upper != Order.GT

let is_safe_path_char = fun char ->
  Char.equal char '.'
  || Char.equal char '-'
  || Char.equal char '_'
  || char_between char 'a' 'z'
  || char_between char 'A' 'Z'
  || char_between char '0' '9'

let is_edge_trim_char = fun char -> Char.equal char '.' || Char.equal char '-'

let trim_path_edges = fun value ->
  let length = String.length value in
  let rec left index =
    if index >= length then
      length
    else if is_edge_trim_char (String.get_unchecked value ~at:index) then
      left (index + 1)
    else
      index
  in
  let rec right index =
    if index < 0 then (
      (-1)
    ) else if is_edge_trim_char (String.get_unchecked value ~at:index) then
      right (index - 1)
    else
      index
  in
  let left = left 0 in
  let right = right (length - 1) in
  if left > right then
    ""
  else
    String.sub value ~offset:left ~len:(right - left + 1)

let sanitize_name = fun name ->
  let name = String.trim name in
  let length = String.length name in
  let buffer = IO.Buffer.create ~size:length in
  let rec loop previous_separator index =
    if index >= length then
      ()
    else
      let char = String.get_unchecked name ~at:index in
      if is_safe_path_char char then (
        IO.Buffer.add_char buffer char;
        loop false (index + 1)
      ) else if previous_separator then
        loop true (index + 1)
      else (
        IO.Buffer.add_char buffer '-';
        loop true (index + 1)
      )
  in
  loop true 0;
  match trim_path_edges (IO.Buffer.contents buffer) with
  | "" -> "recording"
  | sanitized -> sanitized

let name = fun recording -> recording.name

let path_stem = fun recording -> sanitize_name recording.name

let record_mode = fun recording -> recording.record_mode

let with_record_mode = fun recording ~record_mode -> { recording with record_mode }

let interactions = fun recording -> recording.interactions

let body_from_string = fun bytes -> { sha256 = sha256 bytes; bytes }

let request_fingerprint = fun (request: H.Request.t) -> {
  method_ = H.Request.method_to_string request.method_;
  url = request.url;
  body_sha256 = Option.map request.body ~fn:sha256;
}

let stored_request_fingerprint = fun request -> {
  method_ = request.method_;
  url = request.url;
  body_sha256 = Option.map request.body ~fn:(fun body -> body.sha256);
}

let from_blink_request = fun ~redact_headers (request: H.Request.t) ->
  {
    method_ = H.Request.method_to_string request.method_;
    url = request.url;
    headers = redact_headers request.headers;
    body = Option.map request.body ~fn:body_from_string;
  }

let from_blink_response = fun ~redact_headers (response: H.Response.t) -> {
  status = response.status;
  headers = redact_headers response.headers;
  body = body_from_string response.body;
}

let response_to_blink = fun response ->
  H.Response.make
    ~status:response.status
    ~headers:response.headers
    ~body:response.body.bytes
    ()

let request_matches = fun (incoming: H.Request.t) stored ->
  let left = request_fingerprint incoming in
  let right = stored_request_fingerprint stored in
  String.equal left.method_ right.method_
  && String.equal left.url right.url
  && left.body_sha256 = right.body_sha256

let find_interaction = fun recording request ->
  let rec loop index =
    if index >= Vector.length recording.interactions then
      None
    else
      match Vector.get recording.interactions ~at:index with
      | None -> None
      | Some interaction ->
          if request_matches request interaction.request then
            Some interaction
          else
            loop (index + 1)
  in
  loop 0

let body_to_json = fun body ->
  Json.obj
    [
      ("encoding", Json.string "base64");
      ("data", Json.string (Encoding.Base64.encode body.bytes));
      ("sha256", Json.string body.sha256);
    ]

let optional_body_to_json = fun body ->
  match body with
  | Some body -> body_to_json body
  | None -> Json.null

let header_to_json = fun (name, value) ->
  Json.obj
    [ ("name", Json.string name); ("value", Json.string value) ]

let headers_to_json = fun headers -> Json.array (List.map headers ~fn:header_to_json)

let request_to_json = fun request ->
  Json.obj
    [
      ("method", Json.string request.method_);
      ("url", Json.string request.url);
      ("headers", headers_to_json request.headers);
      ("body", optional_body_to_json request.body);
    ]

let response_to_json = fun response ->
  Json.obj
    [
      ("status", Json.int response.status);
      ("headers", headers_to_json response.headers);
      ("body", body_to_json response.body);
    ]

let interaction_to_json = fun interaction ->
  Json.obj
    [
      ("request", request_to_json interaction.request);
      ("response", response_to_json interaction.response);
    ]

let to_json = fun recording ->
  Json.obj
    [
      ("version", Json.int recording.version);
      ("name", Json.string recording.name);
      ("record_mode", Json.string (Record_mode.to_string recording.record_mode));
      (
        "interactions",
        Vector.to_array recording.interactions
        |> Array.to_list
        |> List.map ~fn:interaction_to_json
        |> Json.array
      );
    ]

let require_field = fun field json ->
  match Json.get_field field json with
  | Some value -> Ok value
  | None -> Error (Error.MissingField field)

let require_string = fun field json ->
  match Json.get_string json with
  | Some value -> Ok value
  | None -> Error (Error.InvalidField field)

let require_int = fun field json ->
  match Json.get_int json with
  | Some value -> Ok value
  | None -> Error (Error.InvalidField field)

let require_array = fun field json ->
  match Json.get_array json with
  | Some value -> Ok value
  | None -> Error (Error.InvalidField field)

let decode_body = fun field data expected_sha256 ->
  match Encoding.Base64.decode data with
  | Error Encoding.Base64.InvalidBase64 -> Error Error.InvalidBase64Body
  | Ok bytes ->
      let body = body_from_string bytes in
      if String.equal body.sha256 expected_sha256 then
        Ok body
      else
        Error (Error.InvalidField (field ^ ".sha256"))

let body_from_json = fun field json ->
  require_field "encoding" json
  |> Result.and_then ~fn:(require_string (field ^ ".encoding"))
  |> Result.and_then
    ~fn:(fun encoding ->
      if not (String.equal encoding "base64") then
        Error (Error.InvalidBodyEncoding encoding)
      else
        require_field "data" json
        |> Result.and_then ~fn:(require_string (field ^ ".data"))
        |> Result.and_then
          ~fn:(fun data ->
            require_field "sha256" json
            |> Result.and_then ~fn:(require_string (field ^ ".sha256"))
            |> Result.and_then ~fn:(decode_body field data)))

let optional_body_from_json = fun field json ->
  match json with
  | Json.Null -> Ok None
  | _ ->
      body_from_json field json
      |> Result.map ~fn:(fun body -> Some body)

let header_from_json = fun json ->
  require_field "name" json
  |> Result.and_then ~fn:(require_string "headers[].name")
  |> Result.and_then
    ~fn:(fun name ->
      require_field "value" json
      |> Result.and_then ~fn:(require_string "headers[].value")
      |> Result.map ~fn:(fun value -> (name, value)))

let headers_from_json = fun field json ->
  require_array field json
  |> Result.and_then
    ~fn:(fun values ->
      let rec loop acc values =
        match values with
        | [] -> Ok (List.reverse acc)
        | value :: rest -> (
            match header_from_json value with
            | Ok header -> loop (header :: acc) rest
            | Error error -> Error error
          )
      in
      loop [] values)

let request_from_json = fun json ->
  require_field "method" json
  |> Result.and_then ~fn:(require_string "request.method")
  |> Result.and_then
    ~fn:(fun method_ ->
      require_field "url" json
      |> Result.and_then ~fn:(require_string "request.url")
      |> Result.and_then
        ~fn:(fun url ->
          require_field "headers" json
          |> Result.and_then ~fn:(headers_from_json "request.headers")
          |> Result.and_then
            ~fn:(fun headers ->
              require_field "body" json
              |> Result.and_then ~fn:(optional_body_from_json "request.body")
              |> Result.map
                ~fn:(fun body ->
                  {
                    method_;
                    url;
                    headers;
                    body;
                  }))))

let response_from_json = fun json ->
  require_field "status" json
  |> Result.and_then ~fn:(require_int "response.status")
  |> Result.and_then
    ~fn:(fun status ->
      require_field "headers" json
      |> Result.and_then ~fn:(headers_from_json "response.headers")
      |> Result.and_then
        ~fn:(fun headers ->
          require_field "body" json
          |> Result.and_then ~fn:(body_from_json "response.body")
          |> Result.map ~fn:(fun body -> { status; headers; body })))

let interaction_from_json = fun json ->
  require_field "request" json
  |> Result.and_then ~fn:request_from_json
  |> Result.and_then
    ~fn:(fun request ->
      require_field "response" json
      |> Result.and_then ~fn:response_from_json
      |> Result.map ~fn:(fun response -> { request; response }))

let read_record_mode = fun fallback_mode json ->
  match Json.get_field "record_mode" json with
  | None -> Ok fallback_mode
  | Some value ->
      require_string "record_mode" value
      |> Result.and_then
        ~fn:(fun mode ->
          match Record_mode.from_string mode with
          | Some mode -> Ok mode
          | None -> Error (Error.InvalidMode mode))

let read_name = fun fallback_name json ->
  match Json.get_field "name" json with
  | None -> Ok fallback_name
  | Some value -> require_string "name" value

let interactions_from_json = fun version name record_mode interactions_json ->
  let interactions = Vector.with_capacity ~size:(List.length interactions_json) in
  let rec loop = fun values ->
    match values with
    | [] ->
        Ok {
          version;
          name;
          record_mode;
          interactions;
        }
    | value :: rest -> (
        match interaction_from_json value with
        | Ok interaction ->
            Vector.push interactions ~value:interaction;
            loop rest
        | Error error -> Error error
      )
  in
  loop interactions_json

let from_json = fun ~fallback_name ~fallback_mode json ->
  require_field "version" json
  |> Result.and_then ~fn:(require_int "version")
  |> Result.and_then
    ~fn:(fun version ->
      read_name fallback_name json
      |> Result.and_then
        ~fn:(fun name ->
          read_record_mode fallback_mode json
          |> Result.and_then
            ~fn:(fun record_mode ->
              require_field "interactions" json
              |> Result.and_then ~fn:(require_array "interactions")
              |> Result.and_then ~fn:(interactions_from_json version name record_mode))))

let make = fun ~name ~record_mode () ->
  {
    version = 1;
    name;
    record_mode;
    interactions = Vector.create ();
  }

let push = fun recording ~value -> Vector.push recording.interactions ~value
