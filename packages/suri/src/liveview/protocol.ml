open Std

(** Messages sent between client and server over WebSocket *)
type client_msg =
  | Mount
  | Event of { handler_id: string; event_data: string }

type client_msg_error =
  | InvalidJson of Data.Json.error
  | InvalidEventPayload of Data.Json.t
  | UnknownMessageFormat of Data.Json.t

type server_error =
  | ClientMessageDecodeFailed of client_msg_error
  | InternalServerError

type server_msg =
  | Patch of string
  (* Full HTML replacement *)
  | Error of server_error

let char_to_json = fun __tmp1 ->
  match __tmp1 with
  | Some char -> Data.Json.string (String.make ~len:1 ~char)
  | None -> Data.Json.null

let json_error_to_json = fun __tmp1 ->
  match __tmp1 with
  | Data.Json.Unterminated_string { position } ->
      Data.Json.obj
        [ ("type", Data.Json.string "UnterminatedString"); ("position", Data.Json.int position); ]
  | Invalid_literal { expected; position; found } ->
      Data.Json.obj
        [
          ("type", Data.Json.string "InvalidLiteral");
          ("expected", Data.Json.string expected);
          ("position", Data.Json.int position);
          ("found", Data.Json.string found);
        ]
  | Invalid_number { position; text } ->
      Data.Json.obj
        [
          ("type", Data.Json.string "InvalidNumber");
          ("position", Data.Json.int position);
          ("text", Data.Json.string text);
        ]
  | Expected_comma_or_bracket { kind; position; found } ->
      Data.Json.obj
        [
          ("type", Data.Json.string "ExpectedCommaOrBracket");
          ("kind", Data.Json.string kind);
          ("position", Data.Json.int position);
          ("found", char_to_json found);
        ]
  | Expected_string_key { position; found } ->
      Data.Json.obj
        [
          ("type", Data.Json.string "ExpectedStringKey");
          ("position", Data.Json.int position);
          ("found", char_to_json found);
        ]
  | Expected_colon { position; found } ->
      Data.Json.obj
        [
          ("type", Data.Json.string "ExpectedColon");
          ("position", Data.Json.int position);
          ("found", char_to_json found);
        ]
  | Unexpected_end_of_input { expected } ->
      Data.Json.obj
        [
          ("type", Data.Json.string "UnexpectedEndOfInput");
          ("expected", Data.Json.string expected);
        ]
  | Unexpected_character { position; character; expected } ->
      Data.Json.obj
        [
          ("type", Data.Json.string "UnexpectedCharacter");
          ("position", Data.Json.int position);
          ("character", Data.Json.string (String.make ~len:1 ~char:character));
          ("expected", Data.Json.string expected);
        ]
  | Extra_input_after_value { position } ->
      Data.Json.obj
        [ ("type", Data.Json.string "ExtraInputAfterValue"); ("position", Data.Json.int position); ]
  | Unknown_error message ->
      Data.Json.obj
        [ ("type", Data.Json.string "UnknownError"); ("message", Data.Json.string message); ]

let client_msg_error_to_json = fun __tmp1 ->
  match __tmp1 with
  | InvalidJson error ->
      Data.Json.obj
        [ ("type", Data.Json.string "InvalidJson"); ("error", json_error_to_json error); ]
  | InvalidEventPayload json ->
      Data.Json.obj [ ("type", Data.Json.string "InvalidEventPayload"); ("message", json); ]
  | UnknownMessageFormat json ->
      Data.Json.obj [ ("type", Data.Json.string "UnknownMessageFormat"); ("message", json); ]

let server_error_to_json = fun __tmp1 ->
  match __tmp1 with
  | ClientMessageDecodeFailed error ->
      Data.Json.obj
        [
          ("type", Data.Json.string "ClientMessageDecodeFailed");
          ("error", client_msg_error_to_json error);
        ]
  | InternalServerError -> Data.Json.obj [ ("type", Data.Json.string "InternalServerError"); ]

let client_msg_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | InvalidJson error -> "invalid JSON: " ^ Data.Json.error_to_string error
  | InvalidEventPayload json -> "invalid Event payload: " ^ Data.Json.to_string json
  | UnknownMessageFormat json -> "unknown message format: " ^ Data.Json.to_string json

let server_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | ClientMessageDecodeFailed error -> client_msg_error_to_string error
  | InternalServerError -> "internal LiveView server error"

(** Serialize server message to JSON *)
let serialize_server_msg = fun msg ->
  match msg with
  | Patch html ->
      let json = Data.Json.obj [ ("Patch", Data.Json.string html); ] in
      Data.Json.to_string json
  | Error error ->
      let json = Data.Json.obj [ ("Error", server_error_to_json error); ] in
      Data.Json.to_string json

(** Deserialize client message from JSON *)
let deserialize_client_msg = fun json_str ->
  if json_str = {|"Mount"|} then
    Ok Mount
  else
    (* Parse as JSON *)
    match Data.Json.from_string json_str with
    | Error error -> Error (InvalidJson error)
    | Ok json ->
        (* Check for Event *)
        match Data.Json.get_field "Event" json with
        | Some (Data.Json.Array [ Data.Json.String handler_id; Data.Json.String event_data ]) ->
            Ok (Event { handler_id; event_data })
        | Some value -> Error (InvalidEventPayload value)
        | _ -> Error (UnknownMessageFormat json)
