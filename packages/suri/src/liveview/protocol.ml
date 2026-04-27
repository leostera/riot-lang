open Std

(** Messages sent between client and server over WebSocket *)
type client_msg =
  | Mount
  | Event of { handler_id: string; event_data: string }

type client_msg_error =
  | InvalidJson of Data.Json.error
  | UnknownMessageFormat of Data.Json.t
  | UnexpectedDecodeException of exn

type server_error =
  | ClientMessageDecodeFailed of client_msg_error
  | InternalServerError

type server_msg =
  | Patch of string
  (* Full HTML replacement *)
  | Error of server_error

let client_msg_error_to_string = function
  | InvalidJson error -> "invalid JSON: " ^ Data.Json.error_to_string error
  | UnknownMessageFormat json -> "unknown message format: " ^ Data.Json.to_string json
  | UnexpectedDecodeException exn ->
      "unexpected LiveView protocol decode exception: " ^ Exception.to_string exn

let server_error_to_string = function
  | ClientMessageDecodeFailed error -> client_msg_error_to_string error
  | InternalServerError -> "internal LiveView server error"

(** Serialize server message to JSON *)
let serialize_server_msg = fun msg ->
  match msg with
  | Patch html ->
      let json = Data.Json.obj [ ("Patch", Data.Json.string html); ] in
      Data.Json.to_string json
  | Error error ->
      let json = Data.Json.obj [ ("Error", Data.Json.string (server_error_to_string error)); ] in
      Data.Json.to_string json

(** Deserialize client message from JSON *)
let deserialize_client_msg = fun json_str ->
  try
    if json_str = {|"Mount"|} then
      Ok Mount
    else
      (* Parse as JSON *)
      match Data.Json.of_string json_str with
      | Error error -> Error (InvalidJson error)
      | Ok json ->
          (* Check for Event *)
          match Data.Json.get_field "Event" json with
          | Some (Data.Json.Array [ Data.Json.String handler_id; Data.Json.String event_data ]) ->
              Ok (Event { handler_id; event_data })
          | _ -> Error (UnknownMessageFormat json)
  with
  | exn -> Error (UnexpectedDecodeException exn)
