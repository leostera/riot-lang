open Std

(** Messages sent between client and server over WebSocket *)

type client_msg =
  | Mount
  | Event of { handler_id: string; event_data: string }

type server_msg =
  | Patch of string  (* Full HTML replacement *)
  | Error of string

(** Serialize server message to JSON *)
let serialize_server_msg msg =
  match msg with
  | Patch html ->
      let json = Data.Json.obj [("Patch", Data.Json.string html)] in
      Data.Json.to_string json
  | Error msg ->
      let json = Data.Json.obj [("Error", Data.Json.string msg)] in
      Data.Json.to_string json

(** Deserialize client message from JSON *)
let deserialize_client_msg json_str =
  try
    (* Check for "Mount" *)
    if json_str = {|"Mount"|} then Ok Mount
    else
      (* Parse as JSON *)
      match Data.Json.of_string json_str with
      | Error _ -> Error "Invalid JSON"
      | Ok json ->
          (* Check for Event *)
          match Data.Json.get_field "Event" json with
          | Some (Data.Json.Array [
              Data.Json.String handler_id;
              Data.Json.String event_data
            ]) ->
              Ok (Event { handler_id; event_data })
          | _ -> Error "Unknown message format"
  with exn ->
    Error "Parse error: invalid JSON format"
