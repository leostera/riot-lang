(** Stateful WebSocket message assembly. *)
open Std

type data_opcode =
  | Text
  | Binary

type control_opcode =
  | Close
  | Ping
  | Pong

type fragment = {
  opcode: data_opcode;
  chunks_rev: string list;
  payload_length: int;
}

type t = {
  max_message_size: int;
  fragment: fragment option;
}

type event =
  | DataMessage of {
      opcode: data_opcode;
      payload: string;
    }
  | ControlFrame of Frame.t

type error =
  | InvalidMessageSizeLimit of { max_message_size: int }
  | ContinuationWithoutFragment
  | DataFrameWhileFragmented of {
      opcode: data_opcode;
    }
  | FragmentedControlFrame of {
      opcode: control_opcode;
    }
  | ControlFramePayloadTooLarge of {
      opcode: control_opcode;
      payload_length: int;
    }
  | InvalidClosePayload of Frame.close_payload_error
  | MessagePayloadTooLarge of { payload_length: int; max_message_size: int }
  | InvalidTextMessageUtf8 of { payload_length: int }

let data_opcode_to_string = function
  | Text -> "text"
  | Binary -> "binary"

let control_opcode_to_string = function
  | Close -> "close"
  | Ping -> "ping"
  | Pong -> "pong"

let error_to_string = function
  | InvalidMessageSizeLimit { max_message_size } ->
      "Invalid WebSocket message size limit: " ^ Int.to_string max_message_size
  | ContinuationWithoutFragment -> "WebSocket continuation frame arrived without a fragmented message"
  | DataFrameWhileFragmented { opcode } ->
      "WebSocket "
      ^ data_opcode_to_string opcode
      ^ " frame arrived before the current fragmented message completed"
  | FragmentedControlFrame { opcode } ->
      "WebSocket " ^ control_opcode_to_string opcode ^ " control frames must not be fragmented"
  | ControlFramePayloadTooLarge { opcode; payload_length } ->
      "WebSocket "
      ^ control_opcode_to_string opcode
      ^ " control frame payload must be at most 125 bytes, got "
      ^ Int.to_string payload_length
  | InvalidClosePayload error -> Frame.close_payload_error_to_string error
  | MessagePayloadTooLarge { payload_length; max_message_size } ->
      "WebSocket message payload length "
      ^ Int.to_string payload_length
      ^ " exceeds configured limit "
      ^ Int.to_string max_message_size
  | InvalidTextMessageUtf8 { payload_length } ->
      "WebSocket text message is not valid UTF-8, length " ^ Int.to_string payload_length

let create = fun ?(max_message_size = Int.max_int) () ->
  if max_message_size < 0 then
    Error (InvalidMessageSizeLimit { max_message_size })
  else
    Ok { max_message_size; fragment = None }

let frame_data_opcode = function
  | Frame.Text -> Some Text
  | Frame.Binary -> Some Binary
  | Frame.Continuation
  | Frame.Close
  | Frame.Ping
  | Frame.Pong -> None

let frame_control_opcode = function
  | Frame.Close -> Some Close
  | Frame.Ping -> Some Ping
  | Frame.Pong -> Some Pong
  | Frame.Continuation
  | Frame.Text
  | Frame.Binary -> None

let validate_payload_size = fun state payload_length ->
  if payload_length > state.max_message_size then
    Error (MessagePayloadTooLarge { payload_length; max_message_size = state.max_message_size })
  else
    Ok ()

let validate_text_payload = fun opcode payload ->
  match opcode with
  | Binary -> Ok ()
  | Text ->
      if Unicode.Utf8.is_valid payload then
        Ok ()
      else
        Error (InvalidTextMessageUtf8 { payload_length = String.length payload })

let emit_data = fun state opcode payload ->
  let payload_length = String.length payload in
  match validate_payload_size state payload_length with
  | Error error -> Error error
  | Ok () -> (
      match validate_text_payload opcode payload with
      | Error error -> Error error
      | Ok () -> Ok (state, Some (DataMessage { opcode; payload }))
    )

let start_fragment = fun state opcode payload ->
  let payload_length = String.length payload in
  match validate_payload_size state payload_length with
  | Error error -> Error error
  | Ok () ->
      Ok (
        { state with fragment = Some { opcode; chunks_rev = [ payload ]; payload_length } },
        None
      )

let finish_fragment = fun state fragment payload ->
  let payload_length = fragment.payload_length + String.length payload in
  match validate_payload_size state payload_length with
  | Error error -> Error error
  | Ok () ->
      let chunks = List.reverse (payload :: fragment.chunks_rev) in
      let message = String.concat "" chunks in
      match validate_text_payload fragment.opcode message with
      | Error error -> Error error
      | Ok () ->
          Ok (
            { state with fragment = None },
            Some (DataMessage { opcode = fragment.opcode; payload = message })
          )

let continue_fragment = fun state fragment payload ->
  let payload_length = fragment.payload_length + String.length payload in
  match validate_payload_size state payload_length with
  | Error error -> Error error
  | Ok () ->
      Ok (
        {
          state with
          fragment = Some {
            fragment with
            chunks_rev = payload :: fragment.chunks_rev;
            payload_length;
          };
        },
        None
      )

let handle_data_frame = fun state frame opcode ->
  match state.fragment with
  | Some _ -> Error (DataFrameWhileFragmented { opcode })
  | None ->
      if frame.Frame.fin then
        emit_data state opcode frame.payload
      else
        start_fragment state opcode frame.payload

let handle_continuation_frame = fun state frame ->
  match state.fragment with
  | None -> Error ContinuationWithoutFragment
  | Some fragment ->
      if frame.Frame.fin then
        finish_fragment state fragment frame.payload
      else
        continue_fragment state fragment frame.payload

let handle_control_frame = fun state frame opcode ->
  let payload_length = String.length frame.Frame.payload in
  if not frame.fin then
    Error (FragmentedControlFrame { opcode })
  else if payload_length > 125 then
    Error (ControlFramePayloadTooLarge { opcode; payload_length })
  else
    match opcode with
    | Close -> (
        match Frame.validate_close_payload frame.Frame.payload with
        | Ok () -> Ok (state, Some (ControlFrame frame))
        | Error error -> Error (InvalidClosePayload error)
      )
    | Ping
    | Pong -> Ok (state, Some (ControlFrame frame))

let handle_frame = fun state frame ->
  match frame_data_opcode frame.Frame.opcode with
  | Some opcode -> handle_data_frame state frame opcode
  | None -> (
      match frame.Frame.opcode with
      | Frame.Continuation -> handle_continuation_frame state frame
      | Frame.Close
      | Frame.Ping
      | Frame.Pong -> (
          match frame_control_opcode frame.opcode with
          | Some opcode -> handle_control_frame state frame opcode
          | None -> Ok (state, None)
        )
      | Frame.Text
      | Frame.Binary -> Ok (state, None)
    )
