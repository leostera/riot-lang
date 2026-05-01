(** WebSocket Frame Serializer *)
open Std
open Std.IO

type role =
  | Server
  | Client

type error =
  | MaskGenerationFailed of Random.error
  | ClientFrameNotMasked
  | ServerFrameMasked
  | ReservedBitsSet
  | FragmentedControlFrame of {
      opcode: Frame.opcode;
    }
  | ControlFramePayloadTooLarge of {
      opcode: Frame.opcode;
      payload_length: int;
    }
  | InvalidClosePayload of Frame.close_payload_error
  | InvalidTextPayloadUtf8 of { payload_length: int }

let error_to_string = function
  | MaskGenerationFailed error ->
      "Failed to generate WebSocket mask: " ^ Random.error_to_string error
  | ClientFrameNotMasked -> "WebSocket client frames must be masked"
  | ServerFrameMasked -> "WebSocket server frames must not be masked"
  | ReservedBitsSet -> "WebSocket RSV bits must be zero unless an extension negotiated them"
  | FragmentedControlFrame { opcode } ->
      "WebSocket control frame 0x"
      ^ Int.to_string (Frame.opcode_to_int opcode)
      ^ " must not be fragmented"
  | ControlFramePayloadTooLarge { opcode; payload_length } ->
      "WebSocket control frame 0x"
      ^ Int.to_string (Frame.opcode_to_int opcode)
      ^ " payload must be at most 125 bytes, got "
      ^ Int.to_string payload_length
  | InvalidClosePayload error -> Frame.close_payload_error_to_string error
  | InvalidTextPayloadUtf8 { payload_length } ->
      "WebSocket text frame payload is not valid UTF-8, length " ^ Int.to_string payload_length

let is_control_opcode = function
  | Frame.Close
  | Frame.Ping
  | Frame.Pong -> true
  | Frame.Continuation
  | Frame.Text
  | Frame.Binary -> false

let validate_masking = fun ~role ~masked ->
  match (role, masked) with
  | (Client, false) -> Error ClientFrameNotMasked
  | (Server, true) -> Error ServerFrameMasked
  | (Client, true)
  | (Server, false) -> Ok ()

let validate_control_frame = fun frame ->
  let payload_length = String.length frame.Frame.payload in
  if not frame.Frame.fin then
    Error (FragmentedControlFrame { opcode = frame.opcode })
  else if payload_length > 125 then
    Error (ControlFramePayloadTooLarge { opcode = frame.opcode; payload_length })
  else
    match frame.Frame.opcode with
    | Frame.Close -> (
        match Frame.validate_close_payload frame.payload with
        | Ok () -> Ok ()
        | Error error -> Error (InvalidClosePayload error)
      )
    | Frame.Ping
    | Frame.Pong -> Ok ()
    | Frame.Continuation
    | Frame.Text
    | Frame.Binary -> Ok ()

let validate_data_frame = fun frame ->
  match frame.Frame.opcode with
  | Frame.Text ->
      if frame.fin && not (Unicode.Utf8.is_valid frame.payload) then
        Error (InvalidTextPayloadUtf8 { payload_length = String.length frame.payload })
      else
        Ok ()
  | Frame.Continuation
  | Frame.Binary
  | Frame.Close
  | Frame.Ping
  | Frame.Pong -> Ok ()

let validate_frame = fun ~role frame ->
  match validate_masking ~role ~masked:frame.Frame.masked with
  | Error error -> Error error
  | Ok () ->
      if frame.Frame.rsv1 || frame.rsv2 || frame.rsv3 then
        Error ReservedBitsSet
      else if is_control_opcode frame.opcode then
        validate_control_frame frame
      else
        validate_data_frame frame

(* Serialize a WebSocket frame to bytes *)

let serialize = fun ?rng ?(role = Server) frame ->
  match validate_frame ~role frame with
  | Error error -> Error error
  | Ok () ->
      let Frame.{
        fin;
        rsv1;
        rsv2;
        rsv3;
        opcode;
        masked;
        payload;
      } = frame
      in
      let payload_len = String.length payload in
      (* First byte: FIN, RSV, opcode *)
      let byte0 =
        (
          if fin then
            0b1000_0000
          else
            0
        ) lor (
          if rsv1 then
            0b0100_0000
          else
            0
        ) lor (
          if rsv2 then
            0b0010_0000
          else
            0
        ) lor (
          if rsv3 then
            0b0001_0000
          else
            0
        ) lor Frame.opcode_to_int opcode
      in
      (* Determine payload length encoding *)
      let (length_bytes, extended_length) =
        if payload_len < 126 then
          ([ payload_len ], [])
        else if payload_len < 65_536 then
          let high = (payload_len lsr 8) land 0b1111_1111 in
          let low = payload_len land 0b1111_1111 in
          ([ 126 ], [ high; low ])
        else
          (* 64-bit length *)
          let b0 = (payload_len lsr 24) land 0b1111_1111 in
          let b1 = (payload_len lsr 16) land 0b1111_1111 in
          let b2 = (payload_len lsr 8) land 0b1111_1111 in
          let b3 = payload_len land 0b1111_1111 in
          ([ 127 ], [ 0; 0; 0; 0; b0; b1; b2; b3; ])
      in
      (* Second byte: MASK, payload length *)
      let byte1 =
        (
          if masked then
            0b1000_0000
          else
            0
        ) lor List.get_unchecked length_bytes ~at:0
      in
      (* Build header *)
      let header = Buffer.create ~size:14 in
      Buffer.add_char header (Char.from_int_unchecked byte0);
      Buffer.add_char header (Char.from_int_unchecked byte1);
      List.for_each
        extended_length
        ~fn:(fun byte -> Buffer.add_char header (Char.from_int_unchecked byte));
      (* Add mask and masked payload if needed *)
      if masked then (
        match Frame.generate_mask ?rng () with
        | Error error -> Error (MaskGenerationFailed error)
        | Ok mask ->
            (* Write mask (4 bytes) *)
            Buffer.add_char
              header
              (Char.from_int_unchecked Int32.(to_int (logand (shift_right mask 24) 0b1111_1111l)));
            Buffer.add_char
              header
              (Char.from_int_unchecked Int32.(to_int (logand (shift_right mask 16) 0b1111_1111l)));
            Buffer.add_char
              header
              (Char.from_int_unchecked Int32.(to_int (logand (shift_right mask 8) 0b1111_1111l)));
            Buffer.add_char
              header
              (Char.from_int_unchecked Int32.(to_int (logand mask 0b1111_1111l)));
            (* Apply mask to payload *)
            let masked_payload = Frame.apply_mask mask payload in
            Buffer.add_string header masked_payload;
            Ok (Buffer.contents header)
      ) else (
        Buffer.add_string header payload;
        Ok (Buffer.contents header)
      )
