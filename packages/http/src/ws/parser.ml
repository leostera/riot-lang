(**
   WebSocket Frame Parser

   Incremental parser for WebSocket frames following RFC 6455
*)
open Std

let ( let* ) = Result.and_then

type role =
  | Server
  | Client

type 'a parse_result =
  | Done of { value: 'a; remaining: string }
  | Need_more
  | Error of error

and error =
  | InvalidOpcode of int
  | ReservedBitsSet
  | ClientFrameNotMasked
  | ServerFrameMasked
  | FragmentedControlFrame
  | ControlFramePayloadTooLarge of { payload_length: int }

let error_to_string = function
  | InvalidOpcode opcode -> "Invalid WebSocket opcode: 0x" ^ Int.to_string opcode
  | ReservedBitsSet -> "WebSocket RSV bits must be zero unless an extension negotiated them"
  | ClientFrameNotMasked -> "WebSocket client frames must be masked"
  | ServerFrameMasked -> "WebSocket server frames must not be masked"
  | FragmentedControlFrame -> "WebSocket control frames must not be fragmented"
  | ControlFramePayloadTooLarge { payload_length } ->
      "WebSocket control frame payload must be at most 125 bytes, got "
      ^ Int.to_string payload_length

let byte_at = fun input at ->
  input
  |> String.get_unchecked ~at
  |> Char.to_int

let is_control_opcode = function
  | Frame.Close
  | Frame.Ping
  | Frame.Pong -> true
  | Frame.Continuation
  | Frame.Text
  | Frame.Binary -> false

let validate_masking = fun ~role ~masked ->
  match (role, masked) with
  | (Server, false) -> Result.Error ClientFrameNotMasked
  | (Client, true) -> Result.Error ServerFrameMasked
  | (Server, true)
  | (Client, false) -> Result.Ok ()

let validate_frame_header = fun ~fin ~rsv1 ~rsv2 ~rsv3 ~opcode ~payload_len_initial ->
  if rsv1 || rsv2 || rsv3 then
    Result.Error ReservedBitsSet
  else if is_control_opcode opcode && not fin then
    Result.Error FragmentedControlFrame
  else if is_control_opcode opcode && payload_len_initial > 125 then
    Result.Error (ControlFramePayloadTooLarge { payload_length = payload_len_initial })
  else
    Result.Ok ()

(* Parse a single WebSocket frame *)

let parse = fun ~role input ->
  let len = String.length input in
  (* Need at least 2 bytes for header *)
  if len < 2 then
    Need_more
  else
    let byte0 = byte_at input 0 in
    let byte1 = byte_at input 1 in
    (* Parse first byte *)
    let fin = byte0 land 0x80 != 0 in
    let rsv1 = byte0 land 0x40 != 0 in
    let rsv2 = byte0 land 0x20 != 0 in
    let rsv3 = byte0 land 0x10 != 0 in
    let opcode_int = byte0 land 0x0f in
    (* Parse second byte *)
    let masked = byte1 land 0x80 != 0 in
    let payload_len_initial = byte1 land 0x7f in
    (* Validate opcode *)
    match Frame.opcode_of_int opcode_int with
    | None -> Error (InvalidOpcode opcode_int)
    | Some opcode -> (
        match validate_masking ~role ~masked
        |> Result.and_then
          ~fn:(fun () ->
            validate_frame_header ~fin ~rsv1 ~rsv2 ~rsv3 ~opcode ~payload_len_initial) with
        | Error err -> Error err
        | Ok () ->
            (* Determine actual payload length and header size *)
            let (header_size, payload_length) =
              if payload_len_initial < 126 then
                (2, payload_len_initial)
              else if payload_len_initial = 126 then
                if len < 4 then
                  (0, 0)
                  (* Signal need more *)
                else
                  let len_high = byte_at input 2 in
                  let len_low = byte_at input 3 in
                  (4, (len_high lsl 8) lor len_low)
              else if len < 10 then
                (0, 0)
              (* Signal need more *)
              else
                (* Read 8 bytes, but only use lower 4 bytes for simplicity *)
                let len_bytes = [
                  byte_at input 6;
                  byte_at input 7;
                  byte_at input 8;
                  byte_at input 9;
                ]
                in
                let payload_len =
                  List.fold_left len_bytes ~init:0 ~fn:(fun acc b -> (acc lsl 8) lor b)
                in
                (10, payload_len)
            in
            if header_size = 0 then
              Need_more
            else
              (* Add mask size if masked *)
              let mask_size =
                if masked then
                  4
                else
                  0
              in
              let total_header_size = header_size + mask_size in
              (* Check if we have full frame *)
              if len < total_header_size + payload_length then
                Need_more
              else
                (* Extract mask if present *)
                let mask =
                  if masked then
                    let m0 = Int32.from_int (byte_at input header_size) in
                    let m1 = Int32.from_int (byte_at input (header_size + 1)) in
                    let m2 = Int32.from_int (byte_at input (header_size + 2)) in
                    let m3 = Int32.from_int (byte_at input (header_size + 3)) in
                    Int32.(logor
                      (shift_left m0 24)
                      (logor (shift_left m1 16) (logor (shift_left m2 8) m3)))
                  else
                    Int32.zero
                in
                (* Extract payload *)
                let payload_start = total_header_size in
                let raw_payload = String.sub input ~offset:payload_start ~len:payload_length in
                (* Unmask if needed *)
                let payload =
                  if masked then
                    Frame.unmask mask raw_payload
                  else
                    raw_payload
                in
                (* Create frame *)
                let frame =
                  Frame.{
                    fin;
                    rsv1;
                    rsv2;
                    rsv3;
                    opcode;
                    masked;
                    payload;
                  }
                in
                (* Return frame and remaining data *)
                let remaining =
                  String.sub
                    input
                    ~offset:(total_header_size + payload_length)
                    ~len:(len - total_header_size - payload_length)
                in
                Done { value = frame; remaining }
      )
