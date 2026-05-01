(**
   WebSocket Frame Parser

   Incremental parser for WebSocket frames following RFC 6455
*)
open Std

let ( let* ) = fun result fn -> Result.and_then result ~fn

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
  | PayloadLengthHighBitSet of { first_byte: int }
  | PayloadLengthTooLarge of { most_significant_byte: int; max_payload_length: int }
  | PayloadLengthExceedsLimit of { payload_length: int; max_payload_length: int }
  | InvalidPayloadLengthLimit of { max_payload_length: int }
  | NonMinimalPayloadLength of {
      encoding: payload_length_encoding;
      payload_length: int;
    }
  | InvalidTextPayloadUtf8 of { payload_length: int }
  | ClosePayloadTooShort of { payload_length: int }
  | InvalidCloseCode of { code: int }
  | InvalidCloseReasonUtf8 of { reason_length: int }

and payload_length_encoding =
  | PayloadLength16
  | PayloadLength64

type payload_length_result =
  | PayloadLength of { header_size: int; payload_length: int }
  | PayloadLengthNeedMore
  | PayloadLengthError of error

let payload_length_encoding_to_string = fun __tmp1 ->
  match __tmp1 with
  | PayloadLength16 -> "16-bit extended"
  | PayloadLength64 -> "64-bit extended"

let error_to_string = fun __tmp1 ->
  match __tmp1 with
  | InvalidOpcode opcode -> "Invalid WebSocket opcode: 0x" ^ Int.to_string opcode
  | ReservedBitsSet -> "WebSocket RSV bits must be zero unless an extension negotiated them"
  | ClientFrameNotMasked -> "WebSocket client frames must be masked"
  | ServerFrameMasked -> "WebSocket server frames must not be masked"
  | FragmentedControlFrame -> "WebSocket control frames must not be fragmented"
  | ControlFramePayloadTooLarge { payload_length } ->
      "WebSocket control frame payload must be at most 125 bytes, got "
      ^ Int.to_string payload_length
  | PayloadLengthHighBitSet { first_byte } ->
      "WebSocket 64-bit payload length has the reserved high bit set in byte "
      ^ Int.to_string first_byte
  | PayloadLengthTooLarge { most_significant_byte; max_payload_length } ->
      "WebSocket payload length starting with byte "
      ^ Int.to_string most_significant_byte
      ^ " exceeds the parser limit "
      ^ Int.to_string max_payload_length
  | PayloadLengthExceedsLimit { payload_length; max_payload_length } ->
      "WebSocket payload length "
      ^ Int.to_string payload_length
      ^ " exceeds configured limit "
      ^ Int.to_string max_payload_length
  | InvalidPayloadLengthLimit { max_payload_length } ->
      "Invalid WebSocket payload length limit: " ^ Int.to_string max_payload_length
  | NonMinimalPayloadLength { encoding; payload_length } ->
      "WebSocket "
      ^ payload_length_encoding_to_string encoding
      ^ " payload length encoding was used for length "
      ^ Int.to_string payload_length
      ^ ", but the minimal length encoding is required"
  | InvalidTextPayloadUtf8 { payload_length } ->
      "WebSocket text frame payload is not valid UTF-8, length " ^ Int.to_string payload_length
  | ClosePayloadTooShort { payload_length } ->
      "WebSocket close frame payload must be empty or at least 2 bytes, got "
      ^ Int.to_string payload_length
  | InvalidCloseCode { code } -> "Invalid WebSocket close code: " ^ Int.to_string code
  | InvalidCloseReasonUtf8 { reason_length } ->
      "WebSocket close reason is not valid UTF-8, length " ^ Int.to_string reason_length

let byte_at = fun input at ->
  input
  |> String.get_unchecked ~at
  |> Char.to_int

let is_control_opcode = fun __tmp1 ->
  match __tmp1 with
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

let parse_uint16_payload_length = fun input ->
  let len_high = byte_at input 2 in
  let len_low = byte_at input 3 in
  let payload_length = (len_high lsl 8) lor len_low in
  if payload_length < 126 then
    PayloadLengthError (NonMinimalPayloadLength { encoding = PayloadLength16; payload_length })
  else
    PayloadLength { header_size = 4; payload_length }

let parse_uint64_payload_length = fun input ->
  let first_byte = byte_at input 2 in
  if first_byte land 0b1000_0000 != 0 then
    PayloadLengthError (PayloadLengthHighBitSet { first_byte })
  else
    let rec loop at acc =
      if at = 10 then
        if acc <= 65_535 then
          PayloadLengthError (NonMinimalPayloadLength {
            encoding = PayloadLength64;
            payload_length = acc;
          })
        else
          PayloadLength { header_size = 10; payload_length = acc }
      else
        let byte = byte_at input at in
        if acc > (Int.max_int - byte) / 256 then
          PayloadLengthError (PayloadLengthTooLarge {
            most_significant_byte = first_byte;
            max_payload_length = Int.max_int;
          })
        else
          loop (at + 1) ((acc lsl 8) lor byte)
    in
    loop 2 0

let parse_payload_length = fun input len payload_len_initial ->
  if payload_len_initial < 126 then
    PayloadLength { header_size = 2; payload_length = payload_len_initial }
  else if payload_len_initial = 126 then
    if len < 4 then
      PayloadLengthNeedMore
    else
      parse_uint16_payload_length input
  else if len < 10 then
    PayloadLengthNeedMore
  else
    parse_uint64_payload_length input

let validate_payload_length_limit = fun ~payload_length ~max_payload_length ->
  if max_payload_length < 0 then
    Result.Error (InvalidPayloadLengthLimit { max_payload_length })
  else if payload_length > max_payload_length then
    Result.Error (PayloadLengthExceedsLimit { payload_length; max_payload_length })
  else
    Result.Ok ()

let validate_close_payload = fun opcode payload ->
  if opcode != Frame.Close then
    Result.Ok ()
  else
    match Frame.validate_close_payload payload with
    | Ok () -> Result.Ok ()
    | Error (Frame.ClosePayloadTooShort { payload_length }) ->
        Result.Error (ClosePayloadTooShort { payload_length })
    | Error (Frame.InvalidCloseCode { code }) -> Result.Error (InvalidCloseCode { code })
    | Error (Frame.InvalidCloseReasonUtf8 { reason_length }) ->
        Result.Error (InvalidCloseReasonUtf8 { reason_length })

let validate_text_payload = fun ~fin ~opcode payload ->
  if opcode = Frame.Text && fin && not (Unicode.Utf8.is_valid payload) then
    Result.Error (InvalidTextPayloadUtf8 { payload_length = String.length payload })
  else
    Result.Ok ()

let validate_payload = fun ~fin ~opcode payload ->
  let* () = validate_close_payload opcode payload in
  validate_text_payload ~fin ~opcode payload

(* Parse a single WebSocket frame *)

let parse = fun ?(max_payload_length = Int.max_int) ~role input ->
  let len = String.length input in
  (* Need at least 2 bytes for header *)
  if len < 2 then
    Need_more
  else
    let byte0 = byte_at input 0 in
    let byte1 = byte_at input 1 in
    (* Parse first byte *)
    let fin = byte0 land 0b1000_0000 != 0 in
    let rsv1 = byte0 land 0b0100_0000 != 0 in
    let rsv2 = byte0 land 0b0010_0000 != 0 in
    let rsv3 = byte0 land 0b0001_0000 != 0 in
    let opcode_int = byte0 land 0b0000_1111 in
    (* Parse second byte *)
    let masked = byte1 land 0b1000_0000 != 0 in
    let payload_len_initial = byte1 land 0b0111_1111 in
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
            match parse_payload_length input len payload_len_initial with
            | PayloadLengthNeedMore -> Need_more
            | PayloadLengthError err -> Error err
            | PayloadLength { header_size; payload_length } -> (
                match validate_payload_length_limit ~payload_length ~max_payload_length with
                | Error err -> Error err
                | Ok () ->
                    (* Add mask size if masked *)
                    let mask_size =
                      if masked then
                        4
                      else
                        0
                    in
                    let total_header_size = header_size + mask_size in
                    (* Check if we have full frame *)
                    if payload_length > Int.max_int - total_header_size then
                      Error (
                        PayloadLengthTooLarge {
                          most_significant_byte =
                            if payload_len_initial = 127 then
                              byte_at input 2
                            else
                              0;
                          max_payload_length = Int.max_int;
                        }
                      )
                    else if len < total_header_size + payload_length then
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
                      let raw_payload =
                        String.sub input ~offset:payload_start ~len:payload_length
                      in
                      (* Unmask if needed *)
                      let payload =
                        if masked then
                          Frame.unmask mask raw_payload
                        else
                          raw_payload
                      in
                      match validate_payload ~fin ~opcode payload with
                      | Result.Error err -> Error err
                      | Result.Ok () ->
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
      )
