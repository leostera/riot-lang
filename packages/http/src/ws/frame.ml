(**
   WebSocket Frame Implementation

   Based on RFC 6455: https://datatracker.ietf.org/doc/html/rfc6455#section-5.2

   Frame format:

   0 1 2 3 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   +-+-+-+-+-------+-+-------------+-------------------------------+ |F|R|R|R|
   opcode|M| Payload len | Extended payload length | |I|S|S|S| (4) |A| (7) |
   (16/64) | |N|V|V|V| |S| | (if payload len==126/127) | | |1|2|3| |K| | |
   +-+-+-+-+-------+-+-------------+ - - - - - - - - - - - - - - - +
*)
open Std

type opcode =
  | Continuation
  (* 0x0 *)
  | Text
  (* 0x1 *)
  | Binary
  (* 0x2 *)
  | Close
  (* 0x8 *)
  | Ping
  (* 0x9 *)
  | Pong

(* 0xA *)

type t = {
  fin: bool;
  rsv1: bool;
  rsv2: bool;
  rsv3: bool;
  opcode: opcode;
  masked: bool;
  payload: string;
}

type close_payload_error =
  | ClosePayloadTooShort of { payload_length: int }
  | InvalidCloseCode of { code: int }
  | InvalidCloseReasonUtf8 of { reason_length: int }

let opcode_to_int = function
  | Continuation -> 0x0
  | Text -> 0x1
  | Binary -> 0x2
  | Close -> 0x8
  | Ping -> 0x9
  | Pong -> 0xa

let opcode_of_int = function
  | 0x0 -> Some Continuation
  | 0x1 -> Some Text
  | 0x2 -> Some Binary
  | 0x8 -> Some Close
  | 0x9 -> Some Ping
  | 0xa -> Some Pong
  | _ -> None

(* XOR unmask the payload *)

let unmask = fun mask payload ->
  let len = String.length payload in
  let result = IO.Bytes.create ~size:len in
  for i = 0 to len - 1 do
    let shift = 8 * (3 - (i mod 4)) in
    let mask_byte =
      Int32.(logand (shift_right mask shift) 0b1111_1111l
      |> to_int)
    in
    let payload_byte =
      payload
      |> String.get_unchecked ~at:i
      |> Char.to_int
    in
    let _ = IO.Bytes.set result ~at:i ~char:(Char.from_int_unchecked (payload_byte lxor mask_byte)) in
    ()
  done;
  IO.Bytes.to_string result

(* Generate a random mask *)

let generate_mask = fun ?rng () -> Random.bits32 ?rng ()

(* Apply mask to payload *)

let apply_mask = fun mask payload -> unmask mask payload

let close_payload_error_to_string = function
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

let is_valid_close_code = fun code ->
  (code >= 1_000 && code <= 1_014 && code != 1_004 && code != 1_005 && code != 1_006)
  || (code >= 3_000 && code <= 4_999)

let validate_close_payload = fun payload ->
  let payload_length = String.length payload in
  if payload_length = 0 then
    Ok ()
  else if payload_length = 1 then
    Error (ClosePayloadTooShort { payload_length })
  else
    let code = (byte_at payload 0 lsl 8) lor byte_at payload 1 in
    if not (is_valid_close_code code) then
      Error (InvalidCloseCode { code })
    else
      let reason_length = payload_length - 2 in
      let reason = String.sub payload ~offset:2 ~len:reason_length in
      if Unicode.Utf8.is_valid reason then
        Ok ()
      else
        Error (InvalidCloseReasonUtf8 { reason_length })

(* Create frame helpers *)

let text = fun ?(fin = true) payload ->
  {
    fin;
    rsv1 = false;
    rsv2 = false;
    rsv3 = false;
    opcode = Text;
    masked = false;
    payload;
  }

let binary = fun ?(fin = true) payload ->
  {
    fin;
    rsv1 = false;
    rsv2 = false;
    rsv3 = false;
    opcode = Binary;
    masked = false;
    payload;
  }

let close = fun ?(payload = "") () ->
  {
    fin = true;
    rsv1 = false;
    rsv2 = false;
    rsv3 = false;
    opcode = Close;
    masked = false;
    payload;
  }

let ping = fun ?(payload = "") () ->
  {
    fin = true;
    rsv1 = false;
    rsv2 = false;
    rsv3 = false;
    opcode = Ping;
    masked = false;
    payload;
  }

let pong = fun ?(payload = "") () ->
  {
    fin = true;
    rsv1 = false;
    rsv2 = false;
    rsv3 = false;
    opcode = Pong;
    masked = false;
    payload;
  }

let continuation = fun ?(fin = false) payload ->
  {
    fin;
    rsv1 = false;
    rsv2 = false;
    rsv3 = false;
    opcode = Continuation;
    masked = false;
    payload;
  }
