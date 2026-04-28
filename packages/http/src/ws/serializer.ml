(** WebSocket Frame Serializer *)
open Std
open Std.IO

type error =
  | MaskGenerationFailed of Random.error

let error_to_string = function
  | MaskGenerationFailed error ->
      "Failed to generate WebSocket mask: " ^ Random.error_to_string error

(* Serialize a WebSocket frame to bytes *)

let serialize = fun ?rng frame ->
  let Frame.{
    fin;
    rsv1;
    rsv2;
    rsv3;
    opcode;
    masked;
    payload
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
  if masked then
    (
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
          Buffer.add_char header (Char.from_int_unchecked Int32.(to_int (logand mask 0b1111_1111l)));
          (* Apply mask to payload *)
          let masked_payload = Frame.apply_mask mask payload in
          Buffer.add_string header masked_payload;
          Ok (Buffer.contents header)
    )
  else (
    Buffer.add_string header payload;
    Ok (Buffer.contents header)
  )
