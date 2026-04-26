(** WebSocket Frame Serializer *)
open Std
open Std.IO

(* Serialize a WebSocket frame to bytes *)

let serialize = fun frame ->
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
        0x80
      else
        0x00
    ) lor (
      if rsv1 then
        0x40
      else
        0x00
    ) lor (
      if rsv2 then
        0x20
      else
        0x00
    ) lor (
      if rsv3 then
        0x10
      else
        0x00
    ) lor Frame.opcode_to_int opcode
  in
  (* Determine payload length encoding *)
  let (length_bytes, extended_length) =
    if payload_len < 126 then
      ([ payload_len ], [])
    else if payload_len < 65_536 then
      let high = (payload_len lsr 8) land 0xff in
      let low = payload_len land 0xff in
      ([ 126 ], [ high; low ])
    else
      (* 64-bit length *)
      let b0 = (payload_len lsr 24) land 0xff in
      let b1 = (payload_len lsr 16) land 0xff in
      let b2 = (payload_len lsr 8) land 0xff in
      let b3 = payload_len land 0xff in
      ([ 127 ], [ 0; 0; 0; 0; b0; b1; b2; b3; ])
  in
  (* Second byte: MASK, payload length *)
  let byte1 =
    (
      if masked then
        0x80
      else
        0x00
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
      let mask = Frame.generate_mask () in
      (* Write mask (4 bytes) *)
      Buffer.add_char
        header
        (Char.from_int_unchecked Int32.(to_int (logand (shift_right mask 24) 0xffl)));
      Buffer.add_char
        header
        (Char.from_int_unchecked Int32.(to_int (logand (shift_right mask 16) 0xffl)));
      Buffer.add_char
        header
        (Char.from_int_unchecked Int32.(to_int (logand (shift_right mask 8) 0xffl)));
      Buffer.add_char header (Char.from_int_unchecked Int32.(to_int (logand mask 0xffl)));
      (* Apply mask to payload *)
      let masked_payload = Frame.apply_mask mask payload in
      Buffer.add_string header masked_payload
    )
  else
    Buffer.add_string header payload;
  Buffer.contents header
