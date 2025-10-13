(** WebSocket Frame Serializer *)

open Std

(* Serialize a WebSocket frame to bytes *)
let serialize frame =
  let Frame.{ fin; rsv1; rsv2; rsv3; opcode; masked; payload } = frame in

  let payload_len = String.length payload in

  (* First byte: FIN, RSV, opcode *)
  let byte0 =
    (if fin then 0x80 else 0x00)
    lor (if rsv1 then 0x40 else 0x00)
    lor (if rsv2 then 0x20 else 0x00)
    lor (if rsv3 then 0x10 else 0x00)
    lor Frame.opcode_to_int opcode
  in

  (* Determine payload length encoding *)
  let length_bytes, extended_length =
    if payload_len < 126 then ([ payload_len ], [])
    else if payload_len < 65536 then
      (* 16-bit length *)
      let high = (payload_len lsr 8) land 0xFF in
      let low = payload_len land 0xFF in
      ([ 126 ], [ high; low ])
    else
      (* 64-bit length *)
      let b0 = (payload_len lsr 24) land 0xFF in
      let b1 = (payload_len lsr 16) land 0xFF in
      let b2 = (payload_len lsr 8) land 0xFF in
      let b3 = payload_len land 0xFF in
      ([ 127 ], [ 0; 0; 0; 0; b0; b1; b2; b3 ])
  in

  (* Second byte: MASK, payload length *)
  let byte1 = (if masked then 0x80 else 0x00) lor List.hd length_bytes in

  (* Build header *)
  let header = Buffer.create 14 in
  Buffer.add_char header (Char.chr byte0);
  Buffer.add_char header (Char.chr byte1);
  List.iter (fun b -> Buffer.add_char header (Char.chr b)) extended_length;

  (* Add mask and masked payload if needed *)
  if masked then (
    let mask = Frame.generate_mask () in
    (* Write mask (4 bytes) *)
    Buffer.add_char header
      (Char.chr Int32.(to_int (logand (shift_right mask 24) 0xFFl)));
    Buffer.add_char header
      (Char.chr Int32.(to_int (logand (shift_right mask 16) 0xFFl)));
    Buffer.add_char header
      (Char.chr Int32.(to_int (logand (shift_right mask 8) 0xFFl)));
    Buffer.add_char header (Char.chr Int32.(to_int (logand mask 0xFFl)));

    (* Apply mask to payload *)
    let masked_payload = Frame.apply_mask mask payload in
    Buffer.add_string header masked_payload)
  else Buffer.add_string header payload;

  Buffer.contents header
