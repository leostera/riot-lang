(** WebSocket Frame Parser

    Incremental parser for WebSocket frames following RFC 6455 *)
open Std

let ( let* ) = Result.and_then

type 'a parse_result =
  | Done of { value: 'a; remaining: string; }
  | Need_more
  | Error of string

(* Parse a single WebSocket frame *)

let parse = fun input ->
  let len = String.length input in
  (* Need at least 2 bytes for header *)
  if len < 2 then
    Need_more
  else
    let byte0 = Char.code input.[0] in
    let byte1 = Char.code input.[1] in
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
    | None -> Error ("Invalid opcode: 0x" ^ Int.to_string opcode_int)
    | Some opcode ->
        (* Determine actual payload length and header size *)
        let header_size, payload_length =
          if payload_len_initial < 126 then
            (2, payload_len_initial)
          else if payload_len_initial = 126 then
            if len < 4 then
              (0, 0)
              (* Signal need more *)
            else
              let len_high = Char.code input.[2] in
              let len_low = Char.code input.[3] in
              (4, (len_high lsl 8) lor len_low)
          else if len < 10 then
            (0, 0)
            (* Signal need more *)
          else
            (* Read 8 bytes, but only use lower 4 bytes for simplicity *)
            let len_bytes = [
              Char.code input.[6];
              Char.code input.[7];
              Char.code input.[8];
              Char.code input.[9];
            ] in
            let payload_len =
              List.fold_left (fun acc b -> (acc lsl 8) lor b) 0 len_bytes
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
                let m0 = Int32.of_int (Char.code input.[header_size]) in
                let m1 = Int32.of_int (Char.code input.[header_size + 1]) in
                let m2 = Int32.of_int (Char.code input.[header_size + 2]) in
                let m3 = Int32.of_int (Char.code input.[header_size + 3]) in
                Int32.(logor
                  (shift_left m0 24)
                  (logor (shift_left m1 16) (logor (shift_left m2 8) m3)))
              else
                Int32.zero
            in
            (* Extract payload *)
            let payload_start = total_header_size in
            let raw_payload = String.sub input payload_start payload_length in
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
            let remaining = String.sub
              input
              (total_header_size + payload_length)
              (len - total_header_size - payload_length) in
            Done {value = frame;remaining;}
