(** WebSocket Frame Implementation

    Based on RFC 6455: https://datatracker.ietf.org/doc/html/rfc6455#section-5.2

    Frame format:

    0 1 2 3 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
    +-+-+-+-+-------+-+-------------+-------------------------------+ |F|R|R|R|
    opcode|M| Payload len | Extended payload length | |I|S|S|S| (4) |A| (7) |
    (16/64) | |N|V|V|V| |S| | (if payload len==126/127) | | |1|2|3| |K| | |
    +-+-+-+-+-------+-+-------------+ - - - - - - - - - - - - - - - + *)
open Std
open Std.IO

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

let opcode_to_int =
  function
  | Continuation -> 0x0
  | Text -> 0x1
  | Binary -> 0x2
  | Close -> 0x8
  | Ping -> 0x9
  | Pong -> 0xa

let opcode_of_int =
  function
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
  let result = Bytes.create len in
  for i = 0 to len - 1 do
    let mask_byte = Int32.(logand (shift_right mask (8 * (3 - (i mod 4)))) 0xffl |> to_int) in
    let payload_byte = Char.code payload.[i] in
    Bytes.set result i (Char.chr (payload_byte lxor mask_byte))
  done;
  Bytes.to_string result

(* Generate a random mask *)

let generate_mask = fun () -> Random.int32 Int32.max_int

(* Apply mask to payload *)

let apply_mask = fun mask payload -> unmask mask payload

(* Create frame helpers *)

let text = fun ?(fin = true) payload -> {
  fin;
  rsv1 = false;
  rsv2 = false;
  rsv3 = false;
  opcode = Text;
  masked = false;
  payload;

}

let binary = fun ?(fin = true) payload -> {
  fin;
  rsv1 = false;
  rsv2 = false;
  rsv3 = false;
  opcode = Binary;
  masked = false;
  payload;

}

let close = fun ?(payload = "") () -> {
  fin = true;
  rsv1 = false;
  rsv2 = false;
  rsv3 = false;
  opcode = Close;
  masked = false;
  payload;

}

let ping = fun ?(payload = "") () -> {
  fin = true;
  rsv1 = false;
  rsv2 = false;
  rsv3 = false;
  opcode = Ping;
  masked = false;
  payload;

}

let pong = fun ?(payload = "") () -> {
  fin = true;
  rsv1 = false;
  rsv2 = false;
  rsv3 = false;
  opcode = Pong;
  masked = false;
  payload;

}

let continuation = fun ?(fin = false) payload -> {
  fin;
  rsv1 = false;
  rsv2 = false;
  rsv3 = false;
  opcode = Continuation;
  masked = false;
  payload;

}
