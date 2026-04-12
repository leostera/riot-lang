open Kernel
module Iovec = IO.Iovec

type state = {
  mutable buffers_rev: bytes list;
}

external int32_to_int: int32 -> int = "%int32_to_int"

external int64_bits_of_float: float -> int64
  = "caml_int64_bits_of_float" "caml_int64_bits_of_float_unboxed" [@@unboxed] [@@noalloc]

external int64_logand: int64 -> int64 -> int64 = "%int64_and"

external int64_shift_right_logical: int64 -> int -> int64 = "%int64_lsr"

let create_state = fun () -> { buffers_rev = [] }

let push_bytes = fun state buffer ->
  if Bytes.length buffer > 0 then
    state.buffers_rev <- buffer :: state.buffers_rev

let write_string = fun state value ->
  if String.length value > 0 then
    push_bytes state (Bytes.of_string value)

let write_hash = fun state hash -> push_bytes state (Hash.to_bytes hash)

let bytes_of_unit = fun () -> Bytes.create 1

let bytes_of_bool = fun value ->
  let out = Bytes.create 1 in
  Bytes.set out 0
    (
      if value then
        Char.unsafe_of_int 1
      else
        Char.unsafe_of_int 0
    );
  out

let bytes_of_int = fun value ->
  let out = Bytes.create 8 in
  let rec loop index =
    if index < 8 then
      (
        let byte = (value lsr (index * 8)) land 0xff in
        Bytes.set out index (Char.unsafe_of_int byte);
        loop (index + 1)
      )
  in
  loop 0;
  out

let bytes_of_int32 = fun value ->
  let value = int32_to_int value in
  let out = Bytes.create 4 in
  let rec loop index =
    if index < 4 then
      (
        let byte = (value lsr (index * 8)) land 0xff in
        Bytes.set out index (Char.unsafe_of_int byte);
        loop (index + 1)
      )
  in
  loop 0;
  out

let bytes_of_int64 = fun value ->
  let out = Bytes.create 8 in
  let mask = 0xffL in
  let rec loop index =
    if index < 8 then
      (
        let shifted = int64_shift_right_logical value (index * 8) in
        let byte = Int64.to_int (int64_logand shifted mask) in
        Bytes.set out index (Char.unsafe_of_int byte);
        loop (index + 1)
      )
  in
  loop 0;
  out

let bytes_of_float = fun value -> bytes_of_int64 (int64_bits_of_float value)

let rec list_length = function
  | [] -> 0
  | _ :: tail -> 1 + list_length tail

let rec iter_list f = function
  | [] -> ()
  | head :: tail ->
      f head;
      iter_list f tail

let rec reverse_append left right =
  match left with
  | [] -> right
  | head :: tail -> reverse_append tail (head :: right)

let finish_iovec = fun digest state ->
  let buffers = Array.of_list (reverse_append state.buffers_rev []) in
  digest (Iovec.of_bytes_array buffers)

let hash_string_with = fun digest value ->
  let state = create_state () in
  write_string state value;
  finish_iovec digest state

let hash_bytes_with = fun digest value ->
  digest (Iovec.of_bytes (Bytes.sub value 0 (Bytes.length value)))
