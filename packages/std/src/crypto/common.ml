open Kernel
module IoVec = IO.IoVec

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
    push_bytes state (Bytes.from_string value)

let write_hash = fun state hash -> push_bytes state (Hash.to_bytes hash)

let bytes_of_unit = fun () -> Bytes.create ~size:1

let bytes_of_bool = fun value ->
  let out = Bytes.create ~size:1 in
  let _ =
    Bytes.set out ~at:0
      ~char:(
        if value then
          Char.from_int_unchecked 1
        else
          Char.from_int_unchecked 0
      )
  in
  out

let bytes_of_int = fun value ->
  let out = Bytes.create ~size:8 in
  let rec loop index =
    if index < 8 then
      (
        let byte = (value lsr (index * 8)) land 0xff in
        let _ = Bytes.set out ~at:index ~char:(Char.from_int_unchecked byte) in
        loop (index + 1)
      )
  in
  loop 0;
  out

let bytes_of_int32 = fun value ->
  let value = int32_to_int value in
  let out = Bytes.create ~size:4 in
  let rec loop index =
    if index < 4 then
      (
        let byte = (value lsr (index * 8)) land 0xff in
        let _ = Bytes.set out ~at:index ~char:(Char.from_int_unchecked byte) in
        loop (index + 1)
      )
  in
  loop 0;
  out

let bytes_of_int64 = fun value ->
  let out = Bytes.create ~size:8 in
  let mask = 0xffL in
  let rec loop index =
    if index < 8 then
      (
        let shifted = int64_shift_right_logical value (index * 8) in
        let byte = Int64.to_int (int64_logand shifted mask) in
        let _ = Bytes.set out ~at:index ~char:(Char.from_int_unchecked byte) in
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
  let buffers = Array.from_list (reverse_append state.buffers_rev []) in
  match IoVec.from_bytes_array buffers with
  | Ok iovec -> digest iovec
  | Error error ->
      SystemError.panic ("Std.Crypto.Common.finish_iovec: " ^ Kernel.IO.Error.message error)

let hash_string_with = fun digest value ->
  let state = create_state () in
  write_string state value;
  finish_iovec digest state

let hash_bytes_with = fun digest value ->
  match IoVec.from_bytes (Bytes.sub_unchecked value ~offset:0 ~len:(Bytes.length value)) with
  | Ok iovec -> digest iovec
  | Error error ->
      SystemError.panic ("Std.Crypto.Common.hash_bytes_with: " ^ Kernel.IO.Error.message error)
