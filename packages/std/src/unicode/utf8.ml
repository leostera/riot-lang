open Global
module String = Kernel.String
module Uchar = Kernel.Uchar

(** UTF-8 encoding/decoding *)
let decode_rune = fun s pos ->
  if pos < 0 || pos >= String.length s then
    None
  else
    let decode = String.get_utf_8_uchar s pos in
    if Uchar.utf_decode_is_valid decode then
      let r = Uchar.utf_decode_uchar decode in
      let len = Uchar.utf_decode_length decode in
      Some (r, pos + len)
    else
      None

let encode_rune = Rune.to_string

let is_valid = fun s ->
  let rec check = fun pos ->
    if pos >= String.length s then
      true
    else
      let decode = String.get_utf_8_uchar s pos in
      if Uchar.utf_decode_is_valid decode then
        let len = Uchar.utf_decode_length decode in
        check (pos + len)
      else
        false
  in
  check 0

let is_continuation = fun c ->
  let b = Char.code c in
  b land 0xc0 = 0x80

(* 10xxxxxx *)

let rune_length = fun c ->
  let b = Char.code c in
  if b land 0x80 = 0 then
    1
    (* 0xxxxxxx *)
  else if b land 0xe0 = 0xc0 then
    2
    (* 110xxxxx *)
  else if b land 0xf0 = 0xe0 then
    3
    (* 1110xxxx *)
  else if b land 0xf8 = 0xf0 then
    4
    (* 11110xxx *)
  else
    0

(* Invalid or continuation *)
