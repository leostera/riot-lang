open Prelude

module String = Kernel.String

module Scalar = Kernel.Unicode.Rune

module Rune = Rune

(** UTF-8 encoding/decoding *)
let decode_rune = fun s pos ->
  if pos < 0 || pos >= String.length s then
    None
  else
    match String.get_utf_8_rune s ~at:pos with
    | Some decode when Scalar.utf_decode_is_valid decode ->
        let r = Scalar.utf_decode_rune decode in
        let len = Scalar.utf_decode_length decode in Some (r, pos + len)
    | _ -> None

let encode_rune = Rune.to_string

let is_valid = fun s ->
  let rec check pos =
    if pos >= String.length s then
      true
    else
      match String.get_utf_8_rune s ~at:pos with
      | Some decode when Scalar.utf_decode_is_valid decode ->
          let len = Scalar.utf_decode_length decode in check (pos + len)
      | _ -> false
  in
  check 0

let is_continuation = fun c ->
  let b = Char.code c in b land 0xc0 = 0x80

(* 10xxxxxx *)
let rune_length = fun c ->
  let b = Char.code c in
  if b land 0x80 = 0 then
    1
  (* 0xxxxxxx *)
  else
    if b land 0xe0 = 0xc0 then
      2
    (* 110xxxxx *)
    else
      if b land 0xf0 = 0xe0 then
        3
      (* 1110xxxx *)
      else
        if b land 0xf8 = 0xf0 then
          4
        (* 11110xxx *)
        else 0(* Invalid or continuation *)
