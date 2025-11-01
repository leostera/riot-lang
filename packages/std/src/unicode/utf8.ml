(** UTF-8 encoding/decoding *)

let decode_rune s pos =
  if pos < 0 || pos >= Stdlib.String.length s then None
  else
    let decode = Stdlib.String.get_utf_8_uchar s pos in
    if Uchar.utf_decode_is_valid decode then
      let r = Uchar.utf_decode_uchar decode in
      let len = Uchar.utf_decode_length decode in
      Some (r, pos + len)
    else
      None

let encode_rune = Rune.to_string

let is_valid s =
  let rec check pos =
    if pos >= Stdlib.String.length s then true
    else
      let decode = Stdlib.String.get_utf_8_uchar s pos in
      if Uchar.utf_decode_is_valid decode then
        let len = Uchar.utf_decode_length decode in
        check (pos + len)
      else
        false
  in
  check 0

let is_continuation c =
  let b = Char.code c in
  b land 0xC0 = 0x80  (* 10xxxxxx *)

let rune_length c =
  let b = Char.code c in
  if b land 0x80 = 0 then 1          (* 0xxxxxxx *)
  else if b land 0xE0 = 0xC0 then 2  (* 110xxxxx *)
  else if b land 0xF0 = 0xE0 then 3  (* 1110xxxx *)
  else if b land 0xF8 = 0xF0 then 4  (* 11110xxx *)
  else 0  (* Invalid or continuation *)
