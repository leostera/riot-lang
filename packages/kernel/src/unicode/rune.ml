open Prelude

type t = int [@@ immediate]

type utf_decode = int [@@ immediate]

type error =
  | BadRune of { int: int }

let min = 0x0000

let max = 0x10_ffff

let replacement = 0xfffd

let max_ascii = 0x007f

let max_latin1 = 0x00ff

let lo_bound = 0xd7ff

let hi_bound = 0xe000

let valid_bit = 27

let decode_bits = 24

let is_valid = fun value -> (min <= value && value <= lo_bound) || (hi_bound <= value
&& value <= max)

let from_int = fun value ->
  if is_valid value then
    Ok value
  else
    Error (BadRune { int = value })

let from_int_unchecked = fun value -> value

let to_int = fun value -> value

let from_char = fun value -> Char.to_int value

let to_char = fun value ->
  if value > max_latin1 then
    System_error.panic "unicode scalar value is not a latin1 character"
  else
    Char.from_int_unchecked value

let equal = Int.equal

let compare = Int.compare

let utf_decode_is_valid = fun decode -> decode lsr valid_bit = 1

let utf_decode_rune = fun decode -> from_int_unchecked (decode land 0xff_ffff)

let utf_decode_length = fun decode -> (decode lsr decode_bits) land 0b111

let utf_decode = fun consumed rune -> ((8 lor consumed) lsl decode_bits) lor to_int rune

let utf_decode_invalid = fun consumed -> (consumed lsl decode_bits) lor replacement

let utf_8_byte_length = fun rune ->
  let code = to_int rune in
  if code <= 0x007f then
    1
  else if code <= 0x07ff then
    2
  else if code <= 0xffff then
    3
  else
    4

let to_string = fun rune ->
  let width = utf_8_byte_length rune in
  let out = Bytes.create ~size:width in
  let code = to_int rune in
  match width with
  | 1 ->
      Bytes.set_unchecked out ~at:0 ~char:(Char.from_int_unchecked code);
      Bytes.to_string out
  | 2 ->
      Bytes.set_unchecked out ~at:0 ~char:(Char.from_int_unchecked (0xc0 lor (code lsr 6)));
      Bytes.set_unchecked out ~at:1 ~char:(Char.from_int_unchecked (0x80 lor (code land 0x3f)));
      Bytes.to_string out
  | 3 ->
      Bytes.set_unchecked out ~at:0 ~char:(Char.from_int_unchecked (0xe0 lor (code lsr 12)));
      Bytes.set_unchecked out ~at:1 ~char:(Char.from_int_unchecked (0x80 lor ((code lsr 6) land 0x3f)));
      Bytes.set_unchecked out ~at:2 ~char:(Char.from_int_unchecked (0x80 lor (code land 0x3f)));
      Bytes.to_string out
  | _ ->
      Bytes.set_unchecked out ~at:0 ~char:(Char.from_int_unchecked (0xf0 lor (code lsr 18)));
      Bytes.set_unchecked out ~at:1 ~char:(Char.from_int_unchecked (0x80 lor ((code lsr 12) land 0x3f)));
      Bytes.set_unchecked out ~at:2 ~char:(Char.from_int_unchecked (0x80 lor ((code lsr 6) land 0x3f)));
      Bytes.set_unchecked out ~at:3 ~char:(Char.from_int_unchecked (0x80 lor (code land 0x3f)));
      Bytes.to_string out
