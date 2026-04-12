open Prelude

type t = int

let zero = 0

let one = 1

let add = Caml_runtime.add_int

let sub = Caml_runtime.sub_int

let mul = Caml_runtime.mul_int

let div = Caml_runtime.div_int

let rem = Caml_runtime.mod_int

let equal = Caml_runtime.equal

let compare = Caml_runtime.compare

let abs = fun value ->
  if value < 0 then
    0 - value
  else
    value

let min = fun left right ->
  if compare left right <= 0 then
    left
  else
    right

let max = fun left right ->
  if compare left right >= 0 then
    left
  else
    right

let succ = fun value -> add value 1

let pred = fun value -> sub value 1

let of_float = Caml_runtime.int_of_float

let parse_unchecked = Caml_runtime.int_of_string

let parse = fun value ->
  try Some (parse_unchecked value) with
  | _ -> None

let hash = fun value -> value

let to_string = fun value ->
  if value = 0 then
    "0"
  else
    let negative = value < 0 in
    let rec digit_count count current =
      if current = 0 then
        count
      else
        digit_count (count + 1) (current / 10)
    in
    let digits = digit_count 0 value in
    let width =
      if negative then
        digits + 1
      else
        digits
    in
    let out = Caml_runtime.bytes_create width in
    let rec fill index current =
      if current != 0 then
        (
          let digit = current mod 10 in
          let digit =
            if digit < 0 then
              -digit
            else
              digit
          in
          Caml_runtime.bytes_set out index (Caml_runtime.char_of_int (48 + digit));
          fill (index - 1) (current / 10)
        )
    in
    if negative then
      Caml_runtime.bytes_set out 0 '-';
    fill (width - 1) value;
    Caml_runtime.bytes_unsafe_to_string out
