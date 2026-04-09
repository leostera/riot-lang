open Prelude

type t = int

let zero = 0

let one = 1

let add = Primitives.add_int

let sub = Primitives.sub_int

let mul = Primitives.mul_int

let div = Primitives.div_int

let rem = Primitives.mod_int

let equal = Primitives.equal

let compare = Primitives.compare

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
    let out = Primitives.bytes_create width in
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
          Primitives.bytes_set out index (Primitives.char_of_int (48 + digit));
          fill (index - 1) (current / 10)
        )
      else
        ()
    in
    if negative then
      Primitives.bytes_set out 0 '-'
    else
      ();
      fill (width - 1) value;
      Primitives.bytes_to_string out
