type ('value, 'error) result =
  | Ok of 'value
  | Error of 'error

exception Invalid_argument of string

exception Failure of string

exception Not_found

external raise: exn -> 'a = "%raise"

external raise_notrace: exn -> 'a = "%raise_notrace"

external ignore: 'value -> unit = "%ignore"

let max_int = Caml_runtime.shift_right_logical_int (-1) 1

let min_int = Caml_runtime.add_int max_int 1

let ( = ) = Caml_runtime.equal

let compare = Caml_runtime.compare

let min = fun left right ->
  if Caml_runtime.less_or_equal (compare left right) 0 then
    left
  else
    right

let max = fun left right ->
  if Caml_runtime.greater_or_equal (compare left right) 0 then
    left
  else
    right

let ( != ) = Caml_runtime.not_equal

let ( <> ) = Caml_runtime.not_equal

let ( < ) = Caml_runtime.less_than

let ( > ) = Caml_runtime.greater_than

let ( <= ) = Caml_runtime.less_or_equal

let ( >= ) = Caml_runtime.greater_or_equal

let ( ~- ) = Caml_runtime.neg_int

let ( + ) = Caml_runtime.add_int

let ( - ) = Caml_runtime.sub_int

let ( * ) = Caml_runtime.mul_int

let ( / ) = Caml_runtime.div_int

let ( mod ) = Caml_runtime.mod_int

let ( land ) = Caml_runtime.int_logand

let ( lor ) = Caml_runtime.int_logor

let ( lxor ) = Caml_runtime.int_logxor

let lnot value = value lxor (-1)

let ( lsl ) = Caml_runtime.shift_left_int

let ( lsr ) = Caml_runtime.shift_right_logical_int

let ( asr ) = Caml_runtime.shift_right_int

let ( ~-. ) = Caml_runtime.neg_float

let ( +. ) = Caml_runtime.add_float

let ( -. ) = Caml_runtime.sub_float

let ( *. ) = Caml_runtime.mul_float

let ( /. ) = Caml_runtime.div_float

let ( @@ ) = fun f value -> f value

let ( |> ) = fun value f -> f value

let ( ^ ) = fun left right ->
  let left_length = Caml_runtime.string_length left in
  let right_length = Caml_runtime.string_length right in
  let output = Caml_runtime.bytes_create (left_length + right_length) in
  Caml_runtime.string_blit left 0 output 0 left_length;
  Caml_runtime.string_blit right 0 output left_length right_length;
  Caml_runtime.bytes_unsafe_to_string output

let rec ( @ ) = fun left right ->
  match left with
  | [] -> right
  | head :: tail -> head :: (tail @ right)

let ( ** ) = Caml_runtime.pow_float

let float_of_int = Caml_runtime.float_of_int

let int_of_float = Caml_runtime.int_of_float

let float = Caml_runtime.float_of_int

let string_of_int = Caml_runtime.format_int "%d"

let string_of_float =
  let valid_float_lexem value =
    let length = Caml_runtime.string_length value in
    let rec loop index =
      if index >= length then
        (
          let out = Caml_runtime.bytes_create (length + 1) in
          Caml_runtime.string_blit value 0 out 0 length;
          Caml_runtime.bytes_set out length '.';
          Caml_runtime.bytes_unsafe_to_string out
        )
      else
        match Caml_runtime.string_get value index with
        | '0' .. '9'
        | '-' -> loop (index + 1)
        | _ -> value
    in
    loop 0
  in
  fun value -> valid_float_lexem (Caml_runtime.format_float "%.12g" value)

let abs = fun value ->
  if value >= 0 then
    value
  else
    -value

let mod_float = Caml_runtime.rem_float

let sqrt = Caml_runtime.sqrt_float

let floor = Caml_runtime.floor_float

let ceil = Caml_runtime.ceil_float

let not = fun value -> Caml_runtime.not_bool ~value

let ( && ) = fun left right -> Caml_runtime.and_bool ~left ~right

let ( || ) = fun left right -> Caml_runtime.or_bool ~left ~right
