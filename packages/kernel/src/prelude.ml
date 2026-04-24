type ('value, 'error) result =
  | Ok of 'value
  | Error of 'error

external raise: exn -> 'a = "%raise"

let max_int = Caml_runtime.shift_right_logical_int (-1) 1

let min_int = Caml_runtime.add_int max_int 1

let ( = ) = Caml_runtime.equal

let compare = Order.compare

let min = fun left right ->
  match compare left right with
  | Order.LT
  | Order.EQ -> left
  | Order.GT -> right

let max = fun left right ->
  match compare left right with
  | Order.LT -> right
  | Order.EQ
  | Order.GT -> left

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

let not = fun value -> Caml_runtime.not_bool ~value

let ( && ) = fun left right -> Caml_runtime.and_bool ~left ~right

let ( || ) = fun left right -> Caml_runtime.or_bool ~left ~right
