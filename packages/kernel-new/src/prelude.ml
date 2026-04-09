let ( = ) = Caml_runtime.equal

let ( != ) = Caml_runtime.not_equal

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

let not = Caml_runtime.not_bool

let ( && ) = Caml_runtime.and_bool

let ( || ) = Caml_runtime.or_bool
