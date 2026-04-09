external equal: 'a -> 'a -> bool = "%equal"

external compare: 'a -> 'a -> int = "%compare"

external not_equal: 'a -> 'a -> bool = "%notequal"

external less_than: 'a -> 'a -> bool = "%lessthan"

external greater_than: 'a -> 'a -> bool = "%greaterthan"

external less_or_equal: 'a -> 'a -> bool = "%lessequal"

external greater_or_equal: 'a -> 'a -> bool = "%greaterequal"

external not_bool: value:bool -> bool = "%boolnot"

external and_bool: left:bool -> right:bool -> bool = "%sequand"

external or_bool: left:bool -> right:bool -> bool = "%sequor"

external neg_int: int -> int = "%negint"

external add_int: int -> int -> int = "%addint"

external sub_int: int -> int -> int = "%subint"

external mul_int: int -> int -> int = "%mulint"

external div_int: int -> int -> int = "%divint"

external mod_int: int -> int -> int = "%modint"

external int_logand: int -> int -> int = "%andint"

external int_logor: int -> int -> int = "%orint"

external int_logxor: int -> int -> int = "%xorint"

external shift_left_int: int -> int -> int = "%lslint"

external shift_right_logical_int: int -> int -> int = "%lsrint"

external shift_right_int: int -> int -> int = "%asrint"

external int_of_char: char -> int = "%identity"

external char_of_int: int -> char = "%identity"

external argv: string array = "%sys_argv"

external string_length: string -> int = "%string_length"

external string_get: string -> int -> char = "%string_safe_get"

external bytes_length: bytes -> int = "%bytes_length"

external bytes_get: bytes -> int -> char = "%bytes_safe_get"

external bytes_set: bytes -> int -> char -> unit = "%bytes_safe_set"

external bytes_create: int -> bytes = "caml_create_bytes"

external bytes_fill: bytes -> int -> int -> char -> unit = "caml_fill_bytes" [@@noalloc]

external bytes_blit: bytes -> int -> bytes -> int -> int -> unit = "caml_blit_bytes" [@@noalloc]

external string_blit: string -> int -> bytes -> int -> int -> unit = "caml_blit_string" [@@noalloc]

external bytes_to_string: bytes -> string = "%bytes_to_string"

external bytes_of_string: string -> bytes = "%bytes_of_string"

external int64_of_int: int -> int64 = "%int64_of_int"

external int64_to_int: int64 -> int = "%int64_to_int"

external int64_add: int64 -> int64 -> int64 = "%int64_add"

external int64_mul: int64 -> int64 -> int64 = "%int64_mul"

external int64_div: int64 -> int64 -> int64 = "%int64_div"

external int64_rem: int64 -> int64 -> int64 = "%int64_mod"

external array_length: 'a array -> int = "%array_length"

external array_get: 'a array -> int -> 'a = "%array_safe_get"

external array_set: 'a array -> int -> 'a -> unit = "%array_safe_set"

external array_make: int -> 'a -> 'a array = "caml_array_make"
