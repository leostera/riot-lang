external equal: 'value -> 'value -> bool = "%equal"

external ptr_eq: 'value -> 'value -> bool = "%eq"

external compare: 'value -> 'value -> int = "%compare"

external not_equal: 'value -> 'value -> bool = "%notequal"

external ptr_not_eq: 'value -> 'value -> bool = "%noteq"

external less_than: 'value -> 'value -> bool = "%lessthan"

external greater_than: 'value -> 'value -> bool = "%greaterthan"

external less_or_equal: 'value -> 'value -> bool = "%lessequal"

external greater_or_equal: 'value -> 'value -> bool = "%greaterequal"

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

external round_float: float -> float = "caml_round_float" "caml_round" [@@ unboxed] [@@ noalloc]

external float_of_int: int -> float = "%floatofint"

external int_of_float: float -> int = "%intoffloat"

external neg_float: float -> float = "%negfloat"

external add_float: float -> float -> float = "%addfloat"

external sub_float: float -> float -> float = "%subfloat"

external mul_float: float -> float -> float = "%mulfloat"

external div_float: float -> float -> float = "%divfloat"

external pow_float: float -> float -> float = "caml_power_float" "pow" [@@ unboxed] [@@ noalloc]

external rem_float: float -> float -> float = "caml_fmod_float" "fmod" [@@ unboxed] [@@ noalloc]

external sqrt_float: float -> float = "caml_sqrt_float" "sqrt" [@@ unboxed] [@@ noalloc]

external ceil_float: float -> float = "caml_ceil_float" "ceil" [@@ unboxed] [@@ noalloc]

external floor_float: float -> float = "caml_floor_float" "floor" [@@ unboxed] [@@ noalloc]

external format_int: string -> int -> string = "caml_format_int"

external int_of_string: string -> int = "caml_int_of_string"

external format_float: string -> float -> string = "caml_format_float"

external float_of_string: string -> float = "caml_float_of_string"

external int_of_char: char -> int = "%identity"

(**
   Internal unchecked bridge from an integer that is already known to be a valid byte-sized
   character. Public checked constructors should live above this module.
*)
external char_of_int: int -> char = "%identity"

external argv: string array = "%sys_argv"

external recommended_domain_count: unit -> int = "caml_recommended_domain_count" [@@ noalloc]

type gc_stat = {
  minor_words: float;
  promoted_words: float;
  major_words: float;
  minor_collections: int;
  major_collections: int;
  heap_words: int;
  heap_chunks: int;
  live_words: int;
  live_blocks: int;
  free_words: int;
  free_blocks: int;
  largest_free: int;
  fragments: int;
  compactions: int;
  top_heap_words: int;
  stack_size: int;
  forced_major_collections: int;
}

external gc_quick_stat: unit -> gc_stat = "caml_gc_quick_stat"

external gc_major: unit -> unit = "caml_gc_major"

external gc_full_major: unit -> unit = "caml_gc_full_major"

external gc_compact: unit -> unit = "caml_gc_compaction"

external string_length: string -> int = "%string_length"

external string_get: string -> int -> char = "%string_safe_get"

external bytes_length: bytes -> int = "%bytes_length"

external bytes_get: bytes -> int -> char = "%bytes_safe_get"

external bytes_set: bytes -> int -> char -> unit = "%bytes_safe_set"

external bytes_create: int -> bytes = "caml_create_bytes"

external bytes_fill: bytes -> int -> int -> char -> unit = "caml_fill_bytes" [@@ noalloc]

external bytes_blit: bytes -> int -> bytes -> int -> int -> unit = "caml_blit_bytes" [@@ noalloc]

external string_blit: string -> int -> bytes -> int -> int -> unit = "caml_blit_string" [@@ noalloc]

(**
   Internal zero-copy bridge from owned mutable bytes into an immutable string view. Callers must
   ensure the bytes will not be mutated afterward.
*)
external bytes_unsafe_to_string: bytes -> string = "%bytes_to_string"

(**
   Internal zero-copy bridge from an immutable string into mutable bytes. Callers must ensure the
   bytes view will not be mutated in a way that violates string immutability assumptions.
*)
external bytes_unsafe_of_string: string -> bytes = "%bytes_of_string"

(** Internal copying bridge from bytes into a fresh immutable string. *)
val bytes_to_string: bytes -> string

(** Internal copying bridge from string into fresh mutable bytes. *)
val bytes_of_string: string -> bytes

external int64_of_int: int -> int64 = "%int64_of_int"

external int64_to_int: int64 -> int = "%int64_to_int"

external int64_logand: int64 -> int64 -> int64 = "%int64_and"

external int64_logor: int64 -> int64 -> int64 = "%int64_or"

external int64_logxor: int64 -> int64 -> int64 = "%int64_xor"

external shift_left_int64: int64 -> int -> int64 = "%int64_lsl"

external shift_right_int64: int64 -> int -> int64 = "%int64_asr"

external shift_right_logical_int64: int64 -> int -> int64 = "%int64_lsr"

external int64_neg: int64 -> int64 = "%int64_neg"

external int64_add: int64 -> int64 -> int64 = "%int64_add"

external int64_sub: int64 -> int64 -> int64 = "%int64_sub"

external int64_mul: int64 -> int64 -> int64 = "%int64_mul"

external int64_div: int64 -> int64 -> int64 = "%int64_div"

external int64_rem: int64 -> int64 -> int64 = "%int64_mod"

external int64_of_float: float -> int64 = "caml_int64_of_float" "caml_int64_of_float_unboxed" [@@ unboxed] [@@ noalloc]

external int64_to_float: int64 -> float = "caml_int64_to_float" "caml_int64_to_float_unboxed" [@@ unboxed] [@@ noalloc]

external int64_of_int32: int32 -> int64 = "%int64_of_int32"

external int64_to_int32: int64 -> int32 = "%int64_to_int32"

external int32_neg: int32 -> int32 = "%int32_neg"

external int32_add: int32 -> int32 -> int32 = "%int32_add"

external int32_sub: int32 -> int32 -> int32 = "%int32_sub"

external int32_mul: int32 -> int32 -> int32 = "%int32_mul"

external int32_div: int32 -> int32 -> int32 = "%int32_div"

external int32_rem: int32 -> int32 -> int32 = "%int32_mod"

external int32_logand: int32 -> int32 -> int32 = "%int32_and"

external int32_logor: int32 -> int32 -> int32 = "%int32_or"

external int32_logxor: int32 -> int32 -> int32 = "%int32_xor"

external shift_left_int32: int32 -> int -> int32 = "%int32_lsl"

external shift_right_int32: int32 -> int -> int32 = "%int32_asr"

external shift_right_logical_int32: int32 -> int -> int32 = "%int32_lsr"

external int32_of_float: float -> int32 = "caml_int32_of_float" "caml_int32_of_float_unboxed" [@@ unboxed] [@@ noalloc]

external int32_to_float: int32 -> float = "caml_int32_to_float" "caml_int32_to_float_unboxed" [@@ unboxed] [@@ noalloc]

external array_length: 'value array -> int = "%array_length"

external array_get: 'value array -> int -> 'value = "%array_safe_get"

external array_set: 'value array -> int -> 'value -> unit = "%array_safe_set"

external array_unsafe_get: 'value array -> int -> 'value = "%array_unsafe_get"

external array_unsafe_set: 'value array -> int -> 'value -> unit = "%array_unsafe_set"

external array_make: int -> 'value -> 'value array = "caml_array_make"
