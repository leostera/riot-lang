val validate_nanos: int -> (unit, unit) Result.t

val compare_parts: left_secs:int -> left_nanos:int -> right_secs:int -> right_nanos:int -> int

val diff_ns: left_secs:int -> left_nanos:int -> right_secs:int -> right_nanos:int -> int64

val split_ns: int64 -> int * int
