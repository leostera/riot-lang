open Global

val assert_equal: expected:'a -> actual:'a -> unit

val assert_ok: ('a, 'b) Kernel.result -> unit

val assert_error: ('a, 'b) Kernel.result -> unit

val assert_true: bool -> unit

val assert_false: bool -> unit
