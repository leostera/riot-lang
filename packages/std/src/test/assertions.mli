open Global

val assert_equal : expected:'a -> actual:'a -> unit

val assert_ok : ('a, 'b) result -> unit

val assert_error : ('a, 'b) result -> unit

val assert_true : bool -> unit

val assert_false : bool -> unit
