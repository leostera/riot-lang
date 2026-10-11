// Intrinsic UTF-8 text operations used by compiler implementations.
use Prelude.{Option, None}

let concat = fn (left: string) -> fn (right: string) -> left + right
let length = fn (value: string) -> 0
let get = fn (value: string) -> fn (index: i32) -> (None: Option<i32>)
let slice = fn (value: string) -> fn (start: i32) -> fn (end: i32) -> ""
let equal = fn (left: string) -> fn (right: string) -> true
