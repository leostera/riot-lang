open Prelude

type compiled

external compile: string -> (compiled, string * int option) result = "kernel_regex_compile"

external is_match: compiled -> string -> bool = "kernel_regex_is_match"

external find: compiled -> string -> (int * int) option = "kernel_regex_find"
