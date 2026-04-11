type helper = Types.Runtime.helper
type t = helper
val module_name: string

val make: module_name:string -> symbol:string -> ?local:string -> unit -> helper

val call_primitive: unit -> helper

val make_curried: unit -> helper

val print_endline: unit -> helper

val print_newline: unit -> helper

val print_int: unit -> helper

val print_string: unit -> helper

val print_char: unit -> helper

val helper_for_direct_callee: string -> helper option

val to_import: helper -> Types.Imports.requirement

val to_json: helper -> Std.Data.Json.t
