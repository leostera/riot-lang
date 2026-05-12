type helper = Types.Runtime.helper
type t = helper
val module_ref: Types.Modules.t

val make: module_ref:Types.Modules.t -> symbol:string -> ?local:Types.Binder.t -> unit -> helper

val call_primitive: unit -> helper

val make_curried: unit -> helper

val to_import: helper -> Types.Imports.requirement

val to_json: helper -> Std.Data.Json.t
