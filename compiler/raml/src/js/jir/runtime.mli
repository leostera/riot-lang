type helper = Types.Runtime.helper
type t = helper
val make: module_name:string -> symbol:string -> ?local:string -> unit -> helper

val to_import: helper -> Types.Imports.requirement

val to_json: helper -> Std.Data.Json.t
