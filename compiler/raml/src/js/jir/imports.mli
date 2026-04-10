type requirement = Types.Imports.requirement
val make: from:string -> ?imported:string -> local:string -> unit -> requirement

val namespace: from:string -> local:string -> unit -> requirement

val local: requirement -> string

val equal: requirement -> requirement -> bool

val to_json: requirement -> Std.Data.Json.t
