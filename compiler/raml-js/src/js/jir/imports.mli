type requirement = Types.Imports.requirement
val make: from:string -> ?imported:string -> local:Types.Binder.t -> unit -> requirement

val namespace: from:string -> local:Types.Binder.t -> unit -> requirement

val local: requirement -> Types.Binder.t

val equal: requirement -> requirement -> bool

val to_json: requirement -> Std.Data.Json.t
