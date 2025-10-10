open Std

type t
type result = { tree : Rule.green_tree; diagnostics : Diagnostic.t list }

val make : rules:Rule.t list -> unit -> t
val run : t -> ?filename:string -> string -> result
val default_rules : unit -> Rule.t list
val default : unit -> t
