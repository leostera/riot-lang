open Std
open Tusk_model

val template : Module.t list -> string
val make_node : Namespace.t -> Module.t list -> Module_node.t
