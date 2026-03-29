open Std
open Tusk_model

module G = Std.Graph.SimpleGraph

type t
val create : unit -> t

val register : t -> Module.t -> G.Node_id.t -> unit

val get_by_name : t -> string -> G.Node_id.t list
