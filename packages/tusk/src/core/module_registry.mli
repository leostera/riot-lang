(** Module registry for tracking module graph nodes by name.

    This module provides mapping between Module.t and graph node IDs,
    with separate tracking for interface and implementation files. *)

open Std
open Model

type t

val create : unit -> t
(** Create a new empty registry *)

val register : t -> Module.t -> Graph.SimpleGraph.Node_id.t -> unit
(** Register a module with its graph node ID *)

val get : t -> Graph.SimpleGraph.Node_id.t -> Module.t
(** Get the module for a given node ID *)

val get_by_name : t -> string -> Graph.SimpleGraph.Node_id.t list
(** Get all node IDs (interface and/or implementation) for a module name *)
