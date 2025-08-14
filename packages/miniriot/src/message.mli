(** Message passing primitives *)

type t = ..
(** Extensible message type - modules can extend this with new message variants
*)

type envelope = private { msg : t; uid : int }
(** Message envelope with unique identifier *)

val envelope : t -> envelope
(** Wrap a message in an envelope with a unique ID *)
