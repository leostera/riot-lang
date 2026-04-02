(** Message passing primitives *)
(** Extensible message type - modules can extend this with new message variants
*)
(** Message envelope with unique identifier *)
type t = ..
(** Wrap a message in an envelope with a unique ID *)
type envelope = private {
  msg: t;
  uid: int;
}
val envelope: t -> envelope
