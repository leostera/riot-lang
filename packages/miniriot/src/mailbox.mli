(** Process mailbox for message passing *)

type t
(** Opaque mailbox type containing queued messages *)

val create : unit -> t
(** Create a new empty mailbox *)

val queue : t -> Message.envelope -> unit
(** Add a message envelope to the mailbox *)

val next : t -> Message.envelope option
(** Get the next message from the mailbox, returning None if empty *)

val size : t -> int
(** Get the current number of messages in the mailbox *)

val is_empty : t -> bool
(** Check if the mailbox is empty *)