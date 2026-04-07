open Std

(** Protocol descriptor used by Blink transports. *)
module type Intf = sig
  (** Protocol name used for logging and negotiation. *)
  val name: string
end

(** HTTP/1 protocol descriptor. *)
module Http1: Intf
