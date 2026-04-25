open Std

(** Transport backend used to establish Blink connections. *)
module type Intf = sig
  (** Transport name used for logging and selection. *)
  val name: string

  (** Connect using the given stream address and target URI. *)
  val connect: Net.Addr.stream_addr -> Net.Uri.t -> (Connection.t, Error.t) result
end

(** Plain TCP transport. *)
module Tcp : Intf

(** TLS transport. *)
module Tls : Intf

(** Connect to the URI using the appropriate transport. *)
val connect: Net.Uri.t -> (Connection.t, Error.t) result
