(** Application - Supervision tree management with dependency resolution *)

module rec R : sig
  module type Spec = sig
    val name : string
    (** Application name *)
    
    val deps : (module R.Spec) list
    (** Application dependencies *)
    
    val start : unit -> (Miniriot.Pid.t, exn) result
    (** Start the application, returning the root supervisor PID *)
    
    val stop : Miniriot.Pid.t -> unit
    (** Stop the application *)
  end
end
include module type of R

type t = (module Spec)
(** An application is a first-class module *)

val start_applications : t list -> ((string * Miniriot.Pid.t) list, exn) result
(** Start a list of applications in dependency order.
    Uses topological sort with cycle detection. *)
