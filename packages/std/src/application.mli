(** Supervision tree management with dependency resolution. *)
open Global

(**
   Start a list of applications in dependency order.
   Uses topological sort with cycle detection.
*)
type t = {
  name: string;
  deps: t list;
  start: unit -> (Pid.t, exn) result;
  stop: Pid.t -> unit;
}

val start_applications: t list -> ((string * Pid.t) list, exn) result
