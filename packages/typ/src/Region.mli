open Std

(** Query-local region bookkeeping for inference-variable levels. *)
type t
(** One active inference region and the variables created under it. *)
type frame

(** Create empty region state rooted at level 0. *)
val create: unit -> t

(** Allocate one fresh inference variable at the current region level and
    register it with the active region frame, if any. *)
val fresh_var: t -> int -> TypeRepr.t

(** Run a computation in one nested inference region. *)
val with_region: t -> (frame -> 'a) -> 'a

(** Return the reachable local variables for this region in first-occurrence
    order from the given type. *)
val local_reachable_vars: t -> frame -> TypeRepr.t -> int list
