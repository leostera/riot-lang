open Std
open Model

(** Query-local region bookkeeping for type-node levels. *)
type t
(** One active inference region and the type nodes created under it. *)
type frame

(** Create empty region state rooted at level 0. *)
val create: unit -> t

(** Allocate one fresh inference variable at the current region level and
    register it with the active region frame, if any. *)
val fresh_var: t -> int -> TypeRepr.t

(** Register one newly created non-variable type node with the active frame. *)
val track_node: t -> TypeRepr.t -> TypeRepr.t

(** Re-register one node in the pool corresponding to its current level. *)
val add_to_pool: t -> level:int -> TypeRepr.t -> TypeRepr.t

(** Return the current region level. *)
val current_level: t -> int

(** Allocate one fresh query-local traversal mark generation. *)
val next_mark: t -> int

(** Mark each root and return the generation used for the traversal. *)
val mark_roots: t -> TypeRepr.t list -> int

(** Iterate nodes still owned by this frame. *)
val iter_owned_nodes: frame -> (TypeRepr.t -> unit) -> unit

(** Run a computation in one nested inference region. *)
val with_region: t -> (frame -> 'a) -> 'a

(** Return the outer region level captured by this frame. *)
val boundary_level: frame -> int

(** Promote every reachable local variable in the given type to the generic
    solver level. *)
val generalize_reachable_vars: t -> frame -> TypeRepr.t list -> unit

(** Return the reachable local variables for this region in first-occurrence
    order from the given type. *)
val local_reachable_vars: t -> frame -> TypeRepr.t -> int list
