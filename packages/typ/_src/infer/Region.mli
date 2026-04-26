open Std
open Model

type t
type frame
val create: unit -> t

val current_level: t -> int

val next_mark: t -> int

val track_node: t -> TypeRepr.t -> TypeRepr.t

val add_to_pool: t -> level:int -> TypeRepr.t -> TypeRepr.t

val with_region: t -> (frame -> 'a) -> 'a

val with_region_finalize: t -> finalize:(frame -> 'a -> 'b) -> (frame -> 'a) -> 'b

val mark_roots: t -> TypeRepr.t list -> int

val iter_owned_nodes: frame -> (TypeRepr.t -> unit) -> unit

val boundary_level: frame -> int

val generalize_reachable_vars: t -> frame -> TypeRepr.t list -> unit

val local_reachable_vars: t -> frame -> TypeRepr.t -> int list
