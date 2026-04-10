open Std

(** Resolved entity path carried by checker bindings and traces. *)
type t

(** Empty unresolved path. *)
val empty: t

(** Build an unresolved entity path from surface segments. *)
val of_name: string -> t

val of_segments: string list -> t

val of_string: string -> t

val of_surface_path: SurfacePath.t -> t

(** Build one resolved binder entity with its current visible surface path. *)
val resolved: binding_id:BindingId.t -> surface_path:SurfacePath.t -> t

(** Build one resolved binder entity from the binder name alone. *)
val of_binding_id: BindingId.t -> t

(** Recover the semantic binder identity when this entity is resolved. *)
val binding_id: t -> BindingId.t option

(** Recover the current visible surface path for this entity. *)
val surface_path: t -> SurfacePath.t

val is_empty: t -> bool

val is_bare: t -> bool

val bare_name: t -> string option

val to_segments: t -> string list

val to_string: t -> string

val equal: t -> t -> bool

val compare: t -> t -> int

(** Build an unresolved child entity path under the current surface path. *)
val append_name: t -> string -> t

(** Preserve the resolved binder while prepending one visible surface segment. *)
val prepend_name: string -> t -> t

(** Build an unresolved entity path from two surface-compatible entity paths. *)
val append_path: t -> t -> t

(** Preserve the resolved binder while prefixing the visible surface path. *)
val qualify: prefix:SurfacePath.t -> t -> t

val last_name: t -> string option

val uncons: t -> (string * t) option

val split_last: t -> (t * string) option

(** Preserve the resolved binder while stripping one visible surface prefix. *)
val strip_prefix: prefix:SurfacePath.t -> t -> t option

val prefixes: t -> t list
