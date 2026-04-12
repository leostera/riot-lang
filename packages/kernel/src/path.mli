type t = string

(** Use `from_string value` to treat `value` as a path without validation or normalization. *)
val from_string: string -> t

(** Use `to_string value` to recover the path text exactly as stored. *)
val to_string: t -> string

(** Use `join left right` to concatenate two path segments.

    Empty sides are treated as identity. A separator is inserted only when needed. *)
val join: t -> t -> t

(** Use `left / right` as infix sugar for `join left right`. *)
val ( / ): t -> t -> t
