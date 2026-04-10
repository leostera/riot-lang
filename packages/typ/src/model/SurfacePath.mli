open Std

type module_name = string
type t
val empty: t

val is_empty: t -> bool

val is_bare: t -> bool

val bare_name: t -> string option

val of_name: string -> t

val of_segments: string list -> t

val of_string: string -> t

val to_segments: t -> string list

val to_string: t -> string

val equal: t -> t -> bool

val compare: t -> t -> int

val append_name: t -> string -> t

val prepend_name: string -> t -> t

val append_path: t -> t -> t

val last_name: t -> string option

val uncons: t -> (string * t) option

val split_last: t -> (t * string) option

val strip_prefix: prefix:t -> t -> t option

val prefixes: t -> t list
