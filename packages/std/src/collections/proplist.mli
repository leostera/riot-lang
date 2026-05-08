type ('key, 'value) t = ('key * 'value) list

val empty: ('key, 'value) t

val is_empty: ('key, 'value) t -> bool

val length: ('key, 'value) t -> int

val from_list: ('key * 'value) list -> ('key, 'value) t

val from_list: ('key * 'value) list -> ('key, 'value) t

val to_list: ('key, 'value) t -> ('key * 'value) list

(**
   Returns the first matching binding for [key]. Property lists preserve duplicate
   keys, so this is the leftmost binding.
*)
val get: ('key, 'value) t -> key:'key -> 'value option

(** Returns every value bound to [key], preserving left-to-right binding order. *)
val get_all: ('key, 'value) t -> key:'key -> 'value list

val has_key: ('key, 'value) t -> key:'key -> bool

(** Prepends a new binding, preserving any existing bindings for [key]. *)
val add: ('key, 'value) t -> key:'key -> value:'value -> ('key, 'value) t

(**
   Replaces all bindings for [key] with a single binding, preserving the position
   of the first matching binding. If [key] is not present, appends the binding.
*)
val set: ('key, 'value) t -> key:'key -> value:'value -> ('key, 'value) t

(** Removes every binding for [key]. *)
val remove: ('key, 'value) t -> key:'key -> ('key, 'value) t

val keys: ('key, 'value) t -> 'key list

val values: ('key, 'value) t -> 'value list

val for_each: ('key, 'value) t -> fn:('key -> 'value -> unit) -> unit

val fold_left: ('key, 'value) t -> init:'acc -> fn:('acc -> 'key -> 'value -> 'acc) -> 'acc

val iter: ('key, 'value) t -> ('key * 'value) Iter.Iterator.t
