type 'value t

val create: unit -> 'value t

val with_capacity: size:int -> 'value t

val from_list: 'value list -> 'value t

val insert: 'value t -> value:'value -> bool

val remove: 'value t -> value:'value -> bool

val contains: 'value t -> value:'value -> bool

val length: 'value t -> int

val is_empty: 'value t -> bool

val clear: 'value t -> unit

val for_each: 'value t -> fn:('value -> unit) -> unit

val fold_left: 'value t -> init:'acc -> fn:('acc -> 'value -> 'acc) -> 'acc

val to_list: 'value t -> 'value list

val union: 'value t -> 'value t -> 'value t

val intersection: 'value t -> 'value t -> 'value t

val difference: 'value t -> 'value t -> 'value t

val symmetric_difference: 'value t -> 'value t -> 'value t

val is_subset: 'value t -> 'value t -> bool

val is_superset: 'value t -> 'value t -> bool

val is_disjoint: 'value t -> 'value t -> bool

val iter: 'value t -> 'value Iter.Iterator.t

val mut_iter: 'value t -> 'value Iter.MutIterator.t
