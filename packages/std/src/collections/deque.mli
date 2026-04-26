type 'value t
val create: unit -> 'value t

val with_capacity: size:int -> 'value t

val from_list: 'value list -> 'value t

val push_front: 'value t -> value:'value -> unit

val push_back: 'value t -> value:'value -> unit

val insert: 'value t -> at:int -> value:'value -> unit

val pop_front: 'value t -> 'value option

val pop_back: 'value t -> 'value option

val remove: 'value t -> at:int -> 'value option

val clear: 'value t -> unit

val front: 'value t -> 'value option

val back: 'value t -> 'value option

val get: 'value t -> at:int -> 'value option

val length: 'value t -> int

val is_empty: 'value t -> bool

val capacity: 'value t -> int

val for_each: 'value t -> fn:('value -> unit) -> unit

val fold_left: 'value t -> init:'acc -> fn:('acc -> 'value -> 'acc) -> 'acc

val to_list: 'value t -> 'value list

val contains: 'value t -> value:'value -> bool

val append: 'value t -> 'value t -> unit

val split_off: 'value t -> at:int -> 'value t

val iter: 'value t -> 'value Iter.Iterator.t

val mut_iter: 'value t -> 'value Iter.MutIterator.t
