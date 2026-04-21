type 'value t
val create: unit -> 'value t

val with_capacity: size:int -> 'value t

val from_list: 'value list -> 'value t

val push: 'value t -> value:'value -> unit

val pop: 'value t -> 'value option

val front: 'value t -> 'value option

val length: 'value t -> int

val is_empty: 'value t -> bool

val clear: 'value t -> unit

val for_each: 'value t -> fn:('value -> unit) -> unit

val fold_left: 'value t -> init:'acc -> fn:('acc -> 'value -> 'acc) -> 'acc

val to_list: 'value t -> 'value list

val contains: 'value t -> value:'value -> bool

val append: 'value t -> 'value t -> unit

val transfer: src:'value t -> dst:'value t -> unit

val iter: 'value t -> 'value Iter.Iterator.t

val mut_iter: 'value t -> 'value Iter.MutIterator.t
