type 'value t
val create: unit -> 'value t

val create_max: unit -> 'value t

val create_with: compare:('value -> 'value -> int) -> unit -> 'value t

val from_list: 'value list -> 'value t

val from_list_with: compare:('value -> 'value -> int) -> 'value list -> 'value t

val push: 'value t -> value:'value -> unit

val pop: 'value t -> 'value option

val pop_unchecked: 'value t -> 'value

val peek: 'value t -> 'value option

val peek_unchecked: 'value t -> 'value

val length: 'value t -> int

val is_empty: 'value t -> bool

val clear: 'value t -> unit

val to_list: 'value t -> 'value list

val to_list_unordered: 'value t -> 'value list

val for_each: 'value t -> fn:('value -> unit) -> unit

val fold_left: 'value t -> init:'acc -> fn:('acc -> 'value -> 'acc) -> 'acc

val iter: 'value t -> 'value Iter.Iterator.t

val mut_iter: 'value t -> 'value Iter.MutIterator.t
