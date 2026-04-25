type 'value t = 'value list

val length: 'value list -> int

val is_empty: 'value list -> bool

val append: 'value list -> 'value list -> 'value list

val reverse: 'value list -> 'value list

val concat: 'value list list -> 'value list

val init: count:int -> fn:(int -> 'value) -> 'value list

val map: 'value list -> fn:('value -> 'mapped) -> 'mapped list

val for_each: 'value list -> fn:('value -> unit) -> unit

val fold_left: 'value list -> acc:'acc -> fn:('acc -> 'value -> 'acc) -> 'acc

val fold_right: 'value list -> acc:'acc -> fn:('value -> 'acc -> 'acc) -> 'acc

val exists: 'value list -> fn:('value -> bool) -> bool

val contains: 'value list -> value:'value -> bool

val head: 'value list -> 'value option

val tail: 'value list -> 'value list

val get: 'value list -> at:int -> 'value option

val get_unchecked: 'value list -> at:int -> 'value

val find: 'value list -> fn:('value -> bool) -> 'value option

val filter: 'value list -> fn:('value -> bool) -> 'value list

val sort: 'value list -> compare:('value -> 'value -> Order.t) -> 'value list

val unique: 'value list -> compare:('value -> 'value -> Order.t) -> 'value list
