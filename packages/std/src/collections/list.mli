type 'value t = 'value list
val length: 'value list -> int

val compare_lengths: left:'left list -> right:'right list -> int

val is_empty: 'value list -> bool

val append: 'value list -> 'value list -> 'value list

val reverse: 'value list -> 'value list

val rev: 'value list -> 'value list

val reverse_append: 'value list -> 'value list -> 'value list

val concat: 'value list list -> 'value list

val init: count:int -> fn:(int -> 'value) -> 'value list

val head: 'value list -> 'value option

val tail: 'value list -> 'value list

val get: 'value list -> at:int -> 'value option

val get_unchecked: 'value list -> at:int -> 'value

(** Take at most `len` items rom a list. Returns Empty on empty list otherwise
    returns at most Len Elements. If lem is larger can the input list returns
    the whole list.
*)
val take: 'value list -> len:int -> 'value list

val map: 'value list -> fn:('value -> 'mapped) -> 'mapped list

val flat_map: 'value list -> fn:('value -> 'mapped list) -> 'mapped list

val for_each: 'value list -> fn:('value -> unit) -> unit

val iter: ('value -> unit) -> 'value list -> unit

val iteri: (int -> 'value -> unit) -> 'value list -> unit

val fold_left: 'value list -> acc:'acc -> fn:('acc -> 'value -> 'acc) -> 'acc

val fold_right: 'value list -> acc:'acc -> fn:('value -> 'acc -> 'acc) -> 'acc

val enumerate: 'value list -> (int * 'value) list

val all: 'value list -> fn:('value -> bool) -> bool

val for_all: ('value -> bool) -> 'value list -> bool

val any: 'value list -> fn:('value -> bool) -> bool

val exists: ('value -> bool) -> 'value list -> bool

val contains: 'value list -> value:'value -> bool

val mem: 'value -> 'value list -> bool

val find: 'value list -> fn:('value -> bool) -> 'value option

val find_opt: ('value -> bool) -> 'value list -> 'value option

val assoc_opt: 'key -> ('key * 'value) list -> 'value option

val filter: 'value list -> fn:('value -> bool) -> 'value list

val filter_map: 'value list -> fn:('value -> 'mapped option) -> 'mapped list

val sort: 'value list -> compare:('value -> 'value -> int) -> 'value list

val unique: 'value list -> compare:('value -> 'value -> int) -> 'value list

val zip: 'left list -> 'right list -> ('left * 'right) list

val unzip: ('left * 'right) list -> ('left list) * ('right list)
