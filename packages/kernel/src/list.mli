type 'value t = 'value list
val length: 'value list -> int

val compare_lengths: 'left list -> 'right list -> int

val is_empty: 'value list -> bool

val hd: 'value list -> 'value

val tl: 'value list -> 'value list

val nth: 'value list -> int -> 'value

val append: 'value list -> 'value list -> 'value list

val rev_append: 'value list -> 'value list -> 'value list

val rev: 'value list -> 'value list

val init: int -> (int -> 'value) -> 'value list

val concat: 'value list list -> 'value list

val map: ('value -> 'mapped) -> 'value list -> 'mapped list

val mapi: (int -> 'value -> 'mapped) -> 'value list -> 'mapped list

val rev_map: ('value -> 'mapped) -> 'value list -> 'mapped list

val iter: ('value -> unit) -> 'value list -> unit

val iteri: (int -> 'value -> unit) -> 'value list -> unit

val fold_left: ('acc -> 'value -> 'acc) -> 'acc -> 'value list -> 'acc

val fold_right: ('value -> 'acc -> 'acc) -> 'value list -> 'acc -> 'acc

val iter2: ('left -> 'right -> unit) -> 'left list -> 'right list -> unit

val map2: ('left -> 'right -> 'mapped) -> 'left list -> 'right list -> 'mapped list

val rev_map2: ('left -> 'right -> 'mapped) -> 'left list -> 'right list -> 'mapped list

val fold_left2: ('acc -> 'left -> 'right -> 'acc) -> 'acc -> 'left list -> 'right list -> 'acc

val fold_right2: ('left -> 'right -> 'acc -> 'acc) -> 'left list -> 'right list -> 'acc -> 'acc

val for_all2: ('left -> 'right -> bool) -> 'left list -> 'right list -> bool

val exists2: ('left -> 'right -> bool) -> 'left list -> 'right list -> bool

val exists: ('value -> bool) -> 'value list -> bool

val mem: 'value -> 'value list -> bool

val assoc: 'key -> ('key * 'value) list -> 'value

val assoc_opt: 'key -> ('key * 'value) list -> 'value option

val remove_assoc: 'key -> ('key * 'value) list -> ('key * 'value) list

val find: ('value -> bool) -> 'value list -> 'value

val find_opt: ('value -> bool) -> 'value list -> 'value option

val find_map: ('value -> 'mapped option) -> 'value list -> 'mapped option

val filter: ('value -> bool) -> 'value list -> 'value list

val filter_map: ('value -> 'mapped option) -> 'value list -> 'mapped list

val sort: ('value -> 'value -> int) -> 'value list -> 'value list

val sort_uniq: ('value -> 'value -> int) -> 'value list -> 'value list

val combine: 'left list -> 'right list -> ('left * 'right) list
