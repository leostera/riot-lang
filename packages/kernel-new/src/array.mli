type 'value t = 'value array
val make: int -> 'value -> 'value t

val init: int -> (int -> 'value) -> 'value t

val length: 'value t -> int

val get: 'value t -> int -> 'value

val set: 'value t -> int -> 'value -> unit

val iter: ('value -> unit) -> 'value t -> unit

val map: ('value -> 'mapped) -> 'value t -> 'mapped t

val fold_left: ('acc -> 'value -> 'acc) -> 'acc -> 'value t -> 'acc

val of_list: 'value list -> 'value t
