type 'value t
type error =
  | OutOfBoundsSet of { length: int; at: int }
val create: unit -> 'value t

val with_capacity: size:int -> 'value t

val from_list: 'value list -> 'value t

val push: 'value t -> value:'value -> unit

val pop: 'value t -> 'value option

val insert: 'value t -> at:int -> value:'value -> unit

val remove: 'value t -> at:int -> 'value option

val get: 'value t -> at:int -> 'value option

val get_unchecked: 'value t -> at:int -> 'value

val set: 'value t -> at:int -> value:'value -> (unit, error) Kernel.result

val set_unchecked: 'value t -> at:int -> value:'value -> unit

val length: 'value t -> int

val len: 'value t -> int

val is_empty: 'value t -> bool

val capacity: 'value t -> int

val clear: 'value t -> unit

val to_array: 'value t -> 'value array

val reserve: 'value t -> size:int -> unit

val for_each: 'value t -> fn:('value -> unit) -> unit

val append: 'value t -> 'value t -> unit

val split_off: 'value t -> at:int -> 'value t

val sort: 'value t -> unit

val sort_by: 'value t -> compare:('value -> 'value -> Order.t) -> unit

val reverse: 'value t -> unit

val first: 'value t -> 'value option

val last: 'value t -> 'value option

val iter: 'value t -> 'value Iter.Iterator.t

val mut_iter: 'value t -> 'value Iter.MutIterator.t
