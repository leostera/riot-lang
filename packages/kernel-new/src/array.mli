type 'value t = 'value array

(** Use `make count value` to allocate an array filled with repeated aliases of `value`. *)
val make: int -> 'value -> 'value t

(** Use `init count builder` to allocate an array and call `builder` once per index from left to
    right. *)
val init: int -> (int -> 'value) -> 'value t

val length: 'value t -> int

val get: 'value t -> int -> 'value

val set: 'value t -> int -> 'value -> unit

(** Use `iter fn values` to visit each array element from left to right. *)
val iter: ('value -> unit) -> 'value t -> unit

(** Use `map fn values` to allocate a fresh array of mapped elements in the original order. *)
val map: ('value -> 'mapped) -> 'value t -> 'mapped t

(** Use `fold_left fn init values` to accumulate from left to right. *)
val fold_left: ('acc -> 'value -> 'acc) -> 'acc -> 'value t -> 'acc

(** Use `of_list values` to allocate a fresh array in the same order as `values`. *)
val of_list: 'value list -> 'value t
