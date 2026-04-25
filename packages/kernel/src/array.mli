type 'value t = 'value array

(** Use `make count value` to allocate an array filled with repeated aliases of `value`. *)
val make: count:int -> value:'value -> 'value t

(**
   Use `init count builder` to allocate an array and call `builder` once per index from left to
   right.
*)
val init: count:int -> fn:(int -> 'value) -> 'value t

val length: 'value t -> int

val get: 'value t -> at:int -> 'value option

val get_unchecked: 'value t -> at:int -> 'value

val set: 'value t -> at:int -> value:'value -> unit

val set_unchecked: 'value t -> at:int -> value:'value -> unit

(** Use `copy values` to allocate a fresh array with the same elements. *)
val clone: 'value t -> 'value t

(** Use `blit source source_offset dest dest_offset len` to copy elements between arrays. *)
val blit: 'value t -> src_offset:int -> dst:'value t -> dst_offset:int -> len:int -> unit

(** Use `sub values offset len` to copy the selected slice into a fresh array. *)
val sub: 'value t -> offset:int -> len:int -> 'value t

(** Use `for_each fn values` to visit each array element from left to right. *)
val for_each: 'value t -> fn:('value -> unit) -> unit

(** Use `map fn values` to allocate a fresh array of mapped elements in the original order. *)
val map: 'value t -> fn:('value -> 'mapped) -> 'mapped t

(** Use `fold_left fn init values` to accumulate from left to right. *)
val fold_left: 'value t -> fn:('acc -> 'value -> 'acc) -> acc:'acc -> 'acc

(** Use `fold_right fn init values` to accumulate from right to left. *)
val fold_right: 'value t -> fn:('value -> 'acc -> 'acc) -> acc:'acc -> 'acc

(** Use `from_list values` to allocate a fresh array in the same order as `values`. *)
val from_list: 'value list -> 'value t
