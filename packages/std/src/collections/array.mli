type 'value t = 'value array

val make: count:int -> value:'value -> 'value t

val init: count:int -> fn:(int -> 'value) -> 'value t

val length: 'value t -> int

val get: 'value t -> at:int -> 'value option

val get_unchecked: 'value t -> at:int -> 'value

val set: 'value t -> at:int -> value:'value -> unit

val set_unchecked: 'value t -> at:int -> value:'value -> unit

val clone: 'value t -> 'value t

val blit: 'value t -> src_offset:int -> dst:'value t -> dst_offset:int -> len:int -> unit

val sub: 'value t -> offset:int -> len:int -> 'value t

val for_each: 'value t -> fn:('value -> unit) -> unit

val map: 'value t -> fn:('value -> 'mapped) -> 'mapped t

val fold_left: 'value t -> init:'acc -> fn:('acc -> 'value -> 'acc) -> 'acc

val fold_right: 'value t -> init:'acc -> fn:('value -> 'acc -> 'acc) -> 'acc

val from_list: 'value list -> 'value t

(** Use `from_list values` as the conventional alias for `from_list values`. *)
val from_list: 'value list -> 'value t

(** Use `to_list values` to copy the array contents into a list in the same order. *)
val to_list: 'value t -> 'value list

(**
   Converts this array into an immutable iterator.

   ## Examples

   ```ocaml
   let arr = [|1; 2; 3; 4; 5|] in
   arr
   |> Array.iter
   |> Iterator.map ~fn:(fun x -> x * 2)
   |> Iterator.filter ~fn:(fun x -> x > 5)
   |> Iterator.collect
   (* [6; 8; 10] *)
   ```
*)
val iter: 'value t -> 'value Iter.Iterator.t

(**
   Converts this array into a mutable iterator.

   ## Examples

   ```ocaml
   let arr = [|1; 2; 3; 4; 5|] in
   arr
   |> Array.mut_iter
   |> MutIterator.map ~fn:(fun x -> x * 2)
   |> MutIterator.filter ~fn:(fun x -> x > 5)
   |> MutIterator.collect
   (* [6; 8; 10] *)
   ```
*)
val mut_iter: 'value t -> 'value Iter.MutIterator.t

module Syntax: sig
  val get: 'value t -> int -> 'value

  val set: 'value t -> int -> 'value -> unit
end
