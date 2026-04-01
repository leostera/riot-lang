open Std

(** Arbitrary module for complete value specifications.
    
    An arbitrary combines a generator, optional shrinker, and optional printer
    into a complete specification for generating and presenting test values.
*)
(** {1 Core Types} *)

type 'value t = {
  gen: 'value Generator.t;
  shrink: 'value Shrinker.t option;
  print: 'value Printer.t option;
  small: ('value -> int) option;
}

(** An arbitrary combines:
    - [gen]: how to generate random values
    - [shrink]: how to shrink counter-examples (optional)
    - [print]: how to display values (optional)
    - [small]: size metric for values (optional) *)
(** {1 Building Arbitraries} *)

val make:
  ?shrink:'value Shrinker.t ->
  ?print:'value Printer.t ->
  ?small:('value -> int) ->
  'value Generator.t ->
  'value t

(** [make ~shrink ~print ~small gen] creates an arbitrary from components. *)
(** {1 Primitive Arbitraries} *)

val int: int t

val int32: int32 t

val int64: int64 t

val bool: bool t

val float: float t

val char: char t

val rune: Unicode.Rune.t t

val string: string t

(** {1 Collection Arbitraries} *)
val list: 'value t -> 'value list t

val array: 'value t -> 'value array t

val vector: 'value t -> 'value Collections.Vector.t t

val hashmap: 'key t -> 'value t -> ('key, 'value) Collections.HashMap.t t

val hashset: 'value t -> 'value Collections.HashSet.t t

val queue: 'value t -> 'value Collections.Queue.t t

val deque: 'value t -> 'value Collections.Deque.t t

val heap: 'value t -> 'value Collections.Heap.t t

(** {1 Tuple Arbitraries} *)
val pair: 'a t -> 'b t -> ('a * 'b) t

val triple: 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

(** {1 Option & Result Arbitraries} *)
val option: 'value t -> 'value option t

val result: 'value t -> 'error t -> ('value, 'error) result t

(** {1 Combinators} *)
val map: ('a -> 'b) -> ('b -> 'a) -> 'a t -> 'b t

(** [map f f_inv arb] transforms an arbitrary. *)
val map_gen: 'value Generator.t -> 'value t -> 'value t

(** [map_gen gen arb] replaces the generator in an arbitrary. *)
