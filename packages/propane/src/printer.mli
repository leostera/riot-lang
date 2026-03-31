open Std

(** Printer module for pretty-printing values in counter-examples.
    
    Printers convert values to human-readable strings for displaying
    test failures.
*)
(** {1 Core Types} *)

type 'value t = 'value -> string
(** A printer that converts values to strings. *)
(** {1 Primitive Printers} *)

val int : int t

val int32 : int32 t

val int64 : int64 t

val float : ?precision:int -> float t

val bool : bool t

val char : char t

val rune : Unicode.Rune.t t

val string : string t

(** {1 Collection Printers} *)
val list : 'value t -> 'value list t

val array : 'value t -> 'value array t

val vector : 'value t -> 'value Collections.Vector.t t

val hashmap : 'key t -> 'value t -> ('key, 'value) Collections.HashMap.t t

val hashset : 'value t -> 'value Collections.HashSet.t t

val queue : 'value t -> 'value Collections.Queue.t t

val deque : 'value t -> 'value Collections.Deque.t t

val heap : 'value t -> 'value Collections.Heap.t t

(** {1 Tuple Printers} *)
val pair : 'a t -> 'b t -> ('a * 'b) t

val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

(** {1 Option & Result Printers} *)
val option : 'value t -> 'value option t

val result : 'value t -> 'error t -> ('value, 'error) result t
