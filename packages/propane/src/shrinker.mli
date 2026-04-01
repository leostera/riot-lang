open Std

(** Shrinker module for reducing counter-examples to minimal failing cases.
    
    Inspired by PropEr's proper_shrink.erl, shrinkers produce a lazy sequence
    of "smaller" values using Std.Iter.Iterator.
    
    IMPORTANT: A shrinker must NEVER return its input as a shrinking candidate,
    as this would cause infinite loops during shrinking.
    
    {1 Creating Custom Shrinkers}
    
    A shrinker is simply a function ['value -> 'value list] that takes a value
    and returns a list of "smaller" candidate values. For example:
    
    {[
      (* Shrink points towards the origin *)
      let point_shrinker point =
        let x_shrunk = Shrinker.shrink (Shrinker.towards 0) point.x in
        let y_shrunk = Shrinker.shrink (Shrinker.towards 0) point.y in
        
        (* Combine shrinking on both axes *)
        let x_only = List.map (fun x -> { x; y = point.y }) x_shrunk in
        let y_only = List.map (fun y -> { x = point.x; y }) y_shrunk in
        x_only @ y_only
    ]}
*)
(** {1 Core Types} *)

type 'value t = 'value -> 'value list

(** A shrinker that takes a value and produces a list of smaller candidate values.
    
    The returned list should contain values that are "smaller" according to some
    metric, and should NOT include the input value itself. *)
(** {1 Basic Shrinkers} *)

val nil: 'value t

(** [nil] produces no shrinking candidates. Use for types with no meaningful shrinking. *)
val towards: int -> int t

(** [towards target] shrinks integer values towards [target]. *)
(** {1 Primitive Shrinkers} *)

val int: int t

(** Shrinks integers towards 0. *)
val int_towards: int -> int t

(** [int_towards n] shrinks integers towards [n]. *)
val int32: int32 t

(** Shrinks int32 towards 0l. *)
val int64: int64 t

(** Shrinks int64 towards 0L. *)
val float: float t

(** Shrinks floats towards 0.0. *)
val bool: bool t

(** Shrinks booleans towards false. *)
val char: char t

(** Shrinks characters towards 'a'. *)
val rune: Unicode.Rune.t t

(** Shrinks Unicode runes towards U+0000. *)
val string: string t

(** Shrinks strings by:
    - Making them shorter
    - Simplifying characters *)
(** {1 Collection Shrinkers} *)

val list: 'value t -> 'value list t

(** [list elem_shrinker] shrinks lists by:
    - Removing elements
    - Shrinking individual elements *)
val array: 'value t -> 'value array t

(** Shrinks arrays similarly to lists. *)
val vector: 'value t -> 'value Collections.Vector.t t

(** Shrinks Vectors. *)
val hashmap: 'key t -> 'value t -> ('key, 'value) Collections.HashMap.t t

(** Shrinks HashMaps by removing entries. *)
val hashset: 'value t -> 'value Collections.HashSet.t t

(** Shrinks HashSets by removing elements. *)
val queue: 'value t -> 'value Collections.Queue.t t

(** Shrinks Queues. *)
val deque: 'value t -> 'value Collections.Deque.t t

(** Shrinks Deques. *)
val heap: 'value t -> 'value Collections.Heap.t t

(** Shrinks Heaps. *)
(** {1 Tuple Shrinkers} *)

val pair: 'a t -> 'b t -> ('a * 'b) t

(** Shrinks pairs by shrinking each component. *)
val triple: 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

(** Shrinks triples. *)
(** {1 Option & Result Shrinkers} *)

val option: 'value t -> 'value option t

(** [option elem_shrinker] shrinks:
    - Some x to None
    - Some x to Some x' where x' is a shrunk version of x *)
val result: 'value t -> 'error t -> ('value, 'error) result t

(** Shrinks results by shrinking Ok or Error values. *)
(** {1 Combinators} *)

val map: ('a -> 'b) -> ('b -> 'a) -> 'a t -> 'b t

(** [map f f_inv shrinker] transforms a shrinker.
    [f_inv] must be the inverse of [f] for shrinking to work correctly. *)
val filter: ('value -> bool) -> 'value t -> 'value t

(** [filter pred shrinker] only keeps shrinking candidates that satisfy [pred]. *)
(** {1 Low-level Interface} *)

val shrink: 'value t -> 'value -> 'value list

(** [shrink shrinker value] produces a list of smaller values. *)
