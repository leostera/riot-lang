open Std

(**
   Shrink failing values toward simpler counter-examples.

   Use a shrinker when random generation can find a bug, but you still need a
   small, readable value to understand it. A good shrinker turns "this huge
   random structure failed" into "this tiny structure is the real problem".

   Important: a shrinker must never return the original value as one of its
   candidates, otherwise shrinking can loop forever.

   Example:
   ```ocaml
   let point_shrinker point =
     let xs =
       Shrinker.shrink (Shrinker.towards 0) point.x
       |> List.map (fun x -> { point with x })
     in
     let ys =
       Shrinker.shrink (Shrinker.towards 0) point.y
       |> List.map (fun y -> { point with y })
     in
     xs @ ys
   ```
*)
type 'value t = 'value -> 'value list

(** Function that proposes smaller versions of a value. *)

(**
   Shrinker that produces no candidates.

   Use [nil] for types where shrinking is either impossible or not worth the
   noise in failure reports.
*)
val nil: 'value t

(**
   Shrink an integer toward a target value.

   Example:
   ```ocaml
   Shrinker.shrink (Shrinker.towards 0) 10 = [5; 0]
   ```
*)
val towards: int -> int t

(** Shrink integers toward [0]. *)
val int: int t

(** Shrink integers toward the given target. *)
val int_towards: int -> int t

(** Shrink [int32] values toward [0l]. *)
val int32: int32 t

(** Shrink [int64] values toward [0L]. *)
val int64: int64 t

(** Shrink floating-point values toward [0.0]. *)
val float: float t

(** Shrink booleans toward [false]. *)
val bool: bool t

(** Shrink characters toward a simpler representative. *)
val char: char t

(** Shrink Unicode runes toward [U+0000]. *)
val rune: Unicode.Rune.t t

(** Shrink strings by trying shorter and simpler text. *)
val string: string t

(**
   Shrink lists by removing elements and shrinking remaining values.

   Use this for ordered collections where the number of elements is often part
   of the bug.
*)
val list: 'value t -> 'value list t

(** Shrink arrays using the same strategy as {!list}. *)
val array: 'value t -> 'value array t

(** Shrink vectors. *)
val vector: 'value t -> 'value Collections.Vector.t t

(** Shrink hash maps by removing entries and shrinking stored values when possible. *)
val hashmap: 'key t -> 'value t -> ('key, 'value) Collections.HashMap.t t

(** Shrink hash sets by removing elements. *)
val hashset: 'value t -> 'value Collections.HashSet.t t

(** Shrink queues. *)
val queue: 'value t -> 'value Collections.Queue.t t

(** Shrink deques. *)
val deque: 'value t -> 'value Collections.Deque.t t

(** Shrink heaps. *)
val heap: 'value t -> 'value Collections.Heap.t t

(**
   Shrink pairs by shrinking either component.

   Example:
   ```ocaml
   let shrink_pair = Shrinker.pair Shrinker.int Shrinker.int
   ```
*)
val pair: 'a t -> 'b t -> ('a * 'b) t

(** Shrink triples by shrinking one component at a time. *)
val triple: 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

(**
   Shrink optional values.

   [Some x] may shrink to [None] or to [Some x'].
*)
val option: 'value t -> 'value option t

(** Shrink [Ok] and [Error] payloads without changing which branch they use. *)
val result: 'value t -> 'error t -> ('value, 'error) result t

(**
   Transform a shrinker through an isomorphism.

   Use [map to_ from shrinker] when you have a shrinker for a simpler
   representation and want to reuse it for a wrapper type.
*)
val map: ('a -> 'b) -> ('b -> 'a) -> 'a t -> 'b t

(** Keep only shrinking candidates that satisfy [pred]. *)
val filter: ('value -> bool) -> 'value t -> 'value t

(**
   Run a shrinker on a value and collect the candidate results.

   Example return values:
   - [Shrinker.shrink Shrinker.int 4] may produce candidates like [2; 0].
   - [Shrinker.shrink Shrinker.nil "abc"] returns the empty list.
*)
val shrink: 'value t -> 'value -> 'value list
