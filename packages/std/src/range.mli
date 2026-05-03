(**
   Order-aware intervals.

   [Range] represents intervals over any ordered value type.

   Each range stores the ordering chosen at construction time, so later
   operations such as {!contains}, {!is_empty}, or {!intersect} do not need the
   caller to thread a compare function repeatedly.

   Bounds may be inclusive, exclusive, or unbounded.

   ## Examples

   ```ocaml
   open Std

   let workday = Range.closed_open ~compare:Int.compare 9 17 in
   assert (Range.contains workday 9);
   assert (not (Range.contains workday 17));
   ```

   ```ocaml
   let descending = Range.closed ~compare:(fun left right -> Int.compare right left) 5 1 in
   assert (Range.contains descending 3);
   assert (not (Range.contains descending 0));
   ```

   Binary operations such as {!intersect}, {!overlaps}, and {!hull} assume
   both input ranges were built with compatible ordering semantics. The left
   range's comparator is used for those operations.
*)
open Global

(**
   A single interval endpoint.

   - [Included x] includes the endpoint value itself.
   - [Excluded x] excludes the endpoint value itself.
   - [Unbounded] leaves that side of the interval open.
*)
type 'a bound =
  | Included of 'a
  | Excluded of 'a
  | Unbounded
(**
   An interval over ordered values.

   The stored comparator determines how endpoints and membership are evaluated.
   Two ranges can only be meaningfully combined if they were built with
   compatible ordering semantics.
*)
type 'a t

(** Build a range from explicit bounds and a comparator. *)
val make: lower:'a bound -> upper:'a bound -> compare:('a -> 'a -> Order.t) -> 'a t

(** An unbounded range that contains every value in the chosen ordering. *)
val all: compare:('a -> 'a -> Order.t) -> 'a t

(** A closed range containing exactly one endpoint value. *)
val singleton: compare:('a -> 'a -> Order.t) -> 'a -> 'a t

(** Build a fully closed range [[lower,upper]]. *)
val closed: compare:('a -> 'a -> Order.t) -> 'a -> 'a -> 'a t

(** Build a fully open range [(lower,upper)]. *)
val open_: compare:('a -> 'a -> Order.t) -> 'a -> 'a -> 'a t

(** Build a half-open range [[lower,upper)]. *)
val closed_open: compare:('a -> 'a -> Order.t) -> 'a -> 'a -> 'a t

(** Build a half-open range [(lower,upper]]. *)
val open_closed: compare:('a -> 'a -> Order.t) -> 'a -> 'a -> 'a t

(** Build a lower-bounded range [[lower,..)]. *)
val at_least: compare:('a -> 'a -> Order.t) -> 'a -> 'a t

(** Build a lower-bounded range [(lower,..)]. *)
val greater_than: compare:('a -> 'a -> Order.t) -> 'a -> 'a t

(** Build an upper-bounded range [(..,upper]]. *)
val at_most: compare:('a -> 'a -> Order.t) -> 'a -> 'a t

(** Build an upper-bounded range [(..,upper)]. *)
val less_than: compare:('a -> 'a -> Order.t) -> 'a -> 'a t

(** Return the stored lower bound. *)
val lower_bound: 'a t -> 'a bound

(** Return the stored upper bound. *)
val upper_bound: 'a t -> 'a bound

(** Return the comparator captured when the range was built. *)
val compare_values: 'a t -> 'a -> 'a -> Order.t

(** Check whether a value lies inside the interval. *)
val contains: 'a t -> 'a -> bool

(**
   Check whether the interval contains no values under its stored ordering.

   This is order-based rather than step-based. For example, [(1,2)] is not
   treated as empty just because [int] is discrete; it is only empty when the
   chosen ordering makes the bounds collapse or cross.
*)
val is_empty: 'a t -> bool

(**
   Check whether two ranges share at least one value.

   The left-hand range's comparator is used.
*)
val overlaps: 'a t -> 'a t -> bool

(**
   Compute the overlapping portion of two ranges, if any.

   The left-hand range's comparator is used.
*)
val intersect: 'a t -> 'a t -> 'a t option

(**
   Compute the smallest range that contains both input ranges.

   Empty ranges collapse away: [hull empty other] returns [other]. The
   left-hand range's comparator is used for the merged bounds.
*)
val hull: 'a t -> 'a t -> 'a t

(**
   Render a range with interval notation such as [[1,5)], [(..,10]], or
   [(..)].
*)
val to_string: ('a -> string) -> 'a t -> string
