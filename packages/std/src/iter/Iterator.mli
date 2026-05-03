(**
   Immutable iteration protocol.

   Immutable iterator protocol for lazy sequence processing. Each call
   to [next] returns a new iterator, leaving the original unchanged.

   ## Examples

   Creating a custom iterator:

   ```ocaml
   open Std

   module RangeIter = struct
     type state = { current : int; stop : int }
     type item = int

     let next state =
       if state.current >= state.stop then
         (None, state)
       else
         (Some state.current, { state with current = state.current + 1 })

     let size state = max 0 (state.stop - state.current)
   end

   let range start stop =
     let module I = RangeIter in
     Iterator.make (module I) { I.current = start; I.stop }

   let iter = range 0 5 in
   let items = Iterator.to_list iter
   ```
*)
module type Intf = sig
  type state
  type item

  val next: state -> item option * state

  val size: state -> int
end

type ('item, 'state) iter = (module Intf with type item = 'item and type state = 'state)
type 'item t

val make: ('item, 'state) iter -> 'state -> 'item t

val next: 'item t -> 'item option * 'item t

val size: 'item t -> int

val to_list: 'item t -> 'item list

val map: 'a t -> fn:('a -> 'b) -> 'b t

val filter: 'a t -> fn:('a -> bool) -> 'a t

val filter_map: 'a t -> fn:('a -> 'b option) -> 'b t

val fold: 'a t -> init:'acc -> fn:('a -> 'acc -> 'acc) -> 'acc

val reduce: 'a t -> fn:('a -> 'a -> 'a) -> 'a option

val count: 'a t -> int

val find: 'a t -> fn:('a -> bool) -> 'a option

val any: 'a t -> fn:('a -> bool) -> bool

val all: 'a t -> fn:('a -> bool) -> bool

val take: 'a t -> int -> 'a t

val drop: 'a t -> int -> 'a t

val enumerate: 'a t -> (int * 'a) t

val zip: 'a t -> 'b t -> ('a * 'b) t

val chain: 'a t -> 'a t -> 'a t

val for_each: 'a t -> fn:('a -> unit) -> unit
