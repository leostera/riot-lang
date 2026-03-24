(* TODO(@leostera): we need to add more examples here for:

   - [x] recursive modules
   - [x] multi-param functor application
   - [x] open!
   - [x] nested modules
   - [x] inline type sigantures for modules (module M : <sig> = struct .. end)

*)

module type SHOW = sig
  type t
  val show : t -> string
end

module type EQ = sig
  type t
  val equal : t -> t -> bool
end

module Int_show : SHOW with type t = int = struct
  type t = int
  let show = Int.to_string
end

module Int_eq : EQ with type t = int = struct
  type t = int
  let equal left right = left = right
end

module Make (Item : SHOW) : sig
  val show_all : Item.t list -> string list
end = struct
  let show_all items =
    List.map Item.show items
end

module Make_pair (Left : SHOW) (Right : SHOW) = struct
  let show_pair (left, right) =
    Left.show left, Right.show right
end

module Int_list_show = Make (Int_show)
module Int_pair_show = Make_pair (Int_show) (Int_show)

module Nested = struct
  let value = Int_show.show 42

  module Inner = struct
    let equal = Int_eq.equal
  end
end

module Inline : sig
  type t
  val make : int -> t
  val show : t -> string
end = struct
  type t = int
  let make value = value
  let show = Int.to_string
end

module rec Even : sig
  val check : int -> bool
end = struct
  let check n = n = 0 || Odd.check (n - 1)
end
and Odd : sig
  val check : int -> bool
end = struct
  let check n = n <> 0 && Even.check (n - 1)
end

include Nested

open Nested
open! Inline
