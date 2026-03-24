module type SHOW = sig
  type t
  val show : t -> string
end

module Int_show : SHOW with type t = int = struct
  type t = int
  let show = Int.to_string
end

module Make (Item : SHOW) : sig
  val show_all : Item.t list -> string list
end = struct
  let show_all items =
    List.map Item.show items
end

module Int_list_show = Make (Int_show)

module Nested = struct
  let value = Int_show.show 42
end

include Nested

open Nested
