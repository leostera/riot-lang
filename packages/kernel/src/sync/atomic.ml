module Loc = struct
  type 'value t = 'value atomic_loc

  external get: 'value t -> 'value = "%atomic_load_loc"

  external exchange: 'value t -> 'value -> 'value = "%atomic_exchange_loc"

  external compare_and_set: 'value t -> 'value -> 'value -> bool = "%atomic_cas_loc"

  external fetch_and_add: int t -> int -> int = "%atomic_fetch_add_loc"

  let set = fun location value ->
    let _ = exchange location value in ()

  let incr = fun location ->
    let _ = fetch_and_add location 1 in ()

  let decr = fun location ->
    let _ = fetch_and_add location (-1) in ()
end

type !'value t = { mutable contents: 'value [@atomic] }

let make = fun value -> { contents = value }

external make_contended: 'value -> 'value t = "caml_atomic_make_contended"

let get = fun atomic -> atomic.contents

let set = fun atomic value -> atomic.contents <- value

let exchange = fun atomic value -> Loc.exchange [%atomic.loc atomic.contents] value

let compare_and_set = fun atomic current next -> Loc.compare_and_set [%atomic.loc atomic.contents] current next

let fetch_and_add = fun atomic incr -> Loc.fetch_and_add [%atomic.loc atomic.contents] incr

let incr = fun atomic -> Loc.incr [%atomic.loc atomic.contents]

let decr = fun atomic -> Loc.decr [%atomic.loc atomic.contents]
