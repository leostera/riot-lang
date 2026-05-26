(* Test: First-class module expressions without type annotation *)

let x =
  (module M)

let y =
  Source.make (module Src) data

let z =
  (module M : S)
