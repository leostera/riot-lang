open Std

type t = unit Ref.t

let make () = Ref.make ()
let equal = Ref.equal
