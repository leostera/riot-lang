open Kernel
open Kernel.Sync
open Kernel.Sync.Cell

type t = int

let counter = Cell.create (-1)

let main = 0

let next () =
  incr counter;
  !counter

let equal = Int.equal
let compare = Int.compare
let to_string t = "pid<" ^ (Int.to_string t)^ ">"
