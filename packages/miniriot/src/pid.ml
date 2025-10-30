type counter = { mutable last: int }
let incr c = c.last <- c.last + 1
let (!) c = c.last

type t = int

let counter = { last = -1 }
let main = 0

let next () =
  incr counter;
  !counter

let equal = Int.equal
let compare = Int.compare
let pp ppf t = Format.fprintf ppf "pid<%d>" t
let to_string t = Format.sprintf "pid<%d>" t
