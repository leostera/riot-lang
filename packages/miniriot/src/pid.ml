type t = int

let counter = ref 1
let zero = 0
let main = 1

let next () =
  incr counter;
  !counter

let equal = Int.equal
let compare = Int.compare
let pp ppf t = Format.fprintf ppf "pid<%d>" t
let to_string t = Format.sprintf "pid<%d>" t
