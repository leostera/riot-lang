open Global0

type t = int

let make a = if a > 0 then Some a else None
