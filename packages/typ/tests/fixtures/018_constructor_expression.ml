type 'a option =
  | None
  | Some of 'a

type ('a, 'e) result =
  | Ok of 'a
  | Error of 'e

let ok = Ok 1
let err = Error "nope"
let some = Some 2
let none = None
