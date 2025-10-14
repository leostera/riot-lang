open Global

type test_result = Pass | Fail of string | Error of exn
type t = { name : string; fn : unit -> (unit, string) result; skip : bool }

let case name fn = { name; fn; skip = false }
let skip name fn = { name; fn; skip = true }
