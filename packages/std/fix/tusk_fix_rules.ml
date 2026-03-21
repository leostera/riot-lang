let name = "std"
let rules () = [ No_stdlib.rule (); Prefer_bang_equal_inequality.rule () ]

let explanations () =
  No_stdlib.explanations () @ Prefer_bang_equal_inequality.explanations ()
