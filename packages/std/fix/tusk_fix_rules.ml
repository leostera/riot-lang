let name = "std"
let rules () =
  [ No_stdlib.rule (); Prefer_bang_equal_inequality.rule (); No_double_list_rev.rule () ]

let explanations () =
  No_stdlib.explanations ()
  @ Prefer_bang_equal_inequality.explanations ()
  @ No_double_list_rev.explanations ()
