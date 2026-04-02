(* Qualified record field patterns in function parameters *)

(* Simple qualified field *)

let f = fun { Mod.field } -> field

(* Multiple qualified fields *)

let g = fun { Module.SubModule.package; dependency } -> package ^ dependency

(* Mixed qualified and unqualified *)

let h = fun { A.B.x; y; C.z } -> x + y + z

(* With explicit pattern *)

let i = fun { Mod.field=value } -> value

(* With wildcard *)

let j = fun { A.x; B.y; _ } -> x + y
