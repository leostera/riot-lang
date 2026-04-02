(* Test: Nested parentheses in type annotations within patterns *)

let f (x: (a, b) Type.eq) = ()

let g (Equal: (int, string) result) = ()

let h (Some (x: (int * string) option)) = x
