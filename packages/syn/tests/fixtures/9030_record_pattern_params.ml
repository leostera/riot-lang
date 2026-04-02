(* Test: Record patterns as function parameters *)

let f { x; y } = x + y

let g { a=x; b=y } = x * y

let h { name; age; _ } = name

let process { source; current_pos } = source
