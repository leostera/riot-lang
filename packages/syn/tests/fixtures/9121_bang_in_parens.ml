(* Bang operator in parenthesized function application *)

let items = ref [ 1; 2; 3 ]

let reversed = Array (List.rev !items)
