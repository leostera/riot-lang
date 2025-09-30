(* Alias module generation utilities *)

let template ~parent ~child ~stdlib_modules =
  if List.mem child stdlib_modules then []
  else [ Printf.sprintf "module %s = %s__%s" child parent child ]
