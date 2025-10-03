(* Alias module generation utilities *)
  open Std

let template ~parent ~child ~stdlib_modules =
  if List.mem child stdlib_modules then []
  else [ format "module %s = %s__%s" child parent child ]
