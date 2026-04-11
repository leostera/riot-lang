type direct_callee =
  | Runtime_helper of Types.Runtime.helper
  | Primitive of string
  | Boolean_not
  | Boolean_and
  | Boolean_or

let classify_direct_callee = fun name ->
  match name with
  | "print_endline" -> Some (Runtime_helper (Types.Runtime.print_endline ()))
  | "print_newline" -> Some (Runtime_helper (Types.Runtime.print_newline ()))
  | "print_int" -> Some (Runtime_helper (Types.Runtime.print_int ()))
  | "print_string" -> Some (Runtime_helper (Types.Runtime.print_string ()))
  | "print_char" -> Some (Runtime_helper (Types.Runtime.print_char ()))
  | "+." -> Some (Primitive "%addfloat")
  | "-." -> Some (Primitive "%subfloat")
  | "*." -> Some (Primitive "%mulfloat")
  | "/." -> Some (Primitive "%divfloat")
  | "=" -> Some (Primitive "%eq")
  | "<>" -> Some (Primitive "%neq")
  | "<" -> Some (Primitive "%lt")
  | "<=" -> Some (Primitive "%le")
  | ">" -> Some (Primitive "%gt")
  | ">=" -> Some (Primitive "%ge")
  | "+" -> Some (Primitive "%addint")
  | "-" -> Some (Primitive "%subint")
  | "*" -> Some (Primitive "%mulint")
  | "/" -> Some (Primitive "%divint")
  | "mod" -> Some (Primitive "%modint")
  | "^" -> Some (Primitive "%concatstring")
  | "sqrt" -> Some (Primitive "%sqrtfloat")
  | "string_of_int" -> Some (Primitive "%string_of_int")
  | "string_of_float" -> Some (Primitive "%string_of_float")
  | "int_of_string" -> Some (Primitive "%int_of_string")
  | "float_of_string" -> Some (Primitive "%float_of_string")
  | "not" -> Some Boolean_not
  | "&&" -> Some Boolean_and
  | "||" -> Some Boolean_or
  | _ -> None
