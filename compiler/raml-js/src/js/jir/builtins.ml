module Core = Raml_core.Core_ir

type direct_callee =
  | Console_log
  | Console_error
  | Print_newline
  | Stdout_write
  | Stderr_write
  | String_constructor
  | Math_sqrt
  | Primitive of string
  | Unary_operator of Types.Operator.unary
  | Binary_operator of Types.Operator.binary
  | Boolean_and
  | Boolean_or

let is_predef_binding = fun binding_id ->
  String.starts_with ~prefix:"predef(" (Core.Binding_id.to_string binding_id)

let can_classify_entity = fun entity_id ->
  match Core.Entity_id.binding_id entity_id with
  | None -> true
  | Some binding_id -> (
      match Core.Binding_id.stamp binding_id with
      | None -> true
      | Some _ -> is_predef_binding binding_id
    )

let rec equal_segments = fun left right ->
  match (left, right) with
  | ([], []) -> true
  | (left_head :: left_tail, right_head :: right_tail) ->
      String.equal left_head right_head && equal_segments left_tail right_tail
  | _ -> false

let matches_surface_path = fun entity_id expected ->
  equal_segments (Core.Entity_id.to_segments entity_id) expected

let classify_direct_callee = fun entity_id ->
  if not (can_classify_entity entity_id) then
    None
  else if
    matches_surface_path entity_id [ "println" ]
    || matches_surface_path entity_id [ "Std"; "println" ]
    || matches_surface_path entity_id [ "Std"; "Global"; "println" ]
    || matches_surface_path entity_id [ "print_endline" ]
  then
    Some Console_log
  else if
    matches_surface_path entity_id [ "eprintln" ]
    || matches_surface_path entity_id [ "Std"; "eprintln" ]
    || matches_surface_path entity_id [ "Std"; "Global"; "eprintln" ]
  then
    Some Console_error
  else if matches_surface_path entity_id [ "print_newline" ] then
    Some Print_newline
  else if
    matches_surface_path entity_id [ "print" ]
    || matches_surface_path entity_id [ "Std"; "print" ]
    || matches_surface_path entity_id [ "Std"; "Global"; "print" ]
    || matches_surface_path entity_id [ "print_int" ]
    || matches_surface_path entity_id [ "print_string" ]
    || matches_surface_path entity_id [ "print_char" ]
  then
    Some Stdout_write
  else if
    matches_surface_path entity_id [ "eprint" ]
    || matches_surface_path entity_id [ "Std"; "eprint" ]
    || matches_surface_path entity_id [ "Std"; "Global"; "eprint" ]
  then
    Some Stderr_write
  else if matches_surface_path entity_id [ "+." ] then
    Some (Binary_operator Types.Operator.Add)
  else if matches_surface_path entity_id [ "-." ] then
    Some (Binary_operator Types.Operator.Subtract)
  else if matches_surface_path entity_id [ "*." ] then
    Some (Binary_operator Types.Operator.Multiply)
  else if matches_surface_path entity_id [ "/." ] then
    Some (Binary_operator Types.Operator.Divide)
  else if matches_surface_path entity_id [ "=" ] then
    Some (Binary_operator Types.Operator.Equal)
  else if matches_surface_path entity_id [ "<>" ] then
    Some (Binary_operator Types.Operator.Not_equal)
  else if matches_surface_path entity_id [ "<" ] then
    Some (Binary_operator Types.Operator.Less_than)
  else if matches_surface_path entity_id [ "<=" ] then
    Some (Binary_operator Types.Operator.Less_or_equal)
  else if matches_surface_path entity_id [ ">" ] then
    Some (Binary_operator Types.Operator.Greater_than)
  else if matches_surface_path entity_id [ ">=" ] then
    Some (Binary_operator Types.Operator.Greater_or_equal)
  else if matches_surface_path entity_id [ "+" ] then
    Some (Binary_operator Types.Operator.Add)
  else if matches_surface_path entity_id [ "-" ] then
    Some (Binary_operator Types.Operator.Subtract)
  else if matches_surface_path entity_id [ "*" ] then
    Some (Binary_operator Types.Operator.Multiply)
  else if matches_surface_path entity_id [ "/" ] then
    Some (Binary_operator Types.Operator.Divide)
  else if matches_surface_path entity_id [ "mod" ] then
    Some (Binary_operator Types.Operator.Modulo)
  else if matches_surface_path entity_id [ "^" ] then
    Some (Binary_operator Types.Operator.Add)
  else if
    matches_surface_path entity_id [ "string_of_int" ]
    || matches_surface_path entity_id [ "string_of_float" ]
  then
    Some String_constructor
  else if matches_surface_path entity_id [ "int_of_string" ] then
    Some (Primitive "%int_of_string")
  else if matches_surface_path entity_id [ "float_of_string" ] then
    Some (Primitive "%float_of_string")
  else if matches_surface_path entity_id [ "sqrt" ] then
    Some Math_sqrt
  else if matches_surface_path entity_id [ "not" ] then
    Some (Unary_operator Types.Operator.Not)
  else if matches_surface_path entity_id [ "&&" ] then
    Some Boolean_and
  else if matches_surface_path entity_id [ "||" ] then
    Some Boolean_or
  else
    None
