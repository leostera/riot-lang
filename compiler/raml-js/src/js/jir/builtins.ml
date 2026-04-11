open Std
module Core = Raml_core.Core_ir

type surface_path = string list

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
  | (left_head :: left_tail, right_head :: right_tail) -> String.equal left_head right_head
  && equal_segments left_tail right_tail
  | _ -> false

let matches_surface_path = fun entity_id expected ->
  equal_segments (Core.Entity_id.to_segments entity_id) expected

type direct_callee_spec = {
  callee: direct_callee;
  riot_paths: surface_path list;
  compatibility_paths: surface_path list;
}

let matches_any_surface_path = fun entity_id paths ->
  List.exists (fun path -> matches_surface_path entity_id path) paths

let riot_global = fun name -> [ "Std"; "Global"; name ]

let specs = [
  {
    callee = Console_log;
    riot_paths = [ [ "println" ]; [ "Std"; "println" ]; riot_global "println" ];
    compatibility_paths = [ [ "print_endline" ] ];
  };
  {
    callee = Console_error;
    riot_paths = [ [ "eprintln" ]; [ "Std"; "eprintln" ]; riot_global "eprintln" ];
    compatibility_paths = [];
  };
  {
    callee = Stdout_write;
    riot_paths = [ [ "print" ]; [ "Std"; "print" ]; riot_global "print" ];
    compatibility_paths = [ [ "print_int" ]; [ "print_string" ]; [ "print_char" ] ];
  };
  {
    callee = Stderr_write;
    riot_paths = [ [ "eprint" ]; [ "Std"; "eprint" ]; riot_global "eprint" ];
    compatibility_paths = [];
  };
  {
    callee = Print_newline;
    riot_paths = [];
    compatibility_paths = [ [ "print_newline" ] ];
  };
  {
    callee = String_constructor;
    riot_paths = [];
    compatibility_paths = [ [ "string_of_int" ]; [ "string_of_float" ] ];
  };
  {
    callee = Math_sqrt;
    riot_paths = [];
    compatibility_paths = [ [ "sqrt" ] ];
  };
  { callee = Primitive "%int_of_string"; riot_paths = []; compatibility_paths = [ [ "int_of_string" ] ] };
  { callee = Primitive "%float_of_string"; riot_paths = []; compatibility_paths = [ [ "float_of_string" ] ] };
  { callee = Unary_operator Types.Operator.Not; riot_paths = []; compatibility_paths = [ [ "not" ] ] };
  { callee = Boolean_and; riot_paths = []; compatibility_paths = [ [ "&&" ] ] };
  { callee = Boolean_or; riot_paths = []; compatibility_paths = [ [ "||" ] ] };
  { callee = Binary_operator Types.Operator.Add; riot_paths = []; compatibility_paths = [ [ "+." ]; [ "+" ] ] };
  { callee = Binary_operator Types.Operator.Subtract; riot_paths = []; compatibility_paths = [ [ "-." ]; [ "-" ] ] };
  { callee = Binary_operator Types.Operator.Multiply; riot_paths = []; compatibility_paths = [ [ "*." ]; [ "*" ] ] };
  { callee = Binary_operator Types.Operator.Divide; riot_paths = []; compatibility_paths = [ [ "/." ]; [ "/" ] ] };
  { callee = Binary_operator Types.Operator.Modulo; riot_paths = []; compatibility_paths = [ [ "mod" ] ] };
  { callee = Binary_operator Types.Operator.Equal; riot_paths = []; compatibility_paths = [ [ "=" ] ] };
  { callee = Binary_operator Types.Operator.Not_equal; riot_paths = []; compatibility_paths = [ [ "<>" ] ] };
  { callee = Binary_operator Types.Operator.Less_than; riot_paths = []; compatibility_paths = [ [ "<" ] ] };
  { callee = Binary_operator Types.Operator.Less_or_equal; riot_paths = []; compatibility_paths = [ [ "<=" ] ] };
  { callee = Binary_operator Types.Operator.Greater_than; riot_paths = []; compatibility_paths = [ [ ">" ] ] };
  { callee = Binary_operator Types.Operator.Greater_or_equal; riot_paths = []; compatibility_paths = [ [ ">=" ] ] };
  { callee = Binary_operator Types.Operator.Add; riot_paths = []; compatibility_paths = [ [ "^" ] ] };
]

let classify_with = fun project_paths entity_id ->
  specs
  |> List.find_map (fun spec ->
    if matches_any_surface_path entity_id (project_paths spec) then
      Some spec.callee
    else
      None)

let classify_direct_callee = fun entity_id ->
  if not (can_classify_entity entity_id) then
    None
  else
    match classify_with (fun spec -> spec.riot_paths) entity_id with
    | Some direct_callee -> Some direct_callee
    | None -> classify_with (fun spec -> spec.compatibility_paths) entity_id
