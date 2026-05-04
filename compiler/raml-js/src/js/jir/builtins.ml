open Std
module Core = Raml_core.Core_ir

type surface_path = string list

type direct_callee =
  | Console_log
  | Console_error
  | Stdout_write
  | Stderr_write
  | String_constructor
  | Math_sqrt
  | Primitive of Core.Primitive.t
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
      if String.equal left_head right_head then
        equal_segments left_tail right_tail
      else
        false
  | _ -> false

let matches_surface_path = fun entity_id expected ->
  equal_segments (Core.Entity_id.to_segments entity_id) expected

type direct_callee_spec = {
  callee: direct_callee;
  paths: surface_path list;
}

let matches_any_surface_path = fun entity_id paths ->
  List.exists (fun path -> matches_surface_path entity_id path) paths

let riot_global = fun name -> [ "Std"; "Global"; name ]

let riot_module = fun module_name name -> [ [ module_name; name ]; [ "Std"; module_name; name ] ]

let specs = [
  { callee = Console_log; paths = [ [ "println" ]; [ "Std"; "println" ]; riot_global "println" ] };
  {
    callee = Console_error;
    paths = [ [ "eprintln" ]; [ "Std"; "eprintln" ]; riot_global "eprintln" ]
  };
  { callee = Stdout_write; paths = [ [ "print" ]; [ "Std"; "print" ]; riot_global "print" ] };
  { callee = Stderr_write; paths = [ [ "eprint" ]; [ "Std"; "eprint" ]; riot_global "eprint" ] };
  {
    callee = String_constructor;
    paths = riot_module "Int" "to_string" @ riot_module "Float" "to_string"
  };
  { callee = Math_sqrt; paths = riot_module "Float" "sqrt" };
  { callee = Primitive Core.Primitive.Int_of_string; paths = riot_module "Int" "from_string" };
  { callee = Primitive Core.Primitive.Float_of_string; paths = riot_module "Float" "from_string" };
  { callee = Unary_operator Types.Operator.Not; paths = [ [ "not" ] ] };
  { callee = Boolean_and; paths = [ [ "&&" ] ] };
  { callee = Boolean_or; paths = [ [ "||" ] ] };
  { callee = Binary_operator Types.Operator.Add; paths = [ [ "+." ]; [ "+" ] ] };
  { callee = Binary_operator Types.Operator.Subtract; paths = [ [ "-." ]; [ "-" ] ] };
  { callee = Binary_operator Types.Operator.Multiply; paths = [ [ "*." ]; [ "*" ] ] };
  { callee = Binary_operator Types.Operator.Divide; paths = [ [ "/." ]; [ "/" ] ] };
  { callee = Binary_operator Types.Operator.Modulo; paths = [ [ "mod" ] ] };
  { callee = Binary_operator Types.Operator.Equal; paths = [ [ "=" ] ] };
  { callee = Binary_operator Types.Operator.Not_equal; paths = [ [ "<>" ] ] };
  { callee = Binary_operator Types.Operator.Less_than; paths = [ [ "<" ] ] };
  { callee = Binary_operator Types.Operator.Less_or_equal; paths = [ [ "<=" ] ] };
  { callee = Binary_operator Types.Operator.Greater_than; paths = [ [ ">" ] ] };
  { callee = Binary_operator Types.Operator.Greater_or_equal; paths = [ [ ">=" ] ] };
  { callee = Binary_operator Types.Operator.Add; paths = [ [ "^" ] ] };
]

let classify_with = fun entity_id ->
  let rec loop = fun specs ->
    match specs with
    | [] -> None
    | spec :: rest ->
        if matches_any_surface_path entity_id spec.paths then
          Some spec.callee
        else
          loop rest
  in
  loop specs

let classify_direct_callee = fun entity_id ->
  if not (can_classify_entity entity_id) then
    None
  else
    classify_with entity_id
