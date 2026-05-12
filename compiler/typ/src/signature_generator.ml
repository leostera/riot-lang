open Std
open Std.Iter

module SurfacePath = Model.Surface_path
module TypAst = Ast

let is_ident_start = fun __tmp1 ->
  match __tmp1 with
  | 'a' .. 'z'
  | '_' -> true
  | _ -> false

let is_ident_continue = fun __tmp1 ->
  match __tmp1 with
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '_'
  | '\'' -> true
  | _ -> false

let is_value_ident = fun name ->
  match String.get name ~at:0 with
  | Some first when is_ident_start first ->
      let rec loop index =
        if index >= String.length name then
          true
        else
          match String.get name ~at:index with
          | Some char when is_ident_continue char -> loop (index + 1)
          | _ -> false
      in
      loop 1
  | _ -> false

let render_value_name = fun name ->
  if is_value_ident name then
    name
  else
    "( " ^ name ^ " )"

let render_value = fun (name, type_) ->
  "val " ^ render_value_name (SurfacePath.to_string name) ^ " : " ^ TypAst.Type.to_string type_

let render_type_parameter = fun __tmp1 ->
  match __tmp1 with
  | None -> "_"
  | Some name when String.starts_with ~prefix:"'" name -> name
  | Some name -> "'" ^ name

let render_type_parameters = fun __tmp1 ->
  match __tmp1 with
  | [] -> ""
  | [ parameter ] -> render_type_parameter parameter ^ " "
  | parameters ->
      "(" ^ (
        parameters
        |> List.map ~fn:render_type_parameter
        |> String.concat ", "
      ) ^ ") "

let rec render_core_type (type_: TypAst.core_type) =
  match type_.kind with
  | TypAst.TypeIdent ident -> SurfacePath.to_string ident
  | TypAst.Var name -> render_type_parameter name
  | TypAst.Tuple parts ->
      parts
      |> List.map ~fn:render_core_type
      |> String.concat " * "
  | TypAst.Arrow { label = _; parameter; result } ->
      render_core_type_argument parameter ^ " -> " ^ render_core_type result
  | TypAst.Apply { constructor; arguments } -> render_type_application constructor arguments
  | TypAst.Parenthesized inner -> "(" ^ render_core_type inner ^ ")"
  | TypAst.ForAll { body; _ } -> render_core_type body
  | TypAst.Wildcard -> "_"
  | TypAst.PolyVariant _ -> "_"
  | TypAst.Package _ -> "_"

and render_type_application constructor arguments =
  let constructor = render_core_type constructor in
  match arguments with
  | [] -> constructor
  | [ argument ] -> render_core_type_argument argument ^ " " ^ constructor
  | arguments ->
      "(" ^ (
        arguments
        |> List.map ~fn:render_core_type
        |> String.concat ", "
      ) ^ ") " ^ constructor

and render_core_type_argument type_ =
  match type_.kind with
  | TypAst.Arrow _
  | TypAst.Tuple _ -> "(" ^ render_core_type type_ ^ ")"
  | _ -> render_core_type type_

let render_record_field (field: TypAst.record_field_declaration) =
  (
    if field.TypAst.mutable_ then
      "mutable "
    else
      ""
  ) ^ SurfacePath.to_string field.name ^ " : " ^ render_core_type field.type_annotation

let render_constructor_argument (argument: TypAst.constructor_arguments) =
  match argument with
  | TypAst.Tuple [] -> ""
  | TypAst.Tuple [ argument ] -> " of " ^ render_core_type argument
  | TypAst.Tuple arguments ->
      " of " ^ (
        arguments
        |> List.map ~fn:render_core_type
        |> String.concat " * "
      )
  | TypAst.Record fields ->
      " of { " ^ (
        fields
        |> List.map ~fn:render_record_field
        |> String.concat "; "
      ) ^ "; }"

let render_type_constructor (constructor: TypAst.type_constructor) =
  SurfacePath.to_string constructor.name ^ render_constructor_argument constructor.arguments

let render_type_definition (definition: TypAst.type_definition_kind) =
  match definition with
  | TypAst.Abstract -> ""
  | TypAst.Extensible -> " = .."
  | TypAst.Alias type_ -> " = " ^ render_core_type type_
  | TypAst.Record fields ->
      " = { " ^ (
        fields
        |> List.map ~fn:render_record_field
        |> String.concat "; "
      ) ^ "; }"
  | TypAst.Variant constructors ->
      " = " ^ (
        constructors
        |> List.map ~fn:render_type_constructor
        |> String.concat " | "
      )

let render_type_declaration (_name, (declaration: TypAst.type_declaration)) =
  "type "
  ^ render_type_parameters declaration.parameters
  ^ SurfacePath.to_string declaration.name
  ^ render_type_definition declaration.definition.kind

let lines_of_types types =
  types
  |> Iterator.to_list
  |> List.map ~fn:render_type_declaration

let lines_of_values values =
  values
  |> Iterator.to_list
  |> List.map ~fn:render_value

let render_lines = fun __tmp1 ->
  match __tmp1 with
  | [] -> ""
  | lines -> String.concat "\n" lines ^ "\n"

let from_exports ~types ~values =
  render_lines (List.append (lines_of_types types) (lines_of_values values))

let from_values = fun values -> render_lines (lines_of_values values)
