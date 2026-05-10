open Std
open Std.Collections

module Ast = Syn.Ast

let to_list = fun vector ->
  Vector.to_array vector
  |> Array.to_list

let expr_node = Ast.Expr.as_node

let pattern_node = Ast.Pattern.as_node

let parameter_node = Ast.Parameter.as_node

let node_span = fun node ->
  Syn.Span.make
    ~start:(Ast.Node.span_start node)
    ~end_:(Ast.Node.span_end node)

let token_span = fun token ->
  Syn.Span.make
    ~start:(Ast.Token.span_start token)
    ~end_:(Ast.Token.span_end token)

let expr_span = fun expr -> node_span (expr_node expr)

let parameter_span = fun parameter -> node_span (parameter_node parameter)

let rec unwrap_expr = fun expr ->
  match Ast.Expr.view expr with
  | Annotated { expr; _ } -> unwrap_expr expr
  | _ -> expr

let rec unwrap_pattern = fun pattern ->
  match Ast.Pattern.view pattern with
  | Constraint { pattern; _ } -> unwrap_pattern pattern
  | _ -> pattern

let ident_text = Ast.Ident.text

let ident_last_name = fun ident ->
  Ast.Ident.last_segment ident
  |> Option.map ~fn:Ast.Token.text

let rec expr_name = fun expr ->
  match Ast.Expr.view (unwrap_expr expr) with
  | Ident { ident } -> Some (ident_text ident)
  | FieldAccess { target; field } ->
      (match expr_name target with
      | Some target_name -> Some (target_name ^ "." ^ Ast.Ident.text field)
      | None -> None)
  | _ -> None

let simple_expr_name = fun expr ->
  match Ast.Expr.view (unwrap_expr expr) with
  | Ident { ident } when Int.equal (Ast.Ident.segment_count ident) 1 -> ident_last_name ident
  | _ -> None

let flatten_apply = fun expr ->
  let rec loop arguments expr =
    match Ast.Expr.view (unwrap_expr expr) with
    | Apply { callee; argument } -> loop (argument :: arguments) callee
    | _ -> (unwrap_expr expr, arguments)
  in
  loop [] expr

let path_matches = fun ~expected expr ->
  match expr_name expr with
  | Some actual -> String.equal actual expected
  | None -> false

let is_zero_literal = fun expr ->
  match Ast.Expr.view (unwrap_expr expr) with
  | Literal { token } -> String.equal (Ast.Token.text token) "0"
  | _ -> false

let constructor_name_of_pattern = fun pattern ->
  match Ast.Pattern.view (unwrap_pattern pattern) with
  | Constructor { constructor; _ } -> ident_last_name constructor
  | _ -> None

let identifier_name_of_pattern = fun pattern ->
  match Ast.Pattern.view (unwrap_pattern pattern) with
  | Ident { ident } -> ident_last_name ident
  | _ -> None

let constructor_payload_of_pattern = fun pattern ->
  match Ast.Pattern.view (unwrap_pattern pattern) with
  | Constructor { payload; _ } -> payload
  | _ -> None

let is_constructor_expr = fun ~name expr ->
  match Ast.Expr.view (unwrap_expr expr) with
  | Constructor { constructor; payload = None } ->
      (match ident_last_name constructor with
      | Some actual -> String.equal actual name
      | None -> false)
  | Constructor { constructor; payload = Some _ } ->
      (match ident_last_name constructor with
      | Some actual -> String.equal actual name
      | None -> false)
  | Ident { ident } ->
      (match ident_last_name ident with
      | Some actual -> String.equal actual name
      | None -> false)
  | Apply { callee; _ } -> path_matches ~expected:name callee
  | _ -> false

let constructor_payload = fun ~name expr ->
  match Ast.Expr.view (unwrap_expr expr) with
  | Constructor { constructor; payload = Some payload } ->
      (match ident_last_name constructor with
      | Some actual when String.equal actual name -> Some payload
      | _ -> None)
  | Apply { callee; argument } when path_matches ~expected:name callee -> Some argument
  | _ -> None

let fold_to_list = fun fold value ->
  fold value ~init:[] ~fn:(fun item acc -> Ast.Continue (item :: acc))
  |> List.reverse

let match_cases = fun expr -> fold_to_list Ast.Expr.fold_match_case expr

let fun_parameters = fun expr ->
  match Ast.Expr.view (unwrap_expr expr) with
  | Fun { parameters; _ } -> to_list parameters
  | _ -> []

let is_unit_pattern = fun pattern ->
  match Ast.Pattern.view (unwrap_pattern pattern) with
  | Unit -> true
  | _ -> false

let is_unit_parameter = fun parameter ->
  match Ast.Parameter.view parameter with
  | Param { label = NoLabel; pattern = Some pattern } -> is_unit_pattern pattern
  | Param _
  | Unknown _ -> false

let let_binding_parameters = fun binding -> fold_to_list Ast.LetBinding.fold_parameter binding

let let_binding_name = fun binding ->
  match Ast.LetBinding.pattern binding with
  | Some pattern -> identifier_name_of_pattern pattern
  | None -> None

let let_binding_body = Ast.LetBinding.body
