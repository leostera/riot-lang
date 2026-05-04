open Std
open Std.Collections

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "prefer-scoped-field-access"

let rule_description = "Module-qualified field access should use scoped qualification syntax"

let rule_explain =
  {|
Prefer scoped qualification when several fields or values come from the same
module:

```ocaml
Module.{ field = value }
Module.(record.field)
```

This keeps the qualifier at the smallest useful scope instead of repeating it on
every field.
|}

type visitor_ctx = { local_open_depth: int }

let starts_with_uppercase = fun text ->
  String.length text > 0 && H.is_upper (String.get_unchecked text ~at:0)

let starts_with_lowercase = fun text ->
  String.length text > 0 && H.is_lower (String.get_unchecked text ~at:0)

let diagnostic = fun ~span ?fix () ->
  H.diagnostic
    ~rule_id
    ~message:rule_description
    ~span
    ~suggestion:"Prefer Module.(value.field) style for module-qualified record access."
    ?fix
    ()

let split_last = fun segments ->
  let rec loop before = fun __tmp1 ->
    match __tmp1 with
    | [] -> None
    | [ last ] -> Some (List.reverse before, last)
    | segment :: rest -> loop (segment :: before) rest
  in
  loop [] segments

let field_access_replacement = fun ctx base module_segments field ->
  String.concat "." module_segments
  ^ ".("
  ^ (
    H.node_source ctx (Ast.Expr.as_node base)
    |> String.trim
  )
  ^ "."
  ^ field
  ^ ")"

let field_access_diagnostic = fun ctx expr base module_segments field ->
  let replacement = field_access_replacement ctx base module_segments field in
  diagnostic
    ~span:(H.span_of_node (Ast.Expr.as_node expr))
    ~fix:(Fix.make
      ~title:"Rewrite field access to scoped module qualification"
      ~operations:[ Fix.replace_node_with_text ~target:(Ast.Expr.as_node expr) ~text:replacement; ])
    ()

let ident_segments = fun ctx ident ->
  ignore ctx;
  Ast.Ident.text ident
  |> String.split ~by:"."

let path_field_access_replacement = fun segments ->
  match segments with
  | [ base; module_; field ] -> Some (module_ ^ ".(" ^ base ^ "." ^ field ^ ")")
  | _ -> None

let path_field_access_diagnostic = fun expr replacement ->
  diagnostic
    ~span:(H.span_of_node (Ast.Expr.as_node expr))
    ~fix:(Fix.make
      ~title:"Rewrite field access to scoped module qualification"
      ~operations:[ Fix.replace_node_with_text ~target:(Ast.Expr.as_node expr) ~text:replacement; ])
    ()

let check_field_access = fun ctx diagnostics expr target field ->
  let segments =
    Ast.Ident.text field
    |> String.split ~by:"."
  in
  match split_last segments with
  | Some (module_segments, field_name) when starts_with_lowercase field_name -> (
      match module_segments with
      | module_head :: _ when starts_with_uppercase module_head ->
          H.push_diagnostic
            diagnostics
            (field_access_diagnostic ctx expr target module_segments field_name)
      | _ -> ()
    )
  | _ -> ()

let check_path_access = fun ctx diagnostics expr ident ->
  match ident_segments ctx ident with
  | [ base; module_; field ] when starts_with_lowercase base
  && starts_with_uppercase module_
  && starts_with_lowercase field -> (
      match path_field_access_replacement [ base; module_; field ] with
      | Some replacement ->
          H.push_diagnostic diagnostics (path_field_access_diagnostic expr replacement)
      | None -> ()
    )
  | _ -> ()

let module_qualifier_of_path = fun ctx ident ->
  match ident_segments ctx ident with
  | qualifier :: _ :: _ -> Some qualifier
  | _ -> None

let record_expr_repeats_qualifier = fun ctx record ->
  let qualifiers = Vector.with_capacity ~size:(Ast.RecordExpr.field_count record) in
  H.iter_fold
    Ast.RecordExpr.fold_field
    record
    ~fn:(fun field ->
      match field with
      | Ast.RecordExprField { ident; _ } -> (
          match module_qualifier_of_path ctx ident with
          | Some qualifier -> Vector.push qualifiers ~value:qualifier
          | None -> ()
        )
      | Ast.UnknownRecordExprField _ -> ());
  let repeated = ref false in
  Vector.for_each
    qualifiers
    ~fn:(fun qualifier ->
      let count = ref 0 in
      Vector.for_each
        qualifiers
        ~fn:(fun other ->
          if String.equal qualifier other then
            count := !count + 1);
      if !count >= 2 then
        repeated := true);
  !repeated

let body_is_bracket_expr = fun body ->
  match Ast.Expr.view body with
  | Ast.Expr.List _
  | Ast.Expr.Array _
  | Ast.Expr.Record _ -> true
  | _ -> false

let check_local_open = fun diagnostics visitor expr ->
  match Ast.Expr.view expr with
  | Ast.Expr.LocalOpen { body } when (Syn.Visitor.ctx visitor).local_open_depth = 0
  && Option.is_some (Ast.Node.first_child_token (Ast.Expr.as_node expr) ~kind:Syn.SyntaxKind.LET_KW)
  && body_is_bracket_expr body ->
      H.push_diagnostic diagnostics (diagnostic ~span:(H.span_of_node (Ast.Expr.as_node expr)) ())
  | _ -> ()

let check_expr = fun ctx diagnostics visitor expr ->
  (
    match Ast.Expr.view expr with
    | Ast.Expr.FieldAccess { target; field } -> check_field_access ctx diagnostics expr target field
    | Ast.Expr.Ident { ident } -> check_path_access ctx diagnostics expr ident
    | Ast.Expr.Record _ ->
        Option.for_each
          (Ast.cast_result_to_option (Ast.RecordExpr.cast expr))
          ~fn:(fun record ->
            if record_expr_repeats_qualifier ctx record then
              H.push_diagnostic
                diagnostics
                (diagnostic ~span:(H.span_of_node (Ast.Expr.as_node expr)) ()))
    | _ -> ()
  );
  check_local_open diagnostics visitor expr

let check_tree = fun ctx root ->
  let diagnostics = H.diagnostics_for_root root in
  let hooks = {
    Syn.Visitor.empty_hooks with
    enter_expr =
      Some (fun visitor expr ->
        check_expr ctx diagnostics visitor expr;
        let visitor =
          match Ast.Expr.view expr with
          | Ast.Expr.LocalOpen _ ->
              Syn.Visitor.with_ctx
                visitor
                {
                  local_open_depth = (Syn.Visitor.ctx visitor).local_open_depth + 1;
                }
          | _ -> visitor
        in
        (visitor, Syn.Visitor.Continue));
    leave_node =
      Some (fun visitor node ->
        if Ast.Node.kind node = Syn.SyntaxKind.LOCAL_OPEN_EXPR then
          let depth = (Syn.Visitor.ctx visitor).local_open_depth in
          Syn.Visitor.with_ctx
            visitor
            {
              local_open_depth =
                if depth > 0 then
                  depth - 1
                else
                  0;
            }
        else
          visitor);
  }
  in
  Syn.Visitor.make ~ctx:{ local_open_depth = 0 } ~hooks
  |> fun visitor ->
    ignore (Syn.Visitor.visit_node visitor root);
    H.vector_to_list diagnostics

let make = fun () ->
  Rule.make
    ~id:rule_id
    ~description:rule_description
    ~explain:rule_explain
    ~run:check_tree
    ()
