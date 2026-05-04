open Std
open Std.Collections

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "prefer-if-over-bool-match"

let rule_description = "Boolean matches should be written as if expressions"

let rule_explain =
  {|
Matching on `true` and `false` is a long spelling of an `if`.

Prefer `if is_ready then render () else fallback ()` over
`match is_ready with true -> render () | false -> fallback ()`.
|}

type case_kind =
  | Bool of bool
  | Wildcard
  | Unsupported

type bool_case = {
  kind: case_kind;
  body: Ast.Expr.t;
}

let expr_source = fun ctx expr ->
  H.node_source ctx (Ast.Expr.as_node expr)
  |> String.trim

let rec pattern_kind = fun pattern ->
  match Ast.Pattern.view pattern with
  | Ast.Pattern.Wildcard -> Wildcard
  | Ast.Pattern.Literal { token } -> (
      match Ast.Token.text token with
      | "true" -> Bool true
      | "false" -> Bool false
      | _ -> Unsupported
    )
  | Ast.Pattern.Ident { ident } -> (
      match Ast.Ident.text ident with
      | "true" -> Bool true
      | "false" -> Bool false
      | _ -> Unsupported
    )
  | _ -> Unsupported

let case_of_match_case = fun match_case ->
  match Ast.MatchCase.view match_case with
  | Ast.MatchCase.Case { pattern; guard = None; body } ->
      Some { kind = pattern_kind pattern; body }
  | Ast.MatchCase.Case { guard = Some _; _ }
  | Ast.MatchCase.Unknown _ -> None

let collect_cases = fun expr ->
  let cases = Vector.with_capacity ~size:2 in
  H.iter_fold
    Ast.Expr.fold_match_case
    expr
    ~fn:(fun match_case ->
      match case_of_match_case match_case with
      | Some case -> Vector.push cases ~value:case
      | None -> ());
  cases

let is_unit_body = fun ctx expr -> String.equal (expr_source ctx expr) "()"

let if_text = fun ctx ~condition ~then_branch ?else_branch () ->
  let text = "if " ^ condition ^ " then " ^ expr_source ctx then_branch in
  match else_branch with
  | Some else_branch when not (is_unit_body ctx else_branch) ->
      text ^ " else " ^ expr_source ctx else_branch
  | Some _
  | None -> text

let negated = fun source -> "not (" ^ source ^ ")"

let replacement_for_cases = fun ctx scrutinee first second ->
  let scrutinee_text = expr_source ctx scrutinee in
  match (first.kind, second.kind) with
  | (Bool true, Bool false)
  | (Bool true, Wildcard) ->
      Some (if_text
        ctx
        ~condition:scrutinee_text
        ~then_branch:first.body
        ~else_branch:second.body
        ())
  | (Bool false, Bool true)
  | (Wildcard, Bool true) ->
      Some (if_text
        ctx
        ~condition:scrutinee_text
        ~then_branch:second.body
        ~else_branch:first.body
        ())
  | (Bool false, Wildcard) ->
      Some (if_text
        ctx
        ~condition:(negated scrutinee_text)
        ~then_branch:first.body
        ~else_branch:second.body
        ())
  | (Wildcard, Bool false) ->
      Some (if_text
        ctx
        ~condition:(negated scrutinee_text)
        ~then_branch:second.body
        ~else_branch:first.body
        ())
  | _ -> None

let make_diagnostic = fun ctx expr replacement ->
  H.diagnostic
    ~rule_id
    ~message:rule_description
    ~span:(H.span_of_node (Ast.Expr.as_node expr))
    ~suggestion:"Rewrite the boolean match as an if expression."
    ~fix:(Fix.make
      ~title:"Rewrite boolean match as if"
      ~operations:[ Fix.replace_node_with_text ~target:(Ast.Expr.as_node expr) ~text:replacement; ])
    ()

let diagnostic_for_expr = fun ctx expr ->
  match Ast.Expr.view expr with
  | Ast.Expr.Match { scrutinee; _ } ->
      let cases = collect_cases expr in
      if Int.equal (Vector.length cases) 2 then
        match replacement_for_cases
          ctx
          scrutinee
          (Vector.get_unchecked cases ~at:0)
          (Vector.get_unchecked cases ~at:1) with
        | Some replacement -> Some (make_diagnostic ctx expr replacement)
        | None -> None
      else
        None
  | _ -> None

let check_tree = fun ctx root ->
  let diagnostics = H.diagnostics_for_root root in
  let hooks = {
    Syn.Visitor.empty_hooks with
    enter_expr =
      Some (fun visitor expr ->
        (
          match diagnostic_for_expr ctx expr with
          | Some diagnostic -> H.push_diagnostic diagnostics diagnostic
          | None -> ()
        );
        (visitor, Syn.Visitor.Continue));
  }
  in
  Syn.Visitor.make ~ctx:() ~hooks
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
