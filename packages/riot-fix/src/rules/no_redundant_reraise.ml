open Std
open Std.Collections

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "no-redundant-reraise"

let rule_description = "Exception handlers that only re-raise should be removed"

let rule_explain =
  {|
A handler like `with exn -> raise exn` does not handle the exception. It only
adds another frame for the reader to inspect.

Prefer the protected expression directly unless the handler adds context, cleanup,
or recovery behavior.
|}

let rec unwrap_expr = fun expr -> H.unwrap_expr expr

let expr_path_name = fun expr ->
  match Ast.Expr.view (unwrap_expr expr) with
  | Ast.Expr.Ident { ident } -> (
      match Ast.Ident.last_segment ident with
      | Some token -> Some (Ast.Token.text token)
      | None -> None
    )
  | _ -> None

let is_reraise_body = fun name expr ->
  match Ast.Expr.view (unwrap_expr expr) with
  | Ast.Expr.Apply { callee; argument } -> (
      match (expr_path_name callee, expr_path_name argument) with
      | (Some "raise", Some argument_name) -> String.equal name argument_name
      | _ -> false
    )
  | _ -> false

let single_match_case = fun expr ->
  let cases = Vector.with_capacity ~size:1 in
  H.iter_fold
    Ast.Expr.fold_match_case
    expr
    ~fn:(fun match_case -> Vector.push cases ~value:match_case);
  if Int.equal (Vector.length cases) 1 then
    Vector.get cases ~at:0
  else
    None

let diagnostic_for_expr = fun expr ->
  match Ast.Expr.view expr with
  | Ast.Expr.Try { body; _ } -> (
      match single_match_case expr with
      | Some match_case -> (
          match Ast.MatchCase.view match_case with
          | Ast.MatchCase.Case { pattern; body = handler_body; _ } -> (
              match H.pattern_name_token pattern with
              | Some token when is_reraise_body (Ast.Token.text token) handler_body ->
                  Some (H.diagnostic
                    ~rule_id
                    ~message:rule_description
                    ~span:(H.span_of_node (Ast.Expr.as_node expr))
                    ~suggestion:"Remove the handler and keep the protected expression."
                    ~fix:(Fix.make
                      ~title:"Remove redundant reraise handler"
                      ~operations:[
                        Fix.replace_node
                          ~target:(Ast.Expr.as_node expr)
                          ~replacement:(Ast.Expr.as_node body);
                      ])
                    ())
              | _ -> None
            )
          | Ast.MatchCase.Unknown _ -> None
        )
      | None -> None
    )
  | _ -> None

let check_tree = fun _ctx root ->
  let diagnostics = H.diagnostics_for_root root in
  let hooks = {
    Syn.Visitor.empty_hooks with
    enter_expr =
      Some (fun visitor expr ->
        (
          match diagnostic_for_expr expr with
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
