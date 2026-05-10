open Std
open Std.Collections

module Api = Fixme
module H = Ast_rule_helpers

let package_name = "std"

let package_rule_id = Api.Rule_id.from_string (package_name ^ ":prefer-list-is-empty")

let explanation =
  Api.Explanation.{
    rule_id = package_rule_id;
    message = "List emptiness checks should use List.is_empty instead of comparing List.length to zero.";
    body = {|
`List.length xs == 0` and `List.length xs > 0` are both using the size of the list
as a proxy for the real question: is there anything in it?

`List.is_empty xs` answers that question directly, and it reads that way too. It
keeps the code focused on the intent of the check instead of making the reader
translate a numeric comparison back into an emptiness predicate.

This rule only targets the direct shapes:

- `List.length xs == 0`
- `List.length xs > 0`

When the code is asking about emptiness, say so explicitly.
|};
  }

let explanations = fun () -> [ explanation ]

let list_length_argument = fun expr ->
  let (head, arguments) = H.flatten_apply expr in
  match (H.expr_name head, arguments) with
  | (Some "List.length", [ argument ]) -> Some argument
  | _ -> None

let make_diagnostic = fun ~suggestion expr ->
  Api.Diagnostic.make
    ~severity:Warning
    ~kind:(Api.Diagnostic.Known {
      rule_id = package_rule_id;
      message = explanation.Api.Explanation.message;
    })
    ~span:(H.expr_span expr)
    ~suggestion
    ()

let diagnostic_for_expression = fun expr ->
  match Syn.Ast.Expr.view (H.unwrap_expr expr) with
  | Infix { left; operator; right } ->
      let operator = Syn.Ast.Token.text operator in
      (
        match (operator, list_length_argument left, list_length_argument right) with
        | ("==", Some _list_expr, _) when H.is_zero_literal right ->
            Some (make_diagnostic ~suggestion:"Use List.is_empty xs for this emptiness check." expr)
        | (">", Some _list_expr, _) when H.is_zero_literal right ->
            Some (make_diagnostic
              ~suggestion:"Use not (List.is_empty xs) when checking that a list has elements."
              expr)
        | _ -> None
      )
  | _ -> None

let check_tree = fun (ctx: Api.Rule.context) _red_root ->
  Riot_fix.Rule_query.expressions ctx
  |> List.filter_map ~fn:diagnostic_for_expression

let rule = fun () ->
  Api.Rule.make
    ~id:package_rule_id
    ~description:explanation.message
    ~explain:explanation.body
    ~run:check_tree
    ()
