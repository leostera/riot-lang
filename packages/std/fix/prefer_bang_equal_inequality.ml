open Std
open Std.Collections
module Api = Fixme

let package_name = "std"

let package_rule_id = package_name ^ ":prefer-bang-equal-inequality"

let explanation =
  Api.Explanation.
    {rule_id = package_rule_id; message = "Use != instead of <> for inequality."; body = {|
Prefer != instead of <> for inequality.

Examples:
  Instead of:
    if left <> right then ...

  write:
    if left != right then ...

Riot code uses `!=` as the standard inequality spelling. Keeping one operator
shape across the codebase makes comparisons easier to scan and avoids drifting
back toward older OCaml style in the middle of Riot code.
|}; }

let explanations = fun () -> [ explanation ]

let make_fix = fun token -> Api.Fix.make
~title:"Replace <> with !="
~operations:[ Api.Fix.replace_token_with_text ~target:token ~text:"!=";  ]

let make_diagnostic = fun token -> Api.Diagnostic.make
~severity:Warning
~kind:(Api.Diagnostic.Known {
  rule_id = explanation.Api.Explanation.rule_id;
  message = explanation.Api.Explanation.message;

})
~span:(Syn.Ceibo.Red.SyntaxToken.span token)
~suggestion:"Replace <> with !=."
~fix:(make_fix token)
()

let check_tree = fun (ctx:Api.Rule.context) _red_root ->
  match ctx.cst with
  | Syn.Cst.Implementation { items; _ } ->
      items |> List.concat_map Api.Traversal.expressions_of_structure_item |> List.filter_map
        (
          function
          | Syn.Cst.Expression.Infix expr when String.equal (Syn.Cst.InfixExpression.operator expr) "<>" ->
              let token = Syn.Cst.InfixExpression.operator_token expr |> Syn.Cst.Token.syntax_token in
              Some (make_diagnostic token)
          | _ -> None
        )
  | Syn.Cst.Interface _ -> []

let rule = fun () -> Api.Rule.make
~id:package_rule_id
~description:"Prefer != over <> for inequality checks"
~explain:explanation.body
~run:check_tree
()
