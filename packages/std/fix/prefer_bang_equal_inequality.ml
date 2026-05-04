open Std
open Std.Collections

module Api = Fixme

let package_name = "std"

let package_rule_id = Api.Rule_id.from_string (package_name ^ ":prefer-bang-equal-inequality")

let explanation =
  Api.Explanation.{
    rule_id = package_rule_id;
    message = "Use != instead of <> for inequality.";
    body = {|
Prefer != instead of <> for inequality.

Examples:
  Instead of:
    if left <> right then ...

  write:
    if left != right then ...

Riot code uses `!=` as the standard inequality spelling. Keeping one operator
shape across the codebase makes comparisons easier to scan and avoids drifting
back toward older OCaml style in the middle of Riot code.
|};
  }

let explanations = fun () -> [ explanation ]

let make_fix = fun token ->
  Api.Fix.make
    ~title:"Replace <> with !="
    ~operations:[ Api.Fix.replace_token_with_text ~target:token ~text:"!=" ]

let span_of_ast_token = fun token ->
  Syn.Span.make
    ~start:(Syn.Ast.Token.span_start token)
    ~end_:(Syn.Ast.Token.span_end token)

let make_diagnostic = fun token ->
  Api.Diagnostic.make
    ~severity:Warning
    ~kind:(Api.Diagnostic.Known {
      rule_id = explanation.Api.Explanation.rule_id;
      message = explanation.Api.Explanation.message;
    })
    ~span:(span_of_ast_token token)
    ~suggestion:"Replace <> with !=."
    ~fix:(make_fix token)
    ()

let check_tree = fun (ctx: Api.Rule.context) _red_root ->
  Riot_fix.Rule_query.expressions ctx
  |> List.filter_map
    ~fn:(fun expr ->
      match Syn.Ast.Expr.view expr with
      | Infix { operator; _ } when String.equal (Syn.Ast.Token.text operator) "<>" ->
          Some (make_diagnostic operator)
      | _ -> None)

let rule = fun () ->
  Api.Rule.make
    ~id:package_rule_id
    ~description:"Prefer != over <> for inequality checks"
    ~explain:explanation.body
    ~run:check_tree
    ()
