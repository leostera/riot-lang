open Std

module Api = Fixme
module Ast = Syn.Ast
module H = Ast_rule_helpers

let package_name = "std"

let package_rule_id =
  Api.Rule_id.from_string (package_name ^ ":prefer-option-map-over-manual-match")

let explanation =
  Api.Explanation.{
    rule_id = package_rule_id;
    message = "Manual option matches that only rebuild Some/None should use Option.map.";
    body = {|
`Option.map` already names the pattern where you transform the value inside `Some`
while keeping `None` unchanged.

Writing that logic out longhand with a `match` like

```
match x with
| Some x -> Some (f x)
| None -> None
```

forces the reader to reconstruct a very common combinator from first principles.

`Option.map` says it directly, and it keeps the match surface reserved for the places
where the branches really do diverge in a more interesting way.

This rule only targets the obvious shape where the match preserves `None` and rebuilds
`Some` on the success branch.
|};
  }

let explanations = fun () -> [ explanation ]

let option_case_kind = fun case ->
  match Ast.MatchCase.view case with
  | Case { guard = Some _; _ } -> `Other
  | Unknown _ -> `Other
  | Case { pattern; _ } ->
      match Ast.Pattern.view (H.unwrap_pattern pattern) with
      | Constructor { constructor; payload = Some argument_pattern } ->
          match (H.ident_last_name constructor, H.identifier_name_of_pattern argument_pattern) with
          | (Some "Some", Some name) -> `SomeCase name
          | _ -> `Other
      | Constructor { constructor; payload = None } ->
          match H.ident_last_name constructor with
          | Some "None" -> `NoneCase
          | _ -> `Other
      | _ -> `Other

let is_none_expression = fun expr -> H.is_constructor_expr ~name:"None" expr

let is_some_expression = fun expr ->
  match H.constructor_payload ~name:"Some" expr with
  | Some _ -> true
  | None -> false

let matches_option_map_shape = fun expr ->
  match H.match_cases expr with
  | [ first_case; second_case ] ->
      match (
        Ast.MatchCase.view first_case,
        Ast.MatchCase.view second_case,
        option_case_kind first_case,
        option_case_kind second_case
      ) with
      | (
          Case { body = first_body; _ },
          Case { body = second_body; _ },
          `SomeCase _bound_name,
          `NoneCase
        ) -> is_some_expression first_body && is_none_expression second_body
      | (
          Case { body = first_body; _ },
          Case { body = second_body; _ },
          `NoneCase,
          `SomeCase _bound_name
        ) -> is_none_expression first_body && is_some_expression second_body
      | _ -> false
  | _ -> false

let make_diagnostic = fun expr ->
  Api.Diagnostic.make
    ~severity:Warning
    ~kind:(Api.Diagnostic.Known {
      rule_id = package_rule_id;
      message = explanation.Api.Explanation.message;
    })
    ~span:(H.expr_span expr)
    ~suggestion:"Prefer Option.map for this Some/None-preserving transformation."
    ()

let diagnostic_for_expression = fun expr ->
  match Ast.Expr.view (H.unwrap_expr expr) with
  | Match _ when matches_option_map_shape expr -> Some (make_diagnostic expr)
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
