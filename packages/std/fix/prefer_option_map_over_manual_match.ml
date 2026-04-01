open Std
module Api = Fixme

let package_name = "std"

let package_rule_id = package_name ^ ":prefer-option-map-over-manual-match"

let explanation =
  Api.Explanation.{
    rule_id = package_rule_id;
    message = "Manual option matches that only rebuild Some/None should use Option.map.";
    body =
      {|
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

let rec unwrap_expression = fun expr ->
  match expr with
  | Syn.Cst.Expression.Parenthesized { inner; _ } -> unwrap_expression inner
  | Syn.Cst.Expression.TypeAscription { expression; _ }
  | Syn.Cst.Expression.Polymorphic { expression; _ } -> unwrap_expression expression
  | _ -> expr

let constructor_name_of_ident = fun ident -> Syn.Cst.Ident.name ident

let identifier_name_of_pattern = function
  | Syn.Cst.Pattern.Identifier { name_token; _ } -> Some (Syn.Cst.Token.text name_token)
  | _ -> None

let option_case_kind = fun (case: Syn.Cst.match_case) ->
  if Option.is_some case.guard then
    `Other
  else
    match case.pattern with
    | Syn.Cst.Pattern.Constructor { constructor_path; arguments=[ argument_pattern ]; _;  } -> (
        match constructor_name_of_ident constructor_path, identifier_name_of_pattern argument_pattern with
        | Some "Some", Some name -> `SomeCase name
        | _ -> `Other
      )
    | Syn.Cst.Pattern.Constructor { constructor_path; arguments=[]; _;  } -> (
        match constructor_name_of_ident constructor_path with
        | Some "None" -> `NoneCase
        | _ -> `Other
      )
    | _ ->
        `Other

let is_none_expression = fun expr ->
  match unwrap_expression expr with
  | Syn.Cst.Expression.Constructor { constructor_path; payload=None; _ } -> (
      match constructor_name_of_ident constructor_path with
      | Some "None" -> true
      | _ -> false
    )
  | _ -> false

let is_some_expression = fun expr ->
  match unwrap_expression expr with
  | Syn.Cst.Expression.Constructor { constructor_path; payload=Some _; _ } -> (
      match constructor_name_of_ident constructor_path with
      | Some "Some" -> true
      | _ -> false
    )
  | _ -> false

let matches_option_map_shape = fun (expr: Syn.Cst.match_expression) ->
  match expr.cases with
  | [first_case;second_case] -> (
      match option_case_kind first_case, option_case_kind second_case with
      | `SomeCase _bound_name, `NoneCase -> is_some_expression first_case.body
      && is_none_expression second_case.body
      | `NoneCase, `SomeCase _bound_name -> is_none_expression first_case.body
      && is_some_expression second_case.body
      | _ -> false
    )
  | _ -> false

let make_diagnostic = fun (expr: Syn.Cst.match_expression) ->
  Api.Diagnostic.make
    ~severity:Warning
    ~kind:(Api.Diagnostic.Known {
      rule_id = package_rule_id;
      message = explanation.Api.Explanation.message
    })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span expr.syntax_node)
    ~suggestion:"Prefer Option.map for this Some/None-preserving transformation."
    ()

let diagnostic_for_expression = function
  | Syn.Cst.Expression.Match expr when matches_option_map_shape expr -> Some (make_diagnostic expr)
  | _ -> None

let check_tree = fun (ctx: Api.Rule.context) _red_root ->
  let source_file = ctx.cst in
  Syn.Cst.SourceFile.structure_items source_file
  |> Option.unwrap_or ~default:[]
  |> List.concat_map Api.Traversal.expressions_of_structure_item
  |> List.filter_map diagnostic_for_expression

let rule = fun () ->
  Api.Rule.make
    ~id:package_rule_id
    ~description:explanation.message
    ~explain:explanation.body
    ~run:check_tree
    ()
