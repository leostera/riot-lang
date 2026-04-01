open Std
module Api = Fixme

let package_name = "miniriot"

let package_rule_id = package_name ^ ":loop-yield"

let explanation =
  Api.Explanation.{
    rule_id = package_rule_id;
    message = "Miniriot while and for loops should yield immediately.";
    body =
      {|
Long-running loops inside `miniriot` need to cooperate with the scheduler explicitly.

If a `while` or `for` loop starts doing work before it yields, that loop can hold onto a
reduction slice for longer than intended and make the runtime feel unfair under load.
Putting `yield ()` first keeps the loop honest: each iteration gives the scheduler a
chance to run something else before continuing.

This rule only looks for the clear structural shapes:

- `while ... do yield (); ... done`
- `for ... do yield (); ... done`

If the first effect in the loop body is not a yield, the loop is called out.
|};
  }

let explanations = fun () -> [ explanation ]

let rec unwrap_expression = fun expr ->
  match expr with
  | Syn.Cst.Expression.Parenthesized { inner; _ } -> unwrap_expression inner
  | Syn.Cst.Expression.TypeAscription { expression; _ }
  | Syn.Cst.Expression.Polymorphic { expression; _ } -> unwrap_expression expression
  | _ -> expr

let rec flatten_apply = fun expr ->
  match unwrap_expression expr with
  | Syn.Cst.Expression.Apply { callee; argument; _ } ->
      let head, arguments = flatten_apply callee in
      (head, arguments @ [ argument ])
  | _ -> (unwrap_expression expr, [])

let positional_arguments = fun arguments ->
  arguments |> List.filter_map
    (
      function
      | Syn.Cst.Positional expr -> Some expr
      | Syn.Cst.Labeled _
      | Syn.Cst.Optional _ -> None
    )

let rec expression_last_name = fun expr ->
  match unwrap_expression expr with
  | Syn.Cst.Expression.Path { path; _ } -> Syn.Cst.Ident.name path
  | Syn.Cst.Expression.FieldAccess { field_name; _ } -> Some (Syn.Cst.Token.text field_name)
  | _ -> None

let is_unit_expression = fun expr ->
  match unwrap_expression expr with
  | Syn.Cst.Expression.Literal (Syn.Cst.Literal.Unit _) -> true
  | _ -> false

let is_yield_call = fun expr ->
  let head, arguments = flatten_apply expr in
  match expression_last_name head, positional_arguments arguments with
  | Some "yield", [ argument ] -> is_unit_expression argument
  | _ -> false

let body_starts_with_yield = fun expr ->
  match unwrap_expression expr with
  | Syn.Cst.Expression.Sequence { expressions; _ } -> (
      match expressions with
      | first :: _ -> is_yield_call first
      | [] -> false
    )
  | other -> is_yield_call other

let make_diagnostic = fun syntax_node ->
  Api.Diagnostic.make
    ~severity:Warning
    ~kind:(Api.Diagnostic.Known {
      rule_id = package_rule_id;
      message = explanation.Api.Explanation.message
    })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span syntax_node)
    ~suggestion:"Start the loop body with `yield ()` so each iteration cooperates with the scheduler."
    ()

let diagnostic_for_expression = function
  | Syn.Cst.Expression.While { syntax_node; body; _ } when not (body_starts_with_yield body) -> Some (make_diagnostic
    syntax_node)
  | Syn.Cst.Expression.For { syntax_node; body; _ } when not (body_starts_with_yield body) -> Some (make_diagnostic
    syntax_node)
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
