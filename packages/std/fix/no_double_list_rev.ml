open Std
open Std.Collections
module Api = Fixme

let package_name = "std"

let package_rule_id = Api.Rule_id.of_string (package_name ^ ":no-double-list-rev")

let explanation =
  Api.Explanation.{
    rule_id = package_rule_id;
    message = "List.rev (List.rev xs) cancels itself out and should be simplified.";
    body =
      {|
Avoid calling List.rev twice in a row.

Examples:
Instead of:
List.rev (List.rev items)

write either:
items

or, if the reversal is still semantically needed:
List.rev items

`List.rev (List.rev xs)` returns the original list again. The extra reversal
adds work without changing the result, and in real code it usually means an
intermediate transformation was deleted while the cleanup never happened.
|};
  }

let explanations = fun () -> [ explanation ]

let rec unwrap_expression = fun expr ->
  match expr with
  | Syn.Cst.Expression.Parenthesized { inner; _ } -> unwrap_expression inner
  | _ -> expr

let rec expression_name = fun expr ->
  match unwrap_expression expr with
  | Syn.Cst.Expression.Path { path; _ } ->
      Syn.Cst.Ident.segments path
      |> List.map Syn.Cst.Token.text
      |> String.concat "."
      |> fun name -> Some name
  | Syn.Cst.Expression.FieldAccess { receiver; field_name; _ } -> (
      match expression_name receiver with
      | Some receiver_name -> Some (receiver_name ^ "." ^ Syn.Cst.Token.text field_name)
      | None -> None
    )
  | _ ->
      None

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

let rev_argument = fun expr ->
  let head, arguments = flatten_apply expr in
  match expression_name head, positional_arguments arguments with
  | Some "List.rev", [ argument ] -> Some argument
  | _ -> None

let make_fix = fun ~outer ~replacement ->
  Api.Fix.make
    ~title:"Replace List.rev (List.rev xs) with xs"
    ~operations:[
      Api.Fix.replace_node
        ~target:(Syn.Cst.Expression.syntax_node outer)
        ~replacement:(Syn.Cst.Expression.syntax_node replacement);
    ]

let make_diagnostic = fun ~outer ~replacement ->
  Api.Diagnostic.make
    ~severity:Warning
    ~kind:(Api.Diagnostic.Known {
      rule_id = package_rule_id;
      message = explanation.Api.Explanation.message
    })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span (Syn.Cst.Expression.syntax_node outer))
    ~suggestion:"Replace List.rev (List.rev xs) with xs or keep only one rev if order matters."
    ~fix:(make_fix ~outer ~replacement)
    ()

let diagnostic_for_expression = fun expr ->
  match rev_argument expr with
  | Some inner_apply -> (
      match rev_argument inner_apply with
      | Some replacement -> Some (make_diagnostic ~outer:expr ~replacement)
      | None -> None
    )
  | None -> None

let check_tree = fun (ctx: Api.Rule.context) _red_root ->
  let source_file = ctx.cst in
  Syn.Cst.SourceFile.structure_items source_file
  |> Option.unwrap_or ~default:[]
  |> List.concat_map Api.Traversal.expressions_of_structure_item
  |> List.filter_map diagnostic_for_expression

let rule = fun () ->
  Api.Rule.make
    ~id:package_rule_id
    ~description:"Double List.rev calls should be simplified"
    ~explain:explanation.body
    ~run:check_tree
    ()
