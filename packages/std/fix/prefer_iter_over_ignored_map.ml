open Std
open Std.Collections

module Api = Fixme

let package_name = "std"

let package_rule_id = Api.Rule_id.of_string (package_name ^ ":prefer-iter-over-ignored-map")

let explanation =
  Api.Explanation.{
    rule_id = package_rule_id;
    message = "Ignoring the result of List.map or Iter.map should usually use the corresponding iter form instead.";
    body = {|
`map` is for transforming a collection into a new collection. When the result is
immediately discarded with `ignore`, that transformation result was never the real goal.
What the code is really doing is visiting each element for side effects.

Using `List.iter` or `Iter.iter` makes that intent explicit. It tells the reader that
the traversal matters, not the returned collection, and it avoids allocating a result
that the program then throws away.

This rule only targets the clear cases `ignore (List.map f xs)` and
`ignore (Iter.map f iter)`. In those shapes, the iter form is a better statement of the
program you meant to write.
|};
  }

let explanations = fun () -> [ explanation ]

let rec unwrap_expression = fun expr ->
  match expr with
  | Syn.Cst.Expression.Parenthesized { inner; _ } -> unwrap_expression inner
  | _ -> expr

let rec flatten_apply = fun expr ->
  match unwrap_expression expr with
  | Syn.Cst.Expression.Apply { callee; argument; _ } ->
      let (head, arguments) = flatten_apply callee in
      (head, arguments @ [ argument ])
  | _ -> (unwrap_expression expr, [])

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
  | _ -> None

let path_matches = fun ~expected expr ->
  match expression_name expr with
  | Some actual -> String.equal actual expected
  | None -> false

let positional_arguments = fun args ->
  args
  |> List.filter_map
    (
      function
      | Syn.Cst.Positional expr -> Some expr
      | Syn.Cst.Labeled _
      | Syn.Cst.Optional _ -> None
    )

let make_diagnostic = fun ~iter_name expr ->
  Api.Diagnostic.make
    ~severity:Warning
    ~kind:(Api.Diagnostic.Known {
      rule_id = package_rule_id;
      message = explanation.Api.Explanation.message;
    })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span (Syn.Cst.Expression.syntax_node expr))
    ~suggestion:("Use "
    ^ iter_name
    ^ " when the mapped result is ignored and the traversal exists only for side effects.")
    ()

let diagnostic_for_expression = fun expr ->
  let (head, arguments) = flatten_apply expr in
  if not (path_matches ~expected:"ignore" head) then
    None
  else
    match positional_arguments arguments with
    | [ mapped ] -> (
        let (mapped_head, mapped_arguments) = flatten_apply mapped in
        match positional_arguments mapped_arguments with
        | [ _fn; _collection ] when path_matches ~expected:"List.map" mapped_head ->
            Some (make_diagnostic ~iter_name:"List.iter" expr)
        | [ _fn; _collection ] when path_matches ~expected:"Iter.map" mapped_head ->
            Some (make_diagnostic ~iter_name:"Iter.iter" expr)
        | _ -> None
      )
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
