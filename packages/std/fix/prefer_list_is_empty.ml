open Std
open Std.Collections

module Api = Tusk_fix_api

let package_name = "std"
let package_rule_id = package_name ^ ":prefer-list-is-empty"

let explanation =
  Api.Explanation.
    {
      rule_id = package_rule_id;
      message = "List emptiness checks should use List.is_empty instead of comparing List.length to zero.";
      body =
        {|
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

let explanations () = [ explanation ]

let rec unwrap_expression expr =
  match expr with
  | Syn.Cst.Expression.Parenthesized { inner; _ } ->
      unwrap_expression inner
  | _ ->
      expr

let rec expression_name expr =
  match unwrap_expression expr with
  | Syn.Cst.Expression.Path { path; _ } ->
      Syn.Cst.Ident.segments path
      |> List.map Syn.Cst.Token.text
      |> String.concat "."
      |> fun name -> Some name
  | Syn.Cst.Expression.FieldAccess { receiver; field_name; _ } -> (
      match expression_name receiver with
      | Some receiver_name ->
          Some (receiver_name ^ "." ^ Syn.Cst.Token.text field_name)
      | None ->
          None)
  | _ ->
      None

let rec flatten_apply expr =
  match unwrap_expression expr with
  | Syn.Cst.Expression.Apply { callee; argument; _ } ->
      let head, arguments = flatten_apply callee in
      (head, arguments @ [ argument ])
  | _ ->
      (unwrap_expression expr, [])

let positional_arguments arguments =
  arguments
  |> List.filter_map (function
       | Syn.Cst.Positional expr -> Some expr
       | Syn.Cst.Labeled _ | Syn.Cst.Optional _ -> None)

let is_zero_literal expr =
  match unwrap_expression expr with
  | Syn.Cst.Expression.Literal (Syn.Cst.Literal.Int { digits; base; _ }) ->
      base = Syn.Cst.Decimal && String.equal digits "0"
  | _ ->
      false

let list_length_argument expr =
  let head, arguments = flatten_apply expr in
  match expression_name head, positional_arguments arguments with
  | Some "List.length", [ argument ] -> Some argument
  | _ -> None

let make_diagnostic ~suggestion expr =
  Api.Diagnostic.make ~severity:Warning
    ~kind:
      (Api.Diagnostic.Known
         {
           rule_id = package_rule_id;
           message = explanation.Api.Explanation.message;
         })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span (Syn.Cst.Expression.syntax_node expr))
    ~suggestion ()

let diagnostic_for_expression expr =
  match unwrap_expression expr with
  | Syn.Cst.Expression.Infix infix -> (
      let operator = Syn.Cst.InfixExpression.operator infix in
      let left = Syn.Cst.InfixExpression.left infix in
      let right = Syn.Cst.InfixExpression.right infix in
      match operator, list_length_argument left, list_length_argument right with
      | "==", Some _list_expr, _ when is_zero_literal right ->
          Some
            (make_diagnostic
               ~suggestion:"Use List.is_empty xs for this emptiness check."
               expr)
      | ">", Some _list_expr, _ when is_zero_literal right ->
          Some
            (make_diagnostic
               ~suggestion:
                 "Use not (List.is_empty xs) when checking that a list has elements."
               expr)
      | _ ->
          None)
  | _ ->
      None

let check_tree (ctx : Api.Rule.context) _red_root =
  match ctx.cst with
  | None -> []
  | Some source_file ->
      Syn.Cst.SourceFile.structure_items source_file
      |> Option.unwrap_or ~default:[]
      |> List.concat_map Api.Traversal.expressions_of_structure_item
      |> List.filter_map diagnostic_for_expression

let rule () =
  Api.Rule.make ~id:package_rule_id
    ~description:explanation.message
    ~explain:explanation.body ~run:check_tree ()
