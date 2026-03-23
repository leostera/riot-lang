open Std
open Std.Collections
open Tusk_fix_api

let package_name = "std"
let package_rule_id = package_name ^ ":no-double-list-rev"

let explanation = Explanation.{
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


let list_dot_rev = Syn.Cst.Ident.from_string "List.rev"

let severity = Diagnostic.Warning
let kind =
  Diagnostic.Known
    { rule_id = explanation.rule_id; message = explanation.message }

let suggestion = "Replace List.rev (List.rev xs) with xs or keep only one rev if order matters."

let rec unwrap_expression = function
  | Syn.Cst.Expression.Parenthesized expr ->
      unwrap_expression expr.inner
  | expr ->
      expr

let make_fix ~(outer : Syn.Cst.Expression.t) ~(replacement : Syn.Cst.Expression.t) =
  Fix.make
    ~title:"Replace List.rev (List.rev xs) with xs"
    ~operations:[
        Fix.replace_node
          ~target:(outer |> Syn.Cst.Expression.syntax_node)
          ~replacement:(replacement |> Syn.Cst.Expression.syntax_node);
      ]

let make_diagnostic outer replacement =
  let fix = (make_fix ~outer ~replacement) in
  let span = (outer |> Syn.Cst.Expression.syntax_node |> Syn.Ceibo.Red.SyntaxNode.span) in
  Diagnostic.make ~severity ~kind ~span ~suggestion ~fix ()

let is_double_rev expr =
  let open Syn.Cst in
  match expr with
  | Expression.Apply
      {
        callee = Expression.Path { path; _ };
        argument = Positional inner_apply;
        _;
      } -> (
      match unwrap_expression inner_apply with
      | Expression.Apply
          {
            callee = Expression.Path { path = path2; _ };
            argument = Positional inner;
            _;
          }
        when Ident.equal path list_dot_rev && Ident.equal path2 list_dot_rev ->
          `Unwrap inner
      | _ ->
          `Skip)
  | _ -> `Skip

let diagnostic_for_expression diag expr =
  match is_double_rev expr with
  | `Skip -> diag
  | `Unwrap inner -> (make_diagnostic expr inner) :: diag

let visit_expression
    (diagnostics : Diagnostic.t list)
    (walk : Diagnostic.t list Syn.Visit.walker)
    (expression : Syn.Cst.Expression.t) =
  let diagnostics = diagnostic_for_expression diagnostics expression in
  walk.descend_expression diagnostics expression

let visitor = Syn.Visit.{ default with visit_expression = visit_expression }

let run (ctx : Rule.context) _red_root =
  Syn.Visit.source_file visitor [] ctx.cst |> List.rev

let rule () =
  Rule.make ~id:package_rule_id
    ~description:"Double List.rev calls should be simplified"
    ~explain:explanation.body ~run ()

let explanations () = [ explanation ]
