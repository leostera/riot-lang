open Std
open Std.Collections

module Api = Tusk_fix_api

let package_name = "std"
let package_rule_id = package_name ^ ":no-double-list-rev"

let explanation =
  Api.Explanation.
    {
      code = "std:f0006";
      rule_id = package_rule_id;
      title = "Avoid double List.rev";
      message = "List.rev (List.rev xs) cancels itself out and should be simplified.";
      body =
        {|
Avoid calling List.rev twice in a row.

Why this rule exists:
- List.rev (List.rev xs) returns the original list again.
- The double reversal adds work without changing the result.
- When this shows up in real code, it usually means an intermediate transform was removed and the cleanup never happened.

What to do instead:
- Replace List.rev (List.rev xs) with xs.
- If only one reversal is actually needed, keep just the one that reflects the intended order.

Examples:
  Bad:    List.rev (List.rev items)
  Better: items
  Better: List.rev items
|};
    }

let explanations () = [ explanation ]

let rec unwrap_parens = function
  | Syn.Cst.Expression.ParenthesizedExpression expr ->
      unwrap_parens (Syn.Cst.ParenthesizedExpression.inner expr)
  | expr -> expr

let path_text path =
  Syn.Cst.ModulePath.segments path
  |> List.map Syn.Cst.Token.text
  |> String.concat "."

let ends_with_list_rev path =
  let segments =
    Syn.Cst.ModulePath.segments path |> List.map Syn.Cst.Token.text
  in
  match List.rev segments with
  | "rev" :: "List" :: _ -> true
  | _ -> false

let is_list_rev = function
  | Syn.Cst.Expression.PathExpression expr ->
      ends_with_list_rev (Syn.Cst.PathExpression.path expr)
  | _ -> false

let diagnostic_for_expression = function
  | Syn.Cst.Expression.ApplyExpression outer
    when is_list_rev (unwrap_parens (Syn.Cst.ApplyExpression.callee outer)) -> (
        match unwrap_parens (Syn.Cst.ApplyExpression.argument outer) with
        | Syn.Cst.Expression.ApplyExpression inner
          when is_list_rev (unwrap_parens (Syn.Cst.ApplyExpression.callee inner))
            ->
              Some
                (Api.Diagnostic.make ~severity:Warning
                   ~kind:
                     (Api.Diagnostic.Known
                        {
                          code = explanation.code;
                          rule_id = explanation.rule_id;
                          message = explanation.message;
                        })
                   ~span:
                     (Syn.Cst.ApplyExpression.syntax_node outer
                     |> Syn.Ceibo.Red.SyntaxNode.span)
                   ~suggestion:
                     ("Replace " ^ path_text
                        (match unwrap_parens (Syn.Cst.ApplyExpression.callee outer) with
                        | Syn.Cst.Expression.PathExpression expr ->
                            Syn.Cst.PathExpression.path expr
                        | _ -> panic "expected path expression")
                    ^ " (" ^ path_text
                        (match unwrap_parens (Syn.Cst.ApplyExpression.callee inner) with
                        | Syn.Cst.Expression.PathExpression expr ->
                            Syn.Cst.PathExpression.path expr
                        | _ -> panic "expected path expression")
                    ^ " xs) with xs or keep only one rev if order matters.")
                   ())
        | _ -> None)
  | _ -> None

let check_tree (ctx : Api.Rule.context) _red_root =
  match ctx.cst with
  | None -> []
  | Some source_file ->
      Syn.Cst.SourceFile.expressions source_file
      |> List.filter_map diagnostic_for_expression

let rule () =
  Api.Rule.make ~id:package_rule_id ~code:explanation.code
    ~name:"No Double List.rev"
    ~description:"Double List.rev calls should be simplified"
    ~explain:explanation.body ~run:check_tree ()
