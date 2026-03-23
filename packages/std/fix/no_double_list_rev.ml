open Std
open Std.Collections

module Api = Tusk_fix_api

let package_name = "std"
let package_rule_id = package_name ^ ":no-double-list-rev"

let explanation =
  Api.Explanation.
    {
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
  | Syn.Cst.Expression.Parenthesized expr ->
      unwrap_parens expr.inner
  | expr -> expr

let path_text path =
  Syn.Cst.Ident.segments path
  |> List.map Syn.Cst.Token.text
  |> String.concat "."

let ends_with_list_rev path =
  let segments =
    Syn.Cst.Ident.segments path |> List.map Syn.Cst.Token.text
  in
  match List.rev segments with
  | "rev" :: "List" :: _ -> true
  | _ -> false

let is_list_rev = function
  | Syn.Cst.Expression.Path expr ->
      ends_with_list_rev expr.path
  | _ -> false

let expression_of_apply_argument = function
  | Syn.Cst.Positional expr -> Some expr
  | Syn.Cst.Labeled { value; _ } | Syn.Cst.Optional { value; _ } ->
      value

let diagnostic_for_expression = function
  | Syn.Cst.Expression.Apply outer
    when is_list_rev (unwrap_parens outer.callee) -> (
        match
          outer.argument |> expression_of_apply_argument
          |> Option.map unwrap_parens
        with
        | Some (Syn.Cst.Expression.Apply inner)
          when is_list_rev (unwrap_parens inner.callee)
            ->
              Some
                (Api.Diagnostic.make ~severity:Warning
                   ~kind:
                     (Api.Diagnostic.Known
                        {
                          rule_id = explanation.Api.Explanation.rule_id;
                          message = explanation.Api.Explanation.message;
                        })
                   ~span:(outer.syntax_node |> Syn.Ceibo.Red.SyntaxNode.span)
                   ~suggestion:
                     ("Replace " ^ path_text
                        (match unwrap_parens outer.callee with
                        | Syn.Cst.Expression.Path expr ->
                            expr.path
                        | _ -> panic "expected path expression")
                    ^ " (" ^ path_text
                        (match unwrap_parens inner.callee with
                        | Syn.Cst.Expression.Path expr ->
                            expr.path
                        | _ -> panic "expected path expression")
                    ^ " xs) with xs or keep only one rev if order matters.")
                   ())
        | _ -> None)
  | _ -> None

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
    ~description:"Double List.rev calls should be simplified"
    ~explain:explanation.body ~run:check_tree ()
