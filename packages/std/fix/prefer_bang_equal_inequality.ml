open Std
open Std.Collections

module Api = Tusk_fix_api

let package_name = "std"
let package_rule_id = package_name ^ ":prefer-bang-equal-inequality"

let explanation =
  Api.Explanation.
    {
      rule_id = package_rule_id;
      title = "Prefer != for inequality";
      message = "Use != instead of <> for inequality.";
      body =
        {|
Prefer != instead of <> for inequality.

Why this rule exists:
- Riot code uses != as the standard inequality operator.
- Keeping one inequality spelling makes conditionals and comparisons easier to scan.
- It also avoids drifting back toward older OCaml operator style in the middle of Riot code.

What to do instead:
- Replace <> with !=.

Examples:
  Bad:    if left <> right then ...
  Better: if left != right then ...
|};
    }

let explanations () = [ explanation ]

let make_fix token =
  Api.Fix.make ~title:"Replace <> with !="
    ~edits:
      [
        Api.Fix.make_text_edit ~span:(Syn.Ceibo.Red.SyntaxToken.span token)
          ~new_text:"!=";
      ]

let make_diagnostic token =
  Api.Diagnostic.make ~severity:Warning
    ~kind:
      (Api.Diagnostic.Known
         {
           rule_id = explanation.Api.Explanation.rule_id;
           message = explanation.Api.Explanation.message;
         })
    ~span:(Syn.Ceibo.Red.SyntaxToken.span token)
    ~suggestion:"Replace <> with !=."
    ~fix:(make_fix token) ()

let check_tree (ctx : Api.Rule.context) _red_root =
  match ctx.cst with
  | None -> []
  | Some source_file ->
      Syn.Cst.SourceFile.structure_items source_file
      |> Option.unwrap_or ~default:[]
      |> List.concat_map Api.Traversal.expressions_of_structure_item
      |> List.filter_map (function
           | Syn.Cst.Expression.Infix expr
             when String.equal (Syn.Cst.InfixExpression.operator expr) "<>" ->
                 let token =
                   Syn.Cst.InfixExpression.operator_token expr
                   |> Syn.Cst.Token.syntax_token
                 in
                 Some (make_diagnostic token)
           | _ -> None)

let rule () =
  Api.Rule.make ~id:package_rule_id
    ~description:"Prefer != over <> for inequality checks"
    ~explain:explanation.body ~run:check_tree ()
