open Std
open Std.Collections

module Api = Fixme
module H = Ast_rule_helpers

let package_name = "std"

let package_rule_id = Api.Rule_id.from_string (package_name ^ ":no-double-list-rev")

let explanation =
  Api.Explanation.{
    rule_id = package_rule_id;
    message = "List.rev (List.rev xs) cancels itself out and should be simplified.";
    body = {|
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

let rev_argument = fun expr ->
  let (head, arguments) = H.flatten_apply expr in
  match (H.expr_name head, arguments) with
  | (Some "List.rev", [ argument ]) -> Some argument
  | _ -> None

let make_fix = fun ~outer ~replacement ->
  Api.Fix.make
    ~title:"Replace List.rev (List.rev xs) with xs"
    ~operations:[
      Api.Fix.replace_node ~target:(H.expr_node outer) ~replacement:(H.expr_node replacement);
    ]

let make_diagnostic = fun ~outer ~replacement ->
  Api.Diagnostic.make
    ~severity:Warning
    ~kind:(Api.Diagnostic.Known {
      rule_id = package_rule_id;
      message = explanation.Api.Explanation.message;
    })
    ~span:(H.expr_span outer)
    ~suggestion:"Replace List.rev (List.rev xs) with xs or keep only one rev if order matters."
    ~fix:(make_fix ~outer ~replacement)
    ()

let diagnostic_for_expression = fun expr ->
  match rev_argument expr with
  | Some inner_apply ->
      (match rev_argument inner_apply with
      | Some replacement -> Some (make_diagnostic ~outer:expr ~replacement)
      | None -> None)
  | None -> None

let check_tree = fun (ctx: Api.Rule.context) _red_root ->
  Riot_fix.Rule_query.expressions ctx
  |> List.filter_map ~fn:diagnostic_for_expression

let rule = fun () ->
  Api.Rule.make
    ~id:package_rule_id
    ~description:"Double List.rev calls should be simplified"
    ~explain:explanation.body
    ~run:check_tree
    ()
