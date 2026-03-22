open Std

let rule_id = "no-redundant-begin-end"
let rule_name = "No Redundant Begin End"
let rule_code = "F0139"

let rule_description =
  "begin/end blocks should be replaced by ordinary grouping or removed"

let rule_message =
  "Replace begin/end blocks with ordinary grouping or remove them."

let rule_explain =
  {|
Avoid `begin ... end` for ordinary expression grouping.

`begin ... end` behaves like parentheses, but it is heavier on the page and harder to scan.
When the block only exists for grouping, prefer plain parentheses or drop the grouping entirely.

Examples:
  Avoid:   let value = begin render item end
  Better:  let value = render item

  Avoid:   let value = begin render (item + 1) end
  Better:  let value = (render (item + 1))
|}

let opens_with_begin ({ syntax_node; _ } : Syn.Cst.parenthesized_expression) =
  Syn.Ceibo.Red.SyntaxNode.children syntax_node
  |> Std.Collections.Array.to_list
  |> List.find_map (function
         | Syn.Ceibo.Red.Token token ->
             let text = Syn.Ceibo.Red.SyntaxToken.text token in
             if String.equal text " " || String.equal text "\n" || String.equal text "\t" then
               None
             else
               Some (String.equal text "begin")
         | _ -> None)
  |> Option.unwrap_or ~default:false

let make_diagnostic ({ syntax_node; _ } : Syn.Cst.parenthesized_expression) =
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { code = rule_code; rule_id; message = rule_message })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span syntax_node)
    ~suggestion:"Replace begin/end with ordinary grouping or remove it entirely."
    ()

let diagnostic_for_expression = function
  | Syn.Cst.Expression.Parenthesized expr when opens_with_begin expr ->
      Some (make_diagnostic expr)
  | _ -> None

let check_tree (ctx : Rule.context) _red_root =
  match ctx.cst with
  | None -> []
  | Some source_file ->
      Syn.Cst.SourceFile.expressions source_file
      |> List.filter_map diagnostic_for_expression

let make () =
  Rule.make ~id:rule_id ~code:rule_code ~name:rule_name
    ~description:rule_description ~message:rule_message ~explain:rule_explain
    ~run:check_tree ()
