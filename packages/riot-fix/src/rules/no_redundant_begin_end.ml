open Std

let rule_id = "no-redundant-begin-end"

let rule_description = "begin/end blocks should be replaced by ordinary grouping or removed"

let rule_explain = {|
`begin ... end` is a perfectly valid grouping construct, but for ordinary expression
grouping it is usually heavier than necessary. Most readers parse parentheses faster
than `begin` and `end`, especially when the grouped expression is short.

If the block exists only to force grouping, prefer plain parentheses or remove the
grouping entirely when precedence already makes the expression obvious.

This keeps the visual weight of the code proportional to the job the grouping is
actually doing.
|}

let opens_with_begin = fun ({ syntax_node; _ }: Syn.Cst.parenthesized_expression) ->
  Syn.Ceibo.Red.SyntaxNode.children syntax_node
  |> List.filter_map ~fn:(function
    | Syn.Ceibo.Red.Token token ->
        let text = Syn.Ceibo.Red.SyntaxToken.text token in
        if String.equal text " " || String.equal text "\n" || String.equal text "\t" then
          None
        else
          Some (String.equal text "begin")
    | _ ->
        None)
  |> List.head
  |> Option.unwrap_or ~default:false

let make_fix = fun ({ syntax_node; inner; _ }: Syn.Cst.parenthesized_expression) ->
  Fix.make
    ~title:"Replace begin/end with ordinary grouping"
    ~operations:[
      Fix.replace_node ~target:syntax_node ~replacement:(Syn.Cst.Expression.syntax_node inner);
    ]

let make_diagnostic = fun ({ syntax_node; _ } as expr: Syn.Cst.parenthesized_expression) ->
  Diagnostic.make
    ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span syntax_node)
    ~suggestion:"Replace begin/end with ordinary grouping or remove it entirely."
    ~fix:(make_fix expr)
    ()

let diagnostic_for_expression = function
  | Syn.Cst.Expression.Parenthesized expr when opens_with_begin expr -> Some (make_diagnostic expr)
  | _ -> None

let check_tree = fun (ctx: Rule.context) _red_root ->
  let source_file = ctx.cst in
  Syn.Cst.SourceFile.structure_items source_file
  |> Option.unwrap_or ~default:[]
  |> List.map ~fn:Traversal.expressions_of_structure_item
  |> List.concat
  |> List.filter_map ~fn:diagnostic_for_expression

let make = fun () ->
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain ~run:check_tree ()
