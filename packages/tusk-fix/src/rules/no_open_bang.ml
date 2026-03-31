open Std
open Std.Collections

let rule_id = "no-open-bang"

let rule_description = "Avoid open! and prefer plain open or explicit qualification"

let rule_explain = {|
`open!` suppresses the compiler warning that would normally tell you about accidental
shadowing. That is a strong tradeoff to make for a small reduction in module
qualification.

If an open is truly harmless, plain `open` keeps the code readable without disabling
the warning mechanism. If the scope is sensitive, explicit qualification like
`List.map` or `Http.Response.ok` makes the dependency even clearer.

This rule exists because shadowing bugs are cheap to introduce and annoying to notice
late. `open!` makes that problem easier to miss.
|}

let make_diagnostic = fun token ->
  Diagnostic.make
  ~severity:Warning
  ~kind:(Diagnostic.Known {rule_id; message = rule_description})
  ~span:(Syn.Ceibo.Red.SyntaxToken.span token)
  ~suggestion:"Remove ! or qualify the module usage explicitly."
  ()

let diagnostic_for_open_statement = fun stmt ->
  match Syn.Cst.OpenStatement.bang_token stmt with
  | Some bang_token -> Some (make_diagnostic (Syn.Cst.Token.syntax_token bang_token))
  | None -> None

let diagnostics_for_items = fun source_file ->
  match source_file with
  | Syn.Cst.Implementation { items; _ } ->
      items |> List.filter_map
        (
          function
          | Syn.Cst.StructureItem.OpenStatement stmt -> diagnostic_for_open_statement stmt
          | _ -> None
        )
  | Syn.Cst.Interface { items; _ } ->
      items |> List.filter_map
        (
          function
          | Syn.Cst.SignatureItem.OpenStatement stmt -> diagnostic_for_open_statement stmt
          | _ -> None
        )

let check_tree = fun (ctx:Rule.context) _red_root ->
  let source_file = ctx.cst in
  diagnostics_for_items source_file

let make = fun () ->
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain ~run:check_tree ()
