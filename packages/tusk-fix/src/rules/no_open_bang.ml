open Std
open Std.Collections

let rule_id = "no-open-bang"
let rule_name = "No Open Bang"
let rule_code = "F0125"

let rule_description =
  "Avoid open! and prefer plain open or explicit qualification"

let rule_message =
  "Avoid open!; prefer plain open or explicit module qualification."

let rule_explain =
  {|
Avoid open! and prefer plain open or explicit module qualification.

Why this rule exists:
- open! suppresses the compiler's shadowing warning, which makes accidental name collisions easier to miss.
- If an open is safe, plain open communicates that without discarding the warning mechanism.
- If the scope is narrow or sensitive, explicit qualification is easier to audit than a forceful open.

Examples:
  Bad:    open! List
  Better: open List
  Better: List.map f xs
|}

let make_diagnostic token =
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { code = rule_code; rule_id; message = rule_message })
    ~span:(Syn.Ceibo.Red.SyntaxToken.span token)
    ~suggestion:"Remove ! or qualify the module usage explicitly."
    ()

let diagnostic_for_open_statement stmt =
  match Syn.Cst.OpenStatement.bang_token stmt with
  | Some bang_token ->
      Some (make_diagnostic (Syn.Cst.Token.syntax_token bang_token))
  | None -> None

let diagnostics_for_items source_file =
  match source_file with
  | Syn.Cst.Implementation { items; _ } ->
      items
      |> List.filter_map (function
           | Syn.Cst.StructureItem.OpenStatement stmt ->
               diagnostic_for_open_statement stmt
           | _ -> None)
  | Syn.Cst.Interface { items; _ } ->
      items
      |> List.filter_map (function
           | Syn.Cst.SignatureItem.OpenStatement stmt ->
               diagnostic_for_open_statement stmt
           | _ -> None)

let check_tree (ctx : Rule.context) _red_root =
  match ctx.cst with
  | None -> []
  | Some source_file -> diagnostics_for_items source_file

let make () =
  Rule.make ~id:rule_id ~code:rule_code ~name:rule_name
    ~description:rule_description ~message:rule_message ~explain:rule_explain
    ~run:check_tree ()
