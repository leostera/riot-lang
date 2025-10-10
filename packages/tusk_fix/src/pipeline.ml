open Std

type t = { rules : Rule.t list }
type result = { tree : Rule.green_tree; diagnostics : Diagnostic.t list }

let make ~rules () = { rules }

let run pipeline ?filename source =
  let tokens = Syn.tokenize source in
  let parse_result = Syn.Parser.parse ~source ?filename tokens in
  let parse_diagnostics =
    parse_result.diagnostics
    |> List.map (fun diag ->
        Diagnostic.make ~severity:Error
          ~message:(Syn.Diagnostic.to_string diag)
          ~span:diag.span ~rule_id:"parse_error" ())
  in
  let red_tree = Syn.Ceibo.Red.new_root parse_result.tree in
  let file_path = Option.unwrap_or ~default:"<stdin>" filename |> Path.v in
  let ctx = Rule.{ file_path } in
  let lint_diagnostics =
    pipeline.rules
    |> List.map (fun rule -> Rule.run rule ctx red_tree)
    |> List.concat
  in
  {
    tree = parse_result.tree;
    diagnostics = parse_diagnostics @ lint_diagnostics;
  }

let default_rules () = [ Rules.No_stdlib.make () ]
let default () = make ~rules:(default_rules ()) ()
