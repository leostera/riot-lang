open Std

type green_tree = (Syn.SyntaxKind.t, string) Syn.Ceibo.Green.node

type red_tree = (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node

type context = {
  file_path: string;
  source: string;
  cst: Syn.Cst.source_file;
}

type t = {
  id: Rule_id.t;
  description: string;
  explain: string;
  enabled: bool;
  run: context -> red_tree -> Diagnostic.t list;
}

let make = fun ~id ~description ~explain ?(enabled = true) ~run () ->
  {
    id;
    description;
    explain;
    enabled;
    run;
  }

let id = fun rule -> rule.id

let explain = fun rule -> rule.explain

let description = fun rule -> rule.description

let enabled = fun rule -> rule.enabled

let run = fun rule ctx tree ->
  if rule.enabled then
    rule.run ctx tree
  else
    []

let explanation = fun rule ->
  Explanation.{ rule_id = rule.id; body = rule.explain; message = rule.description }
