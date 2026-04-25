open Std

type syntax_tree = Syn.SyntaxTree.t

type syntax_root = Syn.Ast.Node.t

type context = {
  file_path: string;
  source: string;
  source_file: Syn.Ast.SourceFile.t;
}

type t = {
  id: Rule_id.t;
  description: string;
  explain: string;
  enabled: bool;
  run: context -> syntax_root -> Diagnostic.t list;
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
