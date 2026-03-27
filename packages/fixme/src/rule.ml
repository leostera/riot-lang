open Std

type green_tree = (Syn.SyntaxKind.t, string) Syn.Ceibo.Green.node
type red_tree = (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node
type context = {
  file_path : string;
  cst : Syn.Cst.source_file;
}

type t = {
  id : string;
  description : string;
  explain : string;
  enabled : bool;
  run : context -> red_tree -> Diagnostic.t list;
}

let make ~id ~description ~explain ?(enabled = true) ~run () =
  { id; description; explain; enabled; run }

let id rule = rule.id
let explain rule = rule.explain
let description rule = rule.description
let enabled rule = rule.enabled
let run rule ctx tree = if rule.enabled then rule.run ctx tree else []

let explanation rule =
  Explanation.
    {
      rule_id = rule.id;
      body = rule.explain;
      message = rule.description;
    }
