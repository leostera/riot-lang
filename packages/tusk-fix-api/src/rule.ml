open Std

type green_tree = (Syn.SyntaxKind.t, string) Syn.Ceibo.Green.node
type red_tree = (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node
type context = {
  file_path : string;
  cst : Syn.Cst.source_file option;
}

type t = {
  id : string;
  name : string;
  description : string;
  enabled : bool;
  run : context -> red_tree -> Diagnostic.t list;
}

let make ~id ~name ~description ?(enabled = true) ~run () =
  { id; name; description; enabled; run }

let id rule = rule.id
let name rule = rule.name
let description rule = rule.description
let enabled rule = rule.enabled
let run rule ctx tree = if rule.enabled then rule.run ctx tree else []
