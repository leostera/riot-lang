open Std

type green_tree = (Syn.SyntaxKind.t, string) Syn.Ceibo.Green.node
type red_tree = (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node
type context = {
  file_path : string;
  cst : Syn.Cst.source_file option;
}

type t = {
  id : string;
  code : string option;
  name : string;
  description : string;
  message : string option;
  explain : string;
  enabled : bool;
  run : context -> red_tree -> Diagnostic.t list;
}

let make ~id ?code ~name ~description ?message ~explain ?(enabled = true) ~run () =
  { id; code; name; description; message; explain; enabled; run }

let id rule = rule.id
let code rule = rule.code
let name rule = rule.name
let explain rule = rule.explain
let description rule = rule.description
let message rule = rule.message
let enabled rule = rule.enabled
let run rule ctx tree = if rule.enabled then rule.run ctx tree else []

let explanation rule =
  match rule.code with
  | Some code ->
      Some Explanation.
        {
          code;
          rule_id = rule.id;
          title = rule.name;
          body = rule.explain;
          message = Option.unwrap_or ~default:rule.description rule.message;
        }
  | None -> None
