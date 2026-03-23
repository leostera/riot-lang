open Std

type green_tree = (Syn.SyntaxKind.t, string) Syn.Ceibo.Green.node
type red_tree = (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node
type context = {
  file_path : string;
  cst : Syn.Cst.source_file option;
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

let title_of_id id =
  let local_id =
    match String.rindex_opt id ':' with
    | Some idx when idx + 1 < String.length id ->
        String.sub id (idx + 1) (String.length id - idx - 1)
    | _ -> id
  in
  let parts =
    local_id
    |> String.split_on_char '-'
    |> List.filter (fun part -> not (String.equal part ""))
  in
  let capitalize word =
    if String.length word = 0 then
      word
    else
      String.uppercase_ascii (String.sub word 0 1)
      ^ String.lowercase_ascii (String.sub word 1 (String.length word - 1))
  in
  parts |> List.map capitalize |> String.concat " "

let explanation rule =
    Explanation.
      {
        rule_id = rule.id;
        title = title_of_id rule.id;
        body = rule.explain;
        message = rule.description;
      }
