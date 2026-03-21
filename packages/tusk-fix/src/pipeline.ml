open Std

type t = { rules : Rule.t list }
type result = {
  tree : Rule.green_tree;
  diagnostics : Diagnostic.t list;
  parse_diagnostics : Syn.Diagnostic.t list;
}

let make ~rules () = { rules }

let builtin_rule_factories () =
  [
    ("type-name-style", Rules.Type_name_style.make);
  ]

let package_rules () =
  Provider_registry.rules ()

let unqualified_rule_id rule_id =
  match String.rindex_opt rule_id ':' with
  | Some idx ->
      String.sub rule_id (idx + 1) (String.length rule_id - idx - 1)
  | None -> rule_id

let filtered_builtin_rules package_rules =
  let shadowed_ids =
    package_rules |> List.map Rule.id |> List.map unqualified_rule_id
  in
  [ Rules.Type_name_style.make () ]
  |> List.filter (fun rule ->
         not (List.mem (unqualified_rule_id (Rule.id rule)) shadowed_ids))

let run pipeline ?filename source =
  let parse_result =
    match filename with
    | Some filename -> Syn.parse ~filename source
    | None -> Syn.parse_implementation source
  in
  (* Skip linting if there are parse errors *)
  let lint_diagnostics =
    if List.length parse_result.diagnostics > 0 then
      []
    else
      let red_tree = Syn.Ceibo.Red.new_root parse_result.tree in
      let file_path = Option.unwrap_or ~default:"<stdin>" filename in
      let ctx = Rule.{ file_path } in
      pipeline.rules
      |> List.map (fun rule -> Rule.run rule ctx red_tree)
      |> List.concat
  in
  {
    tree = parse_result.tree;
    diagnostics = lint_diagnostics;
    parse_diagnostics = parse_result.diagnostics;
  }

let default_rules () = 
  let package_rules = package_rules () in
  filtered_builtin_rules package_rules @ package_rules

let default_rule_ids () =
  default_rules () |> List.map Rule.id

let matching_rule_ids rules requested_id =
  let available_ids = List.map Rule.id rules in
  if List.mem requested_id available_ids then
    [ requested_id ]
  else if String.contains requested_id ":" then
    []
  else
    let suffix = ":" ^ requested_id in
    List.filter (fun rule_id -> String.ends_with ~suffix rule_id) available_ids

let rules_by_id ids =
  let available_rules = default_rules () in
  ids
  |> List.concat_map (fun id ->
         match matching_rule_ids available_rules id with
         | [] ->
             Log.warn ("Unknown tusk-fix rule '" ^ id ^ "', ignoring");
             []
         | matches -> matches)
  |> List.sort_uniq String.compare
  |> List.filter_map (fun id ->
         List.find_opt (fun rule -> String.equal (Rule.id rule) id) available_rules)
let default () = make ~rules:(default_rules ()) ()
