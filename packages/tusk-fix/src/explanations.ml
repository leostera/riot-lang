open Std

let builtin_entries = fun () -> Pipeline.builtin_rules () |> List.map Rule.explanation

let package_entries = fun () -> Provider_registry.providers () |> List.concat_map Provider.explanations

let all = fun () -> builtin_entries () @ package_entries ()

let normalize_rule_id = fun rule_id ->
  let riot_prefix = "riot:" in
  if String.starts_with ~prefix:riot_prefix rule_id then
    String.sub
    rule_id
    (String.length riot_prefix)
    (String.length rule_id - String.length riot_prefix)
  else
    rule_id

let explain = fun rule_id ->
  let normalized = normalize_rule_id rule_id in
  all () |> List.find_opt
    (fun entry ->
      String.equal entry.Explanation.rule_id normalized)

let format = Explanation.format
