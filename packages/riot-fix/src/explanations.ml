open Std

let builtin_entries = fun () ->
  Pipeline.builtin_rules ()
  |> List.map ~fn:Rule.explanation

let package_entries = fun () ->
  Provider_registry.providers ()
  |> List.map ~fn:Provider.explanations
  |> List.concat

let all = fun () -> builtin_entries () @ package_entries ()

let normalize_rule_id = fun rule_id ->
  if String.equal (Rule_id.package_name ~default_package:"riot" rule_id) "riot" then
    Rule_id.from_string (Rule_id.local_id rule_id)
  else
    rule_id

let explain = fun rule_id ->
  let normalized = normalize_rule_id rule_id in
  all ()
  |> List.find ~fn:(fun entry -> Rule_id.equal Explanation.(entry.rule_id) normalized)

let format = Explanation.format
