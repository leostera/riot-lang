open Std

let builtin_entries () =
  Pipeline.builtin_rules ()
  |> List.filter_map Rule.explanation

let package_entries () =
  Provider_registry.providers ()
  |> List.concat_map Provider.explanations

let all () =
  builtin_entries () @ package_entries ()

let explain code =
  all () |> List.find_opt (fun entry -> String.equal entry.Explanation.code code)

let format = Explanation.format
