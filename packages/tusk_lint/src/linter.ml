open Std

let run_rules rules trees =
  List.concat_map
    (fun (module R : Lint_rule.Rule) -> R.check trees)
    rules

let all_rules =
  [
    (* Temporarily disabled until Rules module is fixed
    (module Rules.Redundant_path_conversion : Lint_rule.Rule);
    (module Rules.Eta_reduction : Lint_rule.Rule);
    (module Rules.Module_type_naming : Lint_rule.Rule);
    *)
  ]
