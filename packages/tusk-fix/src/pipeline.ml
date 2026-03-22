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
    ("avoid-single-letter-function-names", Rules.Avoid_single_letter_function_names.make);
    ("avoid-single-letter-type-names", Rules.Avoid_single_letter_type_names.make);
    ("limit-parenthesis-depth", Rules.Limit_parenthesis_depth.make);
    ("limit-nested-match-depth", Rules.Limit_nested_match_depth.make);
    ("limit-open-statements", Rules.Limit_open_statements.make);
    ("no-redundant-parentheses", Rules.No_redundant_parentheses.make);
    ("no-eta-expansion", Rules.No_eta_expansion.make);
    ("no-exn-suffix-functions", Rules.No_exn_suffix_functions.make);
    ("no-unnecessary-rec", Rules.No_unnecessary_rec.make);
    ("no-useless-let-return", Rules.No_useless_let_return.make);
    ("no-redundant-else-unit", Rules.No_redundant_else_unit.make);
    ("no-boolean-comparisons-in-conditionals", Rules.No_boolean_comparisons_in_conditionals.make);
    ("prefer-sequences-over-let-unit", Rules.Prefer_sequences_over_let_unit.make);
    ("prefer-if-over-bool-match", Rules.Prefer_if_over_bool_match.make);
    ("no-open-bang", Rules.No_open_bang.make);
    ("no-inline-parameter-type-annotations", Rules.No_inline_parameter_type_annotations.make);
    ("no-function-shorthand", Rules.No_function_shorthand.make);
    ("limit-function-parameters", Rules.Limit_function_parameters.make);
    ("prefer-multiline-string-literals", Rules.Prefer_multiline_string_literals.make);
    ("no-custom-operators", Rules.No_custom_operators.make);
    ("prefer-pipelines-for-nested-calls", Rules.Prefer_pipelines_for_nested_calls.make);
    ("snake-case-type-names", Rules.Snake_case_type_names.make);
    ("descriptive-type-variables", Rules.Descriptive_type_variables.make);
    ("snake-case-function-names", Rules.Snake_case_function_names.make);
    ("class-case-module-names", Rules.Class_case_module_names.make);
    ("snake-case-variable-names", Rules.Snake_case_variable_names.make);
    ("no-prime-variables", Rules.No_prime_variables.make);
    ("snake-case-argument-names", Rules.Snake_case_argument_names.make);
    ("ordered-argument-kinds", Rules.Ordered_argument_kinds.make);
    ("alphabetized-named-arguments", Rules.Alphabetized_named_arguments.make);
    ("t-first-named-arguments", Rules.T_first_named_arguments.make);
    ("snake-case-record-fields", Rules.Snake_case_record_fields.make);
    ("class-case-constructors", Rules.Class_case_constructors.make);
    ("snake-case-polyvariant-tags", Rules.Snake_case_polyvariant_tags.make);
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
  builtin_rule_factories ()
  |> List.map snd
  |> List.map (fun make_rule -> make_rule ())
  |> List.filter (fun rule ->
         not (List.mem (unqualified_rule_id (Rule.id rule)) shadowed_ids))

let builtin_rules () =
  builtin_rule_factories ()
  |> List.map snd
  |> List.map (fun make_rule -> make_rule ())

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
      let ctx = Rule.{ file_path; cst = parse_result.cst } in
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
