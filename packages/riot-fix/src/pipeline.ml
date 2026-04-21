open Std

type t = {
  rules: Rule.t list;
}

type result = Fixme.Source_runner.result = {
  tree: Rule.green_tree;
  diagnostics: Diagnostic.t list;
  parse_diagnostics: Syn.Diagnostic.t list;
}

let make = fun ~rules () -> { rules }

type builtin_rule_factory = {
  category: string;
  id: string;
  make: unit -> Rule.t;
}

let builtin_rule_factories = fun () ->
  [
    {
      category = "Correctness";
      id = "limit-open-statements";
      make = Rules.Limit_open_statements.make
    };
    { category = "Correctness"; id = "no-open-bang"; make = Rules.No_open_bang.make };
    {
      category = "Correctness";
      id = "no-positional-bool-parameters";
      make = Rules.No_positional_bool_parameters.make
    };
    {
      category = "Correctness";
      id = "no-public-mutable-fields";
      make = Rules.No_public_mutable_fields.make
    };
    { category = "Correctness"; id = "no-redundant-reraise"; make = Rules.No_redundant_reraise.make };
    {
      category = "Correctness";
      id = "prefer-opaque-record-types";
      make = Rules.Prefer_opaque_record_types.make
    };
    {
      category = "Correctness";
      id = "require-module-interfaces";
      make = Rules.Require_module_interfaces.make
    };
    {
      category = "Correctness";
      id = "t-first-named-arguments";
      make = Rules.T_first_named_arguments.make
    };
    {
      category = "Readability";
      id = "alphabetized-named-arguments";
      make = Rules.Alphabetized_named_arguments.make
    };
    {
      category = "Readability";
      id = "avoid-single-letter-function-names";
      make = Rules.Avoid_single_letter_function_names.make
    };
    {
      category = "Readability";
      id = "avoid-single-letter-type-names";
      make = Rules.Avoid_single_letter_type_names.make
    };
    {
      category = "Readability";
      id = "class-case-constructors";
      make = Rules.Class_case_constructors.make
    };
    {
      category = "Readability";
      id = "class-case-module-names";
      make = Rules.Class_case_module_names.make
    };
    {
      category = "Readability";
      id = "descriptive-type-variables";
      make = Rules.Descriptive_type_variables.make
    };
    {
      category = "Readability";
      id = "limit-function-parameters";
      make = Rules.Limit_function_parameters.make
    };
    {
      category = "Readability";
      id = "limit-nested-match-depth";
      make = Rules.Limit_nested_match_depth.make
    };
    {
      category = "Readability";
      id = "limit-parenthesis-depth";
      make = Rules.Limit_parenthesis_depth.make
    };
    {
      category = "Readability";
      id = "no-boolean-comparisons-in-conditionals";
      make = Rules.No_boolean_comparisons_in_conditionals.make
    };
    { category = "Readability"; id = "no-custom-operators"; make = Rules.No_custom_operators.make };
    { category = "Readability"; id = "no-eta-expansion"; make = Rules.No_eta_expansion.make };
    {
      category = "Readability";
      id = "no-exn-suffix-functions";
      make = Rules.No_exn_suffix_functions.make
    };
    {
      category = "Readability";
      id = "no-function-shorthand";
      make = Rules.No_function_shorthand.make
    };
    {
      category = "Readability";
      id = "no-inline-parameter-type-annotations";
      make = Rules.No_inline_parameter_type_annotations.make
    };
    { category = "Readability"; id = "no-prime-variables"; make = Rules.No_prime_variables.make };
    {
      category = "Readability";
      id = "no-redundant-begin-end";
      make = Rules.No_redundant_begin_end.make
    };
    {
      category = "Readability";
      id = "no-redundant-else-unit";
      make = Rules.No_redundant_else_unit.make
    };
    {
      category = "Readability";
      id = "no-redundant-parentheses";
      make = Rules.No_redundant_parentheses.make
    };
    { category = "Readability"; id = "no-unnecessary-rec"; make = Rules.No_unnecessary_rec.make };
    {
      category = "Readability";
      id = "no-useless-let-return";
      make = Rules.No_useless_let_return.make
    };
    {
      category = "Readability";
      id = "ordered-argument-kinds";
      make = Rules.Ordered_argument_kinds.make
    };
    { category = "Readability"; id = "package-name-style"; make = Rules.Package_name_style.make };
    {
      category = "Readability";
      id = "prefer-if-over-bool-match";
      make = Rules.Prefer_if_over_bool_match.make
    };
    {
      category = "Readability";
      id = "prefer-multiline-string-literals";
      make = Rules.Prefer_multiline_string_literals.make
    };
    {
      category = "Readability";
      id = "prefer-named-closed-polyvariants";
      make = Rules.Prefer_named_closed_polyvariants.make
    };
    {
      category = "Readability";
      id = "prefer-pipelines-for-nested-calls";
      make = Rules.Prefer_pipelines_for_nested_calls.make
    };
    {
      category = "Readability";
      id = "prefer-record-destructuring-parameters";
      make = Rules.Prefer_record_destructuring_parameters.make
    };
    {
      category = "Readability";
      id = "prefer-records-over-large-tuples";
      make = Rules.Prefer_records_over_large_tuples.make
    };
    {
      category = "Readability";
      id = "prefer-scoped-field-access";
      make = Rules.Prefer_scoped_field_access.make
    };
    {
      category = "Readability";
      id = "prefer-t-for-single-type-modules";
      make = Rules.Prefer_t_for_single_type_modules.make
    };
    {
      category = "Readability";
      id = "prefer-sequences-over-let-unit";
      make = Rules.Prefer_sequences_over_let_unit.make
    };
    {
      category = "Readability";
      id = "snake-case-argument-names";
      make = Rules.Snake_case_argument_names.make
    };
    {
      category = "Readability";
      id = "snake-case-function-names";
      make = Rules.Snake_case_function_names.make
    };
    {
      category = "Readability";
      id = "snake-case-polyvariant-tags";
      make = Rules.Snake_case_polyvariant_tags.make
    };
    {
      category = "Readability";
      id = "snake-case-record-fields";
      make = Rules.Snake_case_record_fields.make
    };
    {
      category = "Readability";
      id = "snake-case-source-paths";
      make = Rules.Snake_case_source_paths.make
    };
    {
      category = "Readability";
      id = "snake-case-type-names";
      make = Rules.Snake_case_type_names.make
    };
    {
      category = "Readability";
      id = "snake-case-variable-names";
      make = Rules.Snake_case_variable_names.make
    };
  ]

let builtin_rule_category = fun rule_id ->
  let rule_id = Rule_id.local_id rule_id in
  builtin_rule_factories () |> List.find
    ~fn:(fun factory ->
      String.equal factory.id rule_id) |> Option.map ~fn:(fun factory -> factory.category)

let package_rules = fun () -> Provider_registry.rules ()

let unqualified_rule_id = Rule_id.local_id

let filtered_builtin_rules = fun package_rules ->
  let shadowed_ids = package_rules |> List.map ~fn:Rule.id |> List.map ~fn:unqualified_rule_id in
  builtin_rule_factories ()
  |> List.map ~fn:(fun factory -> factory.make ())
  |> List.filter
    ~fn:(fun rule -> not (List.contains shadowed_ids ~value:(unqualified_rule_id (Rule.id rule))))

let builtin_rules = fun () ->
  builtin_rule_factories () |> List.map ~fn:(fun factory -> factory.make ())

let run = fun pipeline ?filename ?on_progress source ->
  Fixme.Source_runner.run ~rules:pipeline.rules ?filename ?on_progress source

let default_rules = fun () ->
  let package_rules = package_rules () in
  filtered_builtin_rules package_rules @ package_rules

let default_rule_ids = fun () -> default_rules () |> List.map ~fn:Rule.id

let matching_rule_ids = fun rules requested_id ->
  let available_ids = List.map rules ~fn:Rule.id in
  if List.contains available_ids ~value:requested_id then
    [ requested_id ]
  else if Rule_id.has_package_name requested_id then
    []
  else
    let local_id = Rule_id.local_id requested_id in
    List.filter available_ids
      ~fn:(fun available_id ->
        String.equal (Rule_id.local_id available_id) local_id)

let rules_by_id = fun ids ->
  let available_rules = default_rules () in
  ids |> List.map
    ~fn:(fun id ->
      match matching_rule_ids available_rules id with
      | [] ->
          Log.warn ("Unknown riot-fix rule '" ^ Rule_id.to_string id ^ "', ignoring");
          []
      | matches -> matches) |> List.concat |> List.sort ~compare:Rule_id.compare |> List.unique
    ~compare:Rule_id.compare |> List.filter_map
    ~fn:(fun id ->
      List.find available_rules
        ~fn:(fun rule ->
          Rule_id.equal (Rule.id rule) id))

let default = fun () -> make ~rules:(default_rules ()) ()
