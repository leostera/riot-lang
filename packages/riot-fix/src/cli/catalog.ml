open Std

let split_rule_id = fun rule_id -> Rule_id.split ~default_package:"riot" rule_id

let compare_package_name = fun left right ->
  match left = "riot", right = "riot" with
  | (true, true)
  | (false, false) -> String.compare left right
  | true, false -> (-1)
  | false, true -> 1

let display_rule_id_text = fun rule_id ->
  let package_name, local_id = split_rule_id rule_id in
  package_name ^ ":" ^ local_id

let display_rule_id = fun rule -> display_rule_id_text (Rule.id rule)

let sorted_rules = fun () ->
  Pipeline.default_rules () |> List.sort
    ~compare:(fun left right ->
      let left_package, left_local = split_rule_id (Rule.id left) in
      let right_package, right_local = split_rule_id (Rule.id right) in
      let package_cmp = compare_package_name left_package right_package in
      if package_cmp != 0 then
        package_cmp
      else if String.equal left_package "riot" then
        let left_category = Pipeline.builtin_rule_category (Rule.id left)
        |> Option.unwrap_or ~default:"Other" in
        let right_category = Pipeline.builtin_rule_category (Rule.id right)
        |> Option.unwrap_or ~default:"Other" in
        let category_cmp = String.compare left_category right_category in
        if category_cmp != 0 then
          category_cmp
        else
          String.compare left_local right_local
      else
        String.compare left_local right_local)

let sorted_diagnostics = fun () ->
  Explanations.all () |> List.sort
    ~compare:(fun left right ->
      String.compare
        (display_rule_id_text Explanation.(left.rule_id))
        (display_rule_id_text Explanation.(right.rule_id)))

let rule_to_json = fun rule ->
  let open Data.Json in
    let package_name, local_id = split_rule_id (Rule.id rule) in
    Object [
      ("id", string (display_rule_id rule));
      ("local_id", string local_id);
      ("package", string package_name);
      (
        "category",
        (
          if String.equal package_name "riot" then
            match Pipeline.builtin_rule_category (Rule.id rule) with
            | Some category -> string category
            | None -> Null
          else
            Null
        )
      );
      ("description", string (Rule.description rule));
      ("enabled", bool (Rule.enabled rule));
    ]

let diagnostic_to_json = fun entry ->
  let open Data.Json in Object [
    ("rule_id", string (display_rule_id_text Explanation.(entry.rule_id)));
    ("message", string Explanation.(entry.message));
  ]

let list_rules_text = fun rules ->
  let bold text = "\027[1m" ^ text ^ "\027[0m" in
  let rec build_lines = fun current_package current_category acc ->
    function
    | [] -> List.reverse acc
    | rule :: rest ->
        let package_name, local_id = split_rule_id (Rule.id rule) in
        let category =
          if String.equal package_name "riot" then
            Pipeline.builtin_rule_category (Rule.id rule)
          else
            None
        in
        let rule_line =
          if String.equal package_name "riot" then
            "  " ^ bold (display_rule_id rule) ^ " - " ^ Rule.description rule
          else
            bold (display_rule_id rule) ^ " - " ^ Rule.description rule
        in
        let acc =
          match package_name, current_package, category, current_category with
          | "riot", Some "riot", Some category_name, Some current when not
            (String.equal category_name current) -> rule_line :: ("  " ^ category_name ^ ":") :: acc
          | "riot", Some "riot", _, _ -> rule_line :: acc
          | "riot", _, Some category_name, _ -> rule_line
          :: ("  " ^ category_name ^ ":")
          :: "riot:"
          :: ""
          :: acc
          | _, Some current, _, _ when not (String.equal current package_name) -> rule_line
          :: (package_name ^ ":")
          :: ""
          :: acc
          | _, Some _, _, _ -> rule_line :: acc
          | _, None, _, _ -> rule_line :: (package_name ^ ":") :: acc
        in
        build_lines (Some package_name) category acc rest
  in
  build_lines None None [] rules |> String.concat "\n"

let list_diagnostics_text = fun entries ->
  let bold text = "\027[1m" ^ text ^ "\027[0m" in
  entries
  |> List.map
    ~fn:(fun entry ->
      bold (display_rule_id_text Explanation.(entry.rule_id)) ^ " - " ^ Explanation.(entry.message))
  |> String.concat "\n"

let list_rules_output = fun ~format ->
  let rules = sorted_rules () in
  match format with
  | Reporter.Text -> list_rules_text rules
  | Reporter.Json -> Data.Json.Array (List.map rules ~fn:rule_to_json) |> Data.Json.to_string

let list_diagnostics_output = fun ~format ->
  let entries = sorted_diagnostics () in
  match format with
  | Reporter.Text -> list_diagnostics_text entries
  | Reporter.Json -> Data.Json.Array (List.map entries ~fn:diagnostic_to_json) |> Data.Json.to_string

let list_rules = fun format ->
  print (list_rules_output ~format);
  if format = Reporter.Text then
    print "\n";
  Ok ()

let list_diagnostics = fun format ->
  print (list_diagnostics_output ~format);
  if format = Reporter.Text then
    print "\n";
  Ok ()

let explain_rule = fun rule_id ->
  match Explanations.explain rule_id with
  | Some entry ->
      print (Explanations.format entry);
      Ok ()
  | None -> Error (Failure ("Unknown riot-fix rule id: " ^ Rule_id.to_string rule_id))
