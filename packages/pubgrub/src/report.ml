open Std

type package = string

type derivation_tree =
  | External of Incompatibility.external_cause * Term.t list
  | Derived of {
      terms: Term.t list;
      cause1: derivation_tree;
      cause2: derivation_tree;
      shared_id: int option;
    }

let rec build_derivation_tree = fun incompat ->
  match incompat with
  | Incompatibility.External { terms; cause } -> External (cause, terms)
  | Incompatibility.Derived {
      terms;
      cause1;
      cause2;
      shared_id;
    } ->
      Derived {
        terms;
        cause1 = build_derivation_tree cause1;
        cause2 = build_derivation_tree cause2;
        shared_id;
      }

let version_to_string = fun ver -> Version.to_string ver

let version_compare = Version.compare

let format_ranges = fun ranges -> Ranges.to_string ~to_string_v:version_to_string ranges

let ensure_trailing_period = fun text ->
  if String.ends_with ~suffix:"." text then
    text
  else
    text ^ "."

let indent_text = fun spaces text ->
  let prefix = String.make ~len:spaces ~char:' ' in
  text
  |> String.split ~by:"\n"
  |> List.map
    ~fn:(fun line ->
      if String.equal line "" then
        line
      else
        prefix ^ line)
  |> String.concat "\n"

let format_term = fun term ->
  let pkg = Term.package term in
  let ranges = Term.ranges term in
  let is_positive = Term.is_positive term in
  if Ranges.is_empty ranges && is_positive then
    pkg ^ " (no versions)"
  else if Ranges.equal ~compare_v:version_compare ranges Ranges.full && is_positive then
    pkg ^ " (any version)"
  else if is_positive then
    pkg ^ " in " ^ format_ranges ranges
  else
    pkg ^ " not in " ^ format_ranges ranges

let format_terms = fun terms ->
  match terms with
  | [] -> "the constraints are unsatisfiable"
  | [ t ] -> format_term t
  | _ ->
      let formatted = List.map terms ~fn:format_term in
      String.concat ", " formatted

let format_external_cause = function
  | Incompatibility.NotRoot (pkg, ver) ->
      "root package " ^ pkg ^ "@" ^ version_to_string ver ^ " must be selected"
  | Incompatibility.NoVersions (pkg, ranges) ->
      "no versions of " ^ pkg ^ " match " ^ format_ranges ranges
  | Incompatibility.FromDependency (pkg, ver, dep_pkg, dep_ranges) ->
      pkg
      ^ "@"
      ^ version_to_string ver
      ^ " depends on "
      ^ dep_pkg
      ^ " in "
      ^ format_ranges dep_ranges
  | Incompatibility.Custom (pkg, ranges, msg) ->
      if Ranges.equal ~compare_v:version_compare ranges Ranges.full then
        "package " ^ pkg ^ ": " ^ msg
      else
        "package " ^ pkg ^ " in " ^ format_ranges ranges ^ ": " ^ msg

let rec format_derivation_tree = fun tree ->
  match tree with
  | External (cause, _terms) -> format_external_cause cause
  | Derived {
      terms;
      cause1;
      cause2;
      shared_id = _;
    } ->
      let c1_str =
        format_derivation_tree cause1
        |> ensure_trailing_period
        |> indent_text 2
      in
      let c2_str =
        format_derivation_tree cause2
        |> ensure_trailing_period
        |> indent_text 2
      in
      String.concat
        "\n"
        [ "Because:"; c1_str; "And because:"; c2_str; "So " ^ format_terms terms ^ "."; ]

let explain_conflict = fun incompat ->
  let tree = build_derivation_tree incompat in
  let explanation =
    format_derivation_tree tree
    |> ensure_trailing_period
  in
  "Conflict:\n" ^ explanation ^ "\n\nTherefore, version solving failed."
