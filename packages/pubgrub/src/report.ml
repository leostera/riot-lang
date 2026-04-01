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
  | Incompatibility.Derived { terms; cause1; cause2; shared_id } -> Derived {
    terms;
    cause1 = build_derivation_tree cause1;
    cause2 = build_derivation_tree cause2;
    shared_id;
  }

let version_to_string = fun ver -> Version.to_string ver

let format_term = fun term ->
  let pkg = Term.package term in
  let ranges = Term.ranges term in
  let is_positive = Term.is_positive term in
  if Ranges.is_empty ranges && is_positive then
    pkg ^ " (no versions)"
  else if ranges = Ranges.full && is_positive then
    pkg ^ " (any version)"
  else if is_positive then
    pkg ^ " in range"
  else
    "not (" ^ pkg ^ " in range)"

let format_terms = fun terms ->
  match terms with
  | [] ->
      "no terms"
  | [ t ] ->
      format_term t
  | _ ->
      let formatted = List.map format_term terms in
      String.concat ", " formatted

let rec format_derivation_tree = fun tree ->
  match tree with
  | External (cause, terms) -> (
      match cause with
      | Incompatibility.NotRoot (pkg, ver) -> "Root package " ^ pkg ^ "@" ^ version_to_string ver ^ " must be selected"
      | Incompatibility.NoVersions (pkg, ranges) -> "No versions available for package " ^ pkg
      | Incompatibility.FromDependency (pkg, ver, dep_pkg, dep_ranges) -> "Because "
      ^ pkg
      ^ "@"
      ^ version_to_string ver
      ^ " depends on "
      ^ dep_pkg
      ^ ", "
      | Incompatibility.Custom (pkg, ranges, msg) -> "Package " ^ pkg ^ ": " ^ msg
    )
  | Derived { terms; cause1; cause2; shared_id } ->
      let c1_str = format_derivation_tree cause1 in
      let c2_str = format_derivation_tree cause2 in
      "Because " ^ c1_str ^ "\nAnd because " ^ c2_str ^ ",\n  " ^ format_terms terms

let explain_conflict = fun incompat ->
  let tree = build_derivation_tree incompat in
  let explanation = format_derivation_tree tree in
  "Conflict:\n" ^ explanation ^ "\n\nTherefore, version solving failed."
