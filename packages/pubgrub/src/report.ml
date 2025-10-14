open Std

type package = string

type derivation_tree =
  | External of Incompatibility.external_cause * Term.t list
  | Derived of {
      terms : Term.t list;
      cause1 : derivation_tree;
      cause2 : derivation_tree;
      shared_id : int option;
    }

let rec build_derivation_tree incompat =
  match incompat with
  | Incompatibility.External { terms; cause } -> External (cause, terms)
  | Incompatibility.Derived { terms; cause1; cause2; shared_id } ->
      Derived
        {
          terms;
          cause1 = build_derivation_tree cause1;
          cause2 = build_derivation_tree cause2;
          shared_id;
        }

let version_to_string ver = Version.to_string ver

let format_term term =
  let pkg = Term.package term in
  let ranges = Term.ranges term in
  let is_positive = Term.is_positive term in

  if Ranges.is_empty ranges && is_positive then format "%s (no versions)" pkg
  else if ranges = Ranges.full && is_positive then format "%s (any version)" pkg
  else if is_positive then format "%s in range" pkg
  else format "not (%s in range)" pkg

let format_terms terms =
  match terms with
  | [] -> "no terms"
  | [ t ] -> format_term t
  | _ ->
      let formatted = List.map format_term terms in
      String.concat ", " formatted

let rec format_derivation_tree tree =
  match tree with
  | External (cause, terms) -> (
      match cause with
      | Incompatibility.NotRoot (pkg, ver) ->
          format "Root package %s@%s must be selected" pkg
            (version_to_string ver)
      | Incompatibility.NoVersions (pkg, ranges) ->
          format "No versions available for package %s" pkg
      | Incompatibility.FromDependency (pkg, ver, dep_pkg, dep_ranges) ->
          format "Because %s@%s depends on %s, " pkg (version_to_string ver)
            dep_pkg
      | Incompatibility.Custom (pkg, ranges, msg) ->
          format "Package %s: %s" pkg msg)
  | Derived { terms; cause1; cause2; shared_id } ->
      let c1_str = format_derivation_tree cause1 in
      let c2_str = format_derivation_tree cause2 in
      format "Because %s\nAnd because %s,\n  %s" c1_str c2_str
        (format_terms terms)

let explain_conflict incompat =
  let tree = build_derivation_tree incompat in
  let explanation = format_derivation_tree tree in
  format "Conflict:\n%s\n\nTherefore, version solving failed." explanation
