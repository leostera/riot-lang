open Std

type package = string
type version = Version.t
type decision_level = int

type assignment =
  | Decision of package * version * decision_level
  | Derivation of
      package * version Ranges.t * Incompatibility.t * decision_level

type t = {
  assignments : assignment list;
  decisions : (package, version) Collections.HashMap.t;
  decision_level : decision_level;
}

let empty () =
  {
    assignments = [];
    decisions = Collections.HashMap.create ();
    decision_level = 0;
  }

let current_decision_level solution = solution.decision_level

let add_decision solution pkg ver =
  let new_level = solution.decision_level + 1 in
  ignore (Collections.HashMap.insert solution.decisions pkg ver);
  {
    solution with
    assignments = Decision (pkg, ver, new_level) :: solution.assignments;
    decision_level = new_level;
  }

let add_derivation solution pkg ranges incompat =
  {
    solution with
    assignments =
      Derivation (pkg, ranges, incompat, solution.decision_level)
      :: solution.assignments;
  }

let get_decision solution pkg = Collections.HashMap.get solution.decisions pkg
let extract_solution solution = Collections.HashMap.to_list solution.decisions

let backtrack solution target_level =
  let new_decisions = Collections.HashMap.create () in
  let rec filter_assignments acc = function
    | [] -> List.rev acc
    | Decision (pkg, ver, level) :: rest when level <= target_level ->
        ignore (Collections.HashMap.insert new_decisions pkg ver);
        filter_assignments (Decision (pkg, ver, level) :: acc) rest
    | Decision (_, _, level) :: rest when level > target_level ->
        filter_assignments acc rest
    | Derivation (_, _, _, level) :: rest when level <= target_level ->
        filter_assignments acc rest
    | Derivation (_, _, _, level) :: rest when level > target_level ->
        filter_assignments acc rest
    | _ -> filter_assignments acc []
  in
  let new_assignments = filter_assignments [] solution.assignments in
  {
    assignments = new_assignments;
    decisions = new_decisions;
    decision_level = target_level;
  }

let version_compare a b =
  match Version.compare a b with Lt -> -1 | Eq -> 0 | Gt -> 1

let relation solution incompat =
  let terms = Incompatibility.terms incompat in
  let all_satisfied = ref true in
  let almost_satisfied = ref None in
  let contradicted_pkg = ref None in
  let contradicted_count = ref 0 in

  List.iter
    (fun term ->
      let pkg = Term.package term in
      let ranges = Term.ranges term in
      let is_positive = Term.is_positive term in

      match get_decision solution pkg with
      | None ->
          if !almost_satisfied = None && !all_satisfied then
            almost_satisfied := Some pkg
          else all_satisfied := false
      | Some ver ->
          let in_range =
            Ranges.contains ~compare_v:version_compare ranges ver
          in
          if (is_positive && in_range) || ((not is_positive) && not in_range)
          then ()
          else (
            all_satisfied := false;
            contradicted_count := !contradicted_count + 1;
            contradicted_pkg := Some pkg))
    terms;

  if !all_satisfied then (
    match !almost_satisfied with
    | None ->
        Log.debug "Incompatibility SATISFIED (all terms true)";
        `Satisfied
    | Some pkg ->
        Log.debug "Incompatibility ALMOST SATISFIED (one undecided: %s)" pkg;
        `AlmostSatisfied pkg)
  else
    match !contradicted_pkg with
    | Some pkg ->
        Log.debug "Incompatibility CONTRADICTED (%d/%d terms false, pkg=%s)"
          !contradicted_count (List.length terms) pkg;
        `Contradicted pkg
    | None ->
        Log.debug "Incompatibility INCONCLUSIVE";
        `Unknown

let get_assignment_level solution pkg =
  let rec find_level = function
    | [] -> None
    | Decision (p, _, level) :: _ when p = pkg -> Some level
    | Derivation (p, _, _, level) :: _ when p = pkg -> Some level
    | _ :: rest -> find_level rest
  in
  find_level solution.assignments

let satisfier_search solution incompat =
  let terms = Incompatibility.terms incompat in

  let satisfiers =
    List.filter_map
      (fun term ->
        let pkg = Term.package term in
        match get_decision solution pkg with
        | Some _ -> (
            match get_assignment_level solution pkg with
            | Some level -> Some (pkg, level)
            | None -> None)
        | None -> None)
      terms
  in

  let sorted_satisfiers =
    List.sort (fun (_, l1) (_, l2) -> compare l2 l1) satisfiers
  in

  match sorted_satisfiers with
  | [] -> panic "No satisfiers found in satisfier_search"
  | [ (pkg, _) ] -> (pkg, `DifferentDecisionLevels 0)
  | (pkg1, level1) :: (pkg2, level2) :: _ ->
      if level1 = level2 then
        let rec find_cause pkg = function
          | [] -> panic (format "No cause found for package %s" pkg)
          | Decision (p, _, _) :: rest when p = pkg -> find_cause pkg rest
          | Derivation (p, _, cause, _) :: _ when p = pkg -> cause
          | _ :: rest -> find_cause pkg rest
        in
        let cause = find_cause pkg1 solution.assignments in
        (pkg1, `SameDecisionLevels cause)
      else (pkg1, `DifferentDecisionLevels level2)
