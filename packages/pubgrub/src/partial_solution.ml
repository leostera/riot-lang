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

(* Get the effective constraint for a package, considering both decisions and derivations *)
let get_constraint solution pkg =
  match get_decision solution pkg with
  | Some ver -> `Decided ver
  | None ->
      (* Check if there's a derivation *)
      let rec find_derivation = function
        | [] -> `Undecided
        | Derivation (p, ranges, _, _) :: _ when p = pkg -> `Constrained ranges
        | _ :: rest -> find_derivation rest
      in
      find_derivation solution.assignments

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
  let satisfied_count = ref 0 in
  let undecided_pkg = ref None in
  let undecided_count = ref 0 in
  let contradicted_pkg = ref None in
  let contradicted_count = ref 0 in

  List.iter
    (fun term ->
      let pkg = Term.package term in
      let ranges = Term.ranges term in
      let is_positive = Term.is_positive term in

      match get_constraint solution pkg with
      | `Undecided ->
          incr undecided_count;
          undecided_pkg := Some pkg
      | `Decided ver ->
          let in_range =
            Ranges.contains ~compare_v:version_compare ranges ver
          in
          if (is_positive && in_range) || ((not is_positive) && not in_range)
          then incr satisfied_count
          else (
            incr contradicted_count;
            contradicted_pkg := Some pkg)
      | `Constrained constrained_ranges ->
          if is_positive then
            if Ranges.is_empty constrained_ranges then
              if Ranges.is_empty ranges then incr satisfied_count
              else (
                incr contradicted_count;
                contradicted_pkg := Some pkg)
            else if
              Ranges.is_disjoint ~compare_v:version_compare constrained_ranges
                ranges
            then (
              incr contradicted_count;
              contradicted_pkg := Some pkg)
            else if
              Ranges.subset_of ~compare_v:version_compare constrained_ranges
                ranges
            then incr satisfied_count
            else (
              incr undecided_count;
              undecided_pkg := Some pkg)
          else if
            Ranges.is_empty
              (Ranges.intersection ~compare_v:version_compare constrained_ranges
                 ranges)
          then incr satisfied_count
          else (
            incr undecided_count;
            undecided_pkg := Some pkg))
    terms;

  let total = List.length terms in
  if !satisfied_count = total then (
    Log.debug "Incompatibility SATISFIED (all %d terms true)" total;
    `Satisfied)
  else if !satisfied_count = total - 1 && !undecided_count = 1 then
    match !undecided_pkg with
    | Some pkg ->
        Log.debug "Incompatibility ALMOST SATISFIED (one undecided: %s)" pkg;
        `AlmostSatisfied pkg
    | None -> `Unknown
  else if !contradicted_count = total then
    match !contradicted_pkg with
    | Some pkg ->
        Log.debug "Incompatibility CONTRADICTED (all %d terms false, pkg=%s)"
          total pkg;
        `Contradicted pkg
    | None -> `Unknown
  else if !contradicted_count > 0 then
    match !contradicted_pkg with
    | Some pkg ->
        Log.debug "Incompatibility CONTRADICTED (%d/%d terms false, pkg=%s)"
          !contradicted_count total pkg;
        `Contradicted pkg
    | None -> `Unknown
  else (
    Log.debug
      "Incompatibility INCONCLUSIVE (%d satisfied, %d undecided, %d \
       contradicted)"
      !satisfied_count !undecided_count !contradicted_count;
    `Unknown)

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
        match get_assignment_level solution pkg with
        | Some level -> Some (pkg, level)
        | None -> None)
      terms
  in

  let sorted_satisfiers =
    List.sort (fun (_, l1) (_, l2) -> compare l2 l1) satisfiers
  in

  match sorted_satisfiers with
  | [] -> panic "No satisfiers found in satisfier_search"
  | [ (pkg, level) ] -> (pkg, `DifferentDecisionLevels (level - 1))
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
